#!/usr/bin/python

from __future__ import print_function

import argparse
import os
import pwd
import re
import signal
import subprocess
import sys
import time

# proxy port as seen by VM
proxy_vm_port = 8100

# Directory for logs and proxy configs created at run-time.
# This is in memory so shuting down the VM wipes out it.
tinyproxy_root_dir = "/run/tinyproxy"

# Directory with VM proxy rules. Each rule file should be named
#   <vm-name>.<ip-address>
# where vm-name is symbolic name for the AppVM and ip-address is its address.
tinyproxy_config_dir = "/rw/config/tinyproxy"

tinyproxy_config_template_file = tinyproxy_config_dir+"/config"

# Pattern to match the rule files in tinyproxy_config_dir
rule_file_pattern = re.compile(r'^(.+)((?:\.[0-9]{1,3}){4})$')

# map of symbolic AppVM names to their IP addresses
vm_name_ip_map = None

# pattern to match AppVM lines with vif interfaces in "ip route" output
vm_route_pattern = re.compile(r'^([^ \t]+)[ \t]+dev[ \t]+(vif[0-9]+\.[0-9]+)')

# map of running AppVM IP addresses to their network interface names
vm_ip_interface_map = None

# Wait time in seconds to assume that the process is really terminated
# after sending the kill signal
termination_wait_time = 0.1

def error_and_exit(message):
    print(message, file=sys.stderr)
    os.exit(1)

def read_proxy_state():
    read_vm_names()
    read_vm_routes()

def read_vm_names():
    global vm_name_ip_map
    vm_name_ip_map = {}
    ip_map = {}
    last_ip_octet_set = {}
    for file_name in os.listdir(tinyproxy_config_dir):
        m = rule_file_pattern.match(file_name)
        if not m: continue
        vm_name = m.group(1)
        vm_ip = m.group(2)[1:]
        if vm_name in vm_name_ip_map:
            error_and_exit("Duplicatet VM name '{0}' in the directory with "
                           "proxy rules {1}".
                           format(vm_name, tinyproxy_config_dir))
        vm_name_ip_map[vm_name] = vm_ip;
        if vm_ip in ip_map:
            error_and_exit("Duplicatet the IP address {0} in the directory with proxy rules {1}".
                           format(vm_ip, tinyproxy_config_dir))
        ip_map[vm_ip] = vm_name
        last_ip_octet = get_last_ip_octet(vm_ip)
        if last_ip_octet in last_ip_octet_set:
            error_and_exit("Unsupported config - two AppVM have the same last octet {0} in their IP address".
                           format(last_ip_octet))
        last_ip_octet_set[last_ip_octet] = True;

def read_vm_routes():
    global vm_ip_interface_map
    route_text = subprocess.check_output(["ip", "route", "show"])
    vm_ip_interface_map = {}
    for route_line in route_text.splitlines():
        m = vm_route_pattern.match(route_line)
        if not m: continue
        vm_ip_interface_map[m.group(1)] = m.group(2)

def get_last_ip_octet(ipv4_string):
    return int(ipv4_string[ipv4_string.rfind(".")+1:])


class ProxyInfo: pass

def create_proxy_info(vm_name):
    if not len(vm_name):
        error_and_exit("Empty VM name")
    vm_ip = vm_name_ip_map[vm_name]
    if not vm_ip:
        error_and_exit("Unknown VM name - '{0}'".
                       format(self.vm_ip))

    info = ProxyInfo()
    info.vm_name = vm_name
    info.vm_ip = vm_ip
    info.port = proxy_vm_port + get_last_ip_octet(vm_ip)
    info.dir = tinyproxy_root_dir+"/"+vm_name
    info.pid_file = info.dir+"/pid"
    info.config_file = info.dir+"/config"
    info.log_file = info.dir+"/log"
    info.rules_file = tinyproxy_config_dir+"/"+vm_name+"."+vm_ip

    return info

# In Qubes all AppVM interfaces share the same ip so we need to get
# only once on the first call to the function
bind_ip = None

# Pattern to match "ip address show..." output
bind_address_pattern = re.compile(r'^[ \t]*inet[ \t]+([0-9.]+)')

def get_proxy_bind_ip(vm_info):
    global bind_ip
    if bind_ip: return bind_ip

    interface = vm_ip_interface_map[vm_info.vm_ip]
    assert interface

    address_text = subprocess.check_output(
        ["ip", "address", "show", "dev", interface])
    for line in address_text.splitlines():
        m = bind_address_pattern.match(line)
        if m:
            bind_ip = m.group(1)
            break

    return bind_ip

# List of (source, destination, port) tupels with openned tcp ports
# according to iptable output
iptables_opened_ports = None

iptabled_openned_port_pattern = re.compile(
    r'^ACCEPT[ \t]+tcp[ \t]+[^ \t]+[ \t]+([0-9.]+)[ \t]+([0-9.]+)[ \t]+tcp[ \t]+dpt:([0-9]*)')

def ensure_opened_firewall(vm_info):
    global iptables_opened_ports
    if not iptables_opened_ports:
        iptables_opened_ports = []
        filter_text = subprocess.check_output(
            ["iptables", "-t", "filter", "-n", "-L", "INPUT"])
        for line in filter_text.splitlines():
            m = iptabled_openned_port_pattern.match(line)
            if not m: continue
            entry = (m.group(1), m.group(2), int(m.group(3)))
            iptables_opened_ports.append(entry)

    bind_ip = get_proxy_bind_ip(vm_info)

    if (vm_info.vm_ip, bind_ip, vm_info.port) in iptables_opened_ports:
        return

    # Add missing rules
    subprocess.call([
            "iptables", "-t", "filter", "-I", "INPUT",
            "-s",  vm_info.vm_ip, "-d", bind_ip,
            "-p", "tcp", "--dport", str(vm_info.port),
            "-j", "ACCEPT"])

    # Add extra rule to redirect from the standard proxy port to
    # AppVM-specific port
    subprocess.call([
            "iptables", "-t", "nat", "-A", "PREROUTING",
            "-s",  vm_info.vm_ip, "-d", bind_ip,
            "-p", "tcp", "--dport", str(proxy_vm_port),
            "-j", "REDIRECT", "--to-ports", str(vm_info.port)])


def ensure_started_proxy(vm_info):
    ensure_opened_firewall(vm_info)

    # If pid exists, assume the proxy is running with the necessary config
    if os.path.isfile(vm_info.pid_file):
        return

    if not os.path.isdir(tinyproxy_root_dir):
        os.mkdir(tinyproxy_root_dir, 0755)

    if not os.path.isdir(vm_info.dir):
        os.mkdir(vm_info.dir, 0770)
        pwd_entry = pwd.getpwnam("tinyproxy")
        os.chown(vm_info.dir, pwd_entry.pw_uid, pwd_entry.pw_gid);

    with open(tinyproxy_config_template_file, "r") as f:
        config_template = f.read()

    config = config_template.format(
            port=vm_info.port,
            vm_ip=vm_info.vm_ip,
            bind_ip=get_proxy_bind_ip(vm_info),
            log_file=vm_info.log_file,
            pid_file=vm_info.pid_file,
            rules_file=vm_info.rules_file)

    with open(vm_info.config_file, "w") as f:
        f.write(config)

    if 0 != subprocess.call(["tinyproxy", "-c", vm_info.config_file]):
        print("Failed to run tinyproxy for {0}".format(vm_info.vm_name),
              file=sys.stderr)

def read_proxy_pid(vm_info):
    if os.path.isfile(vm_info.pid_file):
        try:
            with open(vm_info.pid_file, "r") as f:
                return int(f.read(20).strip())
        except OSError as ex: pass
    return 0

def ensure_stopped_proxy(vm_info):
    pid = read_proxy_pid(vm_info)
    if not pid:
        return False

    try:
        os.kill(pid, signal.SIGTERM)
    except OSError as ex:
        try: os.remove(vm_info.pid_file)
        except OSError: pass
    return True


def kill_all_proxies():
    should_wait_termination = False
    pid_files_to_remove = []
    for vm_name in vm_name_ip_map.iterkeys():
        info = create_proxy_info(vm_name)
        pid_files_to_remove.append(info.pid_file)
        if ensure_stopped_proxy(info): should_wait_termination = True

    if should_wait_termination: time.sleep(termination_wait_time)

    if 0 == subprocess.call(['killall', '-q', '-SIGKILL', 'tinyproxy']):
        time.sleep(termination_wait_time)

    for pid_file in pid_files_to_remove:
        try: os.remove(pid_file)
        except OSError: pass

def update_command(args):
    for vm_name in vm_name_ip_map.iterkeys():
        info = create_proxy_info(vm_name)
        if info.vm_ip in vm_ip_interface_map:
            ensure_started_proxy(info)
        else:
            ensure_stopped_proxy(info)

def start_command(args):
    for vm_name in args.vm_name_list:
        info = create_proxy_info(vm_name)
        if info.vm_ip in vm_ip_interface_map:
            ensure_started_proxy(info)
        else:
            print("Cannot start proxy for {0} - no IP route information is "
                  "available".format(vm_name), file=sys.stderr);

def stop_command(args):
    for vm_name in args.vm_name_list:
        info = create_proxy_info(vm_name)
        ensure_stopped_proxy(info)

def restart_command(args):
    should_wait_termination = False
    start_list = [];
    for vm_name in args.vm_name_list:
        info = create_proxy_info(vm_name)

        # Sleep when the proxy was running to wait for proxy to terminate
        terminated = ensure_stopped_proxy(info)
        if info.vm_ip in vm_ip_interface_map:
            if terminated: should_wait_termination = True
            start_list.append(info)

    if len(start_list):
        if should_wait_termination: time.sleep(termination_wait_time)
        for info in start_list:
            ensure_started_proxy(info)

def kill_all_command(args):
    kill_all_proxies()

def kill_all_and_restart_command(args):
    kill_all_proxies
    for vm_name in vm_name_ip_map.iterkeys():
        info = create_proxy_info(vm_name)
        if info.vm_ip in vm_ip_interface_map:
            ensure_started_proxy(info)

def show_command(args):
    output_cells = [('Name', 'IP', 'Interface', 'Proxy_Pid')]
    for vm_name in vm_name_ip_map.iterkeys():
        info = create_proxy_info(vm_name)
        interface = vm_ip_interface_map.get(info.vm_ip)
        pid = read_proxy_pid(info)
        output_cells.append((info.vm_name, info.vm_ip,
                             interface or '--', str(pid) if pid else '--'))

    last_column = len(output_cells[0]) - 1
    max_cell_widths = [0 for x in range(last_column)]
    for line in output_cells:
        for i in range(last_column):
            l = len(line[i])
            if l > max_cell_widths[i]: max_cell_widths[i] = l
    for j in range(len(output_cells)):
        line = output_cells[j]
        s = ""
        for i in range(last_column):
            s += line[i].ljust(max_cell_widths[i] + 1)
        print(s+line[last_column])

parser = argparse.ArgumentParser(description='Control HTTP proxy for Qubes App VMs.')
subparsers = parser.add_subparsers(title='subcommands', description='Pass -h after the subcommand name for its detailed help')

sub_parser = subparsers.add_parser('update', help='for each running AppVM ensure that its proxy runs. All other proxies are stopped.')
sub_parser.set_defaults(func=update_command)

sub_parser = subparsers.add_parser('start', help='start proxy for AppVM')
sub_parser.add_argument('vm_name_list', metavar='AppVM', nargs='+', help='ensure that proxy for AppVM is started. Does nothing if AppVM is not started')
sub_parser.set_defaults(func=start_command)

sub_parser = subparsers.add_parser('stop', help='stop proxy for AppVM')
sub_parser.add_argument('vm_name_list', metavar='AppVM', nargs='+', help='ensure that proxy for AppVM is stopped')
sub_parser.set_defaults(func=stop_command)

sub_parser = subparsers.add_parser('restart', help='restart proxy for AppVM')
sub_parser.add_argument('vm_name_list', metavar='AppVM', nargs='+', help='stop the given AppVM proxy and then start it again unless AppVM is running')
sub_parser.set_defaults(func=restart_command)

sub_parser = subparsers.add_parser('kill-all', help='forcefully kill all running proxy instances')
sub_parser.set_defaults(func=kill_all_command)

sub_parser = subparsers.add_parser('kill-all-and-restart', help='forcefully kill all running proxy instances and then restart proxies for running AppVms')
sub_parser.set_defaults(func=kill_all_and_restart_command)

sub_parser = subparsers.add_parser('show', help='show the status of VM with proxies')
sub_parser.set_defaults(func=show_command)

args = parser.parse_args()

read_proxy_state()

args.func(args)
