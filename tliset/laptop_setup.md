
# Installer

Install Fedora. Use net-installer image, not live image, to support
proper LUKS setup and systemd-boot.

When installer boots, edit kernel command line and add:
```
inst.sdboot
```
parameter, see [docs](https://anaconda-installer.readthedocs.io/en/latest/boot-options.html#inst-sdboot)

## Partitioning during install

Configure EFI partition with mount point: `/boot/efi`

Configure Root partition: BTRS + encrypted + (advanced options 4K LUKS sectors)


# System setup after on the first run

Go to Software app, enable 3rd-party repo and install flatpacks:

* Bitwarden
* Brave

Edit `/etc/fstab` to add the option `nofail` to EFI partition at
`/boot/efi`. Otherwise when reorganizing the partitions the system may
refuse to boot.

## Swap

Create the swap file on its own subvolume so snapshoting of root
continue to work.

```
btrfs subvolume create /swap

chmod 0700 /swap

btrfs filesystem mkswapfile --size "66G" --uuid clear /swap/swap-file
```

The size is memory+few extra GB so hibernation works reliably.

Adjust SELinux settings for the swap file:

```
semanage fcontext -a -t var_t /swap

restorecon -rv /swap

semanage fcontext -a -t swapfile_t /swap/swap-file

restorecon -rv /swap/swap-file
```

Edit `/etc/fstab` to add the line there:

```
echo '/swap/swap-file    none    swap    defaults    0   0' >> /etc/fstab
```

Activate the swap:

```
swapon
```

Disable zram enabled by Fedora installer:

```
ln -s /dev/null /etc/systemd/zram-generator.conf
systemctl daemon-reload
```

Check that only swap file is used:

```
swapon -s
```

## Hibernation

Tell the kernel where to find the hibernation file. systemd supports
figuring that automatically and store that in EFI variable, but then
one cannot have multiple systems that hibernate on the laptop.

Find UUID for the partition of the swapfile:

```
findmnt --noheadings --output UUID --target /swap/swap-file
```

Find the swap file offset:

```
btrfs inspect-internal map-swapfile --resume-offset /swap/swap-file
```

Edit `/etc/kernel/cmdline` to add there the following lines

```
resume=UUID=UUID_FROM_ABOVE
resume_offset=OFFSET_FROM_ABOVE
```

Run `kernel-install add-all` to update the kernel configs with the new
command line on the efi partition.

Change suspend-then-hibernate to hiberante after 15 min. For that
create a file `/etc/systemd/sleep.conf.d/local.conf`:

```
mkdir -p
/etc/systemd/sleep.conf.d

printf '[Sleep]
AllowHibernation=yes
HibernateDelaySec=15min
' > /etc/systemd/sleep.conf.d/local.conf
```

Change the power button setup to hibernate via altering `logind.conf`
settings:

```
mkdir -p /etc/systemd/logind.conf.d

printf '[Login]\nHandlePowerKey=hibernate\n' > /etc/systemd/logind.conf.d/local.conf
```

Configure Gnome to hibernate when the power button is pressed (run from
the user account, not root!):

```
gsettings set org.gnome.settings-daemon.plugins.power power-button-action 'hibernate'
```

## Performance

### Turbo Boost

CPU Turbo Boost badly affects performance benchmarking and makes the computer noisy. So disable it:

Create the service unit file:

```
printf '
[Install]
WantedBy=multi-user.target

[Service]
Type=oneshot
ExecStart=/bin/sh -c "printf 1 > /sys/devices/system/cpu/intel_pstate/no_turbo"
ExecStop=/bin/sh -c "printf 0 > /sys/devices/system/cpu/intel_pstate/no_turbo"
RemainAfterExit=yes
' > /etc/systemd/system/local-disable-turbo.service
```

Activate the unit:

```
systemctl daemon-reload

systemctl start local-disable-turbo

systemctl enable local-disable-turbo
```

Now go to Gnome settings and set the `Power Mode` in the Power settings to `Performance`.
