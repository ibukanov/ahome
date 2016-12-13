#!/usr/bin/perl -w

use strict;
use IO::Handle;

print "$ARGV[0] $ARGV[1]\n";

sub copy_fd {
    my $input = shift;
    my $output = shift;
    my $buf = '';
    my $length = 1 << 16;
    for (;;) {
	my $nread = $input->sysread($buf, $length);
	die "failed to read: $!" unless defined $nread;
	last unless $nread;
	my $offset = 0;
	for (;;) {
	    my $nwritten = $output->syswrite($buf, $nread, $offset);
	    die "failed to write: $!" unless defined $nwritten;
	    $nread -= $nwritten;
	    $offset += $nwritten;
	    last unless $nread;
	}
    }
}

my $local_input = IO::Handle->new_from_fd(int($ENV{SAVED_FD_0}), "r");
my $local_output = IO::Handle->new_from_fd(int($ENV{SAVED_FD_1}), "w");
my $remote_input = IO::Handle->new_from_fd(fileno(STDIN), "r");
my $remote_output = IO::Handle->new_from_fd(fileno(STDOUT), "w");

$SIG{CHLD} = "IGNORE";

my $kidpid = fork;
die "can't fork: $!" unless defined $kidpid;

$SIG{PIPE} = sub { exit 0; };

if ($kidpid) {
    # parent proceess, copy from local to remote
    $local_output->close;
    $remote_input->close;
    copy_fd($local_input, $remote_output);
    kill($kidpid);
} else {
    # child proceess, copy from remote to local
    $local_input->close;
    $remote_output->close;
    copy_fd($remote_input, $local_output);
}
