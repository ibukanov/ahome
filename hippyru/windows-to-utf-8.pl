#!/usr/bin/perl -w

use strict;

foreach my $f (@ARGV) {
    print("processing $f\n");
    system("iconv", "-f", "windows-1251", "-t", "utf-8", "-o", "/tmp/x.txt", $f) == 0
	or die "Failed to executed iconv for $f, the exit code $?";
    system("/bin/mv", "/tmp/x.txt", "$f") == 0
	or die "Failed to executed mv /tmp/x.txt $f, thee exit code $?";
}
