#!/usr/bin/perl -w

use strict;

my $phone="00:1B:98:6A:73:69";
my $phone_dir = "Photos";

open F, "obexftp --bluetooth $phone -c $phone_dir --list|";
my @list=();

while(<F>) {
    if (/\t<file name="([\w.]+)" modified="(\d\d\d\d)(\d\d)(\d\d)T(\d\d)(\d\d)(\d\d)Z"/) {
	push @list, $1, "$2-$3-$4 $5:$6:$7";
    }
}
close F;

for (my $i = 0; $i != @list; $i += 2) {
    my $name = $list[$i];
    my $date = $list[$i + 1];
    system "obexftp",  "--bluetooth", $phone, "-c", $phone_dir, "--get", $name;
    system "touch", "--date=$date", $name;
} 

