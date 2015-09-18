#!/usr/bin/perl -w

use strict;
use FindBin;
use lib $FindBin::Bin;

use Generator;

my $top_dir = $ENV{'HOME'} . "/public_html";
my @dirs = Generator::get_all_gallery_dirs($top_dir);
foreach my $dir (@dirs) {
    print "Processing $dir\n";
    Generator::generate($top_dir . "/" . $dir);
}


