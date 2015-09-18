#!/usr/bin/perl -w

use strict;

#use CGI;
#use Data::Dumper;
#use Template;

use FindBin;
use lib $FindBin::Bin;

use Generator;

foreach my $arg (@ARGV) {
    Generator::generate($arg);
}
