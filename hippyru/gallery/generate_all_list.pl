#!/usr/bin/perl -w

use strict;
use FindBin;
use lib $FindBin::Bin;

use Generator;

my $top_dir = $ENV{'HOME'} . "/public_html";
Generator::generate_all_galleries_list($top_dir, "galleries.html");
