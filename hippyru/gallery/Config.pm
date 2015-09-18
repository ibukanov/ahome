package Gallery::Config;

use strict;

require Exporter;

our @ISA = qw(Exporter);

our @EXPORT= qw($PHOTO_DIR $PHOTO_URL $TEMPLATE_DIR $GALLERY $PASSWORD $PHOTO_MIN $PHOTO_MAX $PHOTODIR $MODE $ADMIN $BADCHAR $HTDOCS);

our $PHOTO_DIR="../../gal";
our $HTDOCS = "../..";
our $PHOTO_URL="/gal";
our $TEMPLATE_DIR="./tt2";
our $GALLERY="./data";

# hash of username . password
our $PASSWORD="5165c37e6d63075bd2137b1b9b302160";

our $PHOTO_MIN="-";
our $PHOTO_MAX=".*";
our $PHOTODIR = 20;

our $MODE=1;#if 0, you need use mod_rewrite
our $ADMIN="1";#if 0, admin interface not support
our $BADCHAR="[+ ]";




1;
