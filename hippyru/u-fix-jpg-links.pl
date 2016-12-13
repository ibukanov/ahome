use strict;
use warnings;
use File::Basename;
use File::Find;

sub readfile {
    my $file_name = shift;
    open FILE, $file_name or die $!; 
    binmode FILE; 
    my $size = -s FILE;
    my $data = "";
    while ($size) {
	my $tmp;
	my $n = read FILE, $tmp, $size;
	die "read error $!" unless defined $n;
	$data .= $tmp;
	$size -= $n;
    }
    close(FILE);
    return $data;
}

sub writefile {
    my $file_name = shift;
    my $data = shift;
    open FILE, ">", $file_name or die $!; 
    syswrite FILE, $data;
    close(FILE);
}

sub process_image_src {
    my $base_path = shift;
    my $src = shift;
    my $orig_src = $src;
    $src = "$1" if $src =~ m(^http://www.hippy.ru(/.*)$);
    $src = "$1" if $src =~ m(^http://hippy.ru(/.*)$);
    return $orig_src if $src =~ m(^https?:); 
    $src = "$1" . ".jpg" if $src =~ m(^(.*)\.JPG$);
    return $src;
}

my $top_dir = $ENV{'HOME'} . "/public_html";
my @htmls;

my $file_callback = sub {
    push @htmls, $1 if ($File::Find::name =~ m(^\./(.*\.html?$)));
};

chdir $top_dir or die "Cannot chnage to $top_dir: $!\n";
find({ wanted => $file_callback, no_chdir => 1 }, ".");
    
for my $name (@htmls) {
    next if ! -f $name;
    next if $name =~ m/^(\.\/)?(lubava\.info|freezia)\//s;
    my $data = readfile($name);
    my $old = $data;
    $data =~ s/ src="([^" ]+?)"/" src=\"" . process_image_src($name, $1) . "\""/sge;	
    $data =~ s/ src='([^' ]+?)'/" src='" . process_image_src($name, $1) . "'"/sge;
    if ($data ne $old) {
        print "changed: $name\n";
#        writefile($name . ".new", $data);
        writefile($name, $data);
#	last;
    }
}


