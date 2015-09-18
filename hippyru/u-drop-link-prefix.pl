use strict;
use warnings;
use File::Basename;

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

my %htaccess = ();

sub get_htaccess {
    my $dir = shift;
    my $data = $htaccess{$dir};
    return $data if (defined $data);
    my $file =  $dir . "/" . ".htaccess";
    $data = "";
    if (-f $file) {
        $data = readfile($file);
    }
    $htaccess{$dir} = $data;
    return $data;
}

my %photo_gallery_cache = ();

sub is_photo_gallery {
    my $dir = shift;
    my $value = $photo_gallery_cache{$dir};
    if (! defined $value) {
        if (get_htaccess($dir) =~ m(/cgi-bin/gallery/a.pl)s) {
            $value = 1;
        } else {
            $value = -1;
        }
        $photo_gallery_cache{$dir} = $value;
    }
    return $value == 1;
}

sub process_link {
    my $base_path = shift;
    my $text = shift;
    my $link;
    my $prefix = "";
    return $text if $text =~ m/^#/s;
    if ($text =~ m|^http://(?:www\.)?hippy.ru(/.+)$|s) {
	return $1;
    }
    return $text;
}

for my $name (@ARGV) {
    next if ! -f $name;
    next if $name =~ m/^(\.\/)?(lubava\.info|freezia)\//s;
    my $data = readfile($name);
    my $old = $data;
    $data =~ s/href="([^" ]+?)"/"href=\"" . process_link($name, $1) . "\""/sge;	
    $data =~ s/href='([^' ]+?)'/"href='" . process_link($name, $1) . "'"/sge;
    if ($data ne $old) {
        print "changed $name\n";
#        writefile($name . ".new", $data);
        writefile($name, $data);
    }
}


