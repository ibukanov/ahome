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
return "wall.html" if $text eq "wall.php";
return "/wall.html" if $text eq "/wall.php";
return "/wall.html" if $text eq "http://www.hippy.ru/wall.php";
return "/wall.html" if $text eq "http://hippy.ru/wall.php";
return "/vmeste/hippoviy_narodez.htm" if $text eq "vmeste/hippoviy%20narodez.htm";
return "hippoviy_narodez.htm" if $text eq "hippoviy%20narodez.htm";
return $text;
    return $text if $text =~ m/^#/s;
    if ($text =~ m|^http://(?:www\.)?hippy.ru/(.*)$|s) {
        $link = $1;
        return $text if length($link) == 0;
    } elsif ($text =~ m|^/(.*)$|s){
	$link = $1;
    } elsif ($text =~ m/^((http)|(https)|(mailto)|(javascript)):/s) {
        return $text;
    } else {
        $prefix = dirname($base_path) . "/";
        $link = $text;
    }
    $link =~ s/#.*$//s;
    $link =~ s/\?.*$//s;
    $link =~ s/(.)\/$/$1/s;
    
    return $text if (-f ($prefix . $link) || -d ($prefix . $link));

    my $dir = dirname($link);
    return $text if (is_photo_gallery($dir));

    print "$base_path: ";
    if ( -f ($prefix . $link . ".htm")) {
        print "Missing .htm in $text\n";
        my $i = index $text, $link;
        if ($i >= 0) {
            substr $text, $i, length($link), $link . ".htm";
            return $text;
        }
        print "Cannot find $link in $text\n";
    } elsif ( -f ($prefix . $link . ".html")) {
        print "Missing .html in $text\n";
        my $i = index $text, $link;
        if ($i >= 0) {
            substr $text, $i, length($link), $link . ".html";
            return $text;
        }
        print "Cannot find $link in $text\n";
    } elsif ($link =~ m/\.htm$/ && -f ($prefix . $link . "l")) { 
        print "should be .html, not .htm, in $text\n";
        my $i = index $text, $link;
        if ($i >= 0) {
            substr $text, $i, length($link), ($link . "l");
            return $text;
        }
        print "Cannot find $link in $text\n";
    } elsif ($link =~ m/\.htm$/ && -f ($prefix . (substr $link, 0, -4) . ".php")) { 
        print "should be .php, not .htm, in $text\n";
        my $i = index $text, $link;
        if ($i >= 0) {
            substr $text, $i + length($link) - 4, 4, ".php";
            return $text;
        }
        print "Cannot find $link in $text\n";
    } elsif ($link =~ m/\.html$/ && -f ($prefix . (substr $link, 0, -5) . ".php")) { 
        print "should be .php, not .html, in $text\n";
        my $i = index $text, $link;
        if ($i >= 0) {
            substr $text, $i + length($link) - 5, 5, ".php";
            return $text;
        }
        print "Cannot find $link in $text\n";
    } elsif ($link =~ m/\.html$/ && -f ($prefix . substr $link, 0, -1)) { 
        print "should be .htm, not .html, in $text\n";
        my $i = index $text, $link;
        if ($i >= 0) {
            substr $text, $i, length($link), (substr $link, 0, -1);
            return $text;
        }
        print "Cannot find $link in $text\n";
    } elsif ($link =~ m/page1\.html$/ && -f ($prefix . (substr $link, 0, -10) . "index.php")) { 
        print "should be index.php, not page1.html, in $text\n";
        my $i = index $text, $link;
        if ($i >= 0) {
            substr $text, $i + length($link) - 10, 10, "index.php";
            return $text;
        }
        print "Cannot find $link in $text\n";
    } elsif ($link =~ m/.htm$/s && -d ($prefix . (substr $link, 0, -4))) {
        print "should end with /, not with .htm, in $text";
    } elsif ($link =~ m/.html$/s && -d ($prefix . (substr $link, 0, -5))) {
        print "should end with /, not with .html, in $text";
    } else {
	print "unknown link $text";
    }
    print "\n";    
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
        print "changed: $name\n";
#        writefile($name . ".new", $data);
        writefile($name, $data);
    }
}


