
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

my $do_write = ($ARGV[0] eq "--write") ? 1 : 0;
shift @ARGV if $do_write;

my $do_trim = ($ARGV[0] eq "--trim") ? 1 : 0;
shift @ARGV if $do_trim;

my $pattern = readfile($ARGV[0]);
shift @ARGV;

$pattern =~ s/\s*$//sg;

print "'$pattern'\n";

for my $name (@ARGV) {
    next if ! -f $name;
    my $data = readfile($name);
    my $start = index $data, $pattern;
    if ($start >= 0) {
	substr $data, $start, length($pattern), "";
	print "got: $name\n";
	writefile($name, $data) if $do_write;
    }
}
