package Generator;

use strict;

use File::Find;
use File::Basename;
use Template;
use Fcntl;

my $GALLERY_TEST_FILE = "HIPPYRU_GALLERY";
my $IMAGES_PER_PAGE = 20;

my $template_config = {
    INCLUDE_PATH => $ENV{HOME} . "/include/hippyru_gallery",
    POST_CHOMP   => 1,               # cleanup whitespace
};

our $template = Template->new($template_config) || die $Template::ERROR, "\n";

sub readfile {
    my $file_name = shift;
    sysopen FILE, $file_name, O_RDONLY or return "";
    my $size = -s FILE;
    my $data = "";
    while ($size) {
        my $tmp;
        my $n = sysread FILE, $tmp, $size;
        error("read error $!") unless defined $n;
        $data .= $tmp;
	last if $n == 0;
        $size -= $n;
    }
    close(FILE);
    return $data;
}


sub get_image_html {
    my $index = shift;
    ++$index;
    return (sprintf "%03d", $index).".html";
}

sub generate {
    my $dir = shift;

    my $generated_path = $dir . "/" . "generated.txt";
    my %old_generated = ();
    if (open(my $in, $generated_path)) {
	while ( <$in> ) {
	    chomp;
	    $old_generated{$_} = 1;
	}
	close $in;
    }

    my $gallery_dir_property = $dir . "/" . $GALLERY_TEST_FILE;
    if (open(my $handle, "> $gallery_dir_property")) {
	syswrite $handle,  "hippy.ru gallery\n";
	close $handle;
    } else {
	die "Cannot open $gallery_dir_property for writing: $!";
    }

    opendir (DIR,$dir);
    my @images;
    my @images_no_extension;
    my $count=0;
    my $needrename=1;
    while (defined (my $file=readdir(DIR))) {
	next if $file !~ /^([a-zA-Z0-9_]+)\.(jpg|jpeg|gif|png|ping)$/i;
	push @images_no_extension, $1;
	push @images, $file;
	$count++;
    };
    @images = sort @images;

    my $dirtext = readfile("$dir/index.txt");
    my $backtext = readfile("$dir/_back.txt");
    my $lefttext = readfile("$dir/_lefttext.txt");
    my $title = readfile("$dir/_title.txt");

    my @generated_files = ();

    my $page_count = int((scalar @images + $IMAGES_PER_PAGE - 1) / $IMAGES_PER_PAGE);

    my $p = {};
    $p->{dir_title}   = $title || $backtext;
    $p->{left_text}   = $lefttext;
    $p->{dir_text}    = $dirtext;
    $p->{back_text}   = $backtext;
    $p->{admin}       = 0;
    $p->{image_count} = scalar @images;
    
    my @pages = ();
    for (my $page = 0; $page != $page_count; ++$page) {
	my $index = $page + 1;
	my $url = ($page == 0) ? "index.html" : "page$index.html";
	my $text="$page";
	push @pages, { 'url' => $url, 'index' => $index };
    }

    $p->{pages} = \@pages;

    for (my $page = 0; $page != $page_count; ++$page) {
	my $start_index = $page * $IMAGES_PER_PAGE;
	my $end_index = $start_index  + $IMAGES_PER_PAGE;
	$end_index = scalar @images if $end_index >  scalar @images;
	
	$p->{page} = $page + 1;
	my @page_images = ();
	for (my $i = $start_index; $i != $end_index; ++$i) {
	    my $thumb_source = "-" . $images[$i];
	    my $url = get_image_html($i);
	    push @page_images, { 'src' => $thumb_source, 'alt'=>'', 'url'=> $url };
	}

	$p->{images} = \@page_images;

	my $destination = $pages[$page]->{url};
	$template->process("dir.tt2", $p, $dir . "/" . $destination);
	push @generated_files, $destination;

	if ($page == 0) {
	    $destination = "page1.html";
	    $template->process("dir.tt2", $p, $dir . "/" . $destination);
	    push @generated_files, $destination;
	}

	for (my $i = $start_index; $i != $end_index; ++$i) {
	    $p->{image_index} = $i + 1;
	    $p->{prev_html} = ($i == 0) ? "" : get_image_html($i - 1);
	    $p->{next_html} = ($i + 1 == scalar @images) ? "" : get_image_html($i + 1);
	    $p->{image_src} = $images[$i];
	    $p->{image_text} = readfile($dir . "/" . $images[$i] . ".txt");

	    $destination = get_image_html($i);
	    $template->process("photo.tt2", $p, $dir . "/" . $destination);
	    push @generated_files, $destination;
	}
    }

    foreach my $name (@generated_files) {
	delete $old_generated{$name};
    }

    if (open(my $in, "> $generated_path")) {
	syswrite $in, ((join "\n", @generated_files) . "\n");
	close $in;
    }

    foreach my $name (keys %old_generated) {
	unlink($dir . "/" . $name);
    }

}

sub get_all_gallery_dirs {
    my $top_dir = shift;

    # Omit trailing /
    $top_dir = $1 if ($top_dir ne "/" && $top_dir =~ m(^(.*)/$));
    
    my @gallery_dirs;
    my $top_dir_prefix_length = length($top_dir) + 1;
    
    my $file_callback = sub {
	my $name = basename($File::Find::name);
	if ($name eq $GALLERY_TEST_FILE) {
	    my $dir = substr $File::Find::dir, $top_dir_prefix_length;
	    push @gallery_dirs, $dir; 	
	}
    };

    find({ wanted => $file_callback, no_chdir => 1 }, $top_dir);
    
    return @gallery_dirs;
}

sub generate_all_galleries_list {
    my $top_dir = shift;
    my $gallery_list_file = shift;

    my @dirs = get_all_gallery_dirs($top_dir);
    my @galleries;
    foreach my $dir (@dirs) {
	my $title = readfile("$dir/_title.txt") || 
	            readfile("$dir/_back.txt") || 
		    basename($dir);
	push @galleries, { url => "/$dir", text => $title };
    }
    @galleries = sort { $a->{text} cmp $b->{text}} @galleries;

    my $p = {  galleries => \@galleries };
    $template->process("all_galleries.tt2", $p, "$top_dir/$gallery_list_file");
}
