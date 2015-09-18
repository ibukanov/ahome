
package Gallery::Main;

use strict;

require Exporter;

use Data::Dumper;
use Template;
use CGI qw':standard';


use lib '../';


use Gallery::Config;
use base 'Gallery::Config';

sub new{
	my ($class,$params)=@_;
	my $self={};
	bless $self,ref $class || $class;
	$self->init;
	$self;
}

sub init{
	my $self=shift;

my $config = {
INCLUDE_PATH => $TEMPLATE_DIR,  # or list ref
INTERPOLATE  => 1,               # expand "$var" in plain text
POST_CHOMP   => 1,               # cleanup whitespace
EVAL_PERL    => 1,               # evaluate Perl code blocks
COMPILE_EXT => '.ttc',
COMPILE_DIR => $TEMPLATE_DIR.'/ttc',
};

$self->{tt2}=Template->new($config) || die $Template::ERROR, "\n";
}

sub getParams{
	my $self=shift;
	my $data=<STDIN>;
	$self->log($data);
	my $a={};
	foreach( split /&/,$data){
		my ($key,$value) = (split /=/,$_,2);
		$value =~ s/\+/ /g;
		$value =~ s/%([0-9A-H]{2})/pack('C',hex($1))/ge;
#		$value =~ s/$BADCHAR//g;
		$a->{$key}=$value;
	}
	$data=$ENV{QUERY_STRING};
	foreach( split /&/,$data){
		my ($key,$value) = (split /=/,$_,2);
		$value =~ s/\+/ /g;
		$value =~ s/%([0-9A-H]{2})/pack('C',hex($1))/ge;
#		$value =~ s/$BADCHAR//g;
		$a->{$key}=$value;
	}
	#$data =~ s/\+/ /g;
	#$self->log($data);
	$self->log(Dumper $a);
	$a->{g}=~ s/\W//ge;
	unless ($a->{p} =~ /^(\w|\.)*$/){
		$a->{p}=undef;
	}
	#if ($a->{p} )
	#$a->{p}=~ s/\W//ge;
	$self->{params}=$a;
	$self->log("params:".Dumper $self->{params});
	#print "OK";

}

sub showDirs{
	my $self=shift;
	$self->log("showDirs");
	my $p={};
	foreach (sort @{$self->{gdir}}){
		
		push @{$p->{dirs}},{	'd'=>$_,
								'name'=>($self->{dname}->{$_}?$self->{dname}->{$_}:$_)};
	}

	#$p->{dirs}=$self->{gdir};
	$p->{uroot}=$self->{uroot};
	$self->log("hello".Dumper($p));
	$self->{tt2}->process("dirs.tt2",$p);
}

sub processAction{
	my $self=shift;
	if ($self->{params}->{action} eq 'dirtext'){
		open F,">$GALLERY/$self->{params}->{g}/index.txt";
		print F $self->{params}->{text};
		close F;
		open F,">$GALLERY/$self->{params}->{g}/_back.txt";
                print F $self->{params}->{back};
                close F;
		open F,">$GALLERY/$self->{params}->{g}/_lefttext.txt";
                print F $self->{params}->{lefttext};
                close F;
		open F,">$GALLERY/$self->{params}->{g}/_title.txt";
                print F $self->{params}->{title};
                close F;

	}elsif ($self->{params}->{action} eq 'phototext'){
		open F,">$GALLERY/$self->{params}->{g}/$self->{params}->{photoname}.txt";
		print F $self->{params}->{text};
		close F;
	}
}

sub process{
	my $self=shift;
	$self->log(Dumper(\%ENV));
	$self->getParams;
	$MODE=$self->{params}->{mode} if defined $self->{params}->{mode};
	$ADMIN=$ADMIN && (cookie('sid') eq $PASSWORD);
	if ($ADMIN){
		$self->processAction;
	}
	print "Content-type: text/html \n\n";
#	print "Content-type: text/plain \n\n";
#	print "ADMIN: $ADMIN\n";
#	print "(" . cookie('sid') . ")\n";
	$self->{uroot}=$ENV{REQUEST_URI};
	if ($MODE){
	if ($self->{uroot} =~ /\?/){
		$self->{uroot}.='&';
	}else{
		$self->{uroot}.='?';
	}}
	unless (defined $self->{params}->{g}){
			opendir (DIR,$PHOTO_DIR);
			if (open F,"$GALLERY/dirs.dat"){
			foreach (<F>){
			my ($d,$n) = split /=/,$_,2;
			$self->{dname}->{$d}=$n;
			};
			$self->log("in dirs.dat".Dumper($self->{dname}));
			close F;
			}
			#my $file;
			#$self->log('dirs:');
			while (defined (my $file=readdir(DIR))){
				next if $file eq '.' || $file eq '..' || ! -d "$PHOTO_DIR/$file";
				#$self->log($file);
				push @{$self->{gdir}},$file;
			};
			$self->log("dirs1: ".Dumper($self->{gdir}));
		$self->showDirs;
	}else{
		$self->readDir;
		if(! (defined $self->{params}->{p} && $self->{params}->{p} ne '' && $self->{params}->{p} !~ /^page/)){
			$self->showDir;
		}else{
			$self->findPrevNext;
			$self->showPhoto;
	}
	}
	return;
}

sub findPrevNext{
	my $self=shift;
	my ($prev,$now,$next);
	foreach (@{$self->{bt}}){
		$prev=$now;
		$now=$next;
		if ($MODE){
			$next=$_;
		}else{
			$next=$self->{tmode}->{$_};
		};
		$self->log("$prev;$now;$next;$self->{params}->{p}");
			if ($now eq $self->{params}->{p}){
				$self->{prev}=$prev;
				$self->{next}=$next;
				return;
			}
	}
	if ($next eq $self->{params}->{p}){
			$self->{prev}=$now;
	};
		 
}

sub readDir{
	my $self=shift;
	opendir (DIR,"$PHOTO_DIR/$self->{params}->{g}");
	my @files;
	my $count=0;
	my $needrename=1;
#		my ($nowPage) = $self->{params}->{p} =~ /page(.*?)\.html/;
		while (defined (my $file=readdir(DIR))){
				next if $file eq '.' || $file eq '..' || $file !~ /^$PHOTO_MIN$PHOTO_MAX/;
				#$self->log($file);
				$count++;
#				if ($ADMIN){
#				  if ($needrename && $file !~ /^$PHOTO_MIN\d{3}/)
#				  {
#					my $file1=$file;
#					$file1 =~ s/^$PHOTO_MIN//;
#					my $oldfile="$PHOTO_DIR/$self->{params}->{g}/$file";
#					my $oldfile1="$PHOTO_DIR/$self->{params}->{g}/$file1";
#					my $newfile="$PHOTO_DIR/$self->{params}->{g}/".$PHOTO_MIN."00$count";
#					my $newfile1="$PHOTO_DIR/$self->{params}->{g}/00$count";
#					`mv $oldfile $newfile`;
#					`mv $oldfile1 $newfile1`;
#				  } else{ $needrename = 0}; 
#				  if ($file =~ /$BADCHAR/){
#					my $file1=$file;
#					$file1 =~ s/^$PHOTO_MIN//;
#					my $newfile1 = $file1;
#					$newfile1 =~ s/$BADCHAR/_/g;
#					my $oldfile1="$PHOTO_DIR/$self->{params}->{g}/$file1";

#					$file = $PHOTO_MIN.$newfile1;
#					my $oldfile="$PHOTO_DIR/$self->{params}->{g}/$file";
#					my $newfile = $file;
#					$newfile =~ s/$BADCHAR/_/g;
#					$newfile = "$PHOTO_DIR/$self->{params}->{g}/$newfile";
#					`mv $oldfile $newfile`;
#					my $oldfile1="$PHOTO_DIR/$self->{params}->{g}/$file1";
#					$newfile1 = "$PHOTO_DIR/$self->{params}->{g}/$newfile1";
#					`mv $oldfile1 $newfile1`;
#					$file = $PHOTO_MIN.$newfile1;

#					$file =~ s/$BADCHAR/_/g;
#				  };
#				}
				push @files,$file;
				
			};
			$self->{pages}=int(($count-1)/$PHOTODIR) + 1;
			$self->{gt}=[sort @files];
			$self->{bt}=[map {s/^$PHOTO_MIN//;$_} sort @files];
			my $count=0;
			foreach (sort @files){
				++$count;
				$self->{modet}->{(sprintf "%03d",$count).".html"} = $self->{bt}->[$count-1];
				$self->{tmode}->{$self->{bt}->[$count-1]} = (sprintf "%03d",$count).".html";

			};
#			$self->{modet}={map {( => )} sort @files};
		mkdir "$GALLERY/$self->{params}->{g}" unless -e "$GALLERY/$self->{params}->{g}";
		open F,"$GALLERY/$self->{params}->{g}/index.txt";
		$self->{dirtext} =join '',<F>;
		open F,"$GALLERY/$self->{params}->{g}/_back.txt"; 
		$self->{backtext} =join '',<F>;
		open F,"$GALLERY/$self->{params}->{g}/_lefttext.txt"; 
		$self->{lefttext} =join '',<F>;
		open F,"$GALLERY/$self->{params}->{g}/_title.txt"; 
		$self->{title} = join '',<F>;
#		$self->{};
		#$self->log('modet'.Dumper $self->{modet});
		$self->log('gt'.Dumper $self->{gt});
		#$self->log('tmode'.Dumper $self->{tmode});
		
}

sub showDir{
	my $self=shift;
	$self->log("showDirs");
	my $p={};
	my $count=0;
		my ($nowPage) = $self->{params}->{p} =~ /page(.*?)\.html/;
		$nowPage=1 unless $nowPage;
	foreach (sort @{$self->{gt}}){
		$count++;
				next if $count > $PHOTODIR*$nowPage; 
				next if $count <= $PHOTODIR*($nowPage-1);
		/^$PHOTO_MIN(.*)$/;
		my $url;
		if ($MODE){
			$url="&p=$1";
		}else{
			$url=(sprintf "%03d",$count).".html";
		};
#		my $aa=`cat $GALLERY/$self->{params}->{g}/$1.txt`;
# my $aa1="cat $GALLERY/$self->{params}->{g}/$1.txt";
#$aa1=lc $aa1;
#my $aa=`$aa1`;
		push @{$p->{files}},{	'src'=>$_,
								'alt'=>'',#$aa,
								'url'=>$url};
	}
	#$p->{dirs}=$self->{gdir};
	$p->{uroot}=$self->{uroot};
	$p->{uroot}.="/" if $p->{uroot} !~ /(\/|&|html)$/;
	if ($MODE){
#		$p->{uroot}=~ s/(.*)&.*?$/$1/;
		$p->{uroot}=~ s/(.*)&.*?$/$1/;
	}else{
		$p->{uroot} =~ s/^(.*\/).*$/$1/;
	};

	$p->{froot}="$PHOTO_URL/$self->{params}->{g}/";
	$p->{text}=$self->{dirtext};
	$p->{back}=$self->{backtext};
	$p->{lefttext}=$self->{lefttext};
	$p->{title}=$self->{title};
	$p->{showtitle}=($self->{title} || $self->{backtext})." --- $nowPage";
	$p->{admin}=1 if $ADMIN;
	$p->{mode}=1 if !$MODE;
	$p->{nowPage}=$nowPage;
	my $page=1;
	while ($page <= $self->{pages}){
	  my $url;
	  if ($MODE){
		$url="&p=page$page.html";
	  }else{
		$url="page$page.html";
	  }
		my $text="$page";
		push @{$p->{pages}},{	'url' => $url,
	  							'text' => $text};	  
		$page++;
	};
	
	$self->log("hello".Dumper($p));
	$self->{tt2}->process("dir.tt2",$p);
}

sub showPhoto{
	my $self=shift;
	$self->log("showPhoto");
	my $p={};
	$p->{uroot}=$self->{uroot};
	if ($MODE){
		$p->{uroot}=~ s/(.*)&.*?$/$1/;
		$p->{uroot}=~ s/(.*)&.*?$/$1/;
	}else{
		$p->{uroot} =~ s/^(.*\/).*$/$1/;
	};
	$p->{urootname}=$self->{backtext};
	if ($MODE){
		$p->{photo}= $self->{params}->{p};
	}else{
		$p->{photo}= $self->{modet}->{$self->{params}->{p}};
		$p->{nowPage}="page".(int(($self->{params}->{p}-1)/$PHOTODIR)+1).".html";
	};
	
	unless (-e "$PHOTO_DIR/$self->{params}->{g}/$p->{photo}"){
	$p->{photo} =~ s/\.(.*)$//;
	my $ext=lc $1; 
	$p->{photo} .=".$ext";
	}
	$p->{showtitle}=$self->{title}." --- фото $self->{params}->{p}";
	
	$p->{froot}="$PHOTO_URL/$self->{params}->{g}/";
	open F,"$GALLERY/$self->{params}->{g}/$p->{photo}.txt"; 
#		$self->{title} = join '',<F>;

	$p->{text}= join '',<F>;
	if ($MODE){
	$p->{prev}="&p=".$self->{prev} if defined $self->{prev};
	$p->{next}="&p=".$self->{next} if defined $self->{next};
	}else{
	$p->{prev}=$self->{prev} if defined $self->{prev};
	$p->{next}=$self->{next} if defined $self->{next};
	};
	$p->{admin}=1 if $ADMIN;
	$p->{mode}=1 if !$MODE;
	$self->log("hello".Dumper($p));
	$self->{tt2}->process("photo.tt2",$p,\&myProcess);
}

sub myProcess{
my $o = shift;
print $o;
unless ($MODE){
open F,">".$HTDOCS.$ENV{REQUEST_URI};
print F $o;
close F;
}
}

sub log{
	my ($self,@data)=@_;
	open F,">>/tmp/kartinki.log";
	print F "[".(join '|',@data)."]\n";
	close F;
}
1;
