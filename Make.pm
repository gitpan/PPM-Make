package PPM::Make;
use strict;
#use warnings;
use Cwd;
use Pod::Find qw(pod_find);
use File::Basename;
use File::Path;
use File::Find;
use File::Copy;
use Config;
use CPAN;
use XML::Parser;
use LWP::Simple qw(getstore is_success);
require File::Spec;
use constant WIN32 => $^O eq 'MSWin32';
use vars qw($VERSION);
$VERSION = '0.25';
my @path_ext = ();

my $has_myconfig = 0;
if ($ENV{HOME}) {
  eval 
    {require File::Spec->catfile($ENV{HOME}, '.cpan', 'CPAN', 'MyConfig.pm');};
  $has_myconfig = 1 unless $@;
}
unless ($has_myconfig) {
  eval {require CPAN::Config;};
}

die "CPAN.pm must be configured first" if $@;

sub new {
  my ($class, %opts) = @_;

  my %legal = 
    map {$_ => 1} qw(force ignore binary zip 
		     dist script exec os arch arch_sub);
  foreach (keys %opts) {
    next if $legal{$_};
    warn "Unknown option '$_' ... ignoring\n";
  }
  $opts{zip} = 1 if ($opts{binary} and $opts{binary} =~ /\.zip$/);

  my $self = {
	      opts => \%opts || {},
	      html => 'blib/html/site/lib',
	      ext => qr/(\.tar\.gz|\.tar\.Z|\.tgz|\.zip)$/,
	      protocol => qr!(http|ftp|file)://!,
	      cwd => '',
	      has => {},
	      args => {},
	      abstract => '',
	      author => '',
	     };
  path_ext() if WIN32;
  $self->{has} = what_have_you();
  bless $self, $class;
}

sub what_have_you {
  my %has;
  eval{require Archive::Tar; require Compress::Zlib};
  if ($@) {
    $has{tar} = 
      $Config{tar} || $CPAN::Config->{tar} || which('tar');
    $has{gzip} = 
      $Config{gzip} || $CPAN::Config->{gzip} || which('gzip');
  }
  else {
    $has{tar} = 'perl';
    $has{gzip} = 'perl';
  }

  eval{require Archive::Zip; };
  if ($@) {
    $has{zip} = 
      $Config{zip} || $CPAN::Config->{zip} || which('zip');
    $has{unzip} = 
      $Config{unzip} || $CPAN::Config->{unzip} || which('unzip');
  }
  else {
    require Archive::Zip; import Archive::Zip qw(:ERROR_CODES);
    $has{zip} = 'perl';
    $has{unzip} = 'perl';
  }

  $has{make} = 
    $Config{make} || $CPAN::Config->{make} || which('make');

  $has{perl} = 
    $Config{perlpath} || which('perl');

  for (qw(tar gzip make perl)) {
    die "Cannot find a '$_' program" unless $has{$_};
  }
  return \%has;
}

sub make_ppm {
  my $self = shift;
  die 'No software available to make a zip archive'
     if ($self->{opts}->{zip} and not $self->{has}->{zip});
  my $dist = $self->{opts}->{dist};
  my $protocol = $self->{protocol};
  my $ext = $self->{ext};
  if ($dist) {
    $dist = $self->fetch_dist($dist) 
      if ($dist =~ m!$protocol! or $dist !~ m!$ext!);
    print "Extracting files from $dist ....\n";
    my $name = $self->extract_dist($dist);
    chdir $name or die "Cannot chdir to $name: $!";
  }
  my $force = $self->{opts}->{force};
  $self->{cwd} = cwd;
  $self->check_script() if $self->{opts}->{script};
  $self->adjust_binary() if $self->{opts}->{arch_sub};
  $self->build_dist() 
    unless (-d 'blib' and -f 'Makefile' and not $force);
  my $ret = $self->parse_makepl();
  $self->parse_make() unless $ret;
  $self->abstract();
  $self->author();
  $self->make_html() unless (-d 'blib/html' and not $force);
  $dist = $self->make_dist();
  $self->fix_ppd($dist);
}

sub check_script {
  my $self = shift;
  my $script = $self->{opts}->{script};
  my ($name, $path, $suffix) = fileparse($script, '\..*');
  my $file = $name . $suffix;
  $self->{opts}->{script} = $file;
  return if (-e $file);
  copy($script, $file) or die "Copying $script to $self->{cwd} failed: $!";
}

sub fetch_dist {
  my ($self, $dist) = @_;
  my $ext = $self->{ext};
  my $protocol = $self->{protocol};
  unless ($dist =~ m!$ext!) {
    my $mod = $dist;
    $mod =~ s!-!::!g;
    my $module;
    die qq{Cannot get information for $mod}
      unless ($module = CPAN::Shell->expand('Module', $mod));
    die qq{Cannot get distribution name of $mod}
      unless ($dist = $module->cpan_file);
  }
  my $from = ($dist =~ m!$protocol!) ? $dist :
    'http://www.cpan.org/authors/id/' . $dist;
  (my $to = $dist) =~ s!.*/(.*)!$1!;
  print "Fetching $from ....\n";
  die "Failed to get $dist" 
    unless (is_success(getstore($from, $to)));
  return $to;
}

sub extract_dist {
  my ($self, $file) = @_;

  my $has = $self->{has};
  my ($tar, $gzip, $unzip) = @$has{qw(tar gzip unzip)};

  my ($name, $path, $suffix) = fileparse($file, $self->{ext});
 EXTRACT: {
    if ($suffix eq '.zip') {
      ($unzip eq 'perl') && do {
	my $arc = Archive::Zip->new();
	die "Read of $file failed" unless $arc->read($file) == AZ_OK();
	$arc->extractTree();
	last EXTRACT;
      };
      ($unzip) && do {
	my @args = ($unzip, $file);
	print "@args\n";
	system(@args) == 0 or die "@args failed: $?";
	last EXTRACT;
      };

    }
    else {
      ($tar eq 'perl') && do {
	my $arc = Archive::Tar->new($file, 1);
	$arc->extract($arc->list_files);
	last EXTRACT;
      };
      ($tar and $gzip) && do {
	my @args = ($gzip, '-dc', $file, '|', $tar, 'xvf', '-');
	print "@args\n";
	system(@args) == 0 or die "@args failed: $?";
	last EXTRACT;
      };
    }
    die "Cannot extract $file";
  }
  return $name;
}

sub adjust_binary {
  my $self = shift;
  my $binary = $self->{opts}->{binary};
  my $ext = $self->{ext};
  my $archname = $self->{opts}->{arch} || $Config{archname};
  if ($binary) {
    if ($binary =~ m!$ext!) {
      if ($binary =~ m!/!) {
	$binary =~ s!(.*?)([\w\-]+)$ext!$1$archname/$2$3!;
      }
      else {
	$binary = $archname . '/' . $binary;
      }
    }
    else {
      $binary =~ s!/$!!;
      $binary .= '/' . $archname . '/';	
    }
  }
  else {
    $binary = $archname . '/';
  }
  $self->{opts}->{binary} = $binary;
}

sub build_dist {
  my $self = shift;
  my $binary = $self->{opts}->{binary};
  my $script = $self->{opts}->{script};
  my $exec = $self->{opts}->{exec};

  my $has = $self->{has};
  my ($make, $perl) = @$has{qw(make perl)};

  my @args;
  @args = ($perl, 'Makefile.PL');
  my $makepl_arg = $CPAN::Config->{makepl_arg};
  if ($binary) {
    if ($makepl_arg =~ /BINARY_LOCATION/) {
      $makepl_arg =~ s!(.*BINARY_LOCATION=)\S+(.*)!$1$binary$2!;
    }
    else {
      $makepl_arg .= ' BINARY_LOCATION=' . $binary;
    }
  }
  if ($script) {
    if ($makepl_arg =~ /PPM_INSTALL_SCRIPT/) {
      $makepl_arg =~ s!(.*PPM_INSTALL_SCRIPT=)\S+(.*)!$1$script$2!;
    }
    else {
      $makepl_arg .= ' PPM_INSTALL_SCRIPT=' . $script;
    }
  }
  $exec = 'perl' if (not $exec and $script);
  
  if ($exec) {
    if ($makepl_arg =~ /PPM_INSTALL_EXEC/) {
      $makepl_arg =~ s!(.*PPM_INSTALL_EXEC=)\S+(.*)!$1$exec$2!;
    }
    else {
      $makepl_arg .= ' PPM_INSTALL_EXEC=' . $exec;
    }
  }
  if ($makepl_arg) {
    push @args, (split ' ', $makepl_arg);
  }
  print "@args\n";
  system(@args) == 0 or die qq{@args failed: $?};

  @args = ($make);
  push @args, $CPAN::Config->{make_arg} if $CPAN::Config->{make_arg};
  print "@args\n";
  system(@args) == 0 or die "@args failed: $?";
 
  @args = ($make, 'test');
  print "@args\n";
  unless (system(@args) == 0) {
    die "@args failed: $?" unless $self->{opts}->{ignore};
    warn "@args failed: $?";
  }
  return 1;
}

sub parse_makepl {
  my $self = shift;
  
  open(MAKE, 'Makefile.PL') or die "Couldn't open Makefile.PL: $!";
  my @lines = <MAKE>;
  close MAKE;

  my $content = join "\n", @lines;
  $content =~ s!\r!!g;
  $content =~ m!WriteMakefile(\s*\(.*?\bNAME\b.*?\))\s*;!s;
  my $makeargs;
  unless ($makeargs = $1) {
    warn "Couldn't extract WriteMakefile args";
    return;
  }

  my $c = new Safe();
  my %r = $c->reval($makeargs);
  if ($@) {
    warn "Eval of Makefile.PL failed: $@";
    return;
  }
  unless ($r{NAME}) {
    warn "Cannot determine NAME in Makefile.PL";
    return;
  }
  $self->{args} = \%r;
  return 1;
}

sub parse_make {
  my $self = shift;
  my $flag = 0;
  my @lines;
  open(MAKE, 'Makefile') or die "Couldn't open Makefile: $!";
  while (<MAKE>) {
    if (not $flag and /MakeMaker Parameters/) {
      $flag = 1;
      next;
    }
    next unless $flag;
    last if /MakeMaker post_initialize/;
    chomp;
    s/^#*\s+// or next;
    push @lines, $_;
  }
  close(MAKE);
  my $make = join ',', @lines;
  $make = '(' . $make . ')';
  my $c = new Safe();
  my %r = $c->reval($make);
  die "Eval of Makefile.PL failed: $@" if ($@);
  die 'Cannot determine NAME in Makefile' unless $r{NAME};
  for (qw(NAME DISTNAME ABSTRACT ABSTRACT_FROM AUTHOR VERSION_FROM)) {
    $self->{args}->{$_} = $r{$_} unless $self->{args}->{$_};
  }
  return 1;
}

sub abstract {
  my $self = shift;
  my $args = $self->{args};
  if ($args->{ABSTRACT} or $args->{ABSTRACT_FROM}) {
    $self->{abstract} = 'supplied';
  }
  else {
    if (my $abstract = $self->guess_abstract()) {
      warn "Setting ABSTRACT to '$abstract'\n";
      $self->{abstract} = $abstract;
    }
    else {
      warn "Please check ABSTRACT in the ppd file\n";
    }
  }
}

sub guess_abstract {
  my $self = shift;
  my $args = $self->{args};
  my $cwd = $self->{cwd};
  if (my $version_from = $args->{VERSION_FROM}) {
    print "Trying to get ABSTRACT from $version_from ...\n";
    my $result = parse_abstract($args->{NAME}, $version_from);
    return $result if $result;
  }
  my $hit;
  (my $guess = $args->{NAME}) =~ s!.*::(.*)!$1.pm!;
  finddepth(sub{$_ eq $guess && ($hit = $File::Find::name) 
		  && ($hit !~ m!blib/!)}, $cwd);
  return unless (-f $hit);
  print "Trying to get ABSTRACT from $hit ...\n";
  my $result = parse_abstract($args->{NAME}, $hit);
  return $result;
}

sub parse_abstract {
  my ($package, $file) = @_;
  my $basename = basename($file, qr/\.\w+$/);
  (my $stripped = $basename) =~ s!\.\w+$!!;
  my $result;
  my $inpod = 0;
  open(FILE, $file) or die "Couldn't open $file: $!";
  while (<FILE>) {
    $inpod = /^=(?!cut)/ ? 1 : /^=cut/ ? 0 : $inpod;
    next if !$inpod;
    chop;
    next unless /^\s*($package|$basename|$stripped)\s--*\s(.*)/;
    $result = $2;
    last;
  }
  return $result;
}

sub author {
  my $self = shift;
  my $args = $self->{args};
  if ($args->{AUTHOR}) {
    $self->{author} = 'supplied';
  }
  else {
    if (my $author = $self->guess_author()) {
      warn "Setting AUTHOR to '$author'\n";
      $self->{author} = $author;
    }
    else {
      warn "Please check AUTHOR in the ppd file\n";
    }
  }
}

sub guess_author {
  my $self = shift;
  my $args = $self->{args};
  print "Trying to get AUTHOR from CPAN.pm ...\n";
  my $module = CPAN::Shell->expand('Module', $args->{NAME});
  unless ($module) {
    if (my $version_from = $args->{VERSION_FROM}) {
      $version_from =~ s!^lib/!!;
      $version_from =~ s!\.pm$!!;
      $version_from =~ s!/!::!g;
      $module = CPAN::Shell->expand('Module', $version_from)
    }
  }
  return unless $module;
  return unless (my $userid = $module->cpan_userid);
  return unless (my $author = CPAN::Shell->expand('Author', $userid));
  my $auth_string = $author->fullname;
  my $email = $author->email;
  $auth_string .= ' &lt;' . $email . '&gt;' if $email;
  return $auth_string;
}

sub make_html {
  my $self = shift;
  my $html = $self->{html};
  my $args = $self->{args};
  my $cwd = $self->{cwd};
  unless (-d $html) {
    mkpath($html, 1, 0755) or die "Couldn't mkdir $html: $!";
  }
  my %pods = pod_find({verbose => 1}, $cwd);
  eval {require Pod::Tree};
  my $has_pod_tree = 1 unless $@;
  foreach (keys %pods) {
    (my $infile = File::Spec->abs2rel($_)) =~ s!^\w+:!!;
    $infile =~ s!\\!/!g;
    next if $infile =~ m!(blib/|Makefile.PL|/t/)!;
    (my $outfile = $infile) =~ s!(.*)\.\w+!$1.html!;
    my $dir = dirname($infile);
    my $fulldir;
    if ($dir =~ m!lib/!) {
      $dir =~ s!lib/!!;
      $outfile =~ s!lib/!!;
      ($fulldir = File::Spec->catdir($html, $dir)) =~ s!\\!/!g;
      ($outfile = File::Spec->catfile($html, $outfile)) =~ s!\\!/!g;
    }
    elsif ($dir eq 'lib') {
      $dir = '';
      $outfile =~ s!lib/!!;
      ($fulldir = File::Spec->catdir($html, $dir)) =~ s!\\!/!g;
      ($outfile = File::Spec->catfile($html, $outfile)) =~ s!\\!/!g;     
    }
    else {
      my $top = $args->{NAME};
      $top =~ s!^([^:]+).*!$1!;
      if ($dir =~ m!^$top! or ($dir eq '.' and $outfile =~ m!$top.html!)) {
	($fulldir = File::Spec->catdir($html, $dir)) =~ s!\\!/!g;
	($outfile = File::Spec->catfile($html, $outfile)) =~ s!\\!/!g;
      }
      else {
	($fulldir = File::Spec->catdir($html, $top, $dir)) =~ s!\\!/!g;
	($outfile = File::Spec->catfile($html, $top, $outfile)) =~ s!\\!/!g;
     }
    }
    
    unless (-d $fulldir) {
      mkpath($fulldir, 1, 0755) or die "Couldn't mkdir $fulldir: $!";
    }
    
    unless ($has_pod_tree) {
      $infile = "--infile=$infile ";
      $outfile = "--outfile=$outfile ";
    }
    my $title = qq{--title="$pods{$_}"} if $pods{$_};
    my @args = ('pod2html', $infile, $outfile);
    unless ($has_pod_tree) {
      push @args, $title if $title;
    }
    
    print "@args\n";
    system(@args) == 0 or warn "@args failed: $?";
  }
}

sub make_dist {
  my $self = shift;
  my $args = $self->{args};
  my $has = $self->{has};
  my ($tar, $gzip, $zip) = @$has{qw(tar gzip zip)};
  my $ext = $self->{ext};
  my $force_zip = $self->{opts}->{zip};
  my $binary = $self->{opts}->{binary};
  my $name;
  if ($binary and $binary =~ /$ext/) {
    ($name = $binary) =~ s!.*/(.*)$ext!$1!;
  }
  else {
    $name = $args->{DISTNAME} ? $args->{DISTNAME} :
      $args->{NAME};
    $name  =~ s!::!-!g;
  }

  my $script = $self->{opts}->{script};

  my $arc = $force_zip ? ($name . '.zip') : ($name . '.tar.gz');
  unless ($self->{opts}->{force}) {
    return $arc if (-f $arc);
  }
  unlink $arc if (-e $arc);

 DIST: {
    ($tar eq 'perl' and not $force_zip) && do {
      $name .= '.tar.gz';
      my @f;
      my $arc = Archive::Tar->new();
      finddepth(sub {push @f, $File::Find::name; 
		     print $File::Find::name,"\n"}, 'blib');
      push @f, $script if $script;
      $arc->add_files(@f);
      $arc->write($name, 1);
      last DIST;
    };
    ($tar and $gzip and not $force_zip) && do {
      $name .= '.tar';
      my @args = ($tar, 'cvf', $name, 'blib');
      push @args, $script if $script;
      print "@args\n";
      system(@args) == 0 or die "@args failed: $?";
      @args = ($gzip, $name);
      print "@args\n";
      system(@args) == 0 or die "@args failed: $?";
      $name .= '.gz';
      last DIST;
    };
    ($zip eq 'perl') && do {
      $name .= '.zip';
      my $arc = Archive::Zip->new();
      $arc->addTree('blib', 'blib', sub{print "$_\n";});
      $arc->addTree($script, $script) if $script;
      die "Writing to $name failed" 
	unless $arc->writeToFileNamed($name) == AZ_OK();
      last DIST;
    };
    ($zip) && do {
      $name .= '.zip';
      my @args = ($zip, '-r', $name, 'blib');
      push @args, $script if $script;
       system(@args) == 0 or die "@args failed: $?";
      last DIST;
    };
    die "Cannot make archive for $name";
  }
  return $name;
}

sub fix_ppd {
  my ($self, $dist) = @_;
  my $make = $self->{has}->{make};

  my @args = ($make, 'ppd');
  print "\n@args\n";
  system(@args) == 0 or die "@args failed: $?";

  my $args = $self->{args};
  my $abstract = $self->{abstract};
  my $author = $self->{author};
  my $binary = $self->{opts}->{binary};
  my $ext = $self->{ext};
  my $edit_binary = ($binary and $binary =~ m!\.(tar\.gz|zip)$!) ? 0 : 1;
  my $os = $self->{opts}->{os};
  my $arch = $self->{opts}->{arch};


  my $name = $args->{DISTNAME} ? $args->{DISTNAME} : $args->{NAME};
  $name  =~ s!::!-!g;
  my $ppd = $name . '.ppd';  
  my $copy = $ppd . '.in';
  rename($ppd, $copy) or die "Couldn't rename $ppd to $copy: $!";

  my $d = parse_ppd($copy);

  $d->{OS}->{NAME} = $os if $os;
  $d->{ARCHITECTURE}->{NAME} = $arch if $arch;
  $d->{ABSTRACT} = $abstract 
    if ($abstract and $abstract ne 'supplied');
  $d->{AUTHOR} = $author
    if ($author and $author ne 'supplied');
  if ($binary) {
    unless ($binary =~ /$ext/) {
      $binary .= ($binary =~ m!/$!) ? $dist : '/' . $dist;
    }
  }
  $d->{CODEBASE}->{HREF} = $binary ? $binary : $dist;
  
  my $script = $self->{opts}->{script};
  if ($script) {
    my $exec = $self->{opts}->{exec};
    $exec ||= 'perl';
    $d->{INSTALL}->{SCRIPT} = $script;
    $d->{INSTALL}->{EXEC} = $exec;
  }

  print_ppd($d, $ppd);
  unlink $copy or warn "Couldn't unlink $copy: $!";
}

sub parse_ppd {
  my $file = shift;
  die "$file not found" unless (-e $file);
  my $p = XML::Parser->new(Style => 'Subs',
			   Handlers => {Char => \&char,
					Start => \&start,
					End => \&end,
					Init => \&init,
					Final => \&final,
				       },
			  );
  my $d = $p->parsefile($file);
  return $d;
}

sub print_ppd {
  my ($d, $fn) = @_;
  my $fh = new IO::File ">$fn" or die "Couldn't write to $fn: $!";
  print $fh <<"END";
<SOFTPKG NAME=\"$d->{SOFTPKG}->{NAME}\" VERSION=\"$d->{SOFTPKG}->{VERSION}\">
\t<TITLE>$d->{TITLE}</TITLE>
\t<ABSTRACT>$d->{ABSTRACT}</ABSTRACT>
\t<AUTHOR>$d->{AUTHOR}</AUTHOR>
\t<IMPLEMENTATION>
END

  if (scalar @{$d->{DEPENDENCY}} > 0) {
    foreach (@{$d->{DEPENDENCY}}) {
      print $fh qq{\t\t<DEPENDENCY NAME="$_->{NAME}" VERSION="$_->{VERSION}" />\n};
    }
    
  }
  foreach (qw(OS ARCHITECTURE)) {
    print $fh qq{\t\t<$_ NAME="$d->{$_}->{NAME}" />\n}
  }
  
  if ($d->{INSTALL}->{SCRIPT}) {
    print $fh qq{\t\t<INSTALL EXEC="$d->{INSTALL}->{EXEC}">$d->{INSTALL}->{SCRIPT}</INSTALL>\n};
  }
  print $fh qq{\t\t<CODEBASE HREF="$d->{CODEBASE}->{HREF}" />\n};
  
  print $fh qq{\t</IMPLEMENTATION>\n</SOFTPKG>\n};
  $fh->close;

}

sub init {
  my $self = shift;
  $self->{_mydata} = {
		      SOFTPKG => {NAME => '', VERSION => ''},
		      TITLE => '',
		      AUTHOR => '',
		      ABSTRACT => '',
		      OS => {NAME => ''},
		      ARCHITECTURE => {NAME => ''},
		      CODEBASE => {HREF => ''},
		      DEPENDENCY => [],
		      INSTALL => {EXEC => '', SCRIPT => ''},
		      wanted => {TITLE => 1, ABSTRACT => 1, AUTHOR => 1},
		      _current => '',
		     };
}

sub start {
  my ($self, $tag, %attrs) = @_;
  my $internal = $self->{_mydata};
  $internal->{_current} = $tag;
  
 SWITCH: {
    ($tag eq 'SOFTPKG') and do {
      $internal->{SOFTPKG}->{NAME} = $attrs{NAME};
      $internal->{SOFTPKG}->{VERSION} = $attrs{VERSION};
      last SWITCH;
    };
    ($tag eq 'CODEBASE') and do {
      $internal->{CODEBASE}->{HREF} = $attrs{HREF};
      last SWITCH;
    };
    ($tag eq 'OS') and do {
	$internal->{OS}->{NAME} = $attrs{NAME};
	last SWITCH;
      };
    ($tag eq 'ARCHITECTURE') and do {
      $internal->{ARCHITECTURE}->{NAME} = $attrs{NAME};
      last SWITCH;
    };
    ($tag eq 'INSTALL') and do {
     $internal->{INSTALL}->{EXEC} = $attrs{EXEC};
      last SWITCH;
   };
    ($tag eq 'DEPENDENCY') and do {
      push @{$internal->{DEPENDENCY}}, 
	{NAME => $attrs{NAME}, VERSION => $attrs{VERSION}};
      last SWITCH;
    };
    
  }
}

sub char {
  my ($self, $string) = @_;
  
  my $internal = $self->{_mydata};
  my $tag = $internal->{_current};
  
  if ($tag and $internal->{wanted}->{$tag}) {
    $internal->{$tag} .= escape($string);
  }
  elsif ($tag and $tag eq 'INSTALL') {
    $internal->{INSTALL}->{SCRIPT} .= $string;
  }
  else {
    
  }
}

sub end {
  my ($self, $tag) = @_;
  delete $self->{_mydata}->{_current};
}

sub final {
  my $self = shift;
  return $self->{_mydata};
}

sub escape {
  local $_ = shift;
  s/</&lt;/g;
  s/>/&gt;/g;
  return $_;
}

sub path_ext {
  if ($ENV{PATHEXT}) {
    push @path_ext, split ';', $ENV{PATHEXT};
    for my $ext (@path_ext) {
      $ext =~ s/^\.*(.+)$/$1/;
    }
  }
  else {
    #Win9X: doesn't have PATHEXT
    push @path_ext, qw(com exe bat);
  }
}

sub which {
  my $program = shift;
  return undef unless $program;
  my @results = ();
  for my $base (map { File::Spec->catfile($_, $program) } File::Spec->path()) {
    if ($ENV{HOME} and not WIN32) {
      # only works on Unix, but that's normal:
      # on Win32 the shell doesn't have special treatment of '~'
      $base =~ s/~/$ENV{HOME}/o;
    }
    return $base if -x $base;
    
    if (WIN32) {
      for my $ext (@path_ext) {
	return "$base.$ext" if -x "$base.$ext";
      }
    }
  }
}

1;

=head1 NAME

PPM::Make - Make a ppm package from a CPAN distribution

=head1 SYNOPSIS

  my $ppm = PPM::Make->new( [options] );
  $ppm->make_ppm();

=head1 DESCRIPTION

See the supplied C<make_ppm> script for a command-line interface.

This module automates somewhat some of the steps needed to make
a I<ppm> (Perl Package Manager) package from a CPAN distribution.
It attempts to fill in the I<ABSTRACT> and I<AUTHOR> attributes of 
F<Makefile.PL>, if these are not supplied, and also uses C<pod2html> 
to generate a set of html documentation. It also adjusts I<CODEBASE> 
of I<package.ppd> to reflect the generated I<package.tar.gz> 
or I<package.zip> archive. Such packages are suitable both for 
local installation via

  C:\.cpan\build\package_src> ppm install

and for distribution via a repository.

=head2 OPTIONS

The available options accepted by the I<new> constructor are

=over

=item dist => value

If I<dist> is not specified, it will be assumed that one
is working inside an already unpacked source directory,
and the ppm distribution will be built from there. A value 
for I<dist> will be interpreted either as a CPAN-like source
distribution to fetch and build, or as a module name,
in which case I<CPAN.pm> will be used to infer the
corresponding distribution to grab.

=item binary => value

The value of I<binary> is used in the I<BINARY_LOCATION>
attribute passed to C<perl Makefile.PL>, and arises in
setting the I<HREF> attribute of the I<CODEBASE> field
in the ppd file.

=item arch_sub => boolean

Setting this option will insert the value of C<$Config{archname}>
(or the value of the I<arch> option, if given)
as a relative subdirectory in the I<HREF> attribute of the 
I<CODEBASE> field in the ppd file.

=item script => value

The value of I<script> is used in the I<PPM_INSTALL_SCRIPT>
attribute passed to C<perl Makefile.PL>, and arises in
setting the value of the I<INSTALL> field in the ppd file.

=item exec => value

The value of I<exec> is used in the I<PPM_INSTALL_EXEC>
attribute passed to C<perl Makefile.PL>, and arises in
setting the I<EXEC> attribute of the I<INSTALL> field
in the ppd file. This defaults to C<perl> when a value
of I<script> is specified.

=item zip => boolean

By default, a I<.tar.gz> distribution will be built, if possible. 
Giving I<zip> a true value forces a I<.zip> distribution to be made.

=item force => boolean

If a F<blib/> directory is detected, it will be assumed that
the distribution has already been made. Setting I<force> to
be a true value forces remaking the distribution.

=item ignore => boolean

If when building and testing a distribution, failure of any
supplied tests will be treated as a fatal error. Setting
I<ignore> to a true value causes failed tests to just
issue a warning.

=item os => value

If specified, this value will be used instead of the default
for the I<NAME> attribute of the I<OS> field of the ppd file.

=item arch => value

If specified, this value will be used instead of the default
for the I<NAME> attribute of the I<ARCHITECTURE> field of the ppd file.

=back

=head2 STEPS

The steps to make the PPM distribution are as follows. 

=over

=item determine available programs

For building and making the distribution, certain
programs will be needed. For unpacking and making 
I<.tar.gz> files, either I<Archive::Tar> and I<Compress::Zlib>
must be installed, or a C<tar> and C<gzip> program must
be available. For unpacking and making I<.zip> archives,
either I<Archive::Zip> must be present, or a C<zip> and
C<unzip> program must be available. Finally, a C<make>
program must be present.

=item fetch and unpack the distribution

If I<dist> is specified, the corresponding file is
fetched (by I<LWP::Simple>, if a I<URL> is specified).
If I<dist> appears to be a module name, the associated
distribution is determined by I<CPAN.pm>. The distribution
is then unpacked.

=item build the distribution

If needed, or if specied by the I<force> option, the
distribution is built by the usual

  C:\.cpan\build\package_src> perl Makefile.PL
  C:\.cpan\build\package_src> nmake
  C:\.cpan\build\package_src> nmake test

procedure. A failure in any of the tests will be considered
fatal unless the I<ignore> option is used. Additional
arguments to these commands present in either I<CPAN::Config>
or present in the I<binary> option to specify I<BINARY_LOCATION>
in F<Makefile.PL> will be added.

=item parse Makefile.PL

Some information contained in the I<WriteMakefile> attributes
of F<Makefile.PL> is then extracted.

=item parse Makefile

If certain information in F<Makefile.PL> can't be extracted,
F<Makefile> is tried.

=item determining the ABSTRACT

If an I<ABSTRACT> or I<ABSTRACT_FROM> attribute in F<Makefile.PL> 
is not given, an attempt is made to extract an abstract from the 
pod documentation of likely files.

=item determining the AUTHOR

If an I<AUTHOR> attribute in F<Makefile.PL> is not given,
an attempt is made to get the author information using I<CPAN.pm>.

=item HTML documentation

C<pod2html> is used to generate a set of html documentation.
This is placed under the F<blib/html/site/lib/> subdirectory, 
which C<ppm install> will install into the user's html tree.

=item Make the PPM distribution

A distribution file based on the contents of the F<blib/> directory
is then made. If possible, this will be a I<.tar.gz> file,
unless suitable software isn't available or if the I<zip>
option is used, in which case a I<.zip> archive is made, if possible.

=item adjust the PPD file

The F<package_name.ppd> file generated by C<nmake ppd> will
be edited appropriately. This includes filling in the 
I<ABSTRACT> and I<AUTHOR> fields, if needed and possible,
and also filling in the I<CODEBASE> field with the 
name of the generated archive file. This will incorporate
a possible I<binary> option used to specify
the I<HREF> attribute of the I<CODEBASE> field. 
Two routines are used in doing this - C<parse_ppd>, for
parsing the ppd file, and C<print_ppd>, for generating
the modified file.

=back

=head1 REQUIREMENTS

As well as the needed software for unpacking and
making I<.tar.gz> and I<.zip> archives, and a C<make>
program, it is assumed in this that I<CPAN.pm> is 
available and already configured, either site-wide or
through a user's F<$HOME/.cpan/CPAN/MyConfig.pm>.

Although the examples given above had a Win32 flavour,
like I<PPM>, no assumptions on the operating system are 
made in the module. 

=head1 COPYRIGHT

This program is copyright, 2002, by Randy Kobes <randy@theoryx5.uwinnipeg.ca>.
It is distributed under the same terms as Perl itself.

=head1 SEE ALSO

L<make_ppm>, and L<PPM>.

=cut

