package PPM::Make::Install;
use strict;
use PPM::Make;
use base qw(PPM::Make);
use PPM::Make::Util qw(:all);
use Config;
use Cwd;

sub new {
  my ($class, %opts) = @_;

  die "\nInvalid option specification" unless check_opts(%opts);
  die "Must have the ppm utility to install" unless HAS_PPM;
  my $arch = $Config{archname};
  my $os = $Config{osname};
  if (length($^V) && ord(substr($^V, 1)) >= 8) {
    $arch .= sprintf("-%d.%d", ord($^V), ord(substr($^V, 1)));
  }
  my $has = what_have_you($opts{program}, $arch, $os);
  my $self = {
	      opts => \%opts || {},
	      cwd => '',
	      has => $has,
	      args => {},
	      ppd => '',
	      archive => '',
              prereq_pm => {},
	      file => '',
	      version => '',
              use_mb => '',
	      ARCHITECTURE => $arch,
	      OS => $os,
	     };
  bless $self, $class;
}

sub check_opts {
  my %opts = @_;
  my %legal = 
    map {$_ => 1} qw(force ignore dist program upgrade remove);
  foreach (keys %opts) {
    next if $legal{$_};
    warn "Unknown option '$_'\n";
    return;
  }

  if (defined $opts{program} and my $progs = $opts{program}) {
    unless (ref($progs) eq 'HASH') {
      warn "Please supply a HASH reference to 'program'";
      return;
    }
    my %ok = map {$_ => 1} qw(zip unzip tar gzip make);
    foreach (keys %{$progs}) {
      next if $ok{$_};
      warn "Unknown program option '$_'\n";
      return;
    }
  }  
  return 1;
}

sub make_ppm {
  my $self = shift;
  my $dist = $self->{opts}->{dist};
  if ($dist) {
    my $build_dir = $PPM::Make::Util::build_dir;
    chdir $build_dir or die "Cannot chdir to $build_dir: $!";
    print "Working directory: $build_dir\n"; 
    unless ($dist = fetch_file($dist, no_case => $self->{opts}->{no_case}, 
                               partial => 0)) {
      die $ERROR;
    }
#      if ($dist =~ m!$protocol! 
#          or $dist =~ m!^\w/\w\w/! or $dist !~ m!$ext!);
    print "Extracting files from $dist ....\n";
    my $name = $self->extract_dist($dist, $build_dir);
    chdir $name or die "Cannot chdir to $name: $!";
    $self->{file} = $dist;
  }
  die "Need a Makefile.PL or Build.PL to build"
    unless (-f 'Makefile.PL' or -f 'Build.PL');
  $self->{cwd} = cwd;
  my $mb = -e 'Build.PL';
  $self->{mb} = $mb;
  my $force = $self->{opts}->{force};
  die "This distribution requires Module::Build to build" 
    if ($mb and not HAS_MB);
  $self->build_dist() 
      unless (-d 'blib' and (-f 'Makefile' or ($mb and -f 'Build') )
              and not $force);
  $self->parse_yaml if (-e 'META.yml');
  if ($mb) {
    $self->parse_build();
  }
  else {
#    $self->parse_makepl();
    $self->parse_make() unless $self->{args}->{NAME};
  }
  $self->abstract();
  $self->author();
  $self->{version} = ($self->{args}->{VERSION} ||
                      parse_version($self->{args}->{VERSION_FROM}) ) 
    or warn "Could not extract version information";
  $self->make_html() unless (-d 'blib/html' and not $force);
  $dist = $self->make_dist();
  $self->make_ppd($dist);
}

sub ppm_install {
  my $self = shift;
  my $cwd = $self->{cwd};
  (my $package = $self->{ppd}) =~ s!\.ppd$!!;
  my $version = $self->{version};
  my $status = 
    package_status($package, cpan2ppd_version($version));
  die "Version $version of $package is already installed"
    if $status == 1;
  if ($status == 0 and not $self->{opts}->{upgrade}) {
    die qq{Specify the upgrade switch to upgrade $package.};
  }
  print "Installing $package ...\n";
  if ($status == -1) {
    PPM::InstallPackage(package => $package,
                        location => $cwd) 
        or die "Could not install $package: $PPM::PPMERR";
  }
  else {
    PPM::UpgradePackage(package => $package,
                        location => $cwd) 
        or die "Could not install $package: $PPM::PPMERR";    
  }
#  my @args = ('ppm', 'install', $self->{ppd});
#  print "@args\n";
#  system(@args) == 0 or warn "Can't install $self->{ppd}: $?";
  return unless $self->{opts}->{remove};
  my $file = $self->{file};
  unless ($file) {
    warn "Cannot clean files unless a distribution is specified";
    return;
  }
  if (-f "../$file") {
    print "Removing $cwd/../$file ....\n";
    unlink "$cwd/../$file" or warn "Cannot unlink $cwd/../$file: $!";
  }
  chdir('..') or die "Cannot move up one directory: $!";
  if (-d $cwd) {
    print "Removing $cwd ...\n";
    rmtree($cwd) or warn "Cannot remove $cwd: $!";
  }
}

1;
