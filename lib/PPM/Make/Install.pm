package PPM::Make::Install;
use strict;
use PPM::Make;
use base qw(PPM::Make);
use PPM::Make::Util qw(:all);
use Config;
use Cwd;
our ($VERSION);
$VERSION = '0.71';

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
    unless ($dist = fetch_file($dist)) {
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
  print "Installing $package ...\n";
  if (HAS_PPM >= 3) {
    my @args = $self->{opts}->{upgrade} ?
      ('ppm', 'upgrade', $self->{ppd}) :
        ('ppm', 'install', $self->{ppd});
    system(@args) == 0 or die "Cannot install/upgrade $self->{ppd}: $?";
  }
  else {
    my @args = $self->{opts}->{upgrade} ?
      ('ppm', 'verify', '--upgrade', $self->{ppd}) :
        ('ppm', 'install', $self->{ppd});
    system(@args) == 0 or die "Cannot install/upgrade $self->{ppd}: $?";
  }
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

__END__

=head1 NAME

PPM::Make::Install - build and install a distribution via ppm

=head1 SYNOPSIS

  my $ppm = PPM::Make::Install->new(%opts);
  $ppm->make_ppm();
  $ppm->ppm_install();

=head1 DESCRIPTION

C<PPM::Make::Install> is used to build a PPM (Perl Package Manager) 
distribution from a CPAN source distribution and then install it with 
the C<ppm> utility. See L<PPM::Make> for a discussion of details on
how the ppm package is built. Available options are

=over

=item ignore => 1

By default, C<PPM::Make::Install>, when building the distribution,
will die if all tests do not pass. Turning on this option
instructs the module to ignore any test failures.

=item remove => 1

If specified, the directory used to build the ppm distribution
will be removed after a successful install.

=item force => 1

By default, if C<PPM::Make::Install> detects a F<blib/> directory,
it will assume the distribution has already been made, and
will not remake it. This option forces remaking the distribution.

=item upgrade => 1

Will do an upgrade of the specified package, if applicable.

=item dist => value

A value for I<dist> will be interpreted either as a CPAN-like source
distribution to fetch and build, or as a module name,
in which case I<CPAN.pm> will be used to infer the
corresponding distribution to grab.

=item program => { p1 => '/path/to/q1', p2 => '/path/to/q2', ...}

This option specifies that C</path/to/q1> should be used
for program C<p1>, etc., rather than the ones PPM::Make finds. The
programs specified can be one of C<tar>, C<gzip>, C<zip>, C<unzip>,
or C<make>.

=back

=head1 COPYRIGHT

This program is copyright, 2003, by Randy Kobes <randy@theoryx5.uwinnipeg.ca>.
It is distributed under the same terms as Perl itself.

=head1 SEE ALSO

L<PPM::Make>, and L<PPM>.

=cut
