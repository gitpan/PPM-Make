package PPM::Make::Util;
use strict;
use Exporter;
use File::Basename;
use Safe;
use File::Copy;
use XML::Parser;
use Digest::MD5;
require File::Spec;
use File::Path;
use Config;
use LWP::Simple qw(getstore is_success);
our ($VERSION);
$VERSION = 0.66;

use constant WIN32 => $^O eq 'MSWin32';

sub has_cpan {
  my $has_config = 0;
  require File::Spec;
  if ($ENV{HOME}) {
    eval 
      {require File::Spec->catfile($ENV{HOME}, '.cpan', 
                                   'CPAN', 'MyConfig.pm');};
    $has_config = 1 unless $@;
  }
  unless ($has_config) {
    eval {require CPAN::Config;};
    my $dir;
    unless (WIN32) {
        $dir = $INC{'CPAN/Config.pm'};
    }
    $has_config = 1 unless ($@ or ($dir and not -w $dir));
  }
  require CPAN if $has_config;
  return $has_config;
}
use constant HAS_CPAN => has_cpan();

sub has_ppm {
  my $has_ppm = 0;
  eval {require PPM;};
  $has_ppm = 1 unless $@;
  return $has_ppm;
}
use constant HAS_PPM => has_ppm();

sub has_mb {
  my $has_mb = 0;
  eval {require Module::Build;};
  $has_mb = 1 unless $@;
  return $has_mb;
}
use constant HAS_MB => has_mb();

require Win32 if WIN32;

use base qw(Exporter);

our (@EXPORT_OK, %EXPORT_TAGS, $protocol, $ext, $src_dir, $build_dir, $ERROR);
$protocol = qr{^(http|ftp)://};
$ext = qr{\.(tar\.gz|tgz|tar\.Z|zip)};

my @exports = qw(load_cs verifyMD5 html_escape parse_version $ERROR
                 is_core trim version which parse_ppd parse_ppm
                 ppd2cpan_version cpan2ppd_version tempfile what_have_you
                 mod_search dist_search file_to_dist ppm_search fetch_readme
                 fetch_file WIN32 HAS_CPAN HAS_PPM HAS_MB fix_path
		 package_status module_status fetch_nmake install_package);

%EXPORT_TAGS = (all => [@exports]);
@EXPORT_OK = (@exports);

my @path_ext = ();
path_ext() if WIN32;
src_and_build();
my @url_list = url_list();

my %Escape = ('&' => 'amp',
	      '>' => 'gt',
	      '<' => 'lt',
	      '"' => 'quot'
	     );

my %dists;
my ($soap);
my $soap_uri = 'http://theoryx5.uwinnipeg.ca/Apache/DocServer';
my $soap_proxy = 'http://theoryx5.uwinnipeg.ca/cgi-bin/docserver.pl';

=head1 NAME

  PPM::Make::Util - Utility functions for PPM::Make

=head1 SYNOPSIS

  use PPM::Make qw(:all);

=head1 DESCRIPTION

This module contains a number of utility functions used by PPM::Make.

=over 2

=item fix_path

Ensures a path is a Unix-type path, with no spaces.

  my $path = 'C:\Program Files\';
  my $unix_version = fix_path($path);

=cut

sub fix_path {
  my $path = shift;
  $path = Win32::GetShortPathName($path);
  $path =~ s!\\!/!g;
  $path =~ s!/$!!;
  return $path;
}

=item load_cs

Loads a CHECKSUMS file into $cksum
(adapted from the MD5 check of CPAN.pm)

  my $cksum = load_cs('CHECKSUMS');

=cut

sub load_cs {
  my $cs = shift;
  open(my $fh, $cs);
  unless ($fh) {
    $ERROR = qq{Could not open "$cs": $!};
    return;
  }
  local($/);
  my $eval = <$fh>;
  close $fh;
  $eval =~ s/\015?\012/\n/g;
  my $comp = Safe->new();
  my $cksum = $comp->reval($eval);
  if ($@) {
    $ERROR = qq{eval of "$cs" failed: $@};
    return;
  }
  return $cksum;
}

=item verifyMD5

Verify a CHECKSUM for a $file

   my $ok = verifyMD5($cksum, $file);
   print "$file checked out OK" if $ok;

=cut

sub verifyMD5 {
  my ($cksum, $file) = @_;
  my ($is, $should);
  open (my $fh, $file);
  unless ($fh) {
    $ERROR = qq{Cannot open "$file": $!};
    return;
  }
  binmode($fh);
  unless ($is = Digest::MD5->new->addfile($fh)->hexdigest) {
    $ERROR = qq{Could not compute checksum for "$file": $!};
    close $fh;
    return;
  }
  close $fh;
  if ($should = $cksum->{$file}->{md5}) {
    my $test = ($is eq $should);
    printf qq{  Checksum for "$file" is %s\n}, 
      ($test) ? 'OK.' : 'NOT OK.';
    return $test;
  }
  else {
    $ERROR = qq{Checksum data for "$file" not present.};
    return;
  }
}

=item html_escape

Escapes E<amp>, E<gt>, E<lt>, and E<quot>.

  my $escaped = html_escape('Five is > four');

=cut

sub html_escape {
  local $_ = shift;
  s/([<>\"&])(?!\w+;)/\&$Escape{$1};/mg;
  $_;
}

=item is_core

Tests to see if a module is part of the core, based on
whether or not the file is found within a I<site> type
of directory.

  my $is_core = is_core('Net::FTP');
  print "Net::FTP is a core module" if $is_core;

=cut

sub is_core {
  my $m = shift;
  $m =~ s!::|-!/!g;
  $m .= '.pm';
  my $is_core;
  foreach (@INC) {
    if (-f "$_/$m") {
      $is_core++ if ($_ !~ /site/);
      last;
    }
  }
  return $is_core;
}

=item trim

Trims white space.

  my $string = '    This is a sentence.   ';
  my $trimmed = trim($string);

=cut

sub trim {
  local $_ = shift;
  s/^\s*//;
  s/\s*$//;
  return $_;
}


=item file_to_dist

In scalar context, returns a CPAN distribution name I<filename> based
on an input file I<A/AB/ABC/filename-1.23.tar.gz>:

  my $file = 'A/AB/ABC/defg-1.23.tar.gz';
  my $dist = file_to_dist($file);

In a list context, returns both the distribution name I<filename>
and the version number I<1.23>:

  my $file = 'A/AB/ABC/defg-1.23.tar.gz';
  my ($dist, $version) = file_to_dist($cpan_file);


=cut

sub file_to_dist {
  my $cpan_file = shift;
  return unless $cpan_file;
  my ($file, $path, $suffix) = fileparse($cpan_file, $ext);
  my ($dist, $version) = version($file);
  unless ($dist and $version) {
      $ERROR = qq{Could not find distribution name from $cpan_file.};
      return;
  }
  return wantarray? ($dist, $version) : $dist;
}

=item ppd2cpan_version

Converts a ppd-type of version string (eg, I<1,23,0,0>) into a ppd one
of the form I<1.23>:

  my $s = "1,23,0,0";
  my $v = ppd2cpan_version($v);

=cut

sub ppd2cpan_version {
  local $_ = shift;
  s/(,0)*$//;
  tr/,/./;
  return $_;
}

=item cpan2ppd_version

Converts a cpan-type of version string (eg, I<1.23>) into a ppd one
of the form I<1,23,0,0>:

  my $v = 1.23;
  my $s = cpan2ppd_version($v);

=cut

sub cpan2ppd_version {
  local $_ = shift;
  return join ',', (split (/\./, $_), (0)x4)[0..3];
}

=item version

Makes an attempt to extract the file and version number of a CPAN
distribution file (eg, I<packagename> and I<1.23>
from I<packagename-1.23.tar.gz>).

  my $dist = 'package-1.23.tar.gz';
  my ($file, $version) = version($dist);

=cut

sub version {
  local ($_) = @_;
  s/$ext$//;
  s!.*/(.*)!$1!;

  # remove alpha/beta postfix
  s/([-_\d])(a|b|alpha|beta|src)$/$1/;

  # jperl1.3@4.019.tar.gz
  s/@\d.\d+//;

  # oraperl-v2.4-gk.tar.gz
  s/-v(\d)/$1/;

  # lettered versions - shudder
  s/([-_\d\.])([a-z])([\d\._])/sprintf "$1%02d$3", ord(lc $2) - ord('a') /ei;
  s/([-_\d\.])([a-z])$/sprintf "$1%02d", ord(lc $2) - ord('a') /ei;

  # thanks libwww-5b12 ;-)
  s/(\d+)b/($1-1).'.'/e;
  s/(\d+)a/($1-2).'.'/e;

  # replace '-pre' by '0.'
  s/-pre([\.\d])/-0.$1/;
  s/\.\././g;
  s/(\d)_(\d)/$1$2/g;

  # chop '[-.]' and thelike
  s/\W$//;

  # ram's versions Storable-0.4@p
   s/\@/./;

  if (s/[-_]?(\d+)\.(0\d+)\.(\d+)$//) {
    return($_, $1 + "0.$2" + $3 / 1000000);
  } elsif (s/[-_]?(\d+)\.(\d+)\.(\d+)$//) {
    return($_, $1 + $2/1000 + $3 / 1000000);
  } elsif (s/[-_]?(\d+\.[\d_]+)$//) {
    return($_, $1);
  } elsif (s/[-_]?([\d_]+)$//) {
    return($_, $1);
  } elsif (s/-(\d+.\d+)-/-/) {  # perl-4.019-ref-guide
    return($_, $1);
  } else {
    if ($_ =~ /\d/) {           # smells like an unknown scheme
      #warn "Odd version Numbering: $_ \n";
      return($_, undef);
    } else {                    # assume version 0
      #warn "No  version Numbering: $_ \n";
      return($_, 0);
    }

  }
}

sub path_ext {
  if ($ENV{PATHEXT}) {
    push @path_ext, split ';', $ENV{PATHEXT};
    for my $extention (@path_ext) {
      $extention =~ s/^\.*(.+)$/$1/;
    }
  }
  else {
    #Win9X: doesn't have PATHEXT
    push @path_ext, qw(com exe bat);
  }
}

=item which

Find the full path to a program, if available.

  my $perl = which('perl');

=cut

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
      for my $extention (@path_ext) {
	return "$base.$extention" if -x "$base.$extention";
      }
    }
  }
}

=item parse_ppd

Parse a I<ppd> file.

  my $ppd = 'package.ppd';
  my $d = parse_ppd($ppd);
  print $d->{ABSTRACT};
  print $d->{OS}->{NAME};

=cut

sub parse_ppd {
  my $file = shift;
  unless (-e $file) {
      $ERROR = qq{$file not found.};
      return;
  }
  my $p = XML::Parser->new(Style => 'Subs',
			   Handlers => {Char => \&ppd_char,
					Start => \&ppd_start,
					End => \&ppd_end,
					Init => \&ppd_init,
					Final => \&ppd_final,
				       },
			  );
  my $d = $p->parsefile($file);
  return $d;
}

sub ppd_init {
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

sub ppd_start {
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

sub ppd_char {
  my ($self, $string) = @_;
  
  my $internal = $self->{_mydata};
  my $tag = $internal->{_current};
  
  if ($tag and $internal->{wanted}->{$tag}) {
    $internal->{$tag} .= html_escape($string);
  }
  elsif ($tag and $tag eq 'INSTALL') {
    $internal->{INSTALL}->{SCRIPT} .= $string;
  }
  else {
    
  }
}

sub ppd_end {
  my ($self, $tag) = @_;
  delete $self->{_mydata}->{_current};
}

sub ppd_final {
  my $self = shift;
  return $self->{_mydata};
}

sub parse_ppm {
  my $file = $PPM::PPMdat;
  unless (-e $file) {
    $ERROR = qq{$file not found.};
    return;
  }
  my $p = XML::Parser->new(Style => 'Subs',
			   Handlers => {Char => \&ppm_char,
					Start => \&ppm_start,
					End => \&ppm_end,
					Init => \&ppm_init,
					Final => \&ppm_final,
				       },
			  );
  my $d = $p->parsefile($file);
  return $d;
}

sub ppm_init {
  my $self = shift;
  $self->{_mydata} = {
		      PPMVER => '',
		      OPTIONS => {BUILDDIR => '', CLEAN => ''},
		      wanted => {PPMVER => 1},
		      _current => '',
		     };
}

sub ppm_start {
  my ($self, $tag, %attrs) = @_;
  my $internal = $self->{_mydata};
  $internal->{_current} = $tag;
  
 SWITCH: {
    ($tag eq 'OPTIONS') and do {
      $internal->{OPTIONS}->{BUILDDIR} = $attrs{BUILDDIR};
      $internal->{OPTIONS}->{CLEAN} = $attrs{CLEAN};
      last SWITCH;
    };
    
  }
}

sub ppm_char {
  my ($self, $string) = @_;
  
  my $internal = $self->{_mydata};
  my $tag = $internal->{_current};
  
  if ($tag and $internal->{wanted}->{$tag}) {
    $internal->{$tag} .= html_escape($string);
  }
}

sub ppm_end {
  my ($self, $tag) = @_;
  delete $self->{_mydata}->{_current};
}

sub ppm_final {
  my $self = shift;
  return $self->{_mydata};
}

sub make_soap {
  unless (eval { require SOAP::Lite }) {
    $ERROR = "SOAP::Lite is unavailable to make remote call.";
    return;
  }

  return SOAP::Lite
    ->uri($soap_uri)
      ->proxy($soap_proxy,
	      options => {compress_threshold => 10000})
	->on_fault(sub { my($soap, $res) = @_; 
			 warn "SOAP Fault: ", 
                           (ref $res ? $res->faultstring 
                            : $soap->transport->status),
                              "\n";
                         return undef;
		       });
}

=item src_and_build

Returns the source and build directories used with
CPAN.pm, if present. If not, returns those used with PPM,
if those are present. If neither of these are available,
returns the system temp directory.

  my ($src_dir, $build_dir)= src_and_build;

=cut

sub src_and_build {
  return if ($src_dir and $build_dir);
 SWITCH: {
    HAS_CPAN and do {
      $src_dir = $CPAN::Config->{keep_source_where};
      $build_dir = $CPAN::Config->{build_dir};
      last SWITCH if ($src_dir and $build_dir);
    };
    HAS_PPM and do {
      my $d = parse_ppm();
      $src_dir = $d->{OPTIONS}->{BUILDDIR};
      $build_dir = $src_dir;
      last SWITCH if ($src_dir and $build_dir);
    };
    $src_dir = File::Spec->tmpdir() || '.';
    $build_dir = $src_dir;
  }
}

sub fetch_readme {
  my $search = shift;
  $soap ||= make_soap() or return; # no SOAP::Lite available
  my $result = $soap->get_readme($search);
  defined $result && defined $result->result or do {
    $ERROR = "No results returned";
    return;
  };

  my $text = $result->result();
  return $text eq '1' ? undef : $text;
}

=item tempfile

Generates the name of a random temporary file.

  my $tmpfile = tempfile;

=cut

sub tempfile {
  my $rand = int(rand $$);
  return File::Spec->catfile(File::Spec->tmpdir(), 
                             'ppm-make.' . $rand);
}

=item dist_search

Uses CPAN.pm to perform a distribution search.

  my $dist = 'libnet';
  my $results = dist_search($dist);

A case-insensitive search can be made by giving C<dist_search>
an argument of <no_case =E<gt> 1>, while partial matches can be made
by specifying <partial =E<gt> 1>.
The results are returned as a hash reference of the form

  foreach my $dist (keys %{$results}) {
    print "Distribution: $dist\n";
    print "Version: $results->{$dist}->{version}\n";
    print "Description: $results->{$dist}->{abstract}\n";
    print "Author: $results->{$dist}->{author}\n";
    print "CPAN file: $results->{$dist}->{cpan_file}\n";
  }

Not all fields are guaranteed to have a value.

=cut

sub dist_search {
  my ($query, %args) = @_;
  $query =~ s!::!-!g;
  my $dists;
  foreach my $match (CPAN::Shell->expand('Distribution', qq{/$query/})) {
    my $string = $match->as_string;
    my $cpan_file;
    if ($string =~ /id\s*=\s*(.*?)\n/m) {
      $cpan_file = $1;
      next unless $cpan_file;
    }
    my ($dist, $version) = file_to_dist($cpan_file);
    unless ($args{no_case}) {
      next unless $dist =~ /$query/;
    }
    unless ($args{partial}) {
      if ($args{no_case}) {
        next unless $dist =~ /^$query$/i;
      }
      else {
        next unless $dist eq $query;
      }
    }
    $dists->{$dist}->{cpan_file} = $cpan_file;
    $dists->{$dist}->{version} = $version;
    if ($string =~ /\s+CPAN_USERID.*\s+\((.*)\)\n/m) {
      $dists->{$dist}->{author} = $1;
    }
    my $mod;
    if ($string =~ /\s+CONTAINSMODS\s+(\S+)/m) {
      $mod = $1;
    }
    next unless $mod;
    my $module = CPAN::Shell->expand('Module', $mod);
    if ($module) {
      my $desc = $module->description;
      $dists->{$dist}->{abstract} = $desc if $desc;
    }
  }
  return $dists;
}

=item mod_search

Uses CPAN.pm to perform a module search.

  my $mod = 'Net::FTP';
  my $results = mod_search($mod);

A case-insensitive search can be made by giving C<mod_search>
an argument of <no_case =E<gt> 1>, while partial matches can be made
by specifying <partial =E<gt> 1>.
The results are returned as a hash reference of the form

  foreach my $mod (keys %{$results}) {
    print "Module: $mod\n";
    print "Version: $results->{$mod}->{version}\n";
    print "Description: $results->{$mod}->{abstract}\n";
    print "Author: $results->{$mod}->{author}\n";
    print "CPAN file: $results->{$mod}->{cpan_file}\n";
  }

Not all fields are guaranteed to have a value.

=cut

sub mod_search {
  my ($query, %args) = @_;
  $query =~ s!-!::!g;
  my $mods;
  my @objs = (not $args{partial} and not $args{no_case}) ?
    CPAN::Shell->expand('Module', $query) :
	CPAN::Shell->expand('Module', qq{/$query/});
  unless (@objs > 0) {
    $ERROR = "No results found for $query";
    return;
  }
  foreach my $obj(@objs) {
    my $string = $obj->as_string;
    my $mod;
    if ($string =~ /id\s*=\s*(.*?)\n/m) {
      $mod = $1;
      next unless $mod;
    }
    unless ($args{no_case}) {
      next unless $mod =~ /$query/;
    }
    unless ($args{partial}) {
      if ($args{no_case}) {
        next unless $mod =~ /^$query$/i;
      }
      else {
        next unless $mod eq $query;
      }
    }

    if (my $v = $obj->cpan_version) {
      $mods->{$mod}->{version} = $v;
    }
    if ($string =~ /\s+DESCRIPTION\s+(.*?)\n/m) {
      $mods->{$mod}->{abstract} = $1;
    }
    if ($string =~ /\s+CPAN_USERID.*\s+\((.*)\)\n/m) {
      $mods->{$mod}->{author} = $1;
    }
    if ($string =~ /\s+CPAN_FILE\s+(\S+)\n/m) {
      $mods->{$mod}->{cpan_file} = $1;
    }
  }
  return $mods;
}

=item ppm_search

Uses PPM.pm to perform a distribution search.

  my $dist = 'libnet';
  my $results = ppm_search($dist);

A case-insensitive search can be made by giving C<ppm_search>
an argument of C<no_case =E<gt> 1>, while partial matches can be made
by specifying C<partial =E<gt> 1>. An author or abstract query
can be specified with C<AUTHOR =E<gt> 1> and C<ABSTRACT =E<gt> 1>,
respectively. The search can be restricted to a given
repository by specifying the URL as
C<location =E<gt> $url>; otherwise, a search of all 
available respositories is made.
The results are returned as a hash reference of the form

  foreach my $package (keys %{$results}) {
    print "Package: $package\n";
    print "Version: $results->{$package}->{version}\n";
    print "Author: $results->{$package}->{author}\n";
    print "Abstract: $results->{$package}->{abstract}\n";
    print "Location(s): @{$results->{$package}->{repository}}\n";
  }

Not all fields are guaranteed to have a value.

=cut

sub ppm_search {
  my ($searchRE, %args) = @_;
  
  $searchRE =~ s!::!-!g;
  eval { $searchRE =~ /$searchRE/ };
  if ($@) {
    $ERROR = qq{"$searchRE" is not a valid regular expression.};
    return;
  }
  $searchRE = "(?i)$searchRE" if $args{no_case};
  $searchRE = "^$searchRE\$" unless $args{partial};
  
  my $searchtag;
  for my $type(qw(AUTHOR ABSTRACT)) {
    $searchtag = $type if $args{$type};
  }
  my %reps = PPM::ListOfRepositories();
  my @locations = $args{location} || values %reps;
  my $packages;

  foreach my $loc (@locations) {
    my %summary;
    # see if the repository has server-side searching
    if (defined $searchRE && 
	(%summary = ServerSearch(location => $loc, 
				 searchRE => $searchRE, 
				 searchtag => $searchtag))) {
      # XXX: clean this up
      foreach my $package (keys %{$summary{$loc}}) {
	$packages->{$loc}->{$package} = \%{$summary{$loc}{$package}};
      }
      next;
    }
    
    # see if a summary file is available
    %summary = RepositorySummary(location => $loc);
    if (%summary) {
      foreach my $package (keys %{$summary{$loc}}) {
	next if (defined $searchtag && 
		 $summary{$loc}{$package}{$searchtag} !~ /$searchRE/);
	next if (!defined $searchtag && 
		 defined $searchRE && $package !~ /$searchRE/);
	$packages->{$loc}->{$package} = \%{$summary{$loc}{$package}};
      }
    }
    else {
      my %ppds = PPM::RepositoryPackages(location => $loc);
      # No summary: oh my, nothing but 'Net
      foreach my $package (@{$ppds{$loc}}) {
	my %package_details = 
	  RepositoryPackageProperties(package => $package, 
				      location => $loc);
	next unless %package_details;
	next if (defined $searchtag && 
		 $package_details{$searchtag} !~ /$searchRE/);
	next if (!defined $searchtag && 
		 defined $searchRE && $package !~ /$searchRE/);
	$packages->{$loc}->{$package} = \%package_details;
      }
    }
  }
  unless ($packages) {
    $ERROR = qq{No packages found.};
    return;
  }
  my $results;
  foreach my $location (keys %{$packages}) {
    foreach my $pack (keys %{$packages->{$location}}) {
      $results->{$pack}->{abstract} = 
        $packages->{$location}->{$pack}->{ABSTRACT};
      $results->{$pack}->{author} = 
        $packages->{$location}->{$pack}->{AUTHOR};
      $results->{$pack}->{version} = 
        $packages->{$location}->{$pack}->{VERSION};
      push @{$results->{$pack}->{repository}}, $location;
    }
  }
  return $results;
}

=item fetch_file

Fetches a file, and if successful, returns the stored filename. 
If the file is specified beginning with I<http://> or I<ftp://>:

  my $fetch = 'http://my.server/my_file.tar.gz';
  my $filename = fetch_file($file);

will grab this file directly. Otherwise, if the file has
an extension I<\.(tar\.gz|tgz|tar\.Z|zip)>, it will assume
this is a CPAN distribution and grab it from a CPAN mirror:

  my $dist = 'A/AB/ABC/file.tar.gz';
  my $filename = fetch_file($dist);

which assumes the file lives under I<$CPAN/authors/id/>. If
neither of the above are satisfied, it will assume this
is a module name, and fetch the corresponding CPAN distribution,
if found.

  my $mod = 'Net::FTP';
  my $filename = fetch_file($mod);

=cut

sub fetch_file {
  my ($dist, $no_case) = @_;
  my $to;
  if ($dist =~ m!$protocol!) {
    ($to = $dist) =~ s!.*/(.*)!$1!;
    print "Fetching $dist ....\n";
    my $rc = is_success(getstore($dist, $to));
    unless ($rc) {
      $ERROR = qq{Fetch of $dist failed.};
      return;
    }
    return $to;
  }
  unless ($dist =~ /$ext$/) {
    my $mod = $dist;
    $mod =~ s!-!::!g;
    my $results = mod_search($mod, no_case => $no_case, partial => 0);
    unless ($dist = $results->{$mod}->{cpan_file}) {
      $ERROR = qq{Cannot get distribution name of $mod.};
      return;
    }
  }
  my $id = dirname($dist); 
  $to = basename($dist, $ext); 
  my $src = HAS_CPAN ? 
    File::Spec->catdir($src_dir, 'authors/id', $id) : 
        $src_dir;
  my $CS = 'CHECKSUMS';
  my $get_cs = 0;
  for my $file( ($to, $CS)) {
    my $local = File::Spec->catfile($src, $file);
    if (-e $local and $src_dir ne $build_dir and not $get_cs) {
      copy($local, '.') or do {
        $ERROR = "Cannot copy $local: $!";
        return;
      };
      next;
    }
    else {
      my $from;
      $get_cs = 1;
      foreach my $url(@url_list) {
        $url =~ s!/$!!;
        $from = $url . '/authors/id/' . $id . '/' . $file;
        print "Fetching $from ...\n";
        last if is_success(getstore($from, $file));
      }
      unless (-e $file) {
        $ERROR = "Fetch of $file from $from failed";
        return;
      }
      if ($src_dir ne $build_dir) {
        unless (-d $src) {
          mkpath($src) or do {
            $ERROR = "Cannot mkdir $src: $!";
            return;
          };
        }
        copy($file, $src) or warn "Cannot copy $to to $src: $!";
      }
    }
  }
  return $to unless $to =~ /$ext$/;
  my $cksum;
  unless ($cksum = load_cs($CS)) {
    $ERROR = qq{Checksums check disabled - cannot load $CS file.};
    return;
  }
  unless (verifyMD5($cksum, $to)) {
    $ERROR =  qq{Checksums check for "$to" failed.};
    return;
  }
  unlink $CS or warn qq{Cannot unlink "$CS": $!\n};
  return $to;
}

=item url_list

Gets a list of CPAN mirrors, incorporating any from CPAN.pm.

  my @list = url_list();

=cut

sub url_list {
  my @urls;
  if (HAS_CPAN) {
    push @urls, @{$CPAN::Config->{urllist}};
  }
  push @urls, 'ftp://ftp.cpan.org', 'http://www.cpan.org';
  return @urls;
}

=item module_status

Checks the status of a module I<Foo::Bar> against
a supplied version:

  my ($module, $wanted_version) = ('Foo::Bar', 1.23);
  my $status = module_status($module, $wanted_version);

The returned value of C<$recommendation> will be 1 if
the module is present and it's version is greater than
or equal to the desired version. A return value of 0
means the module is present, but it's version is less
than the wanted version. If the module is not present,
a value of -1 is returned. C<$wanted_version> can be
undefined, which is interpreted to mean any version of
the module is OK.

=cut

sub module_status {
  my ($module, $desired_version) = @_;
  my ($present, $installed_version) = have_module($module);
  return -1 if not $present;
  return 0 if ($desired_version and not $installed_version);
  return 1 if (not $desired_version);
  my $status = vcmp_cpan($installed_version, $desired_version);
  return $status == -1 ? 0 : 1;
}

=item package_status

Checks the status of a PPM package I<Foo-Bar>:

  my ($pack, $wanted_version) = ('Foo-Bar', '1,2,3,4');
  my $status = package_status($pack, $wanted_version);

The returned value of C<$recommendation> will be 1 if
the package is present and it's version is greater than
or equal to the desired version. A return value of 0
means the package is present, but it's version is less
than the wanted version. If the package is not present,
a value of -1 is returned. C<$wanted_version> can be
undefined, which is interpreted to mean any version of
the package is OK.

=cut

sub package_status {
  my ($pack, $desired_version) = @_;
  $pack =~ s!::!-!g;
  my ($present, $installed_version) = have_package($pack);
  return -1 if not $present;
  return 0 if ($desired_version and not $installed_version);
  return 1 if (not $desired_version);
  my $status = vcmp_ppd($installed_version, $desired_version);
  return $status == -1 ? 0 : 1;
}

=item have_module

Checks to see if a module I<Foo::Bar> is already installed:

  my $module = "Foo::Bar";
  my ($present, $version) = have_module($module);

If not present, C<$present> will be undefined. Note that
even if the module is present, C<$version> may be undefined.

=cut

sub have_module {
  my $mod = shift;
  $mod =~ s!-!::!g;
  (my $file = $mod) =~ s!::!/!g;
  $file .= '.pm';
  my $parsefile;
  foreach (@INC) {
    next if $_ eq '.';
    my $candidate = File::Spec->catfile($_, $file);
    if (-e $candidate) {
      $parsefile = $candidate;
      last;
    }
  }
  unless ($parsefile) {
    $ERROR = qq{Cannot find pm file corresponding to $mod.};
    return;
  }
  my $version = parse_version($parsefile);
  return (1, $version);
}

=item parse_version

Extracts a version string from a module file.

  my $version = parse_version('C:/Perl/lib/CPAN.pm');

=cut

# from ExtUtils::MM_Unix
sub parse_version {
  my $parsefile = shift;
  my $version;
  local $/ = "\n";
  my $fh;
  unless (open($fh, $parsefile)) {
    $ERROR = "Could not open '$parsefile': $!";
    return;
  }
  my $inpod = 0;
  while (<$fh>) {
    $inpod = /^=(?!cut)/ ? 1 : /^=cut/ ? 0 : $inpod;
    next if $inpod || /^\s*\#/;
    chop;
    # next unless /\$(([\w\:\']*)\bVERSION)\b.*\=/;
    next unless /([\$*])(([\w\:\']*)\bVERSION)\b.*\=/;
    my $eval = qq{
                  package ExtUtils::MakeMaker::_version;
                  no strict;
                  
                  local $1$2;
                  \$$2=undef; do {
                    $_
                  }; \$$2
                 };
    local $^W = 0;
    $version = eval($eval);
    warn "Could not eval '$eval' in $parsefile: $@" if $@;
    last;
  }
  close $fh;
  return $version;
}

=item have_package

Checks to see if a PPM package I<Foo-Bar> is already installed:

  my $pack = "Foo-Bar";
  my ($present, $version) = have_package($pack);

If not present, C<$present> will be undefined. Note that
C<$version>, being the ppd version string, is expected 
always to be defined.

=cut

sub have_package {
  my $package = shift;
  $package =~ s!::!-!g;
  my $version;
  my %installed = PPM::InstalledPackageProperties();
  if (my $pkg = (grep {/^$package$/} keys %installed)[0]) {
    $version = $installed{$pkg}{'VERSION'};
  }
  else {
    $ERROR = qq{Could not determine version of $package};
    return;
  }
  return (1, $version);
}

=item vcmp_ppd

Compares two ppd version strings ($s1, $s2), and returns 1 
if $s1 > $s2, 0 if $s1 == $s2, and -1 if $s1 < $s2.

  my $s1 = '1,2,3,4'; # the installed version
  my $s2 = '2,3,4,5'; # a potential upgrade
  my $cmp = vcmp_ppd($s1, $s2);

=cut

sub vcmp_ppd {
  my ($s1, $s2) = @_;
  my @installed = split (',', $s1);
  my @compare = split (',', $s2);
  my $available;
  foreach(0..3) {
    next if $installed[$_] == $compare[$_];
    $available-- if $installed[$_] < $compare[$_];
    $available++ if $installed[$_] > $compare[$_];
    last;
  }
  return $available;
}

=item vcmp_cpan

Compares two CPAN version strings ($s1, $s2), and returns 1 
if $s1 > $s2, 0 if $s1 == $s2, and -1 if $s1 < $s2.

  my $s1 = '1.23'; # the installed version
  my $s2 = '4.56'; # a potential upgrade
  my $cmp = vcmp_cpan($s1, $s2);

=cut

# borrowed from CPAN.pm
sub vcmp_cpan {
    my ($l, $r) = @_;
    local($^W) = 0;
    return 0 if $l eq $r; # short circuit for quicker success
    return
        ($l ne "undef") <=> ($r ne "undef") ||
            $l <=> $r ||
                $l cmp $r;
}

=item fetch_nmake

Fetch C<nmake.exe>.

  unless (my $installed_nmake = fetch_nmake) {
      print "I could not retrieve nmake";
  }

=cut

sub fetch_nmake {
  my ($exe, $err) = ('nmake.exe', 'nmake.err');
  if (my $p = which($exe)) {
    warn qq{You already have $exe as "$p". Fetch aborted.};
    return $p;
  }
  my $nmake = 'nmake15.exe';
  my $r = 'http://download.microsoft.com/download/vc15/Patch/1.52/W95/EN-US/Nmake15.exe';
  unless (is_success(getstore($r, $nmake))) {
    $ERROR = "Could not fetch $nmake";
    return;
  }
  unless (-e $nmake) {
    $ERROR = "Getting $nmake failed";
    return;
  }
  my @args = ($nmake);
  system(@args);
  unless (-e $exe and -e $err) {
    $ERROR = "Extraction of $exe and $err failed";
    return;
  }
  use File::Copy;
  my $dir = prompt('Which directory on your PATH should I copy the files to?',
		   $Config{bin});
  unless (-d $dir) {
    my $ans = prompt(qq{$dir doesn\'t exist. Create it?}, 'yes');
    if ($ans =~ /^y/i) {
      mkdir $dir or do {
	$ERROR = "Could not create $dir: $!";
	return;
      };
    }
    else {
      $ERROR = "Will not create $dir";
      return;
    }
  }
  for ($exe, $err, 'README.TXT') {
    move($_, $dir) or do {
      $ERROR = "Moving $_ to $dir failed: $!";
      return;
    };
  }
  unlink $nmake or warn "Unlink of $nmake failed: $!";
  return which($exe);
}

# from Module::Build
sub prompt {
  my ($mess, $def) = @_;
  die "prompt() called without a prompt message" unless @_;
  
# Pipe?
  my $INTERACTIVE = -t STDIN && (-t STDOUT || !(-f STDOUT || -c STDOUT));
  
  ($def, my $dispdef) = defined $def ? ($def, "[$def] ") : ('', ' ');

  {
    local $|=1;
    print "$mess $dispdef";
  }
  my $ans;
  if ($INTERACTIVE) {
    $ans = <STDIN>;
    if ( defined $ans ) {
      chomp $ans;
    } else { # user hit ctrl-D
      print "\n";
    }
  }
  
  unless (defined($ans) and length($ans)) {
    print "$def\n";
    $ans = $def;
  }
  
  return $ans;
}

sub install_package {
  my ($package, %args) = @_;
  my $version = $args{version};
  die "Please specify a version" unless $version;
  my $upgrade = $args{upgrade};
  my $location = $args{location};
  unless ($location) {
    $ERROR = "Please specify a location";
    return;
  }
  my $status = 
      package_status($package, $version);
  if ($status == 1) {
    $ERROR = "Version $version of $package is already installed";
    return;
  }
  if ($status == 0 and not $upgrade) {
    $ERROR = qq{Specify the upgrade switch to upgrade $package.};
    return;
  }
  print "Installing $package ...\n";
  if ($status == -1) {
    PPM::InstallPackage(package => $package,
                        location => $location) 
        or do {
          $ERROR = "Could not install $package: $PPM::PPMERR";
          return;
        };
  }
  else {
    PPM::UpgradePackage(package => $package,
                        location => $location) 
        or do {
          $ERROR = "Could not install $package: $PPM::PPMERR";
          return;
        };
  }
  return 1;
}

sub what_have_you {
  my ($progs, $arch, $os) = @_;
  my %has;
  if (defined $progs->{tar} and defined $progs->{gzip}) {
    $has{tar} = $progs->{tar};
    $has{gzip} = $progs->{gzip};
  }
  elsif ((not WIN32 and 
	  (not $os or $os =~ /Win32/i or not $arch or $arch =~ /Win32/i))) {
    $has{tar} = 
      $Config{tar} || which('tar') || $CPAN::Config->{tar};
    $has{gzip} =
      $Config{gzip} || which('gzip') || $CPAN::Config->{gzip};
  }
  else {
    eval{require Archive::Tar; require Compress::Zlib};
    if ($@) {
      $has{tar} = 
	$Config{tar} || which('tar') || $CPAN::Config->{tar};
      $has{gzip} =
	$Config{gzip} || which('gzip') || $CPAN::Config->{gzip};
    }
    else {
      $has{tar} = 'Archive::Tar';
      $has{gzip} = 'Compress::Zlib';
    }
  }

  if (defined $progs->{zip} and defined $progs->{unzip}) {
    $has{zip} = $progs->{zip};
    $has{unzip} = $progs->{unzip};
  }
  else {
    eval{require Archive::Zip; };
    if ($@) {
      $has{zip} = 
	$Config{zip} || which('zip') || $CPAN::Config->{zip};
      $has{unzip} =
	$Config{unzip} || which('unzip') || $CPAN::Config->{unzip};
    }
    else {
      my $zipv = $Archive::Zip::VERSION + 0;
      if ($zipv >= 1.02) {
	require Archive::Zip; import Archive::Zip qw(:ERROR_CODES);
	$has{zip} = 'Archive::Zip';
	$has{unzip} = 'Archive::Zip';
      }
      else {
	$has{zip} =
	  $Config{zip} || which('zip') || $CPAN::Config->{zip};
	$has{unzip} =
	  $Config{unzip} || which('unzip') || $CPAN::Config->{unzip};
      }
    }
  }
  
  my $make = WIN32 ? 'nmake' : 'make';
  $has{make} = $progs->{make} ||
    $Config{make} || which($make) || $CPAN::Config->{make};
  if (WIN32 and not $has{make}) {
    $has{make} = fetch_nmake();
  }

  $has{perl} = 
    $Config{perlpath} || which('perl');
  
  foreach (qw(tar gzip make perl)) {
    unless ($has{$_}) {
      $ERROR = "Cannot find a '$_' program";
      return;
    }
    print "Using $has{$_} ....\n";
  }

  return \%has;
}

1;

__END__


=back

=head1 COPYRIGHT

This program is copyright, 2003, by Randy Kobes <randy@theoryx5.uwinnipeg.ca>.
It is distributed under the same terms as Perl itself.

=head1 SEE ALSO

L<PPM>.

=cut

