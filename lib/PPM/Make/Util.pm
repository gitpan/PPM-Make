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
use CPAN::DistnameInfo;
our ($VERSION);
$VERSION = '0.71';

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
  my $ppm = File::Spec->catfile($Config{bin}, 'ppm.bat');
  return unless -f $ppm;
  eval{require PPM;};
  return $@ ? 3 : 2;
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
                 is_core trim which parse_ppd parse_ppm
                 ppd2cpan_version cpan2ppd_version tempfile what_have_you
                 mod_search dist_search file_to_dist fetch_nmake
                 fetch_file WIN32 HAS_CPAN HAS_PPM HAS_MB fix_path
		 );

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
my $info_soap;
my $info_uri = 'http://theoryx5.uwinnipeg.ca/Apache/InfoServer';
my $info_proxy = 'http://theoryx5.uwinnipeg.ca/cgi-bin/ppminfo.cgi';

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
  my $d = CPAN::DistnameInfo->new($cpan_file);
  my ($dist, $version) = ($d->dist, $d->version);
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

sub make_info_soap {
  unless (eval { require SOAP::Lite }) {
    $ERROR = "SOAP::Lite is unavailable to make remote call.";
    return;
  }

  return SOAP::Lite
    ->uri($info_uri)
      ->proxy($info_proxy,
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

=item tempfile

Generates the name of a random temporary file.

  my $tmpfile = tempfile;

=cut

sub tempfile {
  my $rand = int(rand $$);
  return File::Spec->catfile(File::Spec->tmpdir(), 
                             'ppm-make.' . $rand);
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

=item mod_search

Uses a remote soap server or CPAN.pm to perform a module search.

  my $mod = 'Net::FTP';
  my $results = mod_search($mod);

The query term must match exactly, in a case
sensitive manner. The results are returned as a hash reference 
of the form

  print <<"END";
    Module: $results->{mod_name}
    Version: $results->{mod_vers}
    Description: $results->{mod_abs}
    Author: $results->{author}
    CPAN file: $results->{dist_file}
    Distribution: $results->{dist_name}
  END

Not all fields are guaranteed to have a value.

=cut

sub mod_search {
  my ($query, %args) = @_;
  my $results = soap_mod_search($query, %args);
  return $results if $results;
  warn $ERROR if $ERROR;
  return unless HAS_CPAN;
  return cpan_mod_search($query, %args);
}

sub cpan_mod_search {
  my ($query, %args) = @_;
  $query =~ s!-!::!g;
  my $mods;
  my @objs = CPAN::Shell->expand('Module', qq{/$query/});
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
    next unless $mod eq $query;
    $mods->{mod_name} = $mod;
    if (my $v = $obj->cpan_version) {
      $mods->{mod_vers} = $v;
    }
    if ($string =~ /\s+DESCRIPTION\s+(.*?)\n/m) {
      $mods->{mod_abs} = $1;
    }
    if ($string =~ /\s+CPAN_USERID.*\s+\((.*)\)\n/m) {
      $mods->{author} = $1;
    }
    if ($string =~ /\s+CPAN_FILE\s+(\S+)\n/m) {
      $mods->{dist_file} = $1;
    }
    $mods->{dist_name} = file_to_dist($mods->{dist_file});
    last;
  }
  return $mods;
}

sub soap_mod_search {
  my ($query, %args) = @_;
  $query =~ s!-!::!g;
  return unless (my $soap = make_info_soap());
  my $result = $soap->mod_info($query);
  eval {$result->fault};
  if ($@) {
      $ERROR = $@;
      return;
  }
  $result->fault and do {
      $ERROR = join ', ', 
          $result->faultcode, 
              $result->faultstring;
      return;
  };
  my $results = $result->result();
  if ($results) {
    my $email = $results->{email} || $results->{cpanid} . '@cpan.org';
    $results->{author} = $results->{fullname} . qq{ &lt;$email&gt; };
  }
  else {
    $ERROR = qq{No results for "$query" were found}
  };
  return $results;
}

=item dist_search

Uses a remote soap server or CPAN.pm to perform a distribution search.

  my $dist = 'libnet';
  my $results = dist_search($dist);

The query term must match exactly, in a case
sensitive manner. The results are returned as a hash reference 
of the form

  print <<"END";
    Distribution: $results->{dist_name}
    Version: $results->{dist_vers}
    Description: $results->{dist_abs}
    Author: $results->{author}
    CPAN file: $results->{dist_file}
  END

Not all fields are guaranteed to have a value.

=cut

sub dist_search {
  my ($query, %args) = @_;
  my $results = soap_dist_search($query, %args);
  return $results if $results;
  warn $ERROR if $ERROR;
  return unless HAS_CPAN;
  return cpan_dist_search($query, %args);
}

sub cpan_dist_search {
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
    next unless $dist eq $query;
    $dists->{dist_name} = $dist;
    $dists->{dist_file} = $cpan_file;
    $dists->{dist_vers} = $version;
    if ($string =~ /\s+CPAN_USERID.*\s+\((.*)\)\n/m) {
      $dists->{author} = $1;
    }
    my $mod;
    if ($string =~ /\s+CONTAINSMODS\s+(\S+)/m) {
      $mod = $1;
    }
    next unless $mod;
    my $module = CPAN::Shell->expand('Module', $mod);
    if ($module) {
      my $desc = $module->description;
      $dists->{dist_abs} = $desc if $desc;
    }
  }
  return $dists;
}

sub soap_dist_search {
  my ($query, %args) = @_;
  $query =~ s!::!-!g;
  return unless (my $soap = make_info_soap());
  my $result = $soap->dist_info($query);
  eval {$result->fault};
  if ($@) {
      $ERROR = $@;
      return;
  }
  $result->fault and do {
    $ERROR = join ', ', 
      $result->faultcode, 
        $result->faultstring;
    return;
  };
  my $results = $result->result();
  if ($results) {
    my $email = $results->{email} || $results->{cpanid} . '@cpan.org';
    $results->{author} = $results->{fullname} . qq{ &lt;$email&gt; };
  }
  else {
    $ERROR = qq{No results for "$query" were found}
  };
  return $results;
}

=item cpan_file {

Given a file of the form C<file.tar.gz> and a CPAN id
of the form <ABCDEFG>, will return the CPAN file
C<A/AB/ABCDEFG/file.tar.gz>.

=cut

sub cpan_file {
  my ($cpanid, $file) = @_;
  (my $cpan_loc = $cpanid) =~ s{^(\w)(\w)(.*)}{$1/$1$2/$1$2$3};
  return qq{$cpan_loc/$file};
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
    my $results = mod_search($mod);
    unless ($dist = cpan_file($results->{cpanid}, $results->{dist_file})) {
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
  if (HAS_CPAN and defined $CPAN::Config->{urllist} and
      ref($CPAN::Config->{urllist}) eq 'ARRAY') {
    push @urls, @{$CPAN::Config->{urllist}};
  }
  push @urls, 'ftp://ftp.cpan.org', 'http://www.cpan.org';
  return @urls;
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
      my $atv = $Archive::Tar::VERSION + 0;
      if (not WIN32 or (WIN32 and $atv >= 1.08)) {
        $has{tar} = 'Archive::Tar';
        $has{gzip} = 'Compress::Zlib';
      }
      else {
         $has{tar} = 
	    $Config{tar} || which('tar') || $CPAN::Config->{tar};
          $has{gzip} =
	    $Config{gzip} || which('gzip') || $CPAN::Config->{gzip};
      }
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
    $^X || which('perl');
  
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

