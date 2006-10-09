package PPM::Make::Util;
use strict;
use warnings;
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
use File::HomeDir;
use HTML::Entities qw(encode_entities encode_entities_numeric);
use File::Spec;
use PPM::Make::Config qw(WIN32 HAS_CPAN HAS_PPM HAS_MB);

=head1 NAME

  PPM::Make::Util - Utility functions for PPM::Make

=head1 SYNOPSIS

  use PPM::Make::Util qw(:all);

=head1 DESCRIPTION

This module contains a number of utility functions used by PPM::Make.

=over 2

=cut

our ($VERSION);
$VERSION = '0.87';

my %encode = ('&' => '&amp;', '>' => '&gt;',
	      '<' => '&lt;', '"' => '&quot;');

use base qw(Exporter);

our (@EXPORT_OK, %EXPORT_TAGS, $protocol, $ext, $src_dir, $build_dir, $ERROR);
$protocol = qr{^(http|ftp)://};
$ext = qr{\.(tar\.gz|tgz|tar\.Z|zip)};
my @url_list = url_list();

my @exports = qw(load_cs verifyMD5 xml_encode parse_version $ERROR
                 is_core is_ap_core url_list
		 trim parse_ppd parse_ppm parse_abstract
                 ppd2cpan_version cpan2ppd_version tempfile
                 mod_search dist_search file_to_dist cpan_file
                 fetch_file fix_path);

%EXPORT_TAGS = (all => [@exports]);
@EXPORT_OK = (@exports);

my %ap_core = map {$_ => 1} qw(
			       Archive-Tar
			       Archive-Zip
			       Compress-Zlib
			       Data-Dump
			       Digest-HMAC
			       Digest-MD2
			       Digest-MD4
			       Digest-SHA1
			       File-CounterFile
			       Font-AFM
			       HTML-Parser
			       HTML-Tagset
			       HTML-Tree
			       IO-String
			       IO-Zlib
			       libwin32
			       libwww-perl
			       MD5
			       MIME-Base64-Scripts
			       SOAP-Lite
			       Term-ReadLine-Perl
			       TermReadKey
			       Text-Autoformat
			       Text-Reform
			       Tk
			       Unicode-String
			       URI
			       XML-Parser
			       XML-Simple  );

if (WIN32 and Win32::BuildNumber > 818) {
  $ap_core{'DBI'}++; $ap_core{'DBD-SQLite'}++;
}
src_and_build();

my %Escape = ('&' => 'amp',
	      '>' => 'gt',
	      '<' => 'lt',
	      '"' => 'quot'
	     );

my %dists;
my $info_soap;
my $info_uri = 'http://theoryx5.uwinnipeg.ca/Apache/InfoServer';
my $info_proxy = 'http://theoryx5.uwinnipeg.ca/cgi-bin/ppminfo.cgi';

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

=item xml_encode

Escapes E<amp>, E<gt>, E<lt>, and E<quot>, as well as high ASCII characters.

  my $escaped = xml_encode('Five is > four');

=cut

sub xml_encode {
    my $s = shift;
    return unless $s;
    $s =~ s/(&(?!(amp|lt|gt|quot);)|>|<|\")/$encode{$1}/g;
    return encode_entities_numeric($s, "\177-\377");
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
  return unless $m;
  $m =~ s!::|-!/!g;
  $m .= '.pm';
  my $is_core = (-e File::Spec->catfile($Config{privlibexp}, $m)) ? 1 : 0;
  return $is_core;
}

=item is_ap_core

Tests to see if a package is part of the ActivePerl core (at
least for recent ActivePerl versions).

  my $is_ap_core = is_ap_core('libwin32');
  print "libwin32 is a core package" if $is_ap_core;

=cut

sub is_ap_core {
  my $p = shift;
  return unless defined $p;
  return defined $ap_core{$p} ? 1 : 0;
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


=item parse_ppd

Parse a I<ppd> file.

  my $ppd = 'package.ppd';
  my $d = parse_ppd($ppd);
  print $d->{ABSTRACT};
  print $d->{OS}->{NAME};

  my $e = parse_ppd($ppd, 'MSWin32-x86-multi-thread');
  print $e->{ABSTRACT};

This routine takes a required argument of a I<ppd> file and,
optionally, an ARCHITECTURE name to restrict the results to.
It returns a data structure containing the information of 
the ppd file:

    $d->{SOFTPKG}->{NAME}
    $d->{SOFTPKG}->{VERSION}
    $d->{TITLE}
    $d->{AUTHOR}
    $d->{ABSTRACT}
    $d->{PROVIDE}
    $d->{DEPENDENCY}
    $d->{OS}->{NAME}
    $d->{ARCHITECTURE}->{NAME}
    $d->{CODEBASE}->{HREF}
    $d->{INSTALL}->{EXEC}
    $d->{INSTALL}->{SCRIPT}
    $d->{INSTALL}->{HREF}

The I<PROVIDE> and I<DEPENDENDENCY> tags are array references
containing lists of, respectively, the prerequisites required and 
the modules supplied by the package, with keys of I<NAME> and
I<VERSION>.

If there is more than one I<IMPLEMENTATION> section in the
ppd file, all the results except for the I<SOFTPKG> elements and
I<TITLE>, I<AUTHOR>, and I<ABSTRACT> will be placed in 
a I<$d-E<gt>{IMPLENTATION}> array
reference. If an optional second argument is passed to 
I<parse_ppd($file, $arch)>, this will filter out all implementation
sections except for the specified I<ARCHITECTURE> given by I<$arch>.

=cut

my $i;

sub parse_ppd {
  my $file = shift;
  my $arch = shift;
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
  my $implem = $d->{IMPLEMENTATION};
  my $size = scalar @$implem;
  if ($size == 1) {
    $d->{PROVIDE} = $implem->[0]->{PROVIDE} || [];
    $d->{DEPENDENCY} = $implem->[0]->{DEPENDENCY} || [];
    $d->{OS}->{NAME} = $implem->[0]->{OS}->{NAME} || '';
    $d->{ARCHITECTURE}->{NAME} = $implem->[0]->{ARCHITECTURE}->{NAME} || '';
    $d->{CODEBASE}->{HREF} = $implem->[0]->{CODEBASE}->{HREF};
    $d->{INSTALL}->{EXEC} = $implem->[0]->{INSTALL}->{EXEC};
    $d->{INSTALL}->{SCRIPT} = $implem->[0]->{INSTALL}->{SCRIPT};
    $d->{INSTALL}->{HREF} = $implem->[0]->{INSTALL}->{HREF};
  }
  elsif (defined $arch) {
    my $flag = 0;
    my $i;
    for ($i=0; $i<$size; $i++) {
      if ($implem->[$i]->{ARCHITECTURE}->{NAME} eq $arch) {
	$flag++;
	last;
      }
    }
    return unless $flag;
    $d->{PROVIDE} = $implem->[$i]->{PROVIDE} || [];
    $d->{DEPENDENCY} = $implem->[$i]->{DEPENDENCY} || [];
    $d->{OS}->{NAME} = $implem->[$i]->{OS}->{NAME} || '';
    $d->{ARCHITECTURE}->{NAME} = $implem->[$i]->{ARCHITECTURE}->{NAME} || '';
    $d->{CODEBASE}->{HREF} = $implem->[$i]->{CODEBASE}->{HREF};
    $d->{INSTALL}->{EXEC} = $implem->[$i]->{INSTALL}->{EXEC};
    $d->{INSTALL}->{SCRIPT} = $implem->[$i]->{INSTALL}->{SCRIPT};
    $d->{INSTALL}->{HREF} = $implem->[$i]->{INSTALL}->{HREF};
  }
  return $d;
}

sub ppd_init {
  my $self = shift;
  $i = 0;
  $self->{_mydata} = {
		      SOFTPKG => {NAME => '', VERSION => ''},
		      TITLE => '',
		      AUTHOR => '',
		      ABSTRACT => '',
		      PROVIDE => [],
		      IMPLEMENTATION => [],
		      OS => {NAME => ''},
		      ARCHITECTURE => {NAME => ''},
		      CODEBASE => {HREF => ''},
		      DEPENDENCY => [],
		      INSTALL => {EXEC => '', SCRIPT => '', HREF => ''},
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
    ($tag eq 'PROVIDE') and do {
      my $name = $attrs{NAME};
      my $version = $attrs{VERSION};
      if ($version) {
	push @{$internal->{IMPLEMENTATION}->[$i]->{PROVIDE}},
	  {NAME => $name, VERSION => $version};
      }
      else {
	push @{$internal->{IMPLEMENTATION}->[$i]->{PROVIDE}},
	  {NAME => $name};	
      }
      last SWITCH;
    };
    ($tag eq 'CODEBASE') and do {
      $internal->{IMPLEMENTATION}->[$i]->{CODEBASE}->{HREF} =
	$attrs{HREF};
      last SWITCH;
    };
    ($tag eq 'OS') and do {
      $internal->{IMPLEMENTATION}->[$i]->{OS}->{NAME} =
	$attrs{NAME};
      last SWITCH;
    };
    ($tag eq 'ARCHITECTURE') and do {
      $internal->{IMPLEMENTATION}->[$i]->{ARCHITECTURE}->{NAME} =
	$attrs{NAME};
      last SWITCH;
    };
    ($tag eq 'INSTALL') and do {
      $internal->{IMPLEMENTATION}->[$i]->{INSTALL}->{EXEC} =
	$attrs{EXEC};
      $internal->{IMPLEMENTATION}->[$i]->{INSTALL}->{HREF} =
	$attrs{HREF};
      last SWITCH;
    };
    ($tag eq 'DEPENDENCY') and do {
      push @{$internal->{IMPLEMENTATION}->[$i]->{DEPENDENCY}}, 
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
    $internal->{$tag} .= xml_encode($string);
  }
  elsif ($tag and $tag eq 'INSTALL') {
    $internal->{IMPLEMENTATION}->[$i]->{INSTALL}->{SCRIPT} .= $string;
  }
  else {
    
  }
}

sub ppd_end {
  my ($self, $tag) = @_;
  $i++ if ($tag eq 'IMPLEMENTATION');
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
    $internal->{$tag} .= xml_encode($string);
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
                    $_;
                    return \$$2;
                  };
                 };
    local $^W = 0;
    $version = eval($eval);
    warn "Could not eval '$eval' in $parsefile: $@" if $@;
    last;
  }
  close $fh;
  return $version;
}

=item parse_abstract

Attempt to obtain an abstract from a module file.

  my $package = 'CPAN';
  my $file = 'C:/Perl/lib/CPAN.pm';
  my $abstract = parse_abstract($package, $file);

=cut

sub parse_abstract {
  my ($package, $file) = @_;
  my $basename = basename($file, qr/\.\w+$/);
  (my $stripped = $basename) =~ s!\.\w+$!!;
  (my $trans = $package) =~ s!-!::!g;
  my $result;
  my $inpod = 0;
  open(my $fh, $file) or die "Couldn't open $file: $!";
  while (<$fh>) {
    $inpod = /^=(?!cut)/ ? 1 : /^=cut/ ? 0 : $inpod;
    next if !$inpod;
    chop;
    next unless /^\s*($package|$basename|$stripped|$trans)\s+--*\s+(.*)/;
    $result = $2;
    last;
  }
  close($fh);
  chomp($result);
  return $result;
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

If an array reference is passed to C<mod_search> containing
a list of modules to be queried, a corresponding
hash reference is returned, the keys being the query terms
and the values being a hash reference as above.

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
  my $ref = ref($query) eq 'ARRAY' ? 1 : 0;
  my @mods = $ref ? (@$query) : ($query);
  my $results;
  foreach my $m (@mods) {
    my @objs = CPAN::Shell->expand('Module', qq{/$m/});
    unless (@objs > 0) {
      $ERROR = "No results found for $query";
      return;
    }
    my $mods;
    foreach my $obj(@objs) {
      my $string = $obj->as_string;
      my $mod;
      if ($string =~ /id\s*=\s*(.*?)\n/m) {
        $mod = $1;
        next unless $mod;
      }
      next unless $mod eq $m;
      $mods->{mod_name} = $mod;
      if (my $v = $obj->cpan_version) {
        $mods->{mod_vers} = $v;
      }
      if ($string =~ /\s+DESCRIPTION\s+(.*?)\n/m) {
        $mods->{mod_abs} = $1;
      }
      if ($string =~ /\s+CPAN_USERID.*\s+\((.*)\)\n/m) {
        $mods->{author} = $1;
	$mods->{cpanid} = $mods->{author};
      }
      if ($string =~ /\s+CPAN_FILE\s+(\S+)\n/m) {
        $mods->{dist_file} = $1;
      }
      $mods->{dist_name} = file_to_dist($mods->{dist_file});
      last;
    }
    if ($ref) {
      $results->{$m} = $mods; 
    }
    else {
      $results = $mods;
      last;
    }
  }
  return $results;
}

sub soap_mod_search {
  my ($query, %args) = @_;
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
    if (ref($query) eq 'ARRAY') {
      foreach my $entry (keys %$results) {
        my $info = $results->{$entry};
        my $email = $info->{email} || $info->{cpanid} . '@cpan.org';
        $info->{author} = $info->{fullname} . qq{ &lt;$email&gt; };
      }
    }
    else {
      my $email = $results->{email} || $results->{cpanid} . '@cpan.org';
      $results->{author} = $results->{fullname} . qq{ &lt;$email&gt;};
    }
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

If an array reference is passed to C<dist_search> with a list
of distributions to be queried, a corresponding
hash reference is returned, the keys being the query terms
and the values being a hash reference as above.

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
  my $ref = ref($query) eq 'ARRAY' ? 1 : 0;
  my @dists = $ref ? (@$query) : ($query);
  my $results;
  foreach my $d (@dists) {
    my $dists;
    foreach my $match (CPAN::Shell->expand('Distribution', qq{/$d/})) {
      my $string = $match->as_string;
      my $cpan_file;
      if ($string =~ /id\s*=\s*(.*?)\n/m) {
	$cpan_file = $1;
	next unless $cpan_file;
      }
      my ($dist, $version) = file_to_dist($cpan_file);
      next unless $dist eq $d;
      $dists->{dist_name} = $dist;
      $dists->{dist_file} = $cpan_file;
      $dists->{dist_vers} = $version;
      if ($string =~ /\s+CPAN_USERID.*\s+\((.*)\)\n/m) {
	$dists->{author} = $1;
	$dists->{cpanid} = $dists->{author};
      }
      my $mods;
      if ($string =~ /\s+CONTAINSMODS\s+(.*)/m) {
	$mods = $1;
      }
      next unless $mods;
      my @mods = split ' ', $mods;
      next unless @mods;
      (my $try = $dist) =~ s{-}{::}g;
      foreach my $mod(@mods) {
	my $module = CPAN::Shell->expand('Module', $mod);
	next unless $module;
	if ($mod eq $try) {
	  my $desc = $module->description;
	  $dists->{dist_abs} = $desc if $desc;
	}
	my $v = $module->cpan_version;
	$v = undef if $v eq 'undef';
	if ($v) {
	  push @{$dists->{mods}}, {mod_name => $mod, mod_vers => $v};
	}
	else {
	  push @{$dists->{mods}}, {mod_name => $mod};	
	}
      }
    }
    if ($ref) {
      $results->{$d} = $dists;
    }
    else {
      $results = $dists;
      last;
    }
  }
  return $results;
}

sub soap_dist_search {
  my ($query, %args) = @_;
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

  my $cpanid = 'GBARR';
  my $file = 'libnet-1.23.tar.gz';
  my $cpan_file = cpan_file($cpanid, $file);

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

will grab this file directly. Otherwise, if the file is
specified with an absolute path name, has
an extension I<\.(tar\.gz|tgz|tar\.Z|zip)>, and if the file
exists locally, it will use that; otherwise, it will assume
this is a CPAN distribution and grab it from a CPAN mirror:

  my $dist = 'A/AB/ABC/file.tar.gz';
  my $filename = fetch_file($dist);

which assumes the file lives under I<$CPAN/authors/id/>. If
neither of the above are satisfied, it will assume this
is, first of all, a module name, and if not found, a distribution
name, and if found, will fetch the corresponding CPAN distribution.

  my $mod = 'Net::FTP';
  my $filename = fetch_file($mod);

=cut

sub fetch_file {
  my ($dist, $no_case) = @_;
  my $to;
  if (-f $dist) {
    $to = basename($dist, $ext);
    unless ($dist eq $to) {
      copy($dist, $to) or die "Cannot cp $dist to $to: $!";
    }
    return $to;
  }
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
    unless ($results) {
      $mod =~ s!::!-!g;
      $results = dist_search($mod);
    }
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

1;

__END__

=back

=head1 COPYRIGHT

This program is copyright, 2003, 2006 by 
Randy Kobes <r.kobes@uwinnipeg.ca>.
It is distributed under the same terms as Perl itself.

=head1 SEE ALSO

L<PPM>.

=cut

