use ExtUtils::MakeMaker;
use strict;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
use Config;
require File::Spec;
use constant WIN32 => $^O eq 'MSWin32';

my %prereqs = ('Pod::Find' => 0.23,
	       'File::Spec' => 0,
	       'LWP' => 0,
	       'XML::Parser' => 2,
	       'Getopt::Long' => 2.32,
	       'Config::IniFiles' => 0,
	       );

my $home = $ENV{HOME};
my $has_myconfig = 0;
if ($home) {
  eval 
    {require File::Spec->catfile($home, '.cpan', 'CPAN', 'MyConfig.pm');};
  $has_myconfig = 1 unless $@;
}

unless ($has_myconfig) {
  eval {require CPAN::Config;};
}

if ($@) {
  die <<'DEATH';
    
It appears you have yet to configure the CPAN.pm module.
PPM::Make requires this to map module to distribution names. 
You can configure CPAN.pm by typing

    C:\> perl -MCPAN -e shell

and following through the dialogue.

DEATH
}

my @path_ext = ();
path_ext();

eval {require Archive::Tar; require Compress::Zlib};
if ($@) {
  my $tar = $Config{tar} || $CPAN::Config->{tar} || which('tar');
  my $gzip = $Config{gzip} || $CPAN::Config->{gzip} || which('gzip');
  if ($tar and $gzip) {
    print "Will use $tar and $gzip for .tar.gz archives\n";
  }
  else {
    print <<'WARN';

I could not find software to create .tar.gz archives.
I can add Compress::Zlib and Archive::Tar to the list 
of prerequisites to install, if you like.

WARN
    my $ans = prompt('Add Compress::Zlib and Archive::Tar?', 'yes');
    if ($ans =~ /^y/i) {
      $prereqs{'Archive::Tar'} = 0.08;
      $prereqs{'Compress::Zlib'} = 1.0;
    }
    else {
      die <<'DEATH';

Software to handle .tar.gz could not be found. Please
reconsider either the Compress::Zlib and Archive::Tar modules,
or else external programs.

DEATH
    }
  }
}
else {
  print "Will use Compress::Zlib and Archive::Tar for .tar.gz archives\n";
}


eval {require Archive::Zip;};
if ($@) {
  my $zip = $Config{zip} || $CPAN::Config->{zip} || which('zip');
  my $unzip = $Config{unzip} || $CPAN::Config->{unzip} || which('unzip');
  if ($zip and $unzip) {
    print "Will use $zip and $unzip for .zip archives\n";
  }
  else {
    print <<'WARN';

I could not find software to handle .zip archives.
I can add Archive::Zip to the list of prerequisites to 
install, if you like.

WARN
    my $ans = prompt('Add Archive::Zip?', 'yes');
    if ($ans =~ /^y/i) {
      $prereqs{'Archive::Zip'} = 1.02;
    }
    else {
      print <<'WARN';

The package will still be able to function in creating
tar.gz archives, but not zip archives.

WARN
    }
  }
}
else {
  my $zipv = $Archive::Zip::VERSION + 0;
  if ($zipv < 1.02) {
    print <<'ZIP';

Archive::Zip version 1.02 or greater is needed.
I can add this to the list of prerequisites, if you like.

ZIP
    my $ans = prompt('Add Archive::Zip?', 'yes');
    if ($ans =~ /^y/i) {
      $prereqs{'Archive::Zip'} = 1.02;
    }
    else {
      print <<'WARN';

The package will still be able to function in creating
tar.gz archives, but not zip archives.

WARN
    }
  }
  else {
    print "Will use Archive::Zip for .zip archives\n";
  }
}

my $make = $Config{make} || $CPAN::Config->{make} || which('make');
if ($make) {
  print "Will use '$make' as your make program\n";
}
else {
  if (WIN32) {
    print <<'NMAKE';

I cannot find a make program. If you are running
an ActivePerl-compatible Win32 perl, you can obtain nmake from
http://download.microsoft.com/download/vc15/Patch/1.52/W95/EN-US/Nmake15.exe.
I can fetch and install that for you, if you like.

NMAKE
  my $ans = prompt('Fetch nmake?', 'yes');
    if ($ans =~ /^y/i) {
      fetch_nmake() or die "Could not install nmake";
    }
    else {
      die 'Cannot function without nmake';
    }
  }
  else {
    die "Cannot find a make program\n";
  }
}

eval{require PPM; };
if ($@) {
  print <<'WARN';

I could not find the PPM module, used to install packages.
I can add PPM to the list of prerequisites to  install, if 
you like.

WARN
  my $ans = prompt('Add PPM?', 'yes');
  if ($ans =~ /^y/i) {
    $prereqs{'PPM'} = 0;
  }
  else {
    print <<'WARN';

The package will still be able to function in creating
ppm files, but cannot install them.

WARN
    
  }
}

WriteMakefile(
	      NAME	     => 'PPM::Make',
	      VERSION_FROM => 'Make.pm',
	      EXE_FILES    => ['make_ppm', 'ppm_install'],
              dist         => {COMPRESS => 'gzip', SUFFIX => 'gz'},
	      PREREQ_PM    => \%prereqs,
	      ($] >= 5.005 ?   
	       (ABSTRACT_FROM => 'Make.pm',
		AUTHOR     => 'Randy Kobes <randy@theory.uwinnipeg.ca>') : ()),
	     );

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

sub fetch_nmake {
  my $nmake = 'nmake15.exe';
  my $r = 'http://download.microsoft.com/download/vc15/Patch/1.52/W95/EN-US/Nmake15.exe';
  eval {require LWP::Simple; import LWP::Simple qw(getstore)};
  if ($@) {
    warn "LWP::Simple is needed to get $nmake";
    return;
  }
  getstore($r, $nmake);
  unless (-e $nmake) {
    warn "Getting $nmake failed";
    return;
  }
  my @args = ($nmake);
  system(@args);
  my ($exe, $err) = ('nmake.exe', 'nmake.err');
  unless (-e $exe and -e $err) {
    warn "Extraction of $exe and $err failed";
    return;
  }
  use File::Copy;
  my $dir = prompt('Which directory on your PATH should I copy the files to?',
		   $Config{bin});
  unless (-d $dir) {
    my $ans = prompt(qq{$dir doesn\'t exist. Create it?}, 'yes');
    if ($ans =~ /^y/i) {
      mkdir $dir or do {
	warn "Could not create $dir: $!";
	return;
      };
    }
    else {
      warn "Will not create $dir";
      return;
    }
  }
  for ($exe, $err, 'README.TXT') {
    move($_, $dir) or do {
      warn "Moving $_ to $dir failed: $!";
      return;
    };
  }
  unlink $nmake or warn "Unlink of $nmake failed: $!";
  return 1;
}
