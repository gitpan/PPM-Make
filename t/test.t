#!perl
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';
use Test;
use strict;
use Cwd;
my $cwd = getcwd;
BEGIN { plan tests => 48 };
use PPM::Make;
use Config;
use File::Path;
use File::Find;
ok(1); # If we made it this far, we're ok.

#########################

# Insert your test code below, the Test module is use()ed here so read
# its man page ( perldoc Test ) for help writing this test script.

my $ppm = PPM::Make->new(upload => {ppd => "$cwd/t"}, no_cfg => 1);
ok($ppm);
my $name = 'PPM-Make';
my $ppd = $name . '.ppd';
my $tgz = $name . '.tar.gz';
$ppm->make_ppm();
for ($ppd, $tgz, "t/$ppd", "t/$tgz") {
  if (-e $_) {
    ok(1);
  }
  else {
    ok(qq{'$_' not created}, 1);
  }
}

my $author = q{Randy Kobes &lt;randy@theory.uwinnipeg.ca&gt;};
my $abstract = q{Make a ppm package from a CPAN distribution};
my $d = PPM::Make::parse_ppd($ppd);
ok($d);
ok($d->{SOFTPKG}->{NAME}, $name);
ok($d->{TITLE}, $name);
ok($d->{ABSTRACT}, $abstract);
ok($d->{AUTHOR}, $author);
ok($d->{OS}->{NAME}, $Config{osname});
my $arch = $Config{archname};
if (length($^V) && ord(substr($^V, 1)) >= 8) {
   $arch .= sprintf("-%d.%d", ord($^V), ord(substr($^V, 1)));
}
ok($d->{ARCHITECTURE}->{NAME}, $arch);
ok($d->{CODEBASE}->{HREF}, $tgz); 

my $is_Win32 = ($d->{OS}->{NAME} =~ /Win32/i); 

my @f;
if ($is_Win32) {
    finddepth(sub { push @f, $File::Find::name
                        unless $File::Find::name =~ m!blib/man\d!;
                    print $File::Find::name,"\n"}, 'blib');
}
else {
    finddepth(sub {push @f, $File::Find::name; 
                   print $File::Find::name,"\n"}, 'blib');
}

my $tar = $ppm->{has}->{tar};
my $gzip = $ppm->{has}->{gzip};
my @files;
if ($tar eq 'Archive::Tar' and $gzip eq 'Compress::Zlib') {
   require Archive::Tar;
   require Compress::Zlib;
   my $tar = Archive::Tar->new($tgz, 1);
   @files = $tar->list_files();
}

else {
   open(TGZ, "$gzip -dc $tgz \| $tar tvf - |");
   while (<TGZ>) {
      chomp;
      s!.* (blib\S*)!$1!;
      push @files, $_;
  }
  close(TGZ) or die "$!\n";;
}

ok($#f, $#files);
unlink ($ppd, $tgz, "t/$ppd", "t/$tgz");
$arch = 'c-wren';
my $url = 'http://www.disney.com/ppmpackages/';
my $script = 'README';
my $exec = 'notepad.exe';
my @args = ($ppm->{has}->{perl}, '-Mblib', 'make_ppm',
        '-n', '-a', $arch, '-b', $url,
        '-s', $script, '-e', $exec, '--no_cfg');
system(@args) == 0 or die "system @args failed: $?";

for ($ppd, $tgz) {
  if (-e $_) {
    ok(1);
  }
  else {
    ok(qq{'$_' not created}, 1);
  }
}

$d = PPM::Make::parse_ppd($ppd);
ok($d);
ok($d->{SOFTPKG}->{NAME}, $name);
ok($d->{TITLE}, $name);
ok($d->{ABSTRACT}, $abstract);
ok($d->{AUTHOR}, $author);
ok($d->{OS}->{NAME}, $Config{osname});
ok($d->{ARCHITECTURE}->{NAME}, $arch);
ok($d->{CODEBASE}->{HREF}, $url . $arch . '/' . $tgz); 
ok($d->{INSTALL}->{SCRIPT}, $script);
ok($d->{INSTALL}->{EXEC}, $exec);

@f = ();
@files = ();
if ($is_Win32) {
    finddepth(sub { push @f, $File::Find::name
                        unless $File::Find::name =~ m!blib/man\d!;
                    print $File::Find::name,"\n"}, 'blib');
}
else {
    finddepth(sub {push @f, $File::Find::name; 
                   print $File::Find::name,"\n"}, 'blib');
}

if ($tar eq 'Archive::Tar' and $gzip eq 'Compress::Zlib') {
   require Archive::Tar;
   require Compress::Zlib;
   my $tar = Archive::Tar->new($tgz, 1);
   @files = $tar->list_files();
}

else {
   open(TGZ, "$gzip -dc $tgz \| $tar tvf - |");
   while (<TGZ>) {
      chomp;
      s!.* (blib\S*)!$1!;
      push @files, $_;
  }
  close(TGZ) or die "$!\n";;
}
ok($#f+1, $#files);
unlink ($ppd, $tgz);

$ENV{PPM_CFG} = "$cwd/t/ppm.cfg";
$ppm = PPM::Make->new(arch => 'foo');
ok($ppm);
my $opts = $ppm->{opts};
ok($opts);
ok( $opts->{upload}->{user}, 'sarah');
ok( $opts->{upload}->{passwd}, 'justina');
ok( $opts->{upload}->{ppd}, '/home/to/wherever');
ok( $opts->{upload}->{host}, 'a.galaxy.far.far.away');
ok( $opts->{upload}->{ar}, undef);
ok( $opts->{binary}, 'http://www.foo.com/bar');
ok( $opts->{vs}, 1);
$ppm = PPM::Make->new(arch => 'bar');
$opts = $ppm->{opts};
ok( $opts->{binary}, 'http://www.foo.com/bar');
ok( $opts->{vs}, undef);
ok( $opts->{upload}->{ppd}, '/path/to/ppds');
ok( $opts->{upload}->{ar}, 'x86');
$ppm = PPM::Make->new(arch => 'harry');
$opts = $ppm->{opts};
ok( $opts->{binary}, 'http://www.foo.com/bar');
ok( $opts->{vs}, 1);
ok( $opts->{upload}->{host}, undef);
$ppm = PPM::Make->new(arch => 'foo', vs => 0, binary => 'http://localhost',
		     upload => {ppd => '/another/path', user => 'lianne'});
$opts = $ppm->{opts};
ok( $opts->{binary}, 'http://localhost');
ok( $opts->{vs}, 0);
ok( $opts->{upload}->{ppd}, '/another/path');
ok( $opts->{upload}->{user}, 'lianne');

