#!perl
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';
use Test;
use strict;
BEGIN { plan tests => 26 };
use PPM::Make;
use Config;
use File::Path;
use File::Find;
ok(1); # If we made it this far, we're ok.

#########################

# Insert your test code below, the Test module is use()ed here so read
# its man page ( perldoc Test ) for help writing this test script.

my $ppm = PPM::Make->new();
ok($ppm);
my $name = 'PPM-Make';
my $ppd = $name . '.ppd';
my $tgz = $name . '.tar.gz';
$ppm->make_ppm();
if (-e $ppd) {
   ok(1);
}
else {
   ok('ppd not created', 1);
}
if (-e $tgz) {
   ok(1);
}
else {
   ok('archive not created', 1);
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
  finddepth(sub {next if $File::Find::name =~ m!blib/man\d!; 
		 push @f, $File::Find::name; 
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
unlink ($ppd, $tgz);
my $os = 'homer-simpson';
my $arch = 'c-wren';
my $url = 'http://www.disney.com/ppmpackages/';
my $script = 'README';
my $exec = 'notepad.exe';
my @args = ($ppm->{has}->{perl}, '-Mblib', 'make_ppm',
	'-n', '-o', $os, '-a', $arch, '-b', $url, 
        '-s', $script, '-e', $exec);
system(@args) == 0 or die "system @args failed: $?";

if (-e $ppd) {
   ok(1);
}
else {
   ok('ppd not created', 1);
}
if (-e $tgz) {
   ok(1);
}
else {
   ok('archive not created', 1);
}
$d = PPM::Make::parse_ppd($ppd);
ok($d);
ok($d->{SOFTPKG}->{NAME}, $name);
ok($d->{TITLE}, $name);
ok($d->{ABSTRACT}, $abstract);
ok($d->{AUTHOR}, $author);
ok($d->{OS}->{NAME}, $os);
ok($d->{ARCHITECTURE}->{NAME}, $arch);
ok($d->{CODEBASE}->{HREF}, $url . $arch . '/' . $tgz); 
ok($d->{INSTALL}->{SCRIPT}, $script);
ok($d->{INSTALL}->{EXEC}, $exec);

@f = ();
@files = ();
finddepth(sub {push @f, $File::Find::name; 
	       print $File::Find::name,"\n"}, 'blib');

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
