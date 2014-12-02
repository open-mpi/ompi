#!/usr/bin/env perl

use strict;
use warnings;

use POSIX qw(strftime);

my $builddir = shift(@ARGV);
my $version = shift(@ARGV);

# Sanity check
die "Must specify builddir, version"
    if (!defined($builddir) || !$builddir || ! -d $builddir ||
        !defined($version) || !$version);

my $today = strftime "%Y-%m-%d", localtime;

#------------------------------------------------------------------------------
# Helper function to re-write files
#------------------------------------------------------------------------------

sub subst {
    my $file = shift;

    my $orig;
    open(IN, $file) || die "Can't read $file: $!";
    $orig .= $_
        while (<IN>);
    close(IN);

    my $copy = $orig;
    $copy =~ s/\@VERSION\@/Libfabric v$version/g;
    $copy =~ s/\@DATE\@/$today/g;

    if ($copy ne $orig) {
        print "*** VERSION/DATE-ifying $file...\n";
        open(OUT, ">$file") || die "Can't write to $file: $!";
        print OUT $copy;
        close(OUT);
    }
}

###############################################################################
# Change into the new distribution tree
###############################################################################

chdir($builddir);
subst("README");

chdir("man");
opendir(my $dh, ".") || die "Can't open man directory: $!";
my @files = grep { /\.\d$/ && -f "./$_" } readdir($dh);
closedir $dh;

foreach my $file (@files) {
    subst($file);
}

exit(0);
