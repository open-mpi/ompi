#!/usr/bin/env perl
#
# Copyright (c) 2008 Cisco Systems, Inc.  All rights reserved.
#

use strict;
use Data::Dumper;
use File::Find;

# Find all files in the tree (except LICENSE) and look for all
# copyright notices.  Build a consolidated list and print it out for a
# human to check and include in the LICENSE file.

my $copyrights;

# Ensure that we're in the top of an SVN or hg directory.

my $good = 0;
$good = 1
    if (-d ".hg");
$good = 1
    if (-d ".svn" && -f "README.WINDOWS.txt" && -f "VERSION");
die "Must be in root of OMPI tree"
    if (!$good);

# Find all interesting files (skip the top-level LICENSE file)
my @files;
&File::Find::find(
    sub {
        push(@files, $File::Find::name)
            if ($_ ne "." && $_ ne ".." && 
                !($_ eq "LICENSE" && $File::Find::dir eq ".") &&
                $_ !~ /~$/ && $_ !~ /\.bak$/ && $_ !~ /\.orig$/ &&
                -f $_ && ! -l $_ &&
                $File::Find::dir !~ /\.svn/ &&
                $File::Find::dir !~ /\.libs/ &&
                $File::Find::dir !~ /\.deps/);
    },
                  ".");

print "Found $#files files.  Checking each one...\n";

# Go through each of them and look for copyrights

my $copyrights;
my $core;

sub save {
    my ($org, $year, $file) = @_;

    # Remove leading and trailing spaces
    $org =~ s/^\s*(\S[\S\s]+?)\s*$/\1/;
    # Remove any of "All rights reserved." at the end
    $org =~ s/\.$//;
    $org =~ s/ *reserved//i;
    $org =~ s/ *rights//i;
    $org =~ s/ *all//i;
    $org =~ s/\.$//;

    # Save a range of years
    if ($year =~ m/([0-9]{4})-([0-9]{4})/) {
        my $y = $1;
        while ($y <= $2) {
            save($org, $y, $file);
            ++$y;
        }
        return;
    }
    
    # Save a single year
    if (!exists($copyrights->{$core}->{$org}->{$year}->{$file})) {
        $copyrights->{$core}->{$org}->{$year}->{$file} = 1;
    } else {
        ++$copyrights->{$core}->{$org}->{$year}->{$file}
    }
}

foreach my $f (@files) {
    # Is this core OMPI or non-core?
    $core = 1;
    $core = 0
        if ($f =~ /ompi\/contrib\/[a-zA-Z0-9]+\// ||
            $f =~ /opal\/event/ ||
            $f =~ /ompi\/mca\/io\/romio\/romio/);

    # Scan the file for copyrights
    open FILE, $f || die "Can't open file: $f";
    my $year;
    my $current;
    while (<FILE>) {
        my $line = $_;
        # End of all copyrights in this file
        if ($line =~ /\$COPYRIGHT\$/) {
            save($current, $year, $f)
                if (defined($current));
            last;
        }
        # Beginning of a new copyright
        elsif ($line =~ m/Copyright \(c\) ([0-9\-]+) (.+)$/) {
            # Save the last copyright
            save($current, $year, $f)
                if (defined($current));
            $year = $1;
            $current = $2;
        }
        # Beginning of something else
        elsif (defined($current) && 
               ($line =~ /\s*\*\s*$/ ||
                $line =~ /^\s*$/ ||
                $line =~ /^\s*\#\s*$/)) {
            save($current, $year, $f);
            $current = undef;
        }
    }
    close(FILE);
}

# Check for duplicate copyrights in the same file
foreach my $c (qw/1 0/) {
    foreach my $org (sort(keys(%{$copyrights->{$c}}))) {
        foreach my $year (sort(keys(%{$copyrights->{$c}->{$org}}))) {
            foreach my $f (keys(%{$copyrights->{$c}->{$org}->{$year}})) {
                if ($copyrights->{$c}->{$org}->{$year}->{$f} > 1) {
                    print "WARNING: repeated copyright in $f:\n    $org ($year)\n";
                }
            }
        }
    }
}

# Check for weird copyright years
my ($sec,$min,$hour,$mday,$mon,$year_now,$wday,$yday,$isdst) = localtime(time);
$year_now += 1970;
foreach my $c (qw/1 0/) {
    foreach my $org (sort(keys(%{$copyrights->{$c}}))) {
        foreach my $year (sort(keys(%{$copyrights->{$c}->{$org}}))) {
            if ($year < 1996 || $year > $year_now) {
                print "WARNING: Suspicious copyright year ($org:$year):\n";
                foreach my $f (keys(%{$copyrights->{$c}->{$org}->{$year}})) {
                    print "    $f\n";
                }
            }
        }
    }
}

# Print out what we found
print "Found copyrights:\n";
foreach my $c (qw/1 0/) {
    print "========= Core: $c\n";
    foreach my $org (sort(keys(%{$copyrights->{$c}}))) {
        print "$org: " . join(",", 
                             sort(keys(%{$copyrights->{$c}->{$org}}))) . "\n";
    }
}

open OUT, ">out.txt" || die "can't open out";
my $d = new Data::Dumper([$copyrights]);
$d->Purity(1)->Indent(1);
my $s = $d->Dump;
print OUT $s;
close(OUT);

print "Done!\n";
