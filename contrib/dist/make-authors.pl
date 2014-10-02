#!/usr/bin/env perl
#
# Copyright (c) 2008 Cisco Systems, Inc.  All rights reserved.
#

use strict;
use Data::Dumper;

# Ensure that we're in the root of a writeable Git clone
my $in_git_clone = 1;

$in_git_clone = 0
    if (! -d ".git" || ! -f "AUTHORS");

######################################################################

# Run git log to get a list of committers

my $committers;
open (GIT, "git log --pretty=format:%ae|") || die "Can't run 'git log'.";
while (<GIT>) {
    chomp;
    m/^\s*([\S]+)\s*$/;

    if (!exists($committers->{$1})) {
        $committers->{$1} = { };
        print "Found Git commit email: $1\n";
    }
}
close(GIT);

# Read the existing AUTHORS file to get the header, footer, and Git
# email ID -> (gecos, affiliation) mappings.

my $header;
my $footer;

print "Matching Git emails to existing names/affiliations...\n";

open (AUTHORS, "AUTHORS") || die "Can't open AUTHORS file";
my $in_header = 1;
my $in_footer = 0;
while (<AUTHORS>) {
    chomp;
    my $line = $_;

    # Slurp down header lines until we hit a line that begins with an
    # Git email
    if ($in_header) {
        foreach my $git_email (keys(%{$committers})) {
            if ($line =~ /$git_email\s+/) {
                $in_header = 0;
            }
        }
        if ($in_header) {
            $header .= "$_\n";
        }
    }

    # If we're in the body, parse to get the existing Git emails, gecos,
    # and affiliations
    if (!$in_header && !$in_footer) {

        # Make sure we have a line that begins with an Git email;
        # otherwise, fall through to the footer.
        my $found = undef;
        my $git_email;
        foreach $git_email (keys(%{$committers})) {
            if ($line =~ /$git_email\s+/) {
                $found = $git_email;
                last;
            }
        }
        if (!$found) {
            $in_footer = 1;
        } else {
            $line =~ m/^$found\s+(.+?)\s{2,}(.+)$/;
            my $gecos = $1;
            my $aff = $2;

            if ($gecos =~ /^\s+$/) {
                $gecos = "<UNKNOWN>";
            } else {
                $committers->{$found}->{gecos} = $gecos;
            }
            if ($aff =~ /^\s+$/) {
                $aff = "<UNKNOWN>";
            } else {
                $committers->{$found}->{affiliation} = $aff;
            }
            print "Git email $found matches: $gecos / $aff\n";
        }
    }

    # If we're in the footer, just save all the lines
    if ($in_footer) {
        $footer .= "$_\n";
    }
}
close(AUTHORS);

# Figure out the 3 column widths.  The last line of the header
# contains -'s for each of the columns.

$header =~ m/\n([\-\s]+?)$/m;
my $div_line = $1;
my @divs = split(/ /, $div_line);
my $id_col = length($divs[0]);
my $gecos_col = length($divs[1]);
my $aff_col = length($divs[2]);

# Print out a new AUTHORS file
open (AUTHORS, ">AUTHORS.new") || die "Can't write to AUTHORS file";
print AUTHORS $header;
my $i;
my $have_unknowns = 0;
foreach my $git_email (sort(keys(%${committers}))) {
    # Skip the automated accounts
    next
        if ($git_email eq "no-author\@open-mpi.org" ||
            $git_email eq "mpiteam\@open-mpi.org");

    print AUTHORS $git_email;
    $i = length($git_email);
    while ($i <= $id_col) {
        print AUTHORS ' ';
        ++$i;
    }

    # if we have gecos/affiliation, print them.  Otherwise, just end
    # the line here
    if ((exists($committers->{$git_email}->{gecos}) &&
         $committers->{$git_email}->{gecos} !~ /^\s+$/) ||
        (exists($committers->{$git_email}->{affiliation}) &&
         $committers->{$git_email}->{affiliation} !~ /^\s+$/)) {
        print AUTHORS $committers->{$git_email}->{gecos};
        $i = length($committers->{$git_email}->{gecos});
        while ($i <= $gecos_col) {
            print AUTHORS ' ';
            ++$i;
        }

        print AUTHORS $committers->{$git_email}->{affiliation}
            if (exists($committers->{$git_email}->{affiliation}));
    } else {
        $have_unknowns = 1;
    }
    print AUTHORS "\n";
}
print AUTHORS $footer;
close(AUTHORS);

unlink("AUTHORS");
rename("AUTHORS.new", "AUTHORS");

print "New AUTHORS file written.\n";
if ($have_unknowns) {
    print "*** WARNING: There were Git committers with unknown real names and/or\n*** affiliations.  You *MUST* edit the AUTHORS file to fill them in!\n";
} else {
    print "All Git emails were matched! No need to hand-edit the AUTHORS file.\n";
}

