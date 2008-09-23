#!/usr/bin/env perl
#
# Copyright (c) 2008 Cisco Systems, Inc.  All rights reserved.
#

use strict;
use Data::Dumper;

# Ensure that we're in the root of a writeable SVN trunk checkout
my $in_svn_tree = 1;

$in_svn_tree = 0
    if (! -d ".svn" || ! -f "AUTHORS");
if ($in_svn_tree) {
    open (SVN, "svn info . |") || die "Can't run 'svn info .'";
    while (<SVN>) {
        chomp;
        if ($_ =~ m/^URL: (.+)$/) {
            $in_svn_tree = 0;
            if ($1 eq "https://svn.open-mpi.org/svn/ompi/trunk") {
                $in_svn_tree = 1;

                # Can't just "last" here of svn will print an error
                # about a broken pipe to stderr -- so consume the rest
                # of the output
                while (<SVN>) { }
                last;
            }
        }
    }
    close(SVN);
}

die "Sorry, this script must be run at the root of a writable SVN trunk checkout"
    if (!$in_svn_tree);

######################################################################

my $committers;

# Run svn log to get the list of SVN IDs for committers

print "Running svn log... (will take a few minutes)\n";

open (SVN, "svn log --xml |") || die "Can't run 'svn log --xml'";
while (<SVN>) {
    if ($_ =~ m@^<author>(.+)</author>@) {
        if ($1 !~ /^\s*$/) {
            if (!exists($committers->{$1})) {
                $committers->{$1} = { };
                print "Found SVN commit ID: $1\n";
            }
        }
    }
}
close(SVN);

# Read the existing AUTHORS file to get the header, footer, and SVN ID
# -> (gecos, affiliation) mappings.

my $header;
my $footer;

print "Matching SVN ID's to existing names/affiliations...\n";

open (AUTHORS, "AUTHORS") || die "Can't open AUTHORS file";
my $in_header = 1;
my $in_footer = 0;
while (<AUTHORS>) {
    chomp;
    my $line = $_;

    # Slurp down header lines until we hit a line that begins with an
    # SVN ID
    if ($in_header) {
        foreach my $svn_id (keys(%{$committers})) {
            if ($line =~ /$svn_id\s+/) {
                $in_header = 0;
            }
        }
        if ($in_header) {
            $header .= "$_\n";
        }
    }

    # If we're in the body, parse to get the existing SVN IDs, gecos,
    # and affiliations
    if (!$in_header && !$in_footer) {

        # Make sure we have a line that begins with an SVN ID;
        # otherwise, fall through to the footer.
        my $found = undef;
        my $svn_id;
        foreach $svn_id (keys(%{$committers})) {
            if ($line =~ /$svn_id\s+/) {
                $found = $svn_id;
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
            print "SVN ID $found matches: $gecos / $aff\n";
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
foreach my $svn_id (sort(keys(%${committers}))) {
    print AUTHORS $svn_id;
    $i = length($svn_id);
    while ($i <= $id_col) {
        print AUTHORS ' ';
        ++$i;
    }
    
    # if we have gecos/affiliation, print them.  Otherwise, just end
    # the line here
    if ((exists($committers->{$svn_id}->{gecos}) &&
         $committers->{$svn_id}->{gecos} !~ /^\s+$/) ||
        (exists($committers->{$svn_id}->{affiliation}) &&
         $committers->{$svn_id}->{affiliation} !~ /^\s+$/)) {
        print AUTHORS $committers->{$svn_id}->{gecos};
        $i = length($committers->{$svn_id}->{gecos});
        while ($i <= $gecos_col) {
            print AUTHORS ' ';
            ++$i;
        }

        print AUTHORS $committers->{$svn_id}->{affiliation}
            if (exists($committers->{$svn_id}->{affiliation}));
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
    print "*** WARNING: There were SVN committers with unknown real names and/or\n*** affiliations.  You *MUST* edit the AUTHORS file to fill them in!\n";
} else {
    print "All SVN ID's were matched! No need to hand-edit the AUTHORS file.\n";
}

