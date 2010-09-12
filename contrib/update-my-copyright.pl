#!/usr/bin/env perl
#
# Copyright (c) 2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#

# Short version: 
#
# This script automates the tedious task of updating copyright notices
# in the tops of OMPI/ORTE/OPAL source files before committing back to
# the respository.  Set the environment variable
# OMPI_COPYRIGHT_SEARCH_NAME to a short (case-insensitive) name that
# indicates your copyright line (e.g., "cisco"), and set the env
# variable OMPI_COPYRIGHT_FORMAL_NAME with your organization's formal
# name and copyright statement (e.g., "Cisco Systems, Inc.  All rights
# reserved.") before running the script.

# More details:
#
# This is a simple script to traverse the tree looking for added and
# changed files (via "svn st ." or "hg st .", depending on what meta
# directory is found in this tree).  Note that the search starts in
# the current directory -- not the top-level directory.
#
# All added and changed files are examined.  If the special
# "$COPYRIGHT$" token is found, then lines above that token are
# examined to find the "search" copyright name.
#
# - If the search name is found, that line is examined to see if the
#   current year is in the copyright year range.  If it is not, the line
#   is modified to include the current year.
# - If the search name is not found, a new line is created in the
#   copyright block of the file using the formal name and the current
#   year.
#
# NOTE: this script currently doesn't handle multi-line copyright
# statements, such as:
# 
# Copyright (c) 2010 University of Blabbityblah and the Trustees of
#                    Schblitbittyboo.  All rights reserved.
#
# Someone could certainly extend this script to do so, if they cared
# (my organizations' copyright fits on a single line, so I wasn't
# motivated to handle the multi-line case :-) ).
#

use strict;
use Cwd;

# Defaults
my $my_search_name = "Cisco";
my $my_formal_name = "Cisco Systems, Inc.  All rights reserved.";

# Override the defaults if some values are set in the environment
$my_search_name = $ENV{OMPI_COPYRIGHT_SEARCH_NAME}
    if (defined($ENV{OMPI_COPYRIGHT_SEARCH_NAME}));
$my_formal_name = $ENV{OMPI_COPYRIGHT_FORMAL_NAME}
    if (defined($ENV{OMPI_COPYRIGHT_FORMAL_NAME}));
print "==> Copyright search name: $my_search_name\n";
print "==> Copyright formal name: $my_formal_name\n";

# Get the year
my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime;
$year += 1900;
print "==> This year: $year\n";

# Find the top-level OMPI source tree dir
my $start = cwd();
my $top = $start;
while (! -f "$top/Makefile.man-page-rules") {
    chdir("..");
    $top = cwd();
    die "Can't find top-level Open MPI directory"
        if ($top eq "/");
}
chdir($start);

print "==> Top-level Open MPI dir: $top\n";
print "==> Current directory: $start\n";

# Are we hg or svn?
my $cmd = "svn st .";
die "This is both an SVN and HG tree -- can't decide what to do!"
    if (-d "$top/.svn" && -d "$top/.hg");
$cmd = "hg st ."
    if (-d "$top/.hg");

# Run the command, parsing the output.  Make a list of files that are
# added or modified.
print "==> Running: \"$cmd\"\n";
open(CMD, "$cmd|") || die "Can't run command";
my @files;
while (<CMD>) {
    chomp;
    if ($_ =~ /^M/ || $_ =~ /^A/) {
        my ($state, $filename) = split(/\s+/, $_);
        push(@files, $filename);
    }
}
close(CMD);

if ($#files < 0) {
    print "No added / changed files -- nothing to do\n";
    exit(0);
}

# Examine each of the files and see if they need an updated copyright
foreach my $f (@files) {
    print "Processing added/changed file: $f\n";
    open(FILE, $f) || die "Can't open file: $f";

    # Read in the file, and look for the "$COPYRIGHT$" token; that's
    # the end of the copyright block that we're allowed to edit.  Do
    # not edit any copyright notices that may appear below that.

    my $i = 0;
    my @lines;
    my $my_line_index;
    my $token_line_index;
    while (<FILE>) {
        push(@lines, $_);
        $token_line_index = $i
            if ($_ =~ /\$COPYRIGHT\$/);
        $my_line_index = $i
            if (!defined($token_line_index) && $_ =~ /$my_search_name/i);
        ++$i;
    }
    close(FILE);

    # If there was not copyright token, don't do anything
    if (!defined($token_line_index)) {
        print "==> WARNING: Did not find the \$COPYRIGHT\$ token!\n";
        print "    File left unchanged\n";
        next;
    }

    # Figure out the line prefix
    $lines[$token_line_index] =~ m/^(.+)\$COPYRIGHT\$/;
    my $prefix = $1;

    # Now act on it
    if (!defined($my_line_index)) {
        print "--- My copyright line not found; adding:\n";
        my $str = "${prefix}Copyright (c) $year $my_formal_name\n";
        print "    $str";
        $lines[$token_line_index] = $str . $lines[$token_line_index];
    } else {
        print "--- Found existing copyright line:\n";
        print "    $lines[$my_line_index]";
        $lines[$my_line_index] =~ m/([\d+\-]+)/;
        my $years = $1;
        die "Could not find years in copyright line!"
            if (!defined($years));

        # If it's a range, separate them out
        my $first_year;
        my $last_year;
        if ($years =~ /\-/) {
            $years =~ m/(\d+)\s*-\s*(\d+)/;
            $first_year = $1;
            $last_year = $2;
        } else {
            $first_year = $last_year = $years;
        }

        # Sanity check
        die "Copyright looks like it extends before 1990...?"
            if ($first_year < 1990);
        die "Copyright in the future...?"
            if ($last_year > $year);

        # Do we need to do anything?
        if ($year > $last_year) {
            $lines[$my_line_index] = "${prefix}Copyright (c) $first_year-$year $my_formal_name\n";
            print "    Updated to:\n";
            print "    $lines[$my_line_index]";
        } else {
            print "    This year already included in copyright; not changing file\n";
            next;
        }
    }

    # If we got this far, we want to write out a new file
    my $newf = "$f.new-copyright";
    unlink($newf);
    open(FILE, ">$newf") || die "Can't open file: $newf";
    print FILE join(//, @lines);
    close(FILE);

    # Now replace the old one
    unlink($f);
    rename($newf, $f);
}
