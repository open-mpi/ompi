#!/usr/bin/env perl
#
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#

# Short version:
#
# This script automates the tedious task of updating copyright notices
# in the tops of PMIX source files before committing back to
# the respository.  Set the environment variable
# PMIX_COPYRIGHT_SEARCH_NAME to a short (case-insensitive) name that
# indicates your copyright line (e.g., "cisco"), and set the env
# variable PMIX_COPYRIGHT_FORMAL_NAME with your organization's formal
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
use Getopt::Long;

# Set to true if the script should merely check for up-to-date copyrights.
# Will exit with status 111 if there are out of date copyrights which this
# script can correct.
my $CHECK_ONLY = 0;
# used by $CHECK_ONLY logic for bookeeping
my $would_replace = 0;

# Set to true to suppress most informational messages.  Only out of date files
# will be printed.
my $QUIET = 0;

# Set to true if we just want to see the help message
my $HELP = 0;

# Defaults
my $my_search_name = "Intel";
my $my_formal_name = "Intel, Inc.  All rights reserved.";

# Override the defaults if some values are set in the environment
$my_search_name = $ENV{PMIX_COPYRIGHT_SEARCH_NAME}
    if (defined($ENV{PMIX_COPYRIGHT_SEARCH_NAME}));
$my_formal_name = $ENV{PMIX_COPYRIGHT_FORMAL_NAME}
    if (defined($ENV{PMIX_COPYRIGHT_FORMAL_NAME}));

GetOptions(
    "help" => \$HELP,
    "quiet" => \$QUIET,
    "check-only" => \$CHECK_ONLY,
    "search-name=s" => \$my_search_name,
    "formal-name=s" => \$my_formal_name,
) or die "unable to parse options, stopped";

if ($HELP) {
    print <<EOT;
$0 [options]

--help | -h          This help message
--quiet | -q         Only output critical messages to stdout
--check-only         exit(111) if there are files with copyrights to edit
--search-name=NAME   Set search name to NAME
--formal-same=NAME   Set formal name to NAME
EOT
    exit(0);
}

#-------------------------------------------------------------------------------
# predeclare sub for print-like syntax
sub quiet_print {
    unless ($QUIET) {
        print @_;
    }
}

#-------------------------------------------------------------------------------

quiet_print "==> Copyright search name: $my_search_name\n";
quiet_print "==> Copyright formal name: $my_formal_name\n";

# Get the year
my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime;
$year += 1900;
quiet_print "==> This year: $year\n";

# Find the top-level PMIx source tree dir
my $start = cwd();
my $top = $start;
while (! -f "$top/VERSION") {
    chdir("..");
    $top = cwd();
    die "Can't find top-level PMIx directory"
        if ($top eq "/");
}
chdir($start);

quiet_print "==> Top-level PMIx dir: $top\n";
quiet_print "==> Current directory: $start\n";

# Select VCS used to obtain modification info.  Choose in increasing priority
# order (last hit wins).
my $vcs;
$vcs = "git"
    if (-d "$top/.git");
$vcs = "hg"
    if (-d "$top/.hg");
$vcs = "svn"
    if (-d "$top/.svn");

my @files = find_modified_files($vcs);

if ($#files < 0) {
    quiet_print "No added / changed files -- nothing to do\n";
    exit(0);
}

# Examine each of the files and see if they need an updated copyright
foreach my $f (@files) {
    quiet_print "Processing added/changed file: $f\n";
    open(FILE, $f) || die "Can't open file: $f";

    # Read in the file, and look for the "$COPYRIGHT$" token; that's
    # the end of the copyright block that we're allowed to edit.  Do
    # not edit any copyright notices that may appear below that.

    my $i = 0;
    my $found_copyright = 0;
    my $found_me = 0;
    my @lines;
    my $my_line_index;
    my $token_line_index;
    while (<FILE>) {
        push(@lines, $_);
        if (!$found_copyright && $_ =~ /\$COPYRIGHT\$/) {
            $token_line_index = $i;
            $found_copyright = 1;
        }
        if (!$found_me &&
            !defined($token_line_index) && $_ =~ /$my_search_name/i) {
            $my_line_index = $i;
            $found_me = 1;
        }
        ++$i;
    }
    close(FILE);

    # If there was not copyright token, don't do anything
    if (!defined($token_line_index)) {
        quiet_print "==> WARNING: Did not find the \$COPYRIGHT\$ token!\n";
        quiet_print "    File left unchanged\n";
        next;
    }

    # Figure out the line prefix
    $lines[$token_line_index] =~ m/^(.+)\$COPYRIGHT\$/;
    my $prefix = $1;

    # Now act on it
    if (!defined($my_line_index)) {
        quiet_print "--- My copyright line not found; adding:\n";
        my $str = "${prefix}Copyright (c) $year      $my_formal_name\n";
        quiet_print "    $str";
        $lines[$token_line_index] = $str . $lines[$token_line_index];
    } else {
        quiet_print "--- Found existing copyright line:\n";
        quiet_print "    $lines[$my_line_index]";
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
            quiet_print "    Updated to:\n";
            quiet_print "    $lines[$my_line_index]";
        } else {
            quiet_print "    This year already included in copyright; not changing file\n";
            next;
        }
    }

    # If we got this far, we want to write out a new file
    my $newf = "$f.new-copyright";
    unlink($newf);
    open(FILE, ">$newf") || die "Can't open file: $newf";
    print FILE join('', @lines);
    close(FILE);

    if ($CHECK_ONLY) {
        # intentional "loud" print to be more useful in a pre-commit hook
        print "==> '$f' has a stale/missing copyright\n";
        unlink($newf);
        ++$would_replace;
    }
    else {
        # Now replace the old one
        unlink($f);
        rename($newf, $f);
    }
}

if ($CHECK_ONLY and $would_replace) {
    exit(111);
}

#-------------------------------------------------------------------------------

# Takes two arguments, the top level directory and the VCS method.  Returns a
# list of file names (relative to pwd) which the VCS considers to be modified.
sub find_modified_files {
    my $vcs = shift;
    my @files = ();

    if ($vcs eq "git") {
        # Number of path entries to remove from ${top}-relative paths.
        # (--show-cdup either returns the empty string or sequence of "../"
        # entries, always ending in a "/")
        my $n_strip = scalar(split(m!/!, scalar(`git rev-parse --show-cdup`))) - 1;

        # "." restricts scope, but does not get us relative path names
        my $cmd = "git status -z --porcelain --untracked-files=no .";
        quiet_print "==> Running: \"$cmd\"\n";
        my $lines = `$cmd`;

        # From git-status(1):
        # X          Y     Meaning
        # -------------------------------------------------
        #           [MD]   not updated
        # M        [ MD]   updated in index
        # A        [ MD]   added to index
        # D         [ M]   deleted from index
        # R        [ MD]   renamed in index
        # C        [ MD]   copied in index
        # [MARC]           index and work tree matches
        # [ MARC]     M    work tree changed since index
        # [ MARC]     D    deleted in work tree
        # -------------------------------------------------
        # D           D    unmerged, both deleted
        # A           U    unmerged, added by us
        # U           D    unmerged, deleted by them
        # U           A    unmerged, added by them
        # D           U    unmerged, deleted by us
        # A           A    unmerged, both added
        # U           U    unmerged, both modified
        # -------------------------------------------------
        # ?           ?    untracked
        # -------------------------------------------------
        foreach my $line (split /\x{00}/, $lines) {
            my $keep = 0;
            my ($s1, $s2, $fullname) = $line =~ m/^(.)(.) (.*)$/;

            # ignore all merge cases
            next if ($s1 eq "D" and $s2 eq "D");
            next if ($s1 eq "A" and $s2 eq "A");
            next if ($s1 eq "U" or $s2 eq "U");

            # only update for actually added/modified cases, no copies,
            # renames, etc.
            $keep = 1 if ($s1 eq "M" or $s2 eq "M");
            $keep = 1 if ($s1 eq "A");

            if ($keep) {
                my $relname = $fullname;
                $relname =~ s!^([^/]*/){$n_strip}!!g;

                push @files, $relname
                    if (-f $relname);
            }
        }
    }
    elsif ($vcs eq "hg" or $vcs eq "svn") {
        my $cmd = "$vcs st .";

        # Run the command, parsing the output.  Make a list of files that are
        # added or modified.
        quiet_print "==> Running: \"$cmd\"\n";
        open(CMD, "$cmd|") || die "Can't run command";
        while (<CMD>) {
            chomp;
            if ($_ =~ /^M/ || $_ =~ /^A/) {
                my @tokens = split(/\s+/, $_);
                # Handle output of both forms:
                # M       filenameA
                # A  +    filenameB
                my $filename = $tokens[1];
                $filename = $tokens[2]
                if ($tokens[1] =~ /\+/);
                # Don't bother saving directory names
                push(@files, $filename)
                    if (-f $filename);
            }
        }
        close(CMD);
    }
    else {
        die "unknown VCS '$vcs', stopped";
    }

    return @files;
}
