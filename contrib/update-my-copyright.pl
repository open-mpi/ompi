#!/usr/bin/env perl
#
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016-2017 Intel, Inc. All rights reserved.
# Copyright (c) 2017      IBM Corporation. All rights reserved.
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

# Short version:
#
# This script automates the tedious task of updating copyright notices
# in the tops of OMPI/ORTE/OPAL source files before committing back to
# the repository.  Set the environment variable
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
use Getopt::Long;

# Set to true if the script should merely check for up-to-date copyrights.
# Will exit with status 111 if there are out of date copyrights which this
# script can correct.
my $CHECK_ONLY = 0;
# used by $CHECK_ONLY logic for bookkeeping
my $would_replace = 0;

# Set to true to suppress most informational messages.  Only out of date files
# will be printed.
my $QUIET = 0;

# Set to true if we just want to see the help message
my $HELP = 0;

# Defaults
my $my_search_name = "Cisco";
my $my_formal_name = "Cisco Systems, Inc.  All rights reserved.";
my $my_manual_list = "";

# Protected directories
my @protected = qw(
    opal\\/mca\\/pmix\\/pmix.+?\\/pmix\\/
    opal\\/mca\\/hwloc\\/hwloc.+?\\/hwloc\\/
    opal\\/mca\\/event\\/libevent.+?\\/libevent\\/
    contrib\\/update-my-copyright.pl
);

# Override the defaults if some values are set in the environment
$my_search_name = $ENV{OMPI_COPYRIGHT_SEARCH_NAME}
    if (defined($ENV{OMPI_COPYRIGHT_SEARCH_NAME}));
$my_formal_name = $ENV{OMPI_COPYRIGHT_FORMAL_NAME}
    if (defined($ENV{OMPI_COPYRIGHT_FORMAL_NAME}));
$my_manual_list = $ENV{OMPI_COPYRIGHT_MANUAL_LIST}
    if (defined($ENV{OMPI_COPYRIGHT_MANUAL_LIST}));

GetOptions(
    "help" => \$HELP,
    "quiet" => \$QUIET,
    "check-only" => \$CHECK_ONLY,
    "search-name=s" => \$my_search_name,
    "formal-name=s" => \$my_formal_name,
    "manual-list=s" => \$my_manual_list,
) or die "unable to parse options, stopped";

if ($HELP) {
    print <<EOT;
$0 [options]

--help | -h          This help message
--quiet | -q         Only output critical messages to stdout
--check-only         exit(111) if there are files with copyrights to edit
--search-name=NAME   Set search name to NAME
--formal-same=NAME   Set formal name to NAME
--manual-list=FNAME  Use specified file as list of files to mod copyright
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

# Find the top-level source tree dir in a git repo
my $start = cwd();
my $top = `git rev-parse --show-toplevel`;
chomp($top);
# The current directory relative to $top (e.g., "contrib/"), or the
# empty string at the top-level directory
my $prefix = `git rev-parse --show-prefix`;
chomp($prefix);

quiet_print "==> Top-level repository dir: $top\n";
quiet_print "==> Current directory: $start\n";

my @files = find_modified_files();

if ($#files < 0) {
    quiet_print "No added / changed files -- nothing to do\n";
    exit(0);
}

# Examine each of the files and see if they need an updated copyright
foreach my $f (@files) {

    # ignore embedded copies of external codes as we shouldn't
    # be overwriting their copyrights - if someone actually
    # modified any of those files, they can manually update
    # the copyright.  $f is relative to the current directory, but the
    # @protected patterns are relative to the top-level directory, so
    # match against the latter.
    my $top_relative = "$prefix$f";
    my $ignore = 0;
    foreach my $p (@protected) {
        if (eval("\$top_relative =~ /$p/")) {
            quiet_print "Ignoring protected file $f\n";
            $ignore = 1;
            last;
        }
    }
    if (1 == $ignore) {
        next;
    }
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
        # Now replace the old one, keeping its permissions (e.g., the
        # execute bits on scripts)
        my $mode = (stat($f))[2] & 07777;
        chmod($mode, $newf);
        unlink($f);
        rename($newf, $f);
    }
}

if ($CHECK_ONLY and $would_replace) {
    exit(111);
}

#-------------------------------------------------------------------------------

# Returns a list of file names (relative to pwd) which git considers
# to be modified.
sub find_modified_files {
    my %seen;
    my @files = ();

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

            if (-f $relname && !$seen{$relname}++) {
                push @files, $relname;
            }
        }
    }

    # Also include files changed in commits on this branch that have not
    # yet been pushed / are not in the base branch.  This covers the common
    # case of running the script after committing.
    #
    # Strategy: of the candidate base refs below, use the nearest one --
    # the one with the fewest commits between its merge-base with HEAD
    # and HEAD.  Those commits are this branch's own.  Candidates:
    #   1. The upstream tracking branch -- but not if it is the current
    #      branch pushed to a remote.
    #   2. Well-known names: origin/main, origin/master, main, master.
    # Taking the nearest one matters: a stale local "main" that is behind
    # origin/main would otherwise make everything merged upstream since
    # then look like this branch's changes.  If the nearest base has no
    # commits between it and HEAD (e.g., a new branch created from
    # origin/main), this branch has no commits of its own.
    my $current_branch = `git rev-parse --abbrev-ref HEAD 2>/dev/null`;
    chomp($current_branch);

    my $base_ref = "";
    my $base_count;
    my @candidates;

    # Returns the number of commits on HEAD that are not on the given
    # ref, or undef if the ref does not exist or shares no history with
    # HEAD.
    my $commits_beyond = sub {
        my ($ref) = @_;
        my $sha = `git rev-parse --verify $ref 2>/dev/null`;
        chomp($sha);
        return undef unless $sha;
        my $mb = `git merge-base HEAD $ref 2>/dev/null`;
        chomp($mb);
        return undef unless $mb;
        my $count = `git rev-list --count $mb..HEAD 2>/dev/null`;
        chomp($count);
        return $count;
    };

    # Upstream tracking branch (skip if it tracks the same branch on the remote)
    my $upstream = `git rev-parse --abbrev-ref \@{upstream} 2>/dev/null`;
    chomp($upstream);
    if ($upstream) {
        # e.g. "origin/bigcount-datatypes" tracks the same branch — skip it
        my $upstream_branch = $upstream;
        $upstream_branch =~ s!^[^/]+/!!;   # strip "origin/" prefix
        # Also skip it if it already contains HEAD: then it is this
        # branch as pushed (perhaps under a different name), and tells us
        # nothing about where the branch started.
        my $count = $commits_beyond->($upstream);
        push @candidates, $upstream
            if ($upstream_branch ne $current_branch && $count);
    }

    # Remote default branch and common well-known names (avoid origin/HEAD —
    # it can be a stale symref pointing to the wrong branch)
    push @candidates, "origin/main", "origin/master", "main", "master";

    for my $candidate (@candidates) {
        my $count = $commits_beyond->($candidate);
        next unless defined($count);
        if (!defined($base_count) || $count < $base_count) {
            $base_ref = $candidate;
            $base_count = $count;
        }
    }

    if ($base_ref && 0 == $base_count) {
        quiet_print "==> Using base ref '$base_ref': no commits on this branch\n";
    } elsif ($base_ref) {
        my $merge_base = `git merge-base HEAD $base_ref 2>/dev/null`;
        chomp($merge_base);
        quiet_print "==> Using base ref '$base_ref' (merge-base: $merge_base)\n";
        my $diff_cmd = "git diff --name-only --diff-filter=ACMR $merge_base HEAD -- .";
        quiet_print "==> Running: \"$diff_cmd\"\n";
        my @diff_files = split /\n/, `$diff_cmd`;
        for my $fullname (@diff_files) {
            my $relname = $fullname;
            $relname =~ s!^([^/]*/){$n_strip}!!g;
            if (-f $relname && !$seen{$relname}++) {
                push @files, $relname;
            }
        }
    } else {
        quiet_print "==> WARNING: Could not determine base branch for branch diff\n";
    }

    return @files;
}
