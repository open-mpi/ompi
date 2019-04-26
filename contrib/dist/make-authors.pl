#!/usr/bin/env perl
#
# Copyright (c) 2008-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
#

use strict;

use Data::Dumper;
use Getopt::Long;
use Cwd;

# Ensure that we're in the root of a writeable Git clone
my $in_git_clone = 1;
my $skip_ok = 0;
my $quiet = 0;
my $srcdir = ".";
my $destdir = getcwd();

GetOptions("skip-ok" => \$skip_ok,
	   "quiet" => \$quiet,
	   "srcdir=s" => \$srcdir,
	   "destdir=s" => \$destdir)
    or die("Error in command line arguments\n");

# we still work with git old enough to not have the -C option, and the
# --git-dir option screws up .mailmap, so just jump into the source
# directory and make life easier.
chdir($srcdir);

if (! -e ".git") {
    if ($skip_ok == 0) {
	print STDERR "I don't seem to be in a git repo :(\n";
	exit(1);
    } else {
	# called from make dist, just exit quietly (for case where
	# user runs "make dist" from a dist tarball)
	exit(0);
    }
}

######################################################################

my $people;

######################################################################

# Run git log to get a list of committers

open (GIT, "git log --no-merges --format=tformat:'%aN <%aE>'|") || die "Can't run 'git log'.";
while (<GIT>) {
    chomp;
    m/^\s*(.+)\s+<(.+)>\s*$/;

    my $email = lc($2);

    # special case from the SVN migration
    if ($email eq 'no-author@open-mpi.org') { next; }
    # skip the mpi bot...
    if ($email eq 'mpiteam@open-mpi.org') { next; }

    if (!exists($people->{$1})) {
        # The person doesn't exist, so save a new entry
        $people->{$1} = {
            name => $1,
            emails => {
                $email => 1,
            }
        };

        if ($quiet == 0) { print STDOUT "Found Git committer: $1 <$email>\n"; }
    } else {
        # The person already exists, so just add (or overwrite) this
        # email address
        $people->{$1}->{emails}->{$email} = 1;
    }
}
close(GIT);

if (scalar(keys(%{$people})) == 0) {
    print STDERR "Found no author entries, assuming git broke.  Aborting!\n";
    exit(1);
}

######################################################################

# Output a new AUTHORS file

open (AUTHORS, ">$destdir/AUTHORS") || die "Can't write to AUTHORS file";

my $header = <<'END_HEADER';
Open MPI Authors
================

The following cumulative list contains the names and email addresses
of all individuals who have committed code to the Open MPI repository
(either directly or through a third party, such as through a
Github.com pull request).  Note that these email addresses are not
guaranteed to be current; they are simply a unique indicator of the
individual who committed them.


END_HEADER
print AUTHORS $header;

my $email_dups;

my @sorted_people = sort(keys(%{$people}));
foreach my $p (@sorted_people) {
    print AUTHORS "$p\n";

    foreach my $e (sort(keys(%{$people->{$p}->{emails}}))) {
        # Sanity check: make sure this email address does not show up
        # with any other person/name
        my $dup;
        foreach my $p2 (@sorted_people) {
            next
                if ($p eq $p2);

            foreach my $e2 (keys(%{$people->{$p2}->{emails}})) {
                if ($e eq $e2) {
                    $dup = $p2;

                    # Record this so that we can warn about it
                    if ($p le $p2) {
                        $email_dups->{$p} = $p2;
                    } else {
                        $email_dups->{$p2} = $p;
                    }
                    last;
                }
            }

            last
                if (defined($dup));
        }

        print AUTHORS "  $e";
        print AUTHORS " (**** DUPLICATE EMAIL ADDRESS WITH $dup ***)"
            if (defined($dup));
        print AUTHORS "\n";
    }
}
close(AUTHORS);

print STDOUT "New AUTHORS file written.\n";

######################################################################

# Output any relevant warnings

my $warned = 0;

my @k = sort(keys(%{$email_dups}));
if ($#k >= 0) {
    $warned = 1;
    print STDERR "\n*** WARNING: The following people had the same email address:\n";
    foreach my $p (@k) {
        print STDERR "***   $p, $email_dups->{$p}\n";
    }
}

if ($warned) {
    print STDERR "
*******************************************************************************
*** YOU SHOULD EDIT THE .mailmap FILE TO RESOLVE THESE WARNINGS!
*******************************************************************************\n";
}

exit($warned);
