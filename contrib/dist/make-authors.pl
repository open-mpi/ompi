#!/usr/bin/env perl
#
# Copyright (c) 2008-2016 Cisco Systems, Inc.  All rights reserved.
#

use strict;

use Data::Dumper;

# Ensure that we're in the root of a writeable Git clone
my $in_git_clone = 1;

$in_git_clone = 0
    if (! -d ".git" || ! -f "AUTHORS");

######################################################################

my $header_sep = "-----";
my $unknown_org = "********* NO ORGANIZATION SET ********";

my $people;

######################################################################

# Run git log to get a list of committers

open (GIT, "git log --format=tformat:'%aN <%aE>'|") || die "Can't run 'git log'.";
while (<GIT>) {
    chomp;
    m/^\s*(.+)\s+<(.+)>\s*$/;

    if (!exists($people->{$1})) {
        # The person doesn't exist, so save a new entry
        $people->{$1} = {
            name => $1,
            org => $unknown_org,
            emails => {
                lc($2) => 1,
            }
        };


        print "Found Git committer: $1 <$2>\n";
    } else {
        # The person already exists, so just add (or overwrite) this
        # email address
        $people->{$1}->{emails}->{$2} = 1;
    }
}
close(GIT);

######################################################################

# Read the existing AUTHORS file

my $header;

print "Matching Git emails to existing names/affiliations...\n";

sub save {
    my $current = shift;

    print "Saving person from AUTHORS: $current->{name}\n";

    # We may overwrite an entry written from the git log, but that's
    # ok
    $people->{$current->{name}} = $current;
}

open (AUTHORS, "AUTHORS") || die "Can't open AUTHORS file";
my $in_header = 1;
my $current = undef;
while (<AUTHORS>) {
    chomp;
    my $line = $_;

    # Slurp down header lines until we hit a line that begins with
    # $header_sep
    if ($in_header) {
        $header .= "$line\n";

        if ($_ =~ /^$header_sep/) {
            $in_header = 0;

            # There should be a blank line after this, too
            $header .= "\n";
        }
        next;
    }

    # Skip blank lines
    next
        if ($line =~ /^\s*$/);

    # Format of body:
    #
    # NAME, Affiliation 1[, Affiliation 2[...]]
    #   Email address 1
    #   [Email address 2]
    #   [...]
    # NAME, Affiliation 1[, Affiliation 2[...]]
    #   Email address 1
    #   [Email address 2]
    #   [...]

    # Found a new email address for an existing person
    if ($line =~ /^  /) {
        m/^  (.+)$/;
        $current->{emails}->{lc($1)} = 1;

        next;
    } else {
        # Found a new person; save the old entry
        save($current)
            if (defined($current));

        $current = undef;
        $current->{org} = $unknown_org;
        if ($line =~ m/^(.+?),\s+(.+)$/) {
            $current->{name} = $1;
            $current->{org} = $2;
        } else {
            $current->{name} = $line;
        }

        next;
    }
}

save($current)
    if (defined($current));

close(AUTHORS);

######################################################################

# Output a new AUTHORS file

open (AUTHORS, ">AUTHORS.new") || die "Can't write to AUTHORS file";

print AUTHORS $header;

my @people_with_unknown_orgs;
my $email_dups;

my @sorted_people = sort(keys(%{$people}));
foreach my $p (@sorted_people) {
    print AUTHORS $p;
    if (exists($people->{$p}->{org})) {
        my $org = $people->{$p}->{org};
        if ($org ne $unknown_org) {
            print AUTHORS ", $org";
        } else {
            # Record this so that we can warn about it
            push(@people_with_unknown_orgs, $p);
        }
    }
    print AUTHORS "\n";

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

# We have a new AUTHORS file!  Replace the old one.
unlink("AUTHORS");
rename("AUTHORS.new", "AUTHORS");

print "New AUTHORS file written.\n";

######################################################################

# Output any relevant warnings

my $warned = 0;
if ($#people_with_unknown_orgs >= 0) {
    $warned = 1;
    print "\n*** WARNING: The following people have unspecified organiations:\n";
    foreach my $p (@people_with_unknown_orgs) {
        print "***   $p\n";
    }
}

my @k = sort(keys(%{$email_dups}));
if ($#k >= 0) {
    $warned = 1;
    print "\n*** WARNING: The following people had the same email address:\n";
    foreach my $p (@k) {
        print "***   $p, $email_dups->{$p}\n";
    }
}

if ($warned) {
    print "
*******************************************************************************
*** YOU SHOULD EDIT THE .mailmap AND/OR AUTHORS FILE TO RESOLVE THESE WARNINGS!
*******************************************************************************\n";
}

exit($warned);
