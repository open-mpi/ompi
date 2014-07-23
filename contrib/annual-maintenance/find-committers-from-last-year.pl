#!/usr/bin/env perl

use strict;

use Data::Dumper;

##########################################################################

# Read in the AUTHORS file

die "Run this script at the top of an OMPI SVN tree"
    if (! -r "AUTHORS");

print "Reading AUTHORS file...\n";
open(AUTHORS, "AUTHORS") || die "Can't open AUTHORS file";
my $in = 0;
my $authors;
my $count = 0;
while (<AUTHORS>) {
    chomp;

    if ($in) {
        if (length($_) == 0) {
            $in = 0;
            next;
        }

        # There's probably a good regexp that will sort this better,
        # but I'm a little too lazy at the moment.  :-( So just use
        # fixed widths for the name and affiliation fields, and then
        # strip off trailing whitespace.
        m/(\S+)\s+/;
        my $username = $1;
        my $name = substr($_, 14, 27);
        my $affiliation = substr($_, 42);
        $name =~ s/(\s+)$//;
        $affiliation =~ s/(\s+)$//;

        $authors->{$username} = {
            username => $username,
            name => $name,
            affiliation => $affiliation,
            active => 0,
        };
        ++$count;
    }

    else {
        $in = 1
            if (/^------.+ .+ .+-----$/);
    }
}
close(AUTHORS);
print "Read $count authors from AUTHORS file\n";

##########################################################################

# Read committers from SVN log over the past year

my $trunk = "https://svn.open-mpi.org/svn/ompi/trunk";

my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
    localtime(time);

$year += 1900;
my $today = sprintf("%04d-%02d-%02d", $year, $mon+1, $mday);
my $year_ago = sprintf("%04d-%02d-%02d", $year-1, $mon+1, $mday);

$count = 0;
my $committers;
print "Reading SVN log to find committers over past year...\n";
open(SVN, "svn log -v -r '{$today}:{$year_ago}' $trunk|") ||
    die "Can't open svn log";
while (<SVN>) {
    if (m/^(r\d+) \| (\S+) \| (\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d)/) {
        print "Indexed $1 ($3)\n";

        if (!exists($committers->{$2})) {
            $committers->{$2} = 1;
            ++$count;
        }

        if (!exists($authors->{$2})) {
            print "WARNING: committer $2 is not in AUTHORS!\n";
        } else {
            $authors->{$2}->{active} = 1;
        }
    }
}
close(SVN);
print "Found $count SVN committers over the last year\n";

##########################################################################

# Print list of committers over the past year

print "
AUTHORS with commits in the past year:
--------------------------------------\n";
foreach my $username (sort(keys(%{$authors}))) {
    if ($authors->{$username}->{active}) {
        print "$username -> $authors->{$username}->{name}, $authors->{$username}->{affiliation}\n";
    }
}

print "
AUTHORS with NO commits in the past year:
-----------------------------------------\n";
foreach my $username (sort(keys(%{$authors}))) {
    if (!$authors->{$username}->{active}) {
        print "$username -> $authors->{$username}->{name}, $authors->{$username}->{affiliation}\n";
    }
}

