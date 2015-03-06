#!/usr/bin/env perl
#
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015 Los Alamos National Security, LLC.  All rights reserved.
# $COPYRIGHT$
#
# Simple script to check all the opal_show_help (and orte_show_help)
# strings against what is found in help files.
#

use strict;

use Cwd;
use File::Find;
use Getopt::Long;
use Data::Dumper;

my $num_warnings = 0;
my $num_errors = 0;

###########################################################################

my $VERBOSE = 0;
my $HELP = 0;

GetOptions(
    "help|h" => \$HELP,
    "verbose|v" => \$VERBOSE,
) or die "unable to parse options, aborted";

if ($HELP) {
    print <<EOF;
%0 [options]

--help | h          This help message
--verbose | v       Be verbose in output
EOF
    exit(0);
}

###########################################################################

sub verbose {
    print @_
        if ($VERBOSE);
}

sub DebugDump {
    my $d = new Data::Dumper([@_]);
    $d->Purity(1)->Indent(1);
    my $s = $d->Dump;
    print $s;
}

sub isTopDir {
    my ($d) = @_;

    # trunk
    if (-f "$d/Makefile.ompi-rules") {
        return 1;
    }

    # v1.8
    if (-f "$d/Makefile.man-page-rules") {
        return 1;
    }

    return 0;
}

###########################################################################

# Find the top-level OMPI source tree dir
my $start = cwd();
my $top = $start;
while (!isTopDir($top)) {
    chdir("..");
    $top = cwd();
    die "Can't find top-level Open MPI directory"
        if ($top eq "/");
}
chdir($start);

###########################################################################

my @owner_files;

# Helper: Search for all owner files
sub match_files {
    # Don't process sym links
    return
        if (-l $_);

    # Don't recurse down "special" directories
    if (-d $_ &&
        ((/^\.deps$/) || (/^\.libs$/) ||
         (/^\.svn$/) || (/^\.hg$/) || (/^\.git$/))) {
        $File::Find::prune = 1;
        return;
    }

    # $File::Find::name is the path relative to the starting point.
    # $_ contains the file's basename.  The code automatically changes
    # to the processed directory, so we want to open / close $_.

    verbose("--> $File::Find::name\n");

    my $relative = $File::Find::name;
    $relative =~ s/^$top//;
    $relative =~ s/^\///;

    my $short = $_;
    if ($short =~ "owner.txt") {
        push(@owner_files, {
            full => $File::Find::name,
            short => $short,
            relative => $relative,
             });
        verbose("    Found owner file: $short\n");
    }
}

# Find all owner files
print "Searching for owner files...\n";
my $startrel = $start;
if ($top ne $start) {
    $startrel =~ s/^$top//;
    $startrel =~ s/^\///;
}
find(\&match_files, ".");

###########################################################################

# Index all help files
my $help_topics;
my $help_file_refs;

print "Indexing owner files (from entire source tree)...\n";

my $old_lib = "";
my $filename;

foreach my $info (@owner_files) {
    verbose("Indexing owner: $info->{full}\n");
    open(OWNERFILE, $info->{full}) || die "Could not open file $info->{full}\n";
    my $owner="unknown";
    my $status="unknown";
    my $label="unknown";
    while (<OWNERFILE>) {
        next if /^#/;
        chop;
        if (/^owner/) {
            ($label,$owner) = split(/:/);
            $owner =~ s/\s//ge;  # get rid of white space, may need something better
        }
        if (/^status/) {
            ($label,$status) = split(/:/);
            $status =~ s/\s//ge;
        }
    }
    my @components = split(/\//,$info->{full});
    shift(@components);
    my $lib = shift(@components);
    if ($lib ne $old_lib) {
        $filename = $lib."_mca_owner.md";
        printf("hey, found a new lib %s filename %s\n",$lib,$filename);
        open(MDFILE, ">$filename") || die "Could not open file $filename\n";
        printf(MDFILE "| Framework | Component | Owner | Status |\n");
        printf(MDFILE "| --- | --- | --- | --- |\n");
        $old_lib = $lib;
    }
    shift(@components);
    my $frame = shift(@components);
    my $comp = shift(@components);
    printf("For lib %s framework %s component %s owner %s status %s\n", $lib, $frame, $comp, $owner, $status);
    printf(MDFILE "| $frame | $comp | $owner | $status |\n");
    close (OWNERFILE);
}
