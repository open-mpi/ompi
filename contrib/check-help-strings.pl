#!/usr/bin/env perl
#
# Copyright (c) 2014-2022 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
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

my @source_files;
my @help_files;

# Helper: Search for all source and help files
sub match_files {
    # Don't process sym links
    return
        if (-l $_);

    # Don't recurse down "special" directories
    if (-d $_ &&
        ((/^\.deps$/) || (/^\.libs$/) ||
         (/3rd-party/) ||
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
    if ($short =~ /^help-.*\.txt$/) {
        push(@help_files, {
            full => $File::Find::name,
            short => $short,
            relative => $relative,
             });
        verbose("    Found help file: $short\n");
    }

    if ($short =~ /\.c$/ ||
        $short =~ /\.h$/ ||
        $short =~ /\.cc$/) {
        push(@source_files, {
            full => $File::Find::name,
            short => $short,
            relative => $relative,
             });
        verbose("    Found source file: $short\n");
    }
}

# Find all source and help files
print "Searching for source and help files...\n";
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

print "Indexing help files (from entire source tree)...\n";

foreach my $info (@help_files) {
    verbose("Indexing help: $info->{full}\n");

    # Check for short name collision
    if (exists($help_topics->{$info->{short}})) {

        # Found a collision!  Find the original's full name.
        my $collide_relative = "unknown";
        foreach my $i (@help_files) {
            if ($i->{short} eq $info->{short}) {
                $collide_relative = $i->{relative};
                last;
            }
        }

        # Print error message
        print "*** ERROR: Help file name collision:
  File 1: $info->{relative}
  File 2: $collide_relative\n";
        ++$num_errors;
    }

    # Read in file, find all of its topics
    my $num_topics = 0;
    open(FH, $info->{full}) || die "Can't open $info->{full}";
    while (<FH>) {
        if (m/^\s*\[(.+?)\]\s*$/) {
            my $topic = $1;
            verbose("  Topic: $topic\n");
            $help_topics->{$info->{short}}->{topic}->{$topic} = 0;
            $help_topics->{$info->{short}}->{full} = $info->{full};
            ++$num_topics;
        }
    }
    close(FH);

    if (0 == $num_topics) {
        print "*** WARNING: Empty help file (no topics)
  Help file: $info->{full}\n";
        ++$num_warnings;
    }
}

###########################################################################

# Search source files for calls to opal_show_help and (o)rte_show_help

if ($start eq $top) {
    print "Searching source files (from entire source tree)...\n";
} else {
    print "Searching source files (under $startrel)...\n";
}

# Helper: for a given filename/topic, see if it exists
sub check_file_topic {
    my $info = shift;
    my $file = shift;
    my $topic = shift;

    verbose("Found $info->{short}: $file / $topic\n");

    # Do we have a help file for this?
    if (!exists($help_topics->{$file})) {
        print "*** ERROR: Source-referenced help file does not exist
  Source file: $info->{relative}
  Help file referenced: $file\n";
        ++$num_errors;
    }

    # Do we have a topic in that help file for this?
    elsif (!exists($help_topics->{$file}->{topic}->{$topic})) {
        print "*** ERROR: Source-referenced help topic does not exist
  Source file: $info->{relative}
  Help file referenced: $file
              which is: $help_topics->{$file}->{full}
  Help topic referenced: $topic\n";
        ++$num_errors;
    }

    # Yes, we do have a topic in that help file for this.
    # Increase its ref count.
    else {
        ++$help_topics->{$file}->{topic}->{$topic};
    }
}

# Helper: search source file for a regexps matching a help filename
# and topic.
sub check_name {
    my $info = shift,
    my $name = shift;
    my $sep = shift;
    my $src = shift;

    while ($src =~ m/$name\s*$sep\s*"(.+?)"\s*,.*?"(.+?)"/) {
        my $file = $1;
        my $topic = $2;
        check_file_topic($info, $file, $topic);

        # Don't find this one again
        $src =~ s/$name\s*$sep\s*"(.+?)"\s*,.*?"(.+?)"/SHOW_HELP_REPLACED/;
    }

    return $src;
}


# Check to ensure helpfile/topic combos exist
foreach my $info (@source_files) {
    verbose("Searching source: $info->{full}\n");

    # If this source file is not in the target area, then skip it
    next
        if ($info->{relative} != /^$startrel/);

    my $src;
    open(FH, $info->{full}) || die "Can't open $info->{full}";
    while (<FH>) {
        # Eliminate newlines, just for regexp simplicity later
        chomp;
        $src .= $_;
    }
    close(FH);

    # Find calls to opal_show_help()
    $src = check_name($info, "opal_show_help", "\\(", $src);
    # Find calls to opal_show_help_string()
    $src = check_name($info, "opal_show_help_string", "\\(", $src);
    # Find calls to rte_show_help() (and also orte_show_help())
    $src = check_name($info, "rte_show_help", "\\(", $src);
    # Find special tokens from comments
    $src = check_name($info, "SHOW_HELP", ":", $src);
}

###########################################################################

# Check that all indexed help strings were referenced

print "Checking for stale help messages / files...\n";

foreach my $file (sort(keys(%{$help_topics}))) {
    my $num_used = 0;
    foreach my $topic (sort(keys(%{$help_topics->{$file}->{topic}}))) {
        if (0 == $help_topics->{$file}->{topic}->{$topic}) {
            print "*** WARNING: Possibly unused help topic
  Help file: $help_topics->{$file}->{full}
  Help topic: $topic\n";
            ++$num_warnings;
        } else {
            ++$num_used;
        }
    }

    # Were no topics used in this file at all?
    if (0 == $num_used) {
            print "*** WARNING: Possibly unused help file (no topics used from this file)
  Help file: $help_topics->{$file}->{full}\n";
            ++$num_warnings;
    }
}

###########################################################################

# All done
if (0 == $num_errors && 0 == $num_warnings) {
    print "+++ All seems good!\n";
    exit(0);
} else {
    print "Total number of warnings: $num_warnings
Total number of errors: $num_errors\n";
    exit(1);
}
