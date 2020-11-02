#!/usr/bin/env perl
#
# Copyright (c) 2020 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This script is friendly to both Python 2 and Python 3.

use strict;

use IPC::Open3;
use File::Basename;
use Getopt::Long;

#--------------------------------------------------------------------------

my $source_arg;
my $dest_arg;
my $pandoc_arg = "pandoc";
my $help_arg;
my $verbose_arg;

my $ok = Getopt::Long::GetOptions("source=s" => \$source_arg,
                                  "dest=s" => \$dest_arg,
                                  "pandoc=s" => \$pandoc_arg,
                                  "help" => \$help_arg,
                                  "verbose" => \$verbose_arg);

if (!$source_arg || !$dest_arg) {
    print("Must specify --source and --dest\n");
    $ok = 0;
}

if (!$ok || $help_arg) {
    print "Invalid command line argument.\n\n"
        if (!$ok);
    print "Options:
  --source FILE  Source Markdown filename
  --dest FILE    Destination nroff file
  --pandoc FILE  Location of pandoc executable
  --help         This help list
  --verbose      Be verbose when running\n";
    exit($ok ? 0 : 1);
}

#--------------------------------------------------------------------------

# If the destination exists, read it in
my $dest_content;
if (-f $dest_arg) {
    open(FILE, $dest_arg) ||
        die "Can't open $dest_arg";
    $dest_content .= $_
        while(<FILE>);
    close(FILE);
}

#--------------------------------------------------------------------------

# Read in the source
die "Error: $source_arg does not exist"
    if (! -f $source_arg);

my $source_content;
open(FILE, $source_arg) ||
    die "Can't open $source_arg";
$source_content .= $_
    while(<FILE>);
close(FILE);

#--------------------------------------------------------------------------

# Figure out the section of man page
die "Cannot figure out man page section from source filename"
    if (!($source_arg =~ m/(\d+).md$/));
my $man_section = $1;

my $shortfile = basename($source_arg);
$shortfile =~ s/\.$man_section\.md$//;

#--------------------------------------------------------------------------

my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();
my $today = sprintf("%04d-%02d-%02d", ($year+1900), $mon, $mday);

# Run opal_get_version.sh to get the OMPI version.
my $config_dir   = dirname($0);
my $get_version  = "$config_dir/opal_get_version.sh";
my $VERSION_file = "$config_dir/../VERSION";
my $out          = `$get_version $VERSION_file --full`;
chomp($out);

# Pandoc does not handle markdown links in output nroff properly, so
# just remove all links.  Specifically: some versions of Pandoc ignore
# the links, but others handle it badly.
$source_content =~ s/\[(.+)\]\((.+)\)/\1/g;

# Add the pandoc header
$source_content = "---
section: $man_section
title: $shortfile
header: Open PMIx
footer: $today
---

$source_content";

#--------------------------------------------------------------------------

print("*** Processing: $source_arg --> $dest_arg\n")
    if ($verbose_arg);

# Run Pandoc
my $pid = open3(my $child_stdin, my $child_stdout, my $child_stderr,
                "$pandoc_arg -s --from=markdown --to=man");
print $child_stdin $source_content;
close($child_stdin);
my $pandoc_rendered;
$pandoc_rendered .= $_
    while(<$child_stdout>);
close($child_stdout);
close($child_stderr)
    if ($child_stderr);
waitpid($pid, 0);

# Write the output to the target file
open(FILE, ">$dest_arg") ||
    die "Can't open $dest_arg for writing";
print FILE $pandoc_rendered;
close(FILE);

exit(0);
