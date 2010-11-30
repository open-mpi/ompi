#!/usr/bin/env perl
#
# Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# This script is run to see the differences between some other PML and
# the ob1 PML.  By default, it looks at the bfo PML but it can also be
# used with the csum PML.  Prior to running the diff, it does some
# preprocessing on the files.  First, it removes all the copyright
# headers as differences in them are of no concern.  Secondly, it
# converts all the PML specific strings in the PML to be compared to
# "ob1".  For example, with the bfo, all "bfo" strings are converted
# to "ob1" and all "BFO" strings are converted to "OB1".  In this way,
# we avoid any spurious differences just related to the difference in
# the names of the functions and variables.
#
# Lastly, in the case of bfo only, it can strip out all code within
# the PML that is contained within specific #ifdef strings.  See the
# code and comments below to see how it works.
#
# This script must be run from this directory as it makes assumptions
# about where the PML directories are located.  Here are some
# examples.
#
# Run using all defaults.
#  > check-ob1-pml-diffs.pl
#
# Run against the csum pml.
#  > check-ob1-pml-diffs.pl -p csum
#
# Do not remove the BFO specific code
#  > check-ob1-pml-diffs.pl -s
#
# Do not remove the BFO specific code and save results in DIFFS
#  > check-ob1-pml-diffs.pl -s -o DIFFS
#
#
use strict;

use File::Copy;
use File::Path;
use Getopt::Long;

my $diffdir = "diffdir";
my $pmlsdir = "../ompi/mca/pml";
my $cmd;
my $cmd_output;
my $contents;
my $bfofile;
my @bfofiles;
my $ob1file;
my @ob1files;
my $alloutput;


# Command line parsing
my $verbose_arg = 0;
my $show_arg = 1;
my $showall_arg = 0;
my $help_arg = 0;
my $pml_arg = "bfo";
my $output_arg = "";

&Getopt::Long::Configure("bundling");
my $ok = Getopt::Long::GetOptions("verbose|v!" => \$verbose_arg,
                                  "showall|s!" => \$showall_arg,
                                  "show|S!" => \$show_arg,
                                  "pml|p=s" => \$pml_arg,
                                  "output|o=s" => \$output_arg,
                                  "help|h!" => \$help_arg);

if (!$ok || $help_arg) {
    print "
Usage: $0 [--show|-S] [--showall|-s] [--pml|-p=PML]  [--output|-o=OUTPUTFILE]
        [--verbose|-v] [--help|-h]

Runs a diff between the the files in the ob1 and the bfo directory and
prints the output to stdout.  Prior to checking the differences, the
script removes all copyright header code.  It also first removes all
failover specific code in the bfo files.  Specifically, the script
removes all code that is within the <#ifdef PML_BFO ... #endif /*
PML_BFO */> macros.  To view the bfo specific code in the diff, run
with the -s switch.

-s   Show differences between the files.
-S   Show all the differences between the files that are not
     within \"\#ifdef BFO\" statements.  (default: -S)
-p   PML - which PML to compare to ob1 (default: bfo)
-o   File name where to write the output to (instead of stdout).
-v   Verbose - show more details of script activities.
-h   This help
\n";
    exit(0);
}

my $pml = $pml_arg;
my $PML = $pml;
$PML =~ tr/a-z/A-Z/;

# Change into PML directory that is being compared to ob1.
# In the default case, we just end up where we started in
# the bfo directory.
chdir "$pmlsdir/$pml";

print "\nStarting script to check differences between $pml and ob1...\n";

if (! -d $diffdir) {
  mkdir ("$diffdir", 0777) || print $!;
}

# Copy bfo files into temp directory.
@bfofiles = <*.[h|c]>;
foreach $bfofile (@bfofiles) {
  copy ($bfofile, $diffdir);
}
if ($verbose_arg) {
  print "Copied all $pml files to temp directory\n";
}

chdir $diffdir;

# Using crude preprocessor, strip out all BFO specific code.
# If -s switch is provided, then leave BFO specific code.
foreach $bfofile (@bfofiles) {
  $contents = Read($bfofile);
  die("Couldn't Read $bfofile!\n") if (!$contents);

  if (!$showall_arg) {
    # First, remove all the #if-#else code.
    # #ifdef PML_BFO
    # ...stuff...
    # # else /* PML_BFO */
    # Then, remove all the #if-#endif code.
    # #ifdef PML_BFO
    # ...stuff...
    # #endif /* PML_BFO */
    # Then, remove leftover #endif from the #if-#else.
    # So, three pattern matching steps.
    # Some notes about the regular expression.
    #   1. Need the .*? so the #endif is matched with the closest if.
    #   2. Added the comment PML_BFO on the #endif to get the right match.
    #   3. Need the \n at the end to avoid leaving extra newlines.
    $contents =~ s/#if PML_BFO(.*?)((#else \/\* PML_BFO \*\/\n)|(#endif \/\* PML_BFO \*\/\n))//gis;
    $contents =~ s/#endif \/\* PML_BFO \*\/\n//gis;
  }

  # Strip off the copyright header also.
  $contents =~ s/\/\*(.*?)\$HEADER\$\n \*\/\n//is;

  # Now replace the string $pml with ob1 so we can
  # not get spurious diffs when comparing to ob1.
  $contents =~ s/$pml/ob1/g;
  $contents =~ s/$PML/OB1/g;

  Write($bfofile, $contents);
}
if ($verbose_arg) {
  print "All $pml specific code and copyrights has been removed from $pml files\n";
  print "All $pml/$PML strings converted to ob1/OB1 strings in bfo files\n";
}

# Copy ob1 files into temp directory
chdir "../../ob1";
@ob1files = <*.[h|c]>;
foreach $ob1file (@ob1files) {
  copy ($ob1file, "../$pml/$diffdir");
}
if ($verbose_arg) {
  print "Copied all ob1 files to temp directory\n";
}

chdir "../$pml/$diffdir";

# Strip off copyright from ob1 files.
foreach $ob1file (@ob1files) {
  # Strip off the copyright header also.
  $contents = Read($ob1file);
  die("Couldn't Read $ob1file!\n") if (!$contents);
  $contents =~ s/\/\*(.*?)\$HEADER\$\n \*\/\n//is;
  Write($ob1file, $contents);
}
if ($verbose_arg) {
  print "Removed copyright strings from all ob1 files\n";
}


# Now do a diff on the files.
if ($verbose_arg) {
  print "Now running diffs on all the files...\n\n";
}
foreach $ob1file (@ob1files) {
  $bfofile = $ob1file;
  $bfofile =~ s/ob1/$pml/;
  $cmd = "diff -c $ob1file $bfofile";
  $cmd_output = "";
  open (CMD, "$cmd|");
  $cmd_output .= $_
      while (<CMD>);
  close(CMD);
  if ($output_arg eq "") {
    print "Files Compared: $ob1file and $bfofile\n";
    print "$cmd_output";
  } else {
    if ($cmd_output ne "No differences encountered\n") {
      $alloutput = $alloutput . $cmd_output;
    }
  }
}

chdir "..";
if ($output_arg ne "") {
  rmtree($output_arg);
  Write($output_arg, $alloutput);
}
rmtree("$diffdir");

# Function to read file into a string.
sub Read {
    my ($file) = @_;

    my $contents;
    open (INPUT, $file) or warn "Can't open $file: $!";
    while (<INPUT>) {
        $contents .= $_;
    }
    close(INPUT) or warn "Can't close $file: $!";
    return $contents;
}

# Function to write string to a file.
sub Write {
    my ($filename, $body) = @_;

    # Write out the file
    die("Failed to write to file: $!") if (! open(FILE, "> $filename"));

    print FILE $body;
    close FILE;
}
