#!/usr/bin/env perl
#
# Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# This script is run to see the differences between some other BTL and
# the sm BTL.  By default, it looks at the sm BTL but it can also be
# used with other BTLs.  Prior to running the diff, it does some
# preprocessing on the files.  First, it removes all the copyright
# headers as differences in them are of no concern.  Secondly, it
# converts all the BTL specific strings in the BTL to be compared to
# "sm".  For example, with the smcuda, all "smcuda" strings are converted
# to "sm" and all "SMCUDA" strings are converted to "SM".  In this way,
# we avoid any spurious differences just related to the difference in
# the names of the functions and variables.
#
# Lastly, in the case of smcuda only, it can strip out all code within
# the BTL that is contained within specific #ifdef strings.  See the
# code and comments below to see how it works.
#
# This script must be run from this directory as it makes assumptions
# about where the PML directories are located.  Here are some
# examples.
#
# Run using all defaults.
#  > check-btl-sm-diffs.pl
#
# Do not remove the SMCUDA specific code
#  > check-ob1-pml-diffs.pl -s
#
# Do not remove the SMCUDA specific code and save results in DIFFS
#  > check-ob1-pml-diffs.pl -s -o DIFFS
#
#
use strict;

use File::Copy;
use File::Path;
use Getopt::Long;

my $diffdir = "diffdir";
my $btlsdir = "../ompi/mca/btl";
my $cmd;
my $cmd_output;
my $contents;
my $smcudafile;
my @smcudafiles;
my $smfile;
my @smfiles;
my $alloutput;


# Command line parsing
my $verbose_arg = 0;
my $show_arg = 1;
my $showall_arg = 0;
my $help_arg = 0;
my $btl_arg = "smcuda";
my $output_arg = "";

&Getopt::Long::Configure("bundling");
my $ok = Getopt::Long::GetOptions("verbose|v!" => \$verbose_arg,
                                  "showall|s!" => \$showall_arg,
                                  "show|S!" => \$show_arg,
                                  "btl|p=s" => \$btl_arg,
                                  "output|o=s" => \$output_arg,
                                  "help|h!" => \$help_arg);

if (!$ok || $help_arg) {
    print "
Usage: $0 [--show|-S] [--showall|-s] [--btl|-p=BTL]  [--output|-o=OUTPUTFILE]
        [--verbose|-v] [--help|-h]

Runs a diff between the the files in the sm and the smcuda directory
and prints the output to stdout.  Prior to checking the differences,
the script removes all copyright header code.  It also first removes
all CUDA specific code in the smcuda files.  Specifically, the script
removes all code that is within the following ifdefs.
#ifdef OMPI_CUDA_SUPPORT
 ...
#endif /*OMPI_CUDA_SUPPORT */
To view the smcuda specific code in the diff, run with the -s switch.

-s   Show all the differences between the files.
-S   Show all the differences between the files that are not
     within \"\#ifdef OMPI_CUDA_SUPPORT\" statements.  (default: -S)
-p   BTL - which BTL to compare to sm (default: smcuda)
-o   File name where to write the output to (instead of stdout).
-v   Verbose - show more details of script activities.
-h   This help
\n";
    exit(0);
}

my $btl = $btl_arg;
my $BTL = $btl;
$BTL =~ tr/a-z/A-Z/;

# Change into BTL directory that is being compared to sm.
# In the default case, we just end up where we started in
# the smcuda directory.
chdir "$btlsdir/$btl";

print "\nStarting script to check differences between $btl and sm...\n";

if (! -d $diffdir) {
  mkdir ("$diffdir", 0777) || print $!;
}

# Copy smcuda files into temp directory.
@smcudafiles = <*.[h|c]>;
foreach $smcudafile (@smcudafiles) {
  copy ($smcudafile, $diffdir);
}
if ($verbose_arg) {
  print "Copied all $btl files to temp directory\n";
}

chdir $diffdir;

# Using crude preprocessor, strip out all SMCUDA specific code.
# If -s switch is provided, then leave SMCUDA specific code.
foreach $smcudafile (@smcudafiles) {
  $contents = Read($smcudafile);
  die("Couldn't Read $smcudafile!\n") if (!$contents);

  if (!$showall_arg) {
    # First, remove all the #if-#else code.
    # #ifdef OPAL_CUDA_SUPPORT
    # ...stuff...
    # # else /* OPAL_CUDA_SUPPORT */
    # Then, remove all the #if-#endif code.
    # #ifdef OPAL_CUDA_SUPPORT
    # ...stuff...
    # #endif /* OPAL_CUDA_SUPPORT */
    # Then, remove leftover #endif from the #if-#else.
    # So, three pattern matching steps.
    # Some notes about the regular expression.
    #   1. Need the .*? so the #endif is matched with the closest if.
    #   2. Added the comment OPAL_CUDA_SUPPORT on the #endif to get the right match.
    #   3. Need the \n at the end to avoid leaving extra newlines.
    $contents =~ s/#if OPAL_CUDA_SUPPORT(.*?)((#else \/\* OPAL_CUDA_SUPPORT \*\/\n)|(#endif \/\* OPAL_CUDA_SUPPORT \*\/\n))//gis;
    $contents =~ s/#endif \/\* OPAL_CUDA_SUPPORT \*\/\n//gis;
  }

  # Strip off the copyright header also.
  $contents =~ s/\/\*(.*?)\$HEADER\$\n \*\/\n//is;

  # Now replace the string $btl with sm so we can
  # not get spurious diffs when comparing to sm.
  $contents =~ s/$btl/sm/g;
  $contents =~ s/$BTL/SM/g;

  Write($smcudafile, $contents);
}
if ($verbose_arg) {
  print "All $btl specific code and copyrights has been removed from $btl files\n";
  print "All $btl/$BTL strings converted to sm/SM strings in bfo files\n";
}

# Copy sm files into temp directory
chdir "../../sm";
@smfiles = <*.[h|c]>;
foreach $smfile (@smfiles) {
  copy ($smfile, "../$btl/$diffdir");
}
if ($verbose_arg) {
  print "Copied all sm files to temp directory\n";
}

chdir "../$btl/$diffdir";

# Strip off copyright from sm files.
foreach $smfile (@smfiles) {
  # Strip off the copyright header also.
  $contents = Read($smfile);
  die("Couldn't Read $smfile!\n") if (!$contents);
  $contents =~ s/\/\*(.*?)\$HEADER\$\n \*\/\n//is;
  # Strip away KNEM as that is not in smcuda
  $contents =~ s/#if OMPI_BTL_SM_HAVE_KNEM \|\| OMPI_BTL_SM_HAVE_CMA(.*?)((#else\n)|(#endif\n)|(#endif \/\* OMPI_BTL_SM_HAVE_KNEM || OMPI_BTL_SM_HAVE_CMA \*\/\n))//gis;
  $contents =~ s/#if OMPI_BTL_SM_HAVE_KNEM(.*?)((#else\n)|(#endif\n)|(#endif \/\* OMPI_BTL_SM_HAVE_KNEM \*\/\n))//gis;
  $contents =~ s/#endif  \/\* OMPI_BTL_SM_HAVE_KNEM \|\| OMPI_BTL_SM_HAVE_CMA \*\/\n//gis;
  $contents =~ s/#endif \/\* OMPI_BTL_SM_HAVE_KNEM \|\| OMPI_BTL_SM_HAVE_CMA \*\/\n//gis;
  $contents =~ s/#endif  \/\* OMPI_BTL_SM_HAVE_KNEM \*\/\n//gis;
  Write($smfile, $contents);
}
if ($verbose_arg) {
  print "Removed copyright strings from all sm files\n";
}


# Now do a diff on the files.
if ($verbose_arg) {
  print "Now running diffs on all the files...\n\n";
}
foreach $smfile (@smfiles) {
  $smcudafile = $smfile;
  $smcudafile =~ s/sm/$btl/;
  $cmd = "diff -c $smfile $smcudafile";
  $cmd_output = "";
  open (CMD, "$cmd|");
  $cmd_output .= $_
      while (<CMD>);
  close(CMD);
  if ($output_arg eq "") {
    print "Files Compared: $smfile and $smcudafile\n";
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
