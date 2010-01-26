#!/usr/bin/env perl
#
# Copyright (c) 2010 Cisco Systems, Inc.
#
# Script to generate PHP-ized files of Open MPI tarball-installed man
# pages.
#

use strict;
use File::Find;
use File::Basename;
use Cwd;

my $mandir;
my $version;

# Read command line arguments
while (@ARGV) {
    my $a = $ARGV[0];
    if ($a eq "--mandir" && $#ARGV >= 1) {
        shift @ARGV;
        $mandir = $ARGV[0];
        print "Found mandir: $mandir\n";
        shift @ARGV;
    }

    elsif ($a eq "--version" && $#ARGV >= 1) {
        shift @ARGV;
        $version = $ARGV[0];
        print "Found version: $version\n";
        shift @ARGV;
    }
}

# Check that we have what we need
if (!defined($mandir) || !defined($version)) {
    print "Usage: $0 --mandir dir --version version\n";
    exit(1);
}

# Setup
my @files;
my $pwd = Cwd::cwd();

# Find all *.[0-9] files in the $mandir tree.
&File::Find::find(
    sub {
        push(@files, $File::Find::name) if (-f $_ && $_ =~ /\.[1-9]$/);
    }, $mandir);

# Must cd into the $mandir directory because some of the man pages
# refer to other man pages by "man/<filename>" relative path names.
chdir($mandir);

my %dirs;
my $outfiles;

# Generate a PHP file for each man page.
foreach my $file (@files) {
    my $b = basename($file);
    $b =~ m/(.*)\.([0-9])$/;

    my $name = $1;
    my $section = $2;

    my $outfile = "$pwd/man$section/$b.php";
    my $outdir = dirname($outfile);
    $dirs{$outdir} = "";
    push(@{$outfiles->{$section}}, {
        name => $name,
        file => "man$section/$b.php",
         });

    mkdir($outdir)
        if (! -d $outdir);

    print "Generating: $name ($section)\n";

    # Run the groff command and send the output to the file
    open(CMD, "groff -mandoc -T html $file|") || die("Can't open command");
    my $text;
    $text .= $_
        while (<CMD>);
    close(CMD);

    # Post-process the text:
    # Remove <head> ... </head>
    # Remove <!doctype ...>
    # Remove <meta ...>
    # Remove <style ...> ... </style>
    # Remove <title> ... </title>
    # Remove <html> and </html>
    # Remove <body> and </body>

    $text =~ s/<head>.*<\/head>//is;
    $text =~ s/<!doctype.*?>//is;
    $text =~ s/<html>//i;
    $text =~ s/<\/html>//i;
    $text =~ s/<body>//i;
    $text =~ s/<\/body>//i;

    # Remove carriage returns, extra white space, and double blank
    # lines
    $text =~ s/\r//g;
    $text =~ s/[ \t]+\n/\n/g;
    $text =~ s/\n{3,}/\n\n/g;

    # Now we're left with what we want.  Output the PHP page.

    # Write the output PHP file with our own header and footer,
    # suitable for www.open-mpi.org.
    unlink($outfile);
    open(FILE, ">$outfile") || die "Can't open $outfile";
    print FILE '<?php
$topdir = "../../..";
$title = "' . "$name($section) man page (version $version)" . '";

include_once("$topdir/includes/header.inc");
?>
<p> <a href="../">&laquo; Return to documentation listing</a></p>
' . $text . '
<p> <a href="../">&laquo; Return to documentation listing</a></p>
<?php
include_once("$topdir/includes/footer.inc");
';
    close(FILE);
}

# Now write out an index.php file in each subdir
foreach my $dir (keys(%dirs)) {
    print "Writing index file in $dir...\n";
    open(FILE, ">$dir/index.php") || die "Can't open $dir/index.php";
    print FILE '<?php header("Location: ..");
';
    close(FILE);
}

# Now write out a top-level data-<version>.inc file
my $file = "$pwd/data-$version.inc";
print "Writing $file...\n";
open(FILE, ">$file") || die "Can't open $file";
print FILE '<?php
$version = "' . $version . '";

';

foreach my $section (sort(keys(%{$outfiles}))) {
    print FILE '$files[] = "' . $section . '";
$files_' . $section . ' = array(';
    my $first = 1;
    my @f;
    # The hash isn't sorted, so build up an array of just the
    # filenames
    foreach my $file (@{$outfiles->{$section}}) {
        push(@f, $file->{name});
    }
    # Now output the sorted filenames
    foreach my $file (sort(@f)) {
        print FILE ", "
            if (!$first);
        $first = 0;
        print FILE '"' . $file . '"';
    }
    print FILE ");\n\n";
}
close(FILE);

# Print the top-level engine file for this version (it will use the
# data-<version>.inc file).
open(FILE, ">index.php") || die "Can't open index.php";
print FILE '<?php 
$topdir = "../..";
include_once("data-' . $version . '.inc");
include_once("../engine.inc");
';
close(FILE);

