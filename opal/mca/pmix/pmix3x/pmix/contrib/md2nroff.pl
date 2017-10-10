#!/usr/bin/env perl

# Script to convert markdown to nroff man pages.
#
# The main conversion work is done via pandoc.  But pandoc doesn't do
# everything exactly the way we want it, so use some perl regular
# expressions to fix up what pandoc doesn't get right.
#
# Do a "smart" write of the resulting output man page -- only write to
# the output file if the contents have actually changed compared to
# what was already there.

use strict;
use warnings;

use POSIX;
use File::Basename;
use Getopt::Long;
use File::Temp qw/tempfile/;

my $source_arg;
my $target_arg;
my $help_arg;

my $ok = Getopt::Long::GetOptions("source=s" => \$source_arg,
                                  "target=s" => \$target_arg,
                                  "help|h" => \$help_arg,
                                  );

if ($help_arg) {
    print "$0 --source input_MD_file --target output_nroff_file\n";
    exit(0);
}

# Sanity checks
die "Must specify a source file"
    if (!defined($source_arg));
die "Source file does not exist ($source_arg)"
    if (! -r $source_arg);

my $pandoc = `which pandoc`;
die "Cannot find pandoc executable"
    if ($pandoc eq "");

#####################################################################

my $file = $source_arg;
$file =~ m/(\d+).md/;
my $section = $1;
die "Could not figure out the man page section: $source_arg"
    if (!defined($section));
my $shortfile = basename($file);
$shortfile =~ s/\.$section\.md$//;

# If the target file was not specified, derive it from the source file
my $target;
if (!defined($target_arg)) {
    $target_arg = $source_arg;

    $target_arg =~ m/\.(\d)\.md$/;
    my $section = $1;

    my $dirname = dirname($target_arg);
    my $basename = basename($target_arg);
    $basename =~ s/\.md$//;

    $target = "$dirname/man$section/$basename";
} else {
    $target = $target_arg;
}

print "*** Processing: $file -> $target\n";

# Read in the file
my $pandoc_input;
open(IN, $file)
    || die "Can't open $file";
$pandoc_input .= $_
    while (<IN>);
close(IN);

# Remove the Jekyll header
$pandoc_input =~ s/.*---\n.+?---\n//s;

# Remove the {% include ... %} directives
$pandoc_input =~ s/\n{0,1}\s*{%\s+include .+?\s+%}\s*\n/\n/g;

# Change {% highlight c %} to ```c
$pandoc_input =~ s/^\s*{%\s+highlight\s+c\s+%}\s*$/\n```c/gmi;

# Change {% endhighlight %} to ```
$pandoc_input =~ s/^\s*\{\%\s+endhighlight\s+\%\}\s*$/```\n/gmi;

# Pandoc does not handle markdown links in output nroff properly,
# so just remove all links.
while ($pandoc_input =~ m/\[(.+?)\]\(.+?\)/) {
    my $text = $1;
    $pandoc_input =~ s/\[(.+?)\]\(.+?\)/$text/;
}

# Add the pandoc header
$pandoc_input = "% $shortfile($section) PMIx Programmer's Manual | \@VERSION\@
% PMIx
% \@DATE\@\n\n$pandoc_input";

# Generate the nroff output
my ($fh, $temp_filename) = tempfile();
print $fh $pandoc_input;
close($fh);

open(IN, "pandoc -s --from=markdown --to=man $temp_filename|")
    || die "Can't run pandoc";
my $pandoc_nroff;
$pandoc_nroff .= $_
    while (<IN>);
close(IN);
unlink($temp_filename);

# Now that we have the nroff string result, is it different than the
# target file?
my $write_nroff = 1;
if (-r $target) {
    # If the target file exists, read it in
    open(IN, $target)
        || die "Can't open $target";
    my $target_nroff;
    $target_nroff .= $_
        while (<IN>);
    close(IN);

    # Remove the date from the target nroff string so that we can
    # compare and ignore if the date has changed.  Note that some
    # versions of pandoc render dates as xxxx\-xx\-xx, and others
    # render it as xxxx-xx-xx.  Handle both.
    $target_nroff =~ s/\"\d\d\d\d\\\-\d\d\\\-\d\d\"/\"\\\@DATE\\\@\"/;
    $target_nroff =~ s/\"\d\d\d\d\-\d\d\-\d\d\"/\"\\\@DATE\\\@\"/;

    $write_nroff = 0
        if ($pandoc_nroff eq $target_nroff);
}

# Do we need to write a new target nroff?
if ($write_nroff) {

    # What's the date right now?
    my $now_string = strftime "%Y\\-%m\\-%d", localtime;
    $pandoc_nroff =~ s/\\\@DATE\\\@/$now_string/g;

    # Make sure the target directory exists
    my $dirname = dirname($target);
    mkdir($dirname)
        if (! -d $dirname);

    open(OUT, ">$target")
        || die "Can't write to $target";
    print OUT $pandoc_nroff;
    close(OUT);

    print "--> Wrote new $target\n";
} else {
    print "--> $target unchanged; not written\n";
}

exit(0);
