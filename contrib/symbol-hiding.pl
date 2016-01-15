#!/usr/bin/env perl
#
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# $COPYRIGHT$

use strict;
use Getopt::Long;

# globals
my $myfile;
my $mylib;
my $myprefix;
my $mysuffix;
my $mycapprefix;

# Set to true if the script should merely check for symbols in
# the library that are not in the provided output file - useful
# for determining if something has changed prior to doing an update
my $CHECK_ONLY = 0;
# Set to true to suppress most informational messages. Only missing
# symbols will be printed.
my $QUIET = 0;
# Set to true if we just want to see the help message
my $HELP = 0;
# Set to true if we want to reverse the hiding direction
my $REVERSE = 0;


GetOptions(
    "help" => \$HELP,
    "quiet" => \$QUIET,
    "check-only" => \$CHECK_ONLY,
    "prefix=s" => \$myprefix,
    "suffix=s" => \$mysuffix,
    "lib=s" => \$mylib,
    "file=s" => \$myfile,
    "reverse" => \$REVERSE,
) or die "unable to parse options, stopped";

if ($HELP) {
    print <<EOT;
$0 [options]

--help | -h          This help message
--quiet | -q         Only output critical messages to stdout
--check-only         Output the symbols we would have prefixed, but don't do anything
--prefix=NAME        Add NAME to the front of all found symbols
--suffix=NAME        Add NAME to the end of all found symbols
--lib=NAME           Library containing symbols that are to be "hidden"
--file=NAME          Output file for results, or existing file to be updated
--reverse            Reverse the direction of hiding (i.e., #define prefix_foo to be foo)
EOT
    exit(0);
}

#-------------------------------------------------------------------------------
# predeclare sub for print-like syntax
sub quiet_print {
    unless ($QUIET) {
        print @_;
    }
}

#-------------------------------------------------------------------------------

$mycapprefix = uc $myprefix;

# get the symbol output for this lib
my $output = qx(nm $mylib);

# cycle across each line and tokenize it
my @symbols;
my $len = 0;
foreach my $line (split /[\r\n]+/, $output) {
    quiet_print "$line\n";
    my @values = split(' ',$line);
    my $val = shift(@values);
    # if the first character isn't a number, then
    # we can ignore the line
    my $str = substr($val,0,1);
    if ("0" ne $str) {
        quiet_print "skipping\n";
        next;
    }
    # this is a line of interest - see if the
    # next token indicates a public symbol by
    # being a 'T' or a 'B'
    $val = shift(@values);
    if ("T" eq $val || "B" eq $val || "D" eq $val) {
        $val = shift(@values);
        # if this symbol contains a '.', then we
        # need to ignore it
        if (index($val, ".") != -1) {
            quiet_print "skipping $val\n";
            next;
        }
        quiet_print "GOT: " . $val . "\n";
        push @symbols, $val;
        if ($len < length($val)) {
            $len = length($val);
        }
    }
}

$len = $len + 5;
if ($myfile ne "") {
    open FILE, ">$myfile" || die "file could not be opened";
}
sub checkCase {
    if ($_[0] =~ /^[[:upper:]]/) {
        return 1;
    }
    else {
        return 0;
    }
}

foreach my $sym (@symbols) {
    my $out;
    if ($REVERSE) {
        # if the first char is a cap, then use the cap prefix
        if (checkCase($sym)) {
            $out = "#define " . $mycapprefix . $sym . $mysuffix;
        } else {
            $out = "#define " . $myprefix . $sym . $mysuffix;
        }
    } else {
        $out = "#define " . $sym;
    }
    my $diff = $len - length($sym);
    for (my $i=0; $i < $diff; $i++) {
       $out = $out . " ";
    }
    if ($REVERSE) {
        $out = $out . $sym . "\n";
    } else {
        # if the first char is a cap, then use the cap prefix
        if (checkCase($sym)) {
            $out = $out . $mycapprefix . $sym . $mysuffix . "\n";
        } else {
            $out = $out . $myprefix . $sym . $mysuffix . "\n";
        }
    }
    if ($myfile ne "") {
        print FILE $out;
    } else {
        print $out;
    }
}
if ($myfile ne "") {
    close FILE;
}

