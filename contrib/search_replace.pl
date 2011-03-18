#!/usr/bin/perl
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

use File::Find;

if (scalar(@ARGV) != 2) {
    print "Usage: search_replace.pl search_string replace_string\n";
    exit 1;
}

$search_string = @ARGV[0];
$replace_string = @ARGV[1];

print "search: $search_string\n";
print "replace: $replace_string\n";

sub replace {
    # don't process directories or links, and dont' recurse down 
    # "special" directories
    if ( -l $_ ) { return; }
    if ( -d $_ ) { 
        if ((/\.svn/) || (/\.deps/) || (/\.libs/) || (/\.hg/) || (/\.git/)) {
            $File::Find::prune = true;
        }
        return;
    }

    # $File::Find::name is the path relative to the starting point.
    # $_ contains the file's basename.  The code automatically changes
    # to the processed directory, so we want to open / close $_.
    $process_file = $_;
    print "--> $File::Find::name\n";
    my $replace = 0;

    open(INFILE, $process_file) || 
        die "Could not open " . $File::Find::name . ": $!\n";
    open(OUTFILE, "> " . $process_file . ".tmp") ||
        die "Could not open " . $File::Find::name . ".tmp: $!\n";

    while (<INFILE>) {
        $replace += s/$search_string/$replace_string/g;
        print OUTFILE $_;
    }

    close(OUTFILE);
    close(INFILE);

    if ($replace) {
        rename($process_file . ".tmp", $process_file);
    } else {
        unlink($process_file . ".tmp");
    }
}
find(\&replace, ".");
