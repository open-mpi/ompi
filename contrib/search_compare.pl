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
use File::Basename;
use File::Compare;

if (scalar(@ARGV) != 2) {
    print "Usage: search_compare.pl src_dir target_dir\n";
    exit 1;
}

my $src_dir = @ARGV[0];
my $target_dir = @ARGV[1];
my $len_src_dir = length($src_dir);
my $len_tgt_dir = length($target_dir);
my @src_tree = ();
my @tgt_tree = ();
my $flag;

sub construct {
    # don't process directories or links, and dont' recurse down 
    # "special" directories
    if ( -l $_ ) { return; }
    if ( -d $_ ) { 
        if ((/\.svn/) || (/\.deps/) || (/\.libs/) || (/\.hg/) || (/\.git/) || ($_ eq "autom4te.cache") || ($_ eq "libltdl")) {
            $File::Find::prune = true;
        }            
        return;
    }

    # $File::Find::name is the path relative to the starting point.
    # $_ contains the file's basename.  The code automatically changes
    # to the processed directory, so we want to add the full pathname.

    # ignore some obvious files we don't care about
    if (($_ =~ /\.dirstamp$/i) || ($_ =~ /\.lo$/i) || ($_ =~ /\.la$/i) || ($_ =~ /\.o$/i) || ($_ =~ /\.\d$/i)) {
        $File::Find::prune = true;
        return;
    }
    if (($_ eq "Makefile") || ($_ eq "Makefile.in") || ($_ eq "config.log") || ($_ eq "config.status")) {
        $File::Find::prune = true;
        return;
    }

    if ($flag == 0) {
        push(@src_tree, $File::Find::name);
    } else {
        push(@tgt_tree, $File::Find::name);
    }
}

# construct a tree of all files in the source directory tree
$flag = 0;
find(\&construct, $src_dir);

# construct a tree of all files in the target directory tree
$flag = 1;
find(\&construct, $target_dir);

print "size of src_tree: " . @src_tree . ".\n";
print "size of tgt_tree: " . @tgt_tree . ".\n";

# print a list of files in the source tree that need to be added to the target
my $found;
my $src_file;
my $tgt_file;
my @modified = ();
my @src_pared = ();
my $i;
foreach $src (@src_tree) {
    # strip the leading elements of the path that was given to us
    $src_file = substr($src, $len_src_dir);
    $found = 0;
    $i = -1;
    foreach $tgt (@tgt_tree) {
        $i = $i + 1;
        $tgt_file = substr($tgt, $len_tgt_dir);
        if ($src_file eq $tgt_file) {
            # printf "Matched: " . $src_file . " " . $tgt_file . "\n";
            # file has been found - ignore it
            $found = 1;
            if (compare($src, $tgt) != 0) {
                push(@modified, $src);
            }
            # remove this file from the target tree as it has been found
            # splice @tgt_tree, $i, 1;
            break;
        }
    }
    if ($found == 0) {
        print "Add: " . $src . "\n";
    } else {
        push(@src_pared, $src);
    }
}

print "\n";

# print a list of files in the target tree that need to be deleted
foreach $tgt (@tgt_tree) {
    $found = 0;
    $tgt_file = substr($tgt, $len_tgt_dir);
    foreach $src (@src_pared) {
        $src_file = substr($src, $len_src_dir);
        if ($src_file eq $tgt_file) {
            # file has been found - ignore it
            $found = 1;
            break;
        }
    }
    if ($found == 0) {
        print "Delete: " . $tgt . "\n";
    }
}

print "\n";

# print a list of files that have been modified
foreach $tgt (@modified) {
    print "Modified: " . $tgt . "\n";
}
