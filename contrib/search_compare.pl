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
# Copyright (c) 2013      Intel, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

use File::Find;
use File::Basename;
use File::Compare;
use File::Copy;
use File::Path;
use Getopt::Long;
use Text::Diff;

my $src_arg;
my $tgt_arg;
my $src_dir;
my $target_dir;
my @src_tree = ();
my @tgt_tree = ();
my $flag;
my $help_arg = 0;
my $diff_file = "";
my $diff_arg;
my $update_arg;
my $modified_arg;
my $repo_type;
my $cmd;

sub construct {
    # don't process directories or links, and dont' recurse down 
    # "special" directories
    if ( -l $_ ) { return; }
    if ( -d $_ ) { 
        if ((/\.deps/) || (/\.libs/) || (/\.git/) || (/\.dSYM/) || ($_ eq "autom4te.cache") || ($_ eq "libltdl")) {
            $File::Find::prune = true;
        }            
        return;
    }

    # $File::Find::name is the path relative to the starting point.
    # $_ contains the file's basename.  The code automatically changes
    # to the processed directory, so we want to add the full pathname.

    # ignore some obvious files we don't care about
    if (($_ =~ /\.dirstamp$/i) || ($_ =~ /\.DS_Store$/i) || ($_ =~ /\.lo$/i) || ($_ =~ /\.la$/i) || ($_ =~ /\.o$/i) || ($_ =~ /\.\d$/i)) {
        $File::Find::prune = true;
        return;
    }
    if (($_ eq "Makefile") || ($_ eq "Makefile.in") || ($_ eq "config.log") || ($_ eq "config.status")) {
        $File::Find::prune = true;
        return;
    }
    # ignore executables
    if (-x $File::Find::name) {
        $File::Find::prune = true;
        return;
    }

    if ($flag == 0) {
        push(@src_tree, $File::Find::name);
    } else {
        push(@tgt_tree, $File::Find::name);
    }
}

# Command line parameters

my $ok = Getopt::Long::GetOptions("help|h" => \$help_arg,
                                  "src=s" => \$src_arg,
                                  "tgt=s" => \$tgt_arg,
                                  "diff=s" => \$diff_arg,
                                  "update" => \$update_arg,
                                  "update-modified" => \$modified_arg
    );

if (!$ok || $help_arg) {
    print "Invalid command line argument.\n\n"
        if (!$ok);
    print "Options:
  --diff | -diff      Output diff of changed files to specified file
  --src | -src        Head of source directory
  --tgt | -tgt        Head of target directory
  --update | -update  Apply changes to update target
  --update-modified   Only update modified files (do not add/delete files)\n";
    exit($ok ? 0 : 1);
}

if (!$src_arg || !$tgt_arg) {
    print "Missing src or tgt directory\n";
    exit(1);
}

$src_dir = File::Spec->rel2abs($src_arg);
$target_dir = File::Spec->rel2abs($tgt_arg);
my @srcpth = ();
my @newpth = ();
my $spath;
my $npath;
# if we are updating, then identify the
# leading elements of the src_dir path that
# must be replaced when pointing to the
# target location for a file copy
if ($update_arg || $modified_arg) {
    my $s;
    my $t;
    my @srcpath = File::Spec->splitdir($src_dir);
    my @tgtpath = File::Spec->splitdir($target_dir);
    # find the place where they first differ - since
    # they cannot be identical, they must differ
    # somewhere
    my $found = 0;
    while ($found == 0) {
        $s = shift(@srcpath);
        $t = shift(@tgtpath);
        push(@srcpth, $s);
        push(@newpth, $t);
        if ($s ne $t) {
            $found = 1;
        }
    }
    # if either path has been exhausted, then we are done
    if (0 != scalar(@srcpath) && 0 != scalar(@tgtpath)) {
        # find the place where they re-converge - this
        # might be nowhere, e.g., if they provided the
        # top of two different source trees
    }
    $spath = join("/", @srcpth);
    $npath = join("/", @newpth);
    print "Source: " . $spath . "  New: " . $npath . "\n";
}

if ($diff_arg) {
    $diff_file = File::Spec->rel2abs($diff_arg);
    unlink ($diff_file);
    open(MYFILE, ">$diff_file");
}

my $len_src_dir = length($src_dir);
my $len_tgt_dir = length($target_dir);

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
                if ($diff_arg) {
                    my $diff = diff $tgt, $src, { STYLE => "Unified" };
                    print MYFILE $diff . "\n";
                    push(@modified, $src);
                } elsif ($update_arg || $modified_arg) {
                    print "Updating $src to $tgt\n";
                    copy("$src", "$tgt") or die "Copy failed: src=$src tgt=$tgt\n";
                } else {
                    push(@modified, $src);
                }
            }
            # remove this file from the target tree as it has been found
            # splice @tgt_tree, $i, 1;
            break;
        }
    }
    if (!$modified_arg && $found == 0) {
        if ($update_arg) {
            my $targetpath = $src;
            $targetpath =~ s/$spath/$npath/;
            my $tpath = dirname($targetpath);
            if (! -d $tpath) {
                my $dirs = eval { mkpath($tpath) };
                if (!$dirs) {
                    print "Failed to create path $tpath\n";
                    exit;
                }
                print "Adding $tpath to repo\n";
                $cmd = "pushd $target_dir >& /dev/null; git add $tpath >& /dev/null; popd >& /dev/null";
                system($cmd);
            }
            print "Adding $src to repo\n";
            copy("$src", "$targetpath") or die "Update failed: src=$src tgt=$targetpath\n";
            $cmd = "pushd $target_dir >& /dev/null; git add $targetpath >& /dev/null; popd >& /dev/null";
            system($cmd);
        } else {
            print "Add: " . $src . "\n";
        }
    } else {
        push(@src_pared, $src);
    }
}

print "\n";

# print a list of files in the target tree that need to be deleted
if (!$modified_arg) {
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
            if ($update_arg) {
                print "Removing $tgt_file from repo\n";
                $cmd = "pushd $target_dir >& /dev/null; git rm .$tgt_file >& /dev/null; popd >& /dev/null";
                system($cmd);
            } else {
                print "Delete: " . $tgt . "\n";
            }
        }
    }
}
print "\n";

# print a list of files that have been modified
foreach $tgt (@modified) {
    print "Modified: " . $tgt . "\n";
}


if ($diff_arg) {
    close(MYFILE);
}
