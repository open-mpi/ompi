#!/usr/bin/env perl
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# Utility to stamp copyrights into files.  This is only being checked
# into SVN because we have LANL and HLRS copyrights pending, and this
# script will be helpful in adding them to the tree (I'm sure the
# script will need to be modified, but it's got all the relevant
# File::Find code, etc.).



use strict;
use File::Find;
use Cwd;

my $verbose = 0;
my %files_found;
my @skip_dirs = ( "doxygen", ".svn", ".deps", ".libs", "libltdl", "autom4te.cache" );

my $iu = "Copyright (c) 2004-2005 The Trustees of Indiana University.
                        All rights reserved.";
my $ut = "Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
                        All rights reserved.";
my $osu = "Copyright (c) 2004 The Ohio State University
                        All rights reserved.";
my $copy = "$iu\n$ut\n\$COPYRIGHT\$\n\nAdditional copyrights may follow\n\n\$HEADER\$";

sub wanted {
    # Setup

    my $file = $_;
    my @dirnames = split('/', $File::Find::dir);
    my $dir = $dirnames[$#dirnames];

    # Is the file a dir?

    my $is_dir = (-d $file);

    # Do we want this dir?

    for (my $i = 0; $i <= $#skip_dirs; ++$i) {
        if ($skip_dirs[$i] eq $dir || 
            ($is_dir && $skip_dirs[$i] eq $file)) {
            print("Skipping dir: $File::Find::dir / $file\n")
                if ($verbose);
            $File::Find::prune = 1;
            return 0;
        }
    }

    # Do we want this file?

    if ($is_dir) {
        print ("Found dir: $File::Find::dir/$file\n")
            if ($verbose);
    } else {
        $files_found{$File::Find::name} = 1;
        print ("Found file: $File::Find::name\n")
            if ($verbose);
    }
    1;
}

find (\&wanted, ".");
my $counts;
my $max_count = -1;
foreach my $file (sort keys %files_found) {
     open FILE, $file;
    my @found = grep(/\$HEADER\$/, <FILE>);
    close(FILE);
    if ($#found >= 0) {
        print "Found file: $file\n";
        open FILE, $file;
        open FILENEW, ">$file.new";
        while (<FILE>) {
            chomp;
            my $line = $_;
            if ($line =~ /\$HEADER\$/) {
                my $prefix = $line;
                $prefix =~ s/(.+)\$HEADER\$.*$/\1/;
                if ($prefix ne "\$HEADER\$") {
                    my $c = $prefix . $copy;
                    $c =~ s/\n/\n$prefix/g;
                    print FILENEW "$c\n";
                } else {
                    print FILENEW "$copy\n";
                }
            } else {
                print FILENEW "$line\n";
            }
        }
        close(FILENEW);
        close(FILE);
        system("cp $file.new $file");
        unlink("$file.new");
    }
}
