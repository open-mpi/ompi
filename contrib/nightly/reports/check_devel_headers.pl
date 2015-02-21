#!/usr/bin/env perl
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
# Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# Check headers in the core opal, orte, and ompi trees and ensure that
# they exist in the installation tree (assuming that the software was
# configured --with-devel-headers).
#

use strict;
use File::Find;
use Getopt::Long;
use Data::Dumper;

my @projects = qw(opal orte ompi);
our $prefix_arg;

&Getopt::Long::Configure("bundling", "require_order");
my $ok = Getopt::Long::GetOptions("prefix|p=s" => \$prefix_arg);
if (!$prefix_arg) {
    print "Must provide --prefix argument\n";
    exit(1);
}
if (! -d $prefix_arg) {
    print "Prefix does not exist ($prefix_arg)\n";
    exit(1);
}

# Check each project
foreach my $p (@projects) {
    if (-d $p) {
        print "Checking: $p\n";
        find(\&wanted, $p);
    } else {
        print "Could not find tree: $p -- skipped\n";
    }
}

sub wanted {
    # don't process directories or links, and dont' recurse down 
    # "special" directories
    if ( -l $_ ) { return; }
    if ( -d $_ ) { 
        if ((/\.svn/) || (/\.deps/) || (/\.libs/)) {
            $File::Find::prune = 1;
        }
        return;
    }

    # $File::Find::name is the path relative to the starting point.
    # $_ contains the file's basename.
    return if (! /\.h$/);

    # Check the fullname and see if it's in the core of the project.
    my $name = $_;
    my @parts = split(/\//, $File::Find::name);
    # If the second part is not "mca", it's good
    if ($parts[1] eq "mca") {
        # If the third or fourth part is "base", then it's good
        if ($parts[2] eq "base") {
            # Good
        } elsif ($parts[3] eq "base") {
            # Except if it's static-components.h
            if ($name eq "static-components.h") {
                return;
            }
        } else {
            return;
        }
    } 
    # We don't want any of the tools headers
    elsif ($parts[1] eq "tools") {
        return;
    }
    # We don't want any of the F90 headers
    elsif ($parts[0] eq "ompi" && $parts[1] eq "mpi" && $parts[2] eq "f90") {
        return;
    }
    # The only file we want in opal/event is event.h
    elsif ($parts[0] eq "opal" && $parts[1] eq "event" &&
           $name ne "event.h") {
        return;
    }

    # Ok, this file should be in the install tree
    if (! -f "$prefix_arg/include/openmpi/$File::Find::name") {
        print("Warning: $File::Find::name not found in install tree\n");
    }
}
