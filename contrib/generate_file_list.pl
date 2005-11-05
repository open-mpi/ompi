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

if (scalar(@ARGV) != 1) {
    print "Usage: generate_file_list <diff file>\n";
    exit(3);
}

$file_name = @ARGV[0];
open(FILE,"$file_name") || print "File count not be opened\n";
open(TEMP,"> file_list") || print "Could not open file for writing\n";
while (<FILE>) {
    if (/Index/) {
        s/^Index:\s*//g;
        print TEMP;
    }
}
close(TEMP);
close($file_name);

