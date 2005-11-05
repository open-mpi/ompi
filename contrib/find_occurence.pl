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

if (scalar(@ARGV) != 2) {
    print "Usage: #find_occurence <string> <source-path>\n";
    exit(3);
}

$search_string = @ARGV[0];
$source_path = @ARGV[1];

open (SOURCE_FILES, "find $source_path -name *.c |") || print "could not open the pipe\n";
while (<SOURCE_FILES>) {
    
    #open the file and delete the occurence
    
    $file_name = $_;
    
    open (FILE, "$file_name") || print "Could not open $file_name for reading\n";
    
    while (<FILE>) {
        if (/$search_string/) {
            print $file_name;
        }
    }
    close(FILE);
}

close(SOURCE_FILES);
