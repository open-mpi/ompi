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
# Copyright (c) 2016      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
#To keep brian happy

use Text::Tabs;

if (scalar(@ARGV) != 1) {
    print "We need a source tree path\n";
    exit(3);
}

$source_path = @ARGV[0];

open(HEADERS, "find $source_path -name *.h |");
while(<HEADERS>) {
    open(TEMP, ">temp.txt");
    $file_name = $_;
    print $file_name;
    open(FILE, "$file_name");
    my @lines_with_tabs = <FILE>;
    close(FILE);
    my @expanded_lines = expand(@lines_with_tabs);
    print TEMP join("\n",@expanded_lines),"\n";
    close(TEMP);
    system("mv temp.txt $file_name");
}
close(HEADERS);

open(SOURCES, "find $source_path -name *.c |");
while(<SOURCES>) {
    open(TEMP, ">temp.txt");
    $file_name = $_;
    print $file_name;
    open(FILE, "$file_name");
    my @lines_with_tabs = <FILE>;
    close(FILE);
    my @expanded_lines = expand(@lines_with_tabs);
    print TEMP join("\n",@expanded_lines),"\n";
    close(TEMP);
    system("mv temp.txt $file_name");
}
close(SOURCES);
