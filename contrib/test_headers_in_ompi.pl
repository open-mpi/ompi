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

#this is the perl scripty foo which does the following tasks
# 1. Extract the #include <*.h> files which are present in both header and source files
# 2. Do some basic formatting
# 3. Check if these included files are present on the platform which has been given

if (scalar(@ARGV) != 2) {
    print "Usage:
           ./depend.pl <compiler-name>\n";
    exit(3);
}

$includes_file = "headers.txt";
$return = &get_header_files($includes_file);

$test_file = "test_headers.txt";
$return = &parse_header_files($includes_file, $test_file);

$source_tree = @ARGV[0];
$CC = @ARGV[1];
$result_file = "results.txt";
$return = &test_for_headers($test_file, $result_file, $CC);

# this file is used to extract which header files are included in a particular
# source file. Kind of a neat implementation
sub get_header_files {
    
    local($dump_file) = @_;
    
    open(C_FILES, "find $source_tree -name \*.c |") || print "could not find source files\n";
    open(H_FILES, "find $source_tree -name \*.c |") || print "could not find header files\n";

    open(DUMP, "> $dump_file") || print "Could not open $dump_file\n";

    while (<C_FILES>) {
        $file_h = $_;
        print DUMP "Processing $file_h";
        open(FILE_H, "$file_h") || print "could not open file for reading\n";

        #grep for the pattern which we want
        while (<FILE_H>) {
            if (/#include </) {
                print DUMP $_, "\n";
            }
        }
        print DUMP "=============================================================================\n"
    }

    while (<H_FILES>) {
        $file_h = $_;
        print DUMP "Processing $file_h";
        open(FILE_H, "$file_h") || print "could not open file for reading\n";

        #grep for the pattern which we want
        while (<FILE_H>) {
            if (/#include </) {
                print DUMP $_, "\n";
            }
        }
        print DUMP "=============================================================================\n"
    }

    close (C_FILES);
    close (H_FILES);
    close (DUMP);

    return 0;
}


#this simply constructs the header file list from dump and dump_pl and then checks whether all the 
#header files are present
sub parse_header_files {

    local($includes_file, $test_file) = @_;

    open(SOURCE,"$includes_file") || print "Could not open $includes_file for reading\n";
    open(DUMP,"> $test_file") || print "Could not open $test_file for reading\n";

    while (<SOURCE>) {
        if (/#include </){
            print DUMP $_;
        }
    }

    close(SOURCE);
    close(DUMP);


    #remove all the unnecessary comments from headers.txt
    open (HEADER, "$test_file") || print "Could not open $test_file for reading\n";
    open(TEMP, "> temp.txt") || print "Could not open temp.txt for writing\n";

    while(<HEADER>) {
        #remove leading white spaces
        s/^\s*//;
        #remove anything after <*.h>
        s/>{1,1}.*\n/>\n/;
        #remove anything before #include 
        s/^.*#include/#include/;
        print TEMP $_;
    }

    close(HEADER);
    close(TEMP);

    #remove duplicate occurences of the file
    system("sort temp.txt | uniq > $test_file");

    return 0;
}

#this suroutine is used to test if a particular header is present or absent in a particular language
sub test_for_headers {

    local($test_file, $result_file, $CC) = @_;
    local($temp) = "temp.c";

    print "CC = $CC\n";

    open(HEADER, "$test_file") || print "Could not open $test_file for reading\n";
    open(RESULTS, "> $result_file") || print "Could not open $result_file for writing\n";

    while(<HEADER>) {
        print $_;
        
        #create the file for compilation
        chomp $_;
        $string = "

        $_ /*this is the include file to be tested for*/

        int main(int argc, char **argv) {
            return 0;
        }
        
        ";
        
        open(TEMP, "> $temp") || print "Could not open $temp for writing\n";
        print TEMP $string;
        close(TEMP);
        
        $compiled = system("$CC $temp");
        
        if ($compiled == 0) {
            print "$_ is present\n";
        } else {
            print RESULTS "$_\n";
        }

        system("rm -Rf $temp");
        system("rm -Rf temp.*");

    }

    close(HEADER);
    close(RESULTS);

    return 0;
}
