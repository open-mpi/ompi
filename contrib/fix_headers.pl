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
    print "Usage:
           fix_headers.pl <header_file_list>\n";
    exit(3);
}

$header_file = @ARGV[0];
$temp_file = "/tmp/temp.c";

open(HEADERS, "$header_file") || print "Could not open $header_file\n";
open(MOD_FILES, "> modified_files.txt") || print "Could not open modified.txt\n";

while (<HEADERS>) {

    #open all the c files
    #check if this header is present
    #if it is present, then substitute it with the protection


    $header_string = $_;
    chomp($header_string);
    $protection = $_;
    $protection =~ s/\./_/;
    $protection =~ s/\//_/;
    $protection =~ s/#include//;
    $protection =~ tr/a-z/A-z/;
    $protection =~ s/\s<//;
    $protection =~ s/>//;
    $protection = "HAVE_" . $protection;

    print $protection;
    
    $string_to_replace = "#ifdef $protection$_#endif\n";

    print $string_to_replace;

    open(C_FILES, "find . -name *.c |") || print "find failed\n";
    while (<C_FILES>) {
        $c_file = $_;
        if (not /mca/) {
            open(C_FILE, "$c_file") || print "Open failed on $c_file\n";
            chomp($protection);

            #ensure that this protection has not been already put in place
            $protected = 0;
            $written_to_file = 0;

            while (<C_FILE>) {
                if (/$protection/) {
                    $protected = 1;
                }
            }
            close (C_FILE);
            if ($protected == 0) { 
                #this file is not yet protected
                open(C_FILE, "$c_file") || print "Open failed on $c_file\n";
                open(TEMP, "> $temp_file") || print "Open failed on temp.c \n";

                while (<C_FILE>) {
                    if (/$header_string/) {
                        print TEMP $string_to_replace;
                        print "Replacing defintion ---- $c_file";
                        if ($written_to_file == 0) {
                            print MOD_FILES $c_file;
                            $written_to_file = 1;
                        }
                    } else {
                        print TEMP $_;
                    }
                }
                close (TEMP_C);
                system("cp $temp_file $c_file");
            }
        }
    }
    close (C_FILES);

    #Now to do the same for header files
    open(H_FILES, "find . -name *.h |") || print "find failed\n";
    while (<H_FILES>) {

        $h_file = $_;
        if (not /mca/) {
            open(H_FILE, "$h_file") || print "Open failed on $h_file\n";
            chomp($protection);

            #ensure that this protection has not been already put in place
            $protected = 0;
            $written_to_file = 0;

            while (<H_FILE>) {
                if (/$protection/) {
                    $protected = 1;
                }
            }
            close (H_FILE);
            if ($protected == 0) { 
                #this file is not yet protected
                open(H_FILE, "$h_file") || print "Open failed on $h_file\n";
                open(TEMP, "> $temp_file") || print "Open failed on temp.c \n";

                while (<H_FILE>) {
                    if (/$header_string/) {
                        print TEMP $string_to_replace;
                        print "Replacing defintion ---- $h_file";
                        if ($written_to_file == 0) {
                            print MOD_FILES $h_file;
                            $written_to_file = 1;
                        }
                    } else {
                        print TEMP $_;
                    }
                }
                close (TEMP_C);
                system("cp $temp_file $h_file");
            }
        }
    }
    close (H_FILES);
}
close(HEADERS);
close(MOD_FILES);
system("rm -f $temp_file");
