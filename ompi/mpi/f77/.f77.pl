#!/usr/bin/perl
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
# This script is used to automatically generate the fortran bindings. It generates
# four files .MPIF_H, mpif.h mpif_.h mpif__.h
#

#open the files
open(MPI_H, ".mpi_src.h") || print "could not open mpi.h \n";
open(MPIF_H, "> .mpif.h") || print "could not open mpif.h \n";

#iterate over the file till we are done with all changes
while (<MPI_H>) {
    if (!/c2f|f2c/) { #c2f functions are not needed
        $_ = lc($_);   
        s/\bconst\b//g; # consts are not a part of fortran bindings
        s/void\s*\*+/char /g; # replaces void * and void ** parameters
        s/void//g; #these are for void arguments
        s/^\s+//; # trim leading whitespace
        s/\s+/ /g; # trim whitespace within text
        s/\s+$/\n/; # trim trailing whitespace
        /(.*[^;])\n/ && ($_ = $1." "); # remove multi-line function declarations
        print MPIF_H;
    }
}

close(MPI_H);
close(MPIF_H);

#
# further processing
#
open(MPIF_H, ".mpif.h") || print "could not open mpif.h \n";
open(TEMP_H, "> temp.h") || print "could not open temp.h \n";

while (<MPIF_H>) {
    s/^int{1,1}/void/; #makes the return type of the functions as void if it was int before
    
    # this loop replaces mpi_comm, mpi_datatype etc with MPI_Fint * 
    #while (/\w+\w+\(.*mpi_.*/){
    #s/(\w+\w+\(.*)mpi_\w+/$1MPI_Fint/g;
    s/, +/,/g;
    s/\( +/\(/g;
    1 while s/(?<=[\(,\t])mpi_\w+/MPI_Fint/g;
    #}
    s/\bint\b/MPI_Fint/g; #this removes all int's and replaces them with MPI_Fints
    s/\*+//g; #remove all pointers and pointers to pointers
    s/\[//g; #remove all array notations
    s/\]//g; #remove all array notations
    # s/ +/ /g; # trim whitespace within text
    s/,\s+/,/g; # remove all spaces after commas, this is for future preparation
    s/(\w+)\s(\w+)([,\)])/$1 \*$2$3/g; #make all parameters pointers
    s/(\w+)\)/$1,MPI_Fint *ierr\)/; #additional argument required for all fortran functions
    s/\(\)/\(MPI_Fint *ierr\)/; #additional argument required for all fortran functions
    s/,/, /g;
    s/,\s*\.\.\.//g; # this is to handle mpi_pcontrol    

    print TEMP_H;
}

close(TEMP_H);
close(MPIF_H);
system ("mv temp.h .mpif.h");

#
#now to generate the different flavors of the fortran bindings, we need to have 
#_f, all caps, single underscore and double underscore. This should be simple to 
#generate from the above file
#

#
# 1. generate single underscore
#
open(MPIF__H, "> .mpif_.h") || print "could not open .mpif_.h \n";
open(MPIF_H, ".mpif.h") || print "could not open .mpif.h \n";

while (<MPIF_H>) {
    s/(mpi_\w+){1,1}/$1_/;
    print MPIF__H;
}

close(MPIF_H);
close(MPIF__H);

#
# 2. generate _f 
#
open(MPIF_F_H, "> .mpif_f.h") || print "could not open mpif_f.h \n";
open(MPIF_H, ".mpif.h") || print "could not open mpif.h \n";

while (<MPIF_H>) {
    s/(mpi_\w+){1,1}/$1_f/;
    print MPIF_F_H;
}

close(MPIF_H);
close(MPIF_F_H);

#
# 3. generate double underscore
#
open(MPIF___H, "> .mpif__.h") || print "could not open mpif__.h \n";
open(MPIF_H, ".mpif.h") || print "could not open mpif.h \n";

while (<MPIF_H>) {
    s/(mpi_\w+){1,1}/$1__/;
    print MPIF___H;
}

close(MPIF_H);
close(MPIF___H);

#
# 4. generate all caps
#
open(mpif_h, "> .MPIF.h") || print "could not open MPIF.h \n";
open(MPIF_H, ".mpif.h") || print "could not open mpif.h \n";

while (<MPIF_H>) {
    (/(\w+\s)(mpi_\w+){1,1}(.*\n)/) && ($_ = $1.uc($2).$3);
    print mpif_h;
}

close(MPIF_H);
close(mpif_h);

#
# Now we have to put all this together to form prototypes_mpi.h
# the template for prototypes_mpi.h is as follows
#
$header = 
"#ifndef OMPI_F77_PROTOTYPES_MPI_H
#define OMPI_F77_PROTOTYPES_MPI_H
/*
 * \$HEADER\$
 * This file prototypes all MPI fortran functions in all four fortran
 * symbol conventions as well as all the internal real OMPI wrapper
 * functions (different from any of the four fortran symbol
 * conventions for clarity, at the cost of more typing for me...).
 * This file is included in the top-level build ONLY. The prototyping
 * is done ONLY for MPI_* bindings
 *
 * Zeroth, the OMPI wrapper functions, with a _f suffix.
 *
 * This is needed ONLY if the lower-level prototypes_pmpi.h has not
 * already been included
 */
#ifndef OMPI_F77_PROTOTYPES_PMPI_H
/* 
 * mpi_*_f definitions go here --- .mpif_f.h 
 */\n";
$endif = "#endif\n\n";

#
# Now lets open up all the files and write it to prototypes.h
#
open(proto, "> prototypes_mpi.h") || print "could not open prototypes_mpi.h \n";
open(MPIF_H, ".mpif.h") || print "could not open .mpif.h \n";
open(MPIF_F_H, ".mpif_f.h") || print "could not open .mpif_f.h \n";
open(MPIF__H, ".mpif_.h") || print "could not open .mpif_.h \n";
open(MPIF___H, ".mpif__.h") || print "could not open .mpif__.h \n";
open(mpif_h, ".MPIF.h") || print "could not open .MPIF.h \n";

print proto $header;

while (<MPIF_F_H>) {
    print proto;
}
print proto $endif;

print proto "/*These are the all lower case prototypes*/\n\n";

while (<MPIF_H>) {
    print proto;
}

print proto "/*This is the single underscore prototypes*/\n\n";

while (<MPIF__H>) {
    print proto;
}

print proto "/*This is the double underscore prototypes*/\n\n";

while (<MPIF___H>) {
    print proto;
}
 
print proto "/*This is the all upper case prototypes*/\n\n";

while (<mpif_h>) {
    print proto;
}

print proto $endif;

close(MPIF_H);
close(MPIF__H);
close(MPIF___H);
close(MPIF_F_H);
close(mpif_h);
close(proto);

#
# Now we have the prototype ready, all we need to do is generate the files. For this, we need to 
# write out all the #defines. To do this we need a template in which we call fill in things wherever
# we need it.
#

# This is the body of the function
$function_body =
"{

}\n";

# This is the boiler plate header which goes on top of every header
$boiler_plate = 
"/*
 * \$HEADER\$
 */

#include \"ompi_config.h\"

#include <stdio.h>

#include \"mpi.h\"
#include \"mpi/f77/bindings.h\"\n\n";

$have_weak_and_want_profile = "#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER";
$no_weak_and_want_profile = "#elif OMPI_PROFILE_LAYER";
$have_weak = "#if OPAL_HAVE_WEAK_SYMBOLS";
$no_weak_and_no_profile = "#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER";

#
# Now lets get down to the actual dirty job 
#
open (MPIF_H, ".mpif.h") || print "Could not open .mpif.h for creating files\n";
#
# This while loop goes through every single entry and creates the files
#
while (<MPIF_H>) {
    (/\w+\s(mpi_\w+)/) && ($function_name = $1);
    # get all the required variables in 
    $function_name_ = $function_name . "_";
    $function_name__ = $function_name . "__";
    $FUNCTION_NAME = uc($function_name);
    $function_name_f = $function_name . "_f"; 
    (/mpi_(\w+)/) && ($file_name = $1 . "_f.c");
    (/(\(.*\))/) && ($function_params_with_types = $1);
    $function_params_without_types =  $function_params_with_types;
    $function_params_without_types =~ s/\w+\s\*//g;
    (/^(\w+\s)(mpi_\w+)(.*);/) && ($function_signature = $1.$function_name_f.$3."\n"); 

    #create the weak symbol declarations
    $pmpi_weak = "
#pragma weak P$FUNCTION_NAME = $function_name_f
#pragma weak p$function_name = $function_name_f
#pragma weak p$function_name_ = $function_name_f
#pragma weak p$function_name__ = $function_name_f\n";

    $mpi_weak = "
#pragma weak $FUNCTION_NAME = $function_name_f
#pragma weak $function_name = $function_name_f
#pragma weak $function_name_ = $function_name_f
#pragma weak $function_name__ = $function_name_f\n";

    #create the non-weak symbol variables
    $redefine_pmpi = "
OMPI_GENERATE_F77_BINDINGS (P$FUNCTION_NAME,
                           p$function_name,
                           p$function_name_,
                           p$function_name__,
                           p$function_name_f,
                           $function_params_with_types,
                           $function_params_without_types )\n";

    $redefine_mpi = "
OMPI_GENERATE_F77_BINDINGS ($FUNCTION_NAME,
                           $function_name,
                           $function_name_,
                           $function_name__,
                           $function_name_f,
                           $function_params_with_types,
                           $function_params_without_types )\n";

    # Now we can start creating the file
    open(FILE, "> $file_name") || print "Could not open $file_name for writing\n";

    #write out the header
    print FILE $boiler_plate;
    print FILE $have_weak_and_want_profile;
    print FILE $pmpi_weak;
    print FILE $no_weak_and_want_profile;
    print FILE $redefine_pmpi;
    print FILE $endif;
    print FILE $have_weak;
    print FILE $mpi_weak;
    print FILE $endif;
    print FILE $no_weak_and_no_profile;
    print FILE $redefine_mpi;
    print FILE $endif;
    print FILE $function_signature;
    print FILE $function_body;
    
    close(FILE);
}

close(MPIF_H);

# Now have to generate profile/defines.h and profile/prototypes_mpi.h
# both of these arew easy. profile/defines.h is nothing but #defines
# of the mpi_*_f functions to be pmpi functions

$header = 
"#ifndef OMPI_F77_PROTOTYPES_PMPI_H
#define OMPI_F77_PROTOTYPES_PMPI_H
/*
 * \$HEADER\$
 * This file prototypes all MPI fortran functions in all four fortran
 * symbol conventions as well as all the internal real OMPI wrapper
 * functions (different from any of the four fortran symbol
 * conventions for clarity, at the cost of more typing for me...).
 * This file is included in the top-level build ONLY. The prototyping
 * is done ONLY for MPI_* bindings
 *
 * Zeroth, the OMPI wrapper functions, with a _f suffix.
 *
 * This is needed ONLY if the lower-level prototypes_pmpi.h has not
 * already been included
 */
/* 
 * pmpi_*_f definitions go here --- .mpif_f.h 
 */\n ";
$endif = "#endif\n\n";

#
# Now lets open up all the files and write it to prototypes.h
#
open(proto, "> prototypes_pmpi.h")|| print "Could not open prototypes_pmpi.h\n";
open(MPIF_H, ".mpif.h") || print "could not open .mpif.h \n";
open(MPIF_F_H, ".mpif_f.h") || print "could not open .mpif_f.h \n";
open(MPIF__H, ".mpif_.h") || print "could not open .mpif_.h \n";
open(MPIF___H, ".mpif__.h") || print "could not open .mpif__.h \n";
open(mpif_h, ".MPIF.h") || print "could not open .MPIF.h \n";

print proto $header;

while (<MPIF_F_H>) {
    (/^(\w+\s)(mpi_\w+)(.*)/) && ($_ = $1."p".$2.$3."\n"); 
    print proto;
}

print proto "/*This is the all lower case prototypes*/\n\n";

while (<MPIF_H>) {
    (/^(\w+\s)(mpi_\w+)(.*)/) && ($_ = $1."p".$2.$3."\n"); 
    print proto;
}

print proto "/*This is the single underscore prototypes*/\n\n";

while (<MPIF__H>) {
    (/^(\w+\s)(mpi_\w+)(.*)/) && ($_ = $1."p".$2.$3."\n"); 
    print proto;
}

print proto "/*This is the double underscore prototypes*/\n\n";

while (<MPIF___H>) {
    (/^(\w+\s)(mpi_\w+)(.*)/) && ($_ = $1."p".$2.$3."\n"); 
    print proto;
}
 
print proto "/*This is the all upper case prototypes*/\n\n";

while (<mpif_h>) {
    (/^(\w+\s)(MPI_\w+)(.*)/) && ($_ = $1."P".$2.$3."\n"); 
    print proto;
}

print proto $endif;

close(MPIF_H);
close(MPIF__H);
close(MPIF___H);
close(MPIF_F_H);
close(mpif_h);
close(proto);

#
# Now to generate the defines for mpi to pmpi
#
open(MPI_H, ".mpif_f.h") || print "Error opening .mpif_f.h";
open(PMPI_H, "> defines.h") || print "Error opening defines.h";

$boiler_plate = "
/*
 * \$HEADER\$
 */
#ifndef OMPI_F77_PROFILE_DEFINES_H
#define OMPI_F77_PROFILE_DEFINES_H\n\n";

print PMPI_H $boiler_plate;

while (<MPI_H>) {
    (/^(\w+\s)(mpi_\w+)(.*)/) && ($mpi_definition = $2); 
    $line_to_print = "#define $mpi_definition p$mpi_definition\n";
    print PMPI_H $line_to_print;
}

print PMPI_H $endif;

close(MPI_H);
close(PMPI_H);
    
system("mv prototypes_pmpi.h profile/prototypes_pmpi.h");
system("mv defines.h profile/defines.h");
system("rm -rf .mpif_f.h .mpif.h .mpif_.h .mpif__.h MPIF.h");
