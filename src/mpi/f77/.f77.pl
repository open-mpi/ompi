#!/usr/bin/perl
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
        s/^\s+//; # trim leading whitespace
        s/\s+/ /g; # trim whitespace within text
        s/\s+$/\n/; # trim trailing whitespace
        /(.*[^;])\n/ && ($_ = $1); # remove multi-line function declarations
        s/void\s*\*+/char \*/g; # replaces void * and void ** parameters
        s/void//g; #these are for void arguments
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
    while (/\w+\w+\(.*mpi_.*/){
        s/(\w+\w+\(.*)mpi_\w+/$1MPI_Fint/;
    }
    s/\bint\b/MPI_Fint/g; #this removes all int's and replaces them with MPI_Fints
    s/\*+//g; #remove all pointers and pointers to pointers
    s/\[//g; #remove all array notations
    s/\]//g; #remove all array notations
    s/,\s+/,/g; # remove all spaces after commas, this is for future preparation
    s/(\w+)\s(\w+)([,\)])/$1 \*$2$3/g; #make all parameters pointers
    s/\)/,MPI_Fint *ierr\)/; #additional argument required for all fortran functions
    
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
    s/(mpi_\w+){1,1}/$1_/g;
    print MPIF__H;
}

close(MPIF_H);
close(MPIF__H);

#
# 2. generate _f 
#
open(MPIF_F_H, "> .mpif_f.h") || print "could not open mpif_f.h \n";
open(MPIF_H, ".mpif.h") || print "could not open mpif.h \n";

while (<MPIF_F_H>) {
    s/(mpi_\w+){1,1}/$1_/g;
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
    s/(mpi_\w+){1,1}/$1__/g;
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
