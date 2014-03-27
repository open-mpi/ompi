#!/usr/bin/env perl
#
# Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This script creates header files to be compiled with the various
# Fortran bindings.  In some cases, we need Fortran PARAMETER values;
# in other cases, we need #define preprocessor macros.
#
# This script generates both cases, and ensures that the values are
# the same between both (e.g., that MPI_COMM_WORLD is both a fortran
# INTEGER PARAMETER of value 0 and is #define'd to be 0).
#
# Additionally, since Open MPI provides the configure ability to
# compile out the entire MPI IO interface, all the IO
# handles/constants are generated in separate .h files in the
# non-preprocessor case, and included in relevant #if's in the
# preprocessor case.
#
# Files are generated in the following directories:
#
#   ompi/include
#   ompi/mpi/fortran/use-mpi-f08
#

use strict;

#----------------------------------------------------------------------------

# Write an output file only if a) the output file does not already
# exist, or b) it exists, but its contents are different than $str.

sub write_file {
    my ($filename_out, $str) = @_;

    my $need_write = 0;
    if (! -f $filename_out) {
        $need_write = 1;
    } else {
        open(FILE_IN, $filename_out) || die "Couldn't open $filename_out";
        my $tmp;
        $tmp .= $_
            while (<FILE_IN>);
        close(FILE_IN);
        if ($str ne $tmp) {
            $need_write = 1;
        }
    }
    
    if ($need_write) {
        open(FILE_OUT, ">$filename_out") || die "Couldn't open $filename_out";
        print FILE_OUT $str;
        close(FILE_OUT);
        print "created $filename_out\n";
    } else {
        print "$filename_out unchanged; not written\n";
    }
}

#----------------------------------------------------------------------------

print "creating Fortran header files (with common constants)...\n";

# Find the OMPI topdir.  It is likely the pwd.
my $topdir;
if (-r "ompi/include/mpi.h.in") {
    $topdir = ".";
} elsif (-r "include/mpi.h.in") {
    $topdir = "..";
} elsif (-r "mpi.h.in") {
    $topdir = "../..";
} else {
    print "Please run this script from the Open MPI topdir or topdir/include/mpi\n";
    print "Aborting.\n";
    exit(1);
}

#----------------------------------------------------------------------------

my $handles;

$handles->{MPI_COMM_WORLD} = 0;
$handles->{MPI_COMM_SELF} = 1;
$handles->{MPI_GROUP_EMPTY} = 1;
$handles->{MPI_ERRORS_ARE_FATAL} = 1;
$handles->{MPI_ERRORS_RETURN} = 2;

$handles->{MPI_MAX} =  1;
$handles->{MPI_MIN} =  2;
$handles->{MPI_SUM} =  3;
$handles->{MPI_PROD} =  4;
$handles->{MPI_LAND} =  5;
$handles->{MPI_BAND} =  6;
$handles->{MPI_LOR} =  7;
$handles->{MPI_BOR} =  8;
$handles->{MPI_LXOR} =  9;
$handles->{MPI_BXOR} = 10;
$handles->{MPI_MAXLOC} = 11;
$handles->{MPI_MINLOC} = 12;
$handles->{MPI_REPLACE} = 13;

$handles->{MPI_COMM_NULL} = 2;
$handles->{MPI_DATATYPE_NULL} = 0;
$handles->{MPI_ERRHANDLER_NULL} = 0;
$handles->{MPI_GROUP_NULL} = 0;
$handles->{MPI_INFO_NULL} = 0;
$handles->{MPI_MESSAGE_NULL} = 0;
$handles->{MPI_OP_NULL} = 0;
$handles->{MPI_REQUEST_NULL} = 0;
$handles->{MPI_WIN_NULL} = 0;
$handles->{MPI_MESSAGE_NULL} = 0;

$handles->{MPI_BYTE} =  1;
$handles->{MPI_PACKED} =  2;
$handles->{MPI_UB} =  3;
$handles->{MPI_LB} =  4;
$handles->{MPI_CHARACTER} =  5;
$handles->{MPI_LOGICAL} =  6;
$handles->{MPI_INTEGER} =  7;
$handles->{MPI_INTEGER1} =  8;
$handles->{MPI_INTEGER2} =  9;
$handles->{MPI_INTEGER4} = 10;
$handles->{MPI_INTEGER8} = 11;
$handles->{MPI_INTEGER16} = 12;
$handles->{MPI_REAL} = 13;
$handles->{MPI_REAL4} = 14;
$handles->{MPI_REAL8} = 15;
$handles->{MPI_REAL16} = 16;
$handles->{MPI_DOUBLE_PRECISION} = 17;
$handles->{MPI_COMPLEX} = 18;
$handles->{MPI_COMPLEX8} = 19;
$handles->{MPI_COMPLEX16} = 20;
$handles->{MPI_COMPLEX32} = 21;
$handles->{MPI_DOUBLE_COMPLEX} = 22;
$handles->{MPI_2REAL} = 23;
$handles->{MPI_2DOUBLE_PRECISION} = 24;
$handles->{MPI_2INTEGER} = 25;
$handles->{MPI_2COMPLEX} = 26;
$handles->{MPI_2DOUBLE_COMPLEX} = 27;
$handles->{MPI_REAL2} = 28;
$handles->{MPI_LOGICAL1} = 29;
$handles->{MPI_LOGICAL2} = 30;
$handles->{MPI_LOGICAL4} = 31;
$handles->{MPI_LOGICAL8} = 32;
$handles->{MPI_WCHAR} = 33;
$handles->{MPI_CHAR} = 34;
$handles->{MPI_UNSIGNED_CHAR} = 35;
$handles->{MPI_SIGNED_CHAR} = 36;
$handles->{MPI_SHORT} = 37;
$handles->{MPI_UNSIGNED_SHORT} = 38;
$handles->{MPI_INT} = 39;
$handles->{MPI_UNSIGNED} = 40;
$handles->{MPI_LONG} = 41;
$handles->{MPI_UNSIGNED_LONG} = 42;
$handles->{MPI_LONG_LONG_INT} = 43;
$handles->{MPI_UNSIGNED_LONG_LONG} = 44;
$handles->{MPI_FLOAT} = 45;
$handles->{MPI_DOUBLE} = 46;
$handles->{MPI_LONG_DOUBLE} = 47;
$handles->{MPI_FLOAT_INT} = 48;
$handles->{MPI_DOUBLE_INT} = 49;
$handles->{MPI_LONGDBL_INT} = 50;
$handles->{MPI_LONG_INT} = 51;
$handles->{MPI_2INT} = 52;
$handles->{MPI_SHORT_INT} = 53;
$handles->{MPI_CXX_BOOL} = 54;
$handles->{MPI_CXX_CPLEX} = 55;
$handles->{MPI_CXX_DBLCPLEX} = 56;
$handles->{MPI_CXX_LDBLCPLEX} = 57;
$handles->{MPI_INT8_T} = 58;
$handles->{MPI_UINT8_T} = 59;
$handles->{MPI_INT16_T} = 60;
$handles->{MPI_UINT16_T} = 61;
$handles->{MPI_INT32_T} = 62;
$handles->{MPI_UINT32_T} = 63;
$handles->{MPI_INT64_T} = 64;
$handles->{MPI_UINT64_T} = 65;
$handles->{MPI_AINT} = 66;
$handles->{MPI_OFFSET} = 67;
$handles->{MPI_C_COMPLEX} = 68;
$handles->{MPI_C_FLOAT_COMPLEX} = 69;
$handles->{MPI_C_DOUBLE_COMPLEX} = 70;
$handles->{MPI_C_LONG_DOUBLE_COMPLEX} = 71;
$handles->{MPI_COUNT} = 72;

$handles->{MPI_MESSAGE_NO_PROC} = 1;

$handles->{MPI_INFO_ENV} = 1;

#----------------------------------------------------------------------------

my $io_handles;

$io_handles->{MPI_FILE_NULL} = 0;

#----------------------------------------------------------------------------

my $constants;

$constants->{MPI_VERSION} = 3;
$constants->{MPI_SUBVERSION} = 0;

$constants->{MPI_ANY_SOURCE} = -1;
$constants->{MPI_ANY_TAG} = -1;
$constants->{MPI_PROC_NULL} = -2;
$constants->{MPI_ROOT} = -4;
$constants->{MPI_UNDEFINED} = -32766;
$constants->{MPI_CART} = 1;
$constants->{MPI_GRAPH} = 2;
$constants->{MPI_DIST_GRAPH} = 3;
$constants->{MPI_KEYVAL_INVALID} = -1;
$constants->{MPI_SOURCE} = 1;
$constants->{MPI_TAG} = 2;
$constants->{MPI_ERROR} = 3;
$constants->{MPI_TAG_UB} = 0;
$constants->{MPI_HOST} = 1;
$constants->{MPI_IO} = 2;
$constants->{MPI_WTIME_IS_GLOBAL} = 3;
$constants->{MPI_APPNUM} = 4;
$constants->{MPI_LASTUSEDCODE} = 5;
$constants->{MPI_UNIVERSE_SIZE} = 6;
$constants->{MPI_WIN_BASE} = 7;
$constants->{MPI_WIN_SIZE} = 8;
$constants->{MPI_WIN_DISP_UNIT} = 9;

$constants->{MPI_BSEND_OVERHEAD} = 128;
$constants->{MPI_ORDER_C} = 0;
$constants->{MPI_ORDER_FORTRAN} = 1;
$constants->{MPI_DISTRIBUTE_BLOCK} = 0;
$constants->{MPI_DISTRIBUTE_CYCLIC} = 1;
$constants->{MPI_DISTRIBUTE_NONE} = 2;
$constants->{MPI_DISTRIBUTE_DFLT_DARG} = -1;
$constants->{MPI_TYPECLASS_INTEGER} = 1;
$constants->{MPI_TYPECLASS_REAL} = 2;
$constants->{MPI_TYPECLASS_COMPLEX} = 3;
$constants->{MPI_MODE_NOCHECK} = 1;
$constants->{MPI_MODE_NOPRECEDE} = 2;
$constants->{MPI_MODE_NOPUT} = 4;
$constants->{MPI_MODE_NOSTORE} = 8;
$constants->{MPI_MODE_NOSUCCEED} = 16;
$constants->{MPI_LOCK_EXCLUSIVE} = 1;
$constants->{MPI_LOCK_SHARED} = 2;

$constants->{MPI_THREAD_SINGLE} = 0;
$constants->{MPI_THREAD_FUNNELED} = 1;
$constants->{MPI_THREAD_SERIALIZED} = 2;
$constants->{MPI_THREAD_MULTIPLE} = 3;

$constants->{MPI_SUCCESS} = 0;
$constants->{MPI_ERR_BUFFER} = 1;
$constants->{MPI_ERR_COUNT} = 2;
$constants->{MPI_ERR_TYPE} = 3;
$constants->{MPI_ERR_TAG} = 4;
$constants->{MPI_ERR_COMM} = 5;
$constants->{MPI_ERR_RANK} = 6;
$constants->{MPI_ERR_REQUEST} = 7;
$constants->{MPI_ERR_ROOT} = 8;
$constants->{MPI_ERR_GROUP} = 9;
$constants->{MPI_ERR_OP} = 10;
$constants->{MPI_ERR_TOPOLOGY} = 11;
$constants->{MPI_ERR_DIMS} = 12;
$constants->{MPI_ERR_ARG} = 13;
$constants->{MPI_ERR_UNKNOWN} = 14;
$constants->{MPI_ERR_TRUNCATE} = 15;
$constants->{MPI_ERR_OTHER} = 16;
$constants->{MPI_ERR_INTERN} = 17;
$constants->{MPI_ERR_IN_STATUS} = 18;
$constants->{MPI_ERR_PENDING} = 19;
$constants->{MPI_ERR_ACCESS} = 20;
$constants->{MPI_ERR_AMODE} = 21;
$constants->{MPI_ERR_ASSERT} = 22;
$constants->{MPI_ERR_BAD_FILE} = 23;
$constants->{MPI_ERR_BASE} = 24;
$constants->{MPI_ERR_CONVERSION} = 25;
$constants->{MPI_ERR_DISP} = 26;
$constants->{MPI_ERR_DUP_DATAREP} = 27;
$constants->{MPI_ERR_FILE_EXISTS} = 28;
$constants->{MPI_ERR_FILE_IN_USE} = 29;
$constants->{MPI_ERR_FILE} = 30;
$constants->{MPI_ERR_INFO_KEY} = 31;
$constants->{MPI_ERR_INFO_NOKEY} = 32;
$constants->{MPI_ERR_INFO_VALUE} = 33;
$constants->{MPI_ERR_INFO} = 34;
$constants->{MPI_ERR_IO} = 35;
$constants->{MPI_ERR_KEYVAL} = 36;
$constants->{MPI_ERR_LOCKTYPE} = 37;
$constants->{MPI_ERR_NAME} = 38;
$constants->{MPI_ERR_NO_MEM} = 39;
$constants->{MPI_ERR_NOT_SAME} = 40;
$constants->{MPI_ERR_NO_SPACE} = 41;
$constants->{MPI_ERR_NO_SUCH_FILE} = 42;
$constants->{MPI_ERR_PORT} = 43;
$constants->{MPI_ERR_QUOTA} = 44;
$constants->{MPI_ERR_READ_ONLY} = 45;
$constants->{MPI_ERR_RMA_CONFLICT} = 46;
$constants->{MPI_ERR_RMA_SYNC} = 47;
$constants->{MPI_ERR_SERVICE} = 48;
$constants->{MPI_ERR_SIZE} = 49;
$constants->{MPI_ERR_SPAWN} = 50;
$constants->{MPI_ERR_UNSUPPORTED_DATAREP} = 51;
$constants->{MPI_ERR_UNSUPPORTED_OPERATION} = 52;
$constants->{MPI_ERR_WIN} = 53;
# these error codes will never be returned by a fortran function
# since there are no fortran bindings for MPI_T
$constants->{MPI_T_ERR_MEMORY} = 54;
$constants->{MPI_T_ERR_NOT_INITIALIZED} = 55;
$constants->{MPI_T_ERR_CANNOT_INIT} = 56;
$constants->{MPI_T_ERR_INVALID_INDEX} = 57;
$constants->{MPI_T_ERR_INVALID_ITEM} = 58;
$constants->{MPI_T_ERR_INVALID_HANDLE} = 59;
$constants->{MPI_T_ERR_OUT_OF_HANDLES} = 60;
$constants->{MPI_T_ERR_OUT_OF_SESSIONS} = 61;
$constants->{MPI_T_ERR_INVALID_SESSION} = 62;
$constants->{MPI_T_ERR_CVAR_SET_NOT_NOW} = 63;
$constants->{MPI_T_ERR_CVAR_SET_NEVER} = 64;
$constants->{MPI_T_ERR_PVAR_NO_STARTSTOP} = 65;
$constants->{MPI_T_ERR_PVAR_NO_WRITE} = 66;
$constants->{MPI_T_ERR_PVAR_NO_ATOMIC} = 67;
$constants->{MPI_ERR_RMA_RANGE} = 68;
$constants->{MPI_ERR_RMA_ATTACH} = 69;
$constants->{MPI_ERR_RMA_FLAVOR} = 70;
$constants->{MPI_ERR_RMA_SHARED} = 71;
$constants->{MPI_ERR_LASTCODE} = $constants->{MPI_ERR_RMA_SHARED};

$constants->{MPI_ERR_SYSRESOURCE} = -2;

$constants->{MPI_IDENT} = 0;
$constants->{MPI_CONGRUENT} = 1;
$constants->{MPI_SIMILAR} = 2;
$constants->{MPI_UNEQUAL} = 3;

$constants->{MPI_COMBINER_NAMED} = 0;
$constants->{MPI_COMBINER_DUP} = 1;
$constants->{MPI_COMBINER_CONTIGUOUS} = 2;
$constants->{MPI_COMBINER_VECTOR} = 3;
$constants->{MPI_COMBINER_HVECTOR_INTEGER} = 4;
$constants->{MPI_COMBINER_HVECTOR} = 5;
$constants->{MPI_COMBINER_INDEXED} = 6;
$constants->{MPI_COMBINER_HINDEXED_INTEGER} = 7;
$constants->{MPI_COMBINER_HINDEXED} = 8;
$constants->{MPI_COMBINER_INDEXED_BLOCK} = 9;
$constants->{MPI_COMBINER_STRUCT_INTEGER} = 10;
$constants->{MPI_COMBINER_STRUCT} = 11;
$constants->{MPI_COMBINER_SUBARRAY} = 12;
$constants->{MPI_COMBINER_DARRAY} = 13;
$constants->{MPI_COMBINER_F90_REAL} = 14;
$constants->{MPI_COMBINER_F90_COMPLEX} = 15;
$constants->{MPI_COMBINER_F90_INTEGER} = 16;
$constants->{MPI_COMBINER_RESIZED} = 17;
$constants->{MPI_COMBINER_HINDEXED_BLOCK} = 18;

$constants->{MPI_COMM_TYPE_SHARED} = 0;

#----------------------------------------------------------------------------

my $io_constants;

$io_constants->{MPI_SEEK_SET} = 600;
$io_constants->{MPI_SEEK_CUR} = 602;
$io_constants->{MPI_SEEK_END} = 604;
$io_constants->{MPI_MODE_CREATE} = 1;
$io_constants->{MPI_MODE_RDONLY} = 2;
$io_constants->{MPI_MODE_WRONLY} = 4;
$io_constants->{MPI_MODE_RDWR} = 8;
$io_constants->{MPI_MODE_DELETE_ON_CLOSE} = 16;
$io_constants->{MPI_MODE_UNIQUE_OPEN} = 32;
$io_constants->{MPI_MODE_EXCL} = 64;
$io_constants->{MPI_MODE_APPEND} = 128;
$io_constants->{MPI_MODE_SEQUENTIAL} = 256;
$io_constants->{MPI_DISPLACEMENT_CURRENT} = -54278278;

#----------------------------------------------------------------------------

# Fortran handles file

my $header = '! -*- fortran -*-
! WARNING! THIS IS A GENERATED FILE!!
! ANY EDITS YOU PUT HERE WILL BE LOST!
! ==> Instead, edit topdir/ompi/include/mpif-values.pl.

! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2010 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

';

sub write_fortran_file {
    my ($header, $vals, $file) = @_;

    foreach my $key (sort(keys(%{$vals}))) {
        $header .= "        integer $key\n";
    }
    $header .= "\n";
    foreach my $key (sort(keys(%{$vals}))) {
        $header .= "        parameter ($key=$vals->{$key})\n";
    }

    write_file($file, $header);
}

write_fortran_file($header, $handles, 
                   "$topdir/ompi/include/mpif-handles.h");
write_fortran_file($header, $constants, 
                   "$topdir/ompi/include/mpif-constants.h");
write_fortran_file($header, $io_handles, 
                   "$topdir/ompi/include/mpif-io-handles.h");
write_fortran_file($header, $io_constants, 
                   "$topdir/ompi/include/mpif-io-constants.h");

#----------------------------------------------------------------------------

# Create preprocessor files

my $output = '/* WARNING! THIS IS A GENERATED FILE!!
 * ANY EDITS YOU PUT HERE WILL BE LOST!
 * Instead, edit topdir/ompi/include/mpif-values.pl
 */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2009-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef USE_MPI_F08_CONSTANTS_H
#define USE_MPI_F08_CONSTANTS_H

';

foreach my $key (sort(keys(%{$constants}))) {
    $output .= "#define OMPI_$key $constants->{$key}\n";
}
$output .= "\n";
foreach my $key (sort(keys(%{$handles}))) {
    $output .= "#define OMPI_$key $handles->{$key}\n";
}

$output .= "\n#if OMPI_PROVIDE_MPI_FILE_INTERFACE\n";
foreach my $key (sort(keys(%{$io_constants}))) {
    $output .= "#define OMPI_$key $io_constants->{$key}\n";
}
$output .= "\n";
foreach my $key (sort(keys(%{$io_handles}))) {
    $output .= "#define OMPI_$key $io_handles->{$key}\n";
}
$output .= "#endif /* OMPI_PROVIDE_MPI_FILE_INTERFACE */

#endif /* USE_MPI_F08_CONSTANTS_H */\n";

write_file("$topdir/ompi/mpi/fortran/use-mpi-f08/constants.h", $output);

exit(0);
