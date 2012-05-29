#!/usr/bin/env perl
#
# Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This file generates common parameter values needed for the three
# MPI bindings.  It creates the files:
#
#  ompi/include/mpif-common.h
#  ommp/mpi/fortran/use-mpi-f08/constants.h

use strict;

#----------------------------------------------------------------------------

# Read a file; return its contents in a string

sub read_file {
    my ($filename_in) = @_;

    open(FILE_IN, $filename_in)  || die "Couldn't open file $filename_in";
    my $fin;
    $fin .= $_
        while (<FILE_IN>);
    close(FILE_IN);

    return $fin;
}

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

my $MPI_COMM_WORLD       = 0;
my $MPI_COMM_SELF        = 1;
my $MPI_GROUP_EMPTY      = 1;
my $MPI_ERRORS_ARE_FATAL = 1;
my $MPI_ERRORS_RETURN    = 2;

#
#  lookup table indices
#
my $MPI_MAX      =  1;
my $MPI_MIN      =  2;
my $MPI_SUM      =  3;
my $MPI_PROD     =  4;
my $MPI_LAND     =  5;
my $MPI_BAND     =  6;
my $MPI_LOR      =  7;
my $MPI_BOR      =  8;
my $MPI_LXOR     =  9;
my $MPI_BXOR     = 10;
my $MPI_MAXLOC   = 11;
my $MPI_MINLOC   = 12;
my $MPI_REPLACE  = 13;

#
# NULL "handles" (indices)
#
my $MPI_COMM_NULL       = 2;
my $MPI_DATATYPE_NULL   = 0;
my $MPI_ERRHANDLER_NULL = 0;
my $MPI_GROUP_NULL      = 0;
my $MPI_INFO_NULL       = 0;
my $MPI_MESSAGE_NULL    = 0;
my $MPI_OP_NULL         = 0;
my $MPI_REQUEST_NULL    = 0;
my $MPI_WIN_NULL        = 0;
my $MPI_MESSAGE_NULL    = 0;

my $MPI_BYTE               =  1;
my $MPI_PACKED             =  2;
my $MPI_UB                 =  3;
my $MPI_LB                 =  4;
my $MPI_CHARACTER          =  5;
my $MPI_LOGICAL            =  6;
my $MPI_INTEGER            =  7;
my $MPI_INTEGER1           =  8;
my $MPI_INTEGER2           =  9;
my $MPI_INTEGER4           = 10;
my $MPI_INTEGER8           = 11;
my $MPI_INTEGER16          = 12;
my $MPI_REAL               = 13;
my $MPI_REAL4              = 14;
my $MPI_REAL8              = 15;
my $MPI_REAL16             = 16;
my $MPI_DOUBLE_PRECISION   = 17;
my $MPI_COMPLEX            = 18;
my $MPI_COMPLEX8           = 19;
my $MPI_COMPLEX16          = 20;
my $MPI_COMPLEX32          = 21;
my $MPI_DOUBLE_COMPLEX     = 22;
my $MPI_2REAL              = 23;
my $MPI_2DOUBLE_PRECISION  = 24;
my $MPI_2INTEGER           = 25;
my $MPI_2COMPLEX           = 26;
my $MPI_2DOUBLE_COMPLEX    = 27;
my $MPI_REAL2              = 28;
my $MPI_LOGICAL1           = 29;
my $MPI_LOGICAL2           = 30;
my $MPI_LOGICAL4           = 31;
my $MPI_LOGICAL8           = 32;
my $MPI_WCHAR              = 33;
my $MPI_CHAR               = 34;
my $MPI_UNSIGNED_CHAR      = 35;
my $MPI_SIGNED_CHAR        = 36;
my $MPI_SHORT              = 37;
my $MPI_UNSIGNED_SHORT     = 38;
my $MPI_INT                = 39;
my $MPI_UNSIGNED           = 40;
my $MPI_LONG               = 41;
my $MPI_UNSIGNED_LONG      = 42;
my $MPI_LONG_LONG_INT      = 43;
my $MPI_UNSIGNED_LONG_LONG = 44;
my $MPI_FLOAT              = 45;
my $MPI_DOUBLE             = 46;
my $MPI_LONG_DOUBLE        = 47;
my $MPI_FLOAT_INT          = 48;
my $MPI_DOUBLE_INT         = 49;
my $MPI_LONGDBL_INT        = 50;
my $MPI_LONG_INT           = 51;
my $MPI_2INT               = 52;
my $MPI_SHORT_INT          = 53;
my $MPI_CXX_BOOL           = 54;
my $MPI_CXX_CPLEX          = 55;
my $MPI_CXX_DBLCPLEX       = 56;
my $MPI_CXX_LDBLCPLEX      = 57;
my $MPI_INT8_T             = 58;
my $MPI_UINT8_T            = 59;
my $MPI_INT16_T            = 60;
my $MPI_UINT16_T           = 61;
my $MPI_INT32_T            = 62;
my $MPI_UINT32_T           = 63;
my $MPI_INT64_T            = 64;
my $MPI_UINT64_T           = 65;
my $MPI_AINT               = 66;
my $MPI_OFFSET             = 67;

#
# create ompi/include/mpif-common.h
#

my $filename_in  = "$topdir/ompi/include/mpif-common.h.fin";
my $filename_out = $filename_in;
$filename_out =~ s/\.fin$//;

my $input = read_file($filename_in);

# Add warning to the top
$input = "! -*- fortran -*-
! WARNING! THIS IS A GENERATED FILE!!
! ANY EDITS YOU PUT HERE WILL BE LOST!
! ==> Instead, edit topdir/ompi/include/mpif-common.pl.

$input";

# Add more to the end
$input .= "!
!     NULL 'handles' (indices)
!

      integer MPI_GROUP_NULL, MPI_COMM_NULL, MPI_DATATYPE_NULL
      integer MPI_REQUEST_NULL, MPI_OP_NULL, MPI_ERRHANDLER_NULL
      integer MPI_INFO_NULL, MPI_WIN_NULL, MPI_MESSAGE_NULL

      parameter (MPI_GROUP_NULL=$MPI_GROUP_NULL)
      parameter (MPI_COMM_NULL=$MPI_COMM_NULL)
      parameter (MPI_DATATYPE_NULL=$MPI_DATATYPE_NULL)
      parameter (MPI_REQUEST_NULL=$MPI_REQUEST_NULL)
      parameter (MPI_OP_NULL=$MPI_OP_NULL)
      parameter (MPI_ERRHANDLER_NULL=$MPI_ERRHANDLER_NULL)
      parameter (MPI_INFO_NULL=$MPI_INFO_NULL)
      parameter (MPI_WIN_NULL=$MPI_WIN_NULL)
      parameter (MPI_MESSAGE_NULL=$MPI_MESSAGE_NULL)

      integer MPI_COMM_WORLD, MPI_COMM_SELF
      integer MPI_GROUP_EMPTY
      integer MPI_ERRORS_ARE_FATAL, MPI_ERRORS_RETURN

      integer OMPI_MPI_COMM_WORLD
      integer OMPI_MPI_COMM_SELF

      parameter (MPI_COMM_WORLD=$MPI_COMM_WORLD)
      parameter (MPI_COMM_SELF=$MPI_COMM_SELF)
      parameter (MPI_GROUP_EMPTY=$MPI_GROUP_EMPTY)
      parameter (MPI_ERRORS_ARE_FATAL=$MPI_ERRORS_ARE_FATAL)
      parameter (MPI_ERRORS_RETURN=$MPI_ERRORS_RETURN)

!
!     lookup table indices
!

      integer MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, MPI_LAND
      integer MPI_BAND, MPI_LOR, MPI_BOR, MPI_LXOR, MPI_BXOR
      integer MPI_MAXLOC, MPI_MINLOC, MPI_REPLACE

      parameter (MPI_MAX=$MPI_MAX)
      parameter (MPI_MIN=$MPI_MIN)
      parameter (MPI_SUM=$MPI_SUM)
      parameter (MPI_PROD=$MPI_PROD)
      parameter (MPI_LAND=$MPI_LAND)
      parameter (MPI_BAND=$MPI_BAND)
      parameter (MPI_LOR=$MPI_LOR)
      parameter (MPI_BOR=$MPI_BOR)
      parameter (MPI_LXOR=$MPI_LXOR)
      parameter (MPI_BXOR=$MPI_BXOR)
      parameter (MPI_MAXLOC=$MPI_MAXLOC)
      parameter (MPI_MINLOC=$MPI_MINLOC)
      parameter (MPI_REPLACE=$MPI_REPLACE)

      integer MPI_BYTE, MPI_PACKED, MPI_UB, MPI_LB
      integer MPI_CHARACTER, MPI_LOGICAL
      integer MPI_INTEGER, MPI_INTEGER1, MPI_INTEGER2, MPI_INTEGER4
      integer MPI_INTEGER8, MPI_INTEGER16
      integer MPI_REAL, MPI_REAL2, MPI_REAL4, MPI_REAL8, MPI_REAL16
      integer MPI_DOUBLE_PRECISION
      integer MPI_COMPLEX, MPI_COMPLEX8, MPI_COMPLEX16, MPI_COMPLEX32
      integer MPI_DOUBLE_COMPLEX
      integer MPI_2REAL, MPI_2DOUBLE_PRECISION, MPI_2INTEGER
      integer MPI_2COMPLEX, MPI_2DOUBLE_COMPLEX

! Note that MPI_LOGICALx are not defined by the MPI spec, but there are
! other MPI implementations that have them, so it's good for us to have
! as well.

      integer MPI_LOGICAL1, MPI_LOGICAL2, MPI_LOGICAL4, MPI_LOGICAL8

! All other MPI types including the C and C++, as well as the
! ones defined in the MPI 2.2

      integer MPI_WCHAR, MPI_CHAR
      integer MPI_SIGNED_CHAR, MPI_UNSIGNED_CHAR
      integer MPI_SHORT, MPI_UNSIGNED_SHORT
      integer MPI_INT, MPI_UNSIGNED, MPI_LONG
      integer MPI_UNSIGNED_LONG, MPI_LONG_LONG_INT
      integer MPI_UNSIGNED_LONG_LONG
      integer MPI_FLOAT, MPI_DOUBLE, MPI_LONG_DOUBLE
      integer MPI_FLOAT_INT, MPI_DOUBLE_INT
      integer MPI_LONGDBL_INT, MPI_LONG_INT
      integer MPI_2INT, MPI_SHORT_INT
      integer MPI_CXX_BOOL, MPI_CXX_CPLEX
      integer MPI_CXX_DBLCPLEX, MPI_CXX_LDBLCPLEX
      integer MPI_INT8_T, MPI_UINT8_T
      integer MPI_INT16_T, MPI_UINT16_T
      integer MPI_INT32_T, MPI_UINT32_T
      integer MPI_INT64_T, MPI_UINT64_T
      integer MPI_AINT, MPI_OFFSET

!
!     Do NOT change the order of these parameters
!

      parameter (MPI_BYTE               =  $MPI_BYTE)
      parameter (MPI_PACKED             =  $MPI_PACKED)
      parameter (MPI_UB                 =  $MPI_UB)
      parameter (MPI_LB                 =  $MPI_LB)
      parameter (MPI_CHARACTER          =  $MPI_CHARACTER)
      parameter (MPI_LOGICAL            =  $MPI_LOGICAL)
      parameter (MPI_INTEGER            =  $MPI_INTEGER)
      parameter (MPI_INTEGER1           =  $MPI_INTEGER1)
      parameter (MPI_INTEGER2           =  $MPI_INTEGER2)
      parameter (MPI_INTEGER4           = $MPI_INTEGER4)
      parameter (MPI_INTEGER8           = $MPI_INTEGER8)
      parameter (MPI_INTEGER16          = $MPI_INTEGER16)
      parameter (MPI_REAL               = $MPI_REAL)
      parameter (MPI_REAL4              = $MPI_REAL4)
      parameter (MPI_REAL8              = $MPI_REAL8)
      parameter (MPI_REAL16             = $MPI_REAL16)
      parameter (MPI_DOUBLE_PRECISION   = $MPI_DOUBLE_PRECISION)
      parameter (MPI_COMPLEX            = $MPI_COMPLEX)
      parameter (MPI_COMPLEX8           = $MPI_COMPLEX8)
      parameter (MPI_COMPLEX16          = $MPI_COMPLEX16)
      parameter (MPI_COMPLEX32          = $MPI_COMPLEX32)
      parameter (MPI_DOUBLE_COMPLEX     = $MPI_DOUBLE_COMPLEX)
      parameter (MPI_2REAL              = $MPI_2REAL)
      parameter (MPI_2DOUBLE_PRECISION  = $MPI_2DOUBLE_PRECISION)
      parameter (MPI_2INTEGER           = $MPI_2INTEGER)
      parameter (MPI_2COMPLEX           = $MPI_2COMPLEX)
      parameter (MPI_2DOUBLE_COMPLEX    = $MPI_2DOUBLE_COMPLEX)
      parameter (MPI_REAL2              = $MPI_REAL2)
      parameter (MPI_LOGICAL1           = $MPI_LOGICAL1)
      parameter (MPI_LOGICAL2           = $MPI_LOGICAL2)
      parameter (MPI_LOGICAL4           = $MPI_LOGICAL4)
      parameter (MPI_LOGICAL8           = $MPI_LOGICAL8)
      parameter (MPI_WCHAR              = $MPI_WCHAR)
      parameter (MPI_CHAR               = $MPI_CHAR)
      parameter (MPI_UNSIGNED_CHAR      = $MPI_UNSIGNED_CHAR)
      parameter (MPI_SIGNED_CHAR        = $MPI_SIGNED_CHAR)
      parameter (MPI_SHORT              = $MPI_SHORT)
      parameter (MPI_UNSIGNED_SHORT     = $MPI_UNSIGNED_SHORT)
      parameter (MPI_INT                = $MPI_INT)
      parameter (MPI_UNSIGNED           = $MPI_UNSIGNED)
      parameter (MPI_LONG               = $MPI_LONG)
      parameter (MPI_UNSIGNED_LONG      = $MPI_UNSIGNED_LONG)
      parameter (MPI_LONG_LONG_INT      = $MPI_LONG_LONG_INT)
      parameter (MPI_UNSIGNED_LONG_LONG = $MPI_UNSIGNED_LONG_LONG)
      parameter (MPI_FLOAT              = $MPI_FLOAT)
      parameter (MPI_DOUBLE             = $MPI_DOUBLE)
      parameter (MPI_LONG_DOUBLE        = $MPI_LONG_DOUBLE)
      parameter (MPI_FLOAT_INT          = $MPI_FLOAT_INT)
      parameter (MPI_DOUBLE_INT         = $MPI_DOUBLE_INT)
      parameter (MPI_LONGDBL_INT        = $MPI_LONGDBL_INT)
      parameter (MPI_LONG_INT           = $MPI_LONG_INT)
      parameter (MPI_2INT               = $MPI_2INT)
      parameter (MPI_SHORT_INT          = $MPI_SHORT_INT)
      parameter (MPI_CXX_BOOL           = $MPI_CXX_BOOL)
      parameter (MPI_CXX_CPLEX          = $MPI_CXX_CPLEX)
      parameter (MPI_CXX_DBLCPLEX       = $MPI_CXX_DBLCPLEX)
      parameter (MPI_CXX_LDBLCPLEX      = $MPI_CXX_LDBLCPLEX)
      parameter (MPI_INT8_T             = $MPI_INT8_T)
      parameter (MPI_UINT8_T            = $MPI_UINT8_T)
      parameter (MPI_INT16_T            = $MPI_INT16_T)
      parameter (MPI_UINT16_T           = $MPI_UINT16_T)
      parameter (MPI_INT32_T            = $MPI_INT32_T)
      parameter (MPI_UINT32_T           = $MPI_UINT32_T)
      parameter (MPI_INT64_T            = $MPI_INT64_T)
      parameter (MPI_UINT64_T           = $MPI_UINT64_T)
      parameter (MPI_AINT               = $MPI_AINT)
      parameter (MPI_OFFSET             = $MPI_OFFSET)\n";

# Write the file
write_file($filename_out, $input);

#
# create ompi/mpi/fortran/use-mpi-f08/constants.h
#

$filename_in  = "$topdir/ompi/mpi/fortran/use-mpi-f08/constants.h.fin";
$filename_out = $filename_in;
$filename_out =~ s/\.fin$//;

$input = read_file($filename_in);

# Add warning to the top
$input = "/* WARNING! THIS IS A GENERATED FILE!!
 * ANY EDITS YOU PUT HERE WILL BE LOST!
 * Instead, edit topdir/ompi/include/mpif-common.pl
 */

$input";

# Add more to the end
$input .= "
#define OMPI_MPI_COMM_WORLD       $MPI_COMM_WORLD
#define OMPI_MPI_COMM_SELF        $MPI_COMM_SELF
#define OMPI_MPI_GROUP_EMPTY      $MPI_GROUP_EMPTY
#define OMPI_MPI_ERRORS_ARE_FATAL $MPI_ERRORS_ARE_FATAL
#define OMPI_MPI_ERRORS_RETURN    $MPI_ERRORS_RETURN

/*
 * lookup table indices
 */

#define OMPI_MPI_MAX       $MPI_MAX
#define OMPI_MPI_MIN       $MPI_MIN
#define OMPI_MPI_SUM       $MPI_SUM
#define OMPI_MPI_PROD      $MPI_PROD
#define OMPI_MPI_LAND      $MPI_LAND
#define OMPI_MPI_BAND      $MPI_BAND
#define OMPI_MPI_LOR       $MPI_LOR
#define OMPI_MPI_BOR       $MPI_BOR
#define OMPI_MPI_LXOR      $MPI_LXOR
#define OMPI_MPI_BXOR      $MPI_BXOR
#define OMPI_MPI_MAXLOC    $MPI_MAXLOC
#define OMPI_MPI_MINLOC    $MPI_MINLOC
#define OMPI_MPI_REPLACE   $MPI_REPLACE

/*
 * NULL 'handles' (indices)
 */

#define OMPI_MPI_GROUP_NULL      $MPI_GROUP_NULL
#define OMPI_MPI_COMM_NULL       $MPI_COMM_NULL
#define OMPI_MPI_DATATYPE_NULL   $MPI_DATATYPE_NULL
#define OMPI_MPI_REQUEST_NULL    $MPI_REQUEST_NULL
#define OMPI_MPI_OP_NULL         $MPI_OP_NULL
#define OMPI_MPI_ERRHANDLER_NULL $MPI_ERRHANDLER_NULL
#define OMPI_MPI_INFO_NULL       $MPI_INFO_NULL
#define OMPI_MPI_WIN_NULL        $MPI_WIN_NULL
#define OMPI_MPI_MESSAGE_NULL    $MPI_MESSAGE_NULL

#define OMPI_MPI_BYTE                $MPI_BYTE
#define OMPI_MPI_PACKED              $MPI_PACKED
#define OMPI_MPI_UB                  $MPI_UB
#define OMPI_MPI_LB                  $MPI_LB
#define OMPI_MPI_CHARACTER           $MPI_CHARACTER
#define OMPI_MPI_LOGICAL             $MPI_LOGICAL
#define OMPI_MPI_INTEGER             $MPI_INTEGER
#define OMPI_MPI_INTEGER1            $MPI_INTEGER1
#define OMPI_MPI_INTEGER2            $MPI_INTEGER2
#define OMPI_MPI_INTEGER4           $MPI_INTEGER4
#define OMPI_MPI_INTEGER8           $MPI_INTEGER8
#define OMPI_MPI_INTEGER16          $MPI_INTEGER16
#define OMPI_MPI_REAL               $MPI_REAL
#define OMPI_MPI_REAL4              $MPI_REAL4
#define OMPI_MPI_REAL8              $MPI_REAL8
#define OMPI_MPI_REAL16             $MPI_REAL16
#define OMPI_MPI_DOUBLE_PRECISION   $MPI_DOUBLE_PRECISION
#define OMPI_MPI_COMPLEX            $MPI_COMPLEX
#define OMPI_MPI_COMPLEX8           $MPI_COMPLEX8
#define OMPI_MPI_COMPLEX16          $MPI_COMPLEX16
#define OMPI_MPI_COMPLEX32          $MPI_COMPLEX32
#define OMPI_MPI_DOUBLE_COMPLEX     $MPI_DOUBLE_COMPLEX
#define OMPI_MPI_2REAL              $MPI_2REAL
#define OMPI_MPI_2DOUBLE_PRECISION  $MPI_2DOUBLE_PRECISION
#define OMPI_MPI_2INTEGER           $MPI_2INTEGER
#define OMPI_MPI_2COMPLEX           $MPI_2COMPLEX
#define OMPI_MPI_2DOUBLE_COMPLEX    $MPI_2DOUBLE_COMPLEX
#define OMPI_MPI_REAL2              $MPI_REAL2
#define OMPI_MPI_LOGICAL1           $MPI_LOGICAL1
#define OMPI_MPI_LOGICAL2           $MPI_LOGICAL2
#define OMPI_MPI_LOGICAL4           $MPI_LOGICAL4
#define OMPI_MPI_LOGICAL8           $MPI_LOGICAL8\n

#endif /* USE_MPI_F08_CONSTANTS_H */\n";

write_file($filename_out, $input);
exit(0);
