#!/usr/bin/env perl
#
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Script to generate the overloaded MPI_SIZEOF interfaces and
# subroutine bodies for both the mpi and mpi_f08 modules.
#
# This script won't really be necessary (i.e., be a whole lot simpler)
# when Fortran compilers uniformly supprort TS 29113 -- i.e., they
# support dimension(..).  Using dimension(..), you can have just *one*
# procedure for every type, and dimension(..) will resolve to both
# scalars and all possible ranks.
#
# But for the meantime, we generate for all ranks so that we support
# as many compilers as possible.  :-\ (we don't check the compiler and
# see if it supports dimension(..) and do a different generation based
# on that, because we already have a zillion different options in the
# Fortran support -- let's just do MPI_Sizeof this one way in the name
# of simplicity...).
#

use strict;

use Getopt::Long;

my $header_arg;
my $impl_arg;
my $ierror_arg;
my $maxrank_arg;
my $generate_arg;
my $mpi_arg;
my $mpi_real16;
my $mpi_complex32;
my $pmpi_arg;
my $help_arg = 0;

&Getopt::Long::Configure("bundling");
my $ok = Getopt::Long::GetOptions("complex32=i" => \$mpi_complex32,
                                  "header=s" => \$header_arg,
                                  "impl=s" => \$impl_arg,
                                  "ierror=s" => \$ierror_arg,
                                  "maxrank=s" => \$maxrank_arg,
                                  "generate=i" => \$generate_arg,
                                  "mpi" => \$mpi_arg,
                                  "pmpi" => \$pmpi_arg,
                                  "real16=i" => \$mpi_real16,
                                  "help|h" => \$help_arg);

die "Must specify header and/or impl filenames to output"
    if (!defined($header_arg) && !defined($impl_arg));
die "ierror handling must be optional or mandatory"
    if (defined($generate_arg) && $generate_arg &&
        (lc($ierror_arg) ne "optional" && lc($ierror_arg) ne "mandatory"));
die "max array rank must be >= 4 and <=15"
    if (defined($generate_arg) && $generate_arg &&
        (!defined($maxrank_arg) || $maxrank_arg < 4 || $maxrank_arg > 15));
die "Must specify --pmpi and/or --mpi if --impl is specified"
    if (defined($generate_arg) && $generate_arg &&
        (defined($impl_arg) && !defined($mpi_arg) && !defined($pmpi_arg)));
die "Must specify real16 and complex32"
    if (!defined($mpi_real16) || !defined($mpi_complex32));

#############################################################################

my $optional_ierror_param;
my $optional_ierror_statement;
if (lc($ierror_arg) eq "optional") {
    $optional_ierror_param = ", OPTIONAL";
    $optional_ierror_statement = "IF (present(ierror)) ";
}

my $indent = "      ";

#############################################################################

my $subs;

sub queue_sub {
    my ($f_type, $suffix, $import_type) = @_;

    # Leave off the MPI/PMI prefix; we'll add that when outputting
    my $sub_name = "Sizeof_$suffix";

    # Make a hash for this subroutine
    my $subr;
    $subr->{name} = $sub_name;
    my $start = "${indent}SUBROUTINE ^PREFIX^$sub_name^RANK^(x, size, ierror)\n";
    $start .= "${indent}  USE, INTRINSIC :: iso_fortran_env, ONLY: " . uc($import_type) . "\n"
        if (defined($import_type));
    # For long type names and large ranks, this first line gets very
    # long and only narrowly squeezed in before 72 columns.  Use no
    # whitespace.
    $start .= $indent . uc($f_type) . "^DIMENSION^::x
${indent}  INTEGER, INTENT(OUT) :: size
${indent}  INTEGER$optional_ierror_param, INTENT(OUT) :: ierror";
    $subr->{start} = $start;
    $subr->{middle} = "${indent}  size = storage_size(x) / 8
${indent}  ${optional_ierror_statement}ierror = 0";
    $subr->{end} = "${indent}END SUBROUTINE ^PREFIX^$sub_name^RANK^";

    # Save it in the overall hash
    $subs->{$sub_name} = $subr;
}

sub generate {
    my ($prefix, $sub_name, $rank, $want_body) = @_;

    my $subr;
    # Deep copy
    %{$subr} = %{$subs->{$sub_name}};

    # Make the initial version
    my $str = $subr->{start} . "\n";
    $str .= "\n" . $subr->{middle} . "\n"
        if ($want_body);
    $str .= $subr->{end} . "\n";

    # Substitute in the relevant parameters
    $str =~ s/\^PREFIX\^/$prefix/g;

    # If rank is 0, generate a scalar version.  Otherwise, generate an
    # array version.
    if (0 == $rank) {
        $str =~ s/\^RANK\^/_scalar/g;
        $str =~ s/\^DIMENSION\^//;
    } else {
        $str =~ s/\^RANK\^/_r$rank/g;
        my $dim;
        my $d = $rank;
        while ($d > 1) {
            $dim .= "1,";
            --$d;
        }
        $str =~ s/\^DIMENSION\^/, DIMENSION($dim*)/;
    }

    # All done
    return $str;
}

#############################################################################
# Main
#############################################################################

for my $size (qw/8 16 32 64/) {
    queue_sub("integer(int${size})", "int${size}", "int${size}");
}
for my $size (qw/32 64 128/) {
    if ($size != 128 || $mpi_real16 == 1) {
        queue_sub("real(real${size})", "real${size}", "real${size}");
    }
    if ($size != 128 || $mpi_complex32 == 1) {
        queue_sub("complex(real${size})", "complex${size}", "real${size}");
    }
}

#######################################################

sub output_content {
    my ($prefix, $want_bodies) = @_;

    print OUT "${indent}INTERFACE ${prefix}Sizeof\n\n"
        if (!$want_bodies);

    # Print all the module procedure lines
    foreach my $sub_name (sort(keys(%{$subs}))) {
        my $rank = 0;
        while ($rank <= $maxrank_arg) {
            my $str = generate($prefix, $sub_name, $rank, $want_bodies);
            print OUT $str . "\n";
            ++$rank;
        }
    }

    print OUT "${indent}END INTERFACE ${prefix}Sizeof\n\n"
        if (!$want_bodies);
}

# Output each file
sub output_file {
    my ($filename, $want_bodies) = @_;

    unlink($filename);
    open(OUT, ">$filename") || die "Can't open $filename for writing";
    print OUT "! -*- f90 -*-
! WARNING: This is a generated file!  Edits will be lost!
!
! Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
! \$COPYRIGHT\$
!
! This file was generated by gen-mpi-sizeof.pl for all the MPI_SIZEOF
! interface possibilities for intrinsic types.  Once TS 29113 is
! supported in all compilers, we can simply have *one* procedure for
! each type and use dimension(..) to indicate scalars+all array ranks.
! But until more compilers support this, we simply generate a
! procedure for scalars and all possible ranks in an attempt to
! support lots of Fortran compilers.\n\n";

    # Only output if the generate arg is 0.  Otherwise, output an
    # empty .h file (that is still safe to include by mpif.h, but
    # won't include the MPI_SIZEOF interface block).
    if ($generate_arg) {
        output_content("MPI_", $want_bodies)
            if (!$want_bodies ||
                ($want_bodies && $mpi_arg));
        output_content("PMPI_", $want_bodies)
            if (!$want_bodies ||
                ($want_bodies && $pmpi_arg));
    } else {
        print OUT "! *** ATTENTION!
!
! Sad panda.
!
! This compiler does not support the Right Stuff to enable MPI_SIZEOF.
! Specifically: we need support for the INTERFACE keyword,
! ISO_FORTRAN_ENV, and the STORAGE_SIZE() intrinsic on all types.
! Apparently, this compiler does not support both of those things, so
! this file will be blank (i.e., we didn't bother generating the
! necessary stuff for MPI_SIZEOF because the compiler doesn't support
! it).
!
! If you want support for MPI_SIZEOF, please use a different Fortran
! compiler to build Open MPI.\n\n";
    }

    close(OUT);
}

output_file($header_arg, 0)
    if (defined($header_arg));
output_file($impl_arg, 1)
    if (defined($impl_arg));

exit(0);
