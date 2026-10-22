/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Exercise MPI_Abi_set_fortran_info().  When Open MPI is configured with
 * --disable-mpi-fortran, the implementation does not know the Fortran type
 * sizes, so MPI_Abi_get_fortran_info() returns MPI_INFO_NULL and the
 * application must supply them.  After doing so, MPI_Type_size() for the
 * Fortran datatypes must return the sizes the application specified.
 *
 * When Open MPI is configured with Fortran support (the default), the
 * implementation already knows the sizes, MPI_Abi_get_fortran_info()
 * returns a populated info object, and this test simply reports that and
 * exits successfully.
 */

#include "mpi.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

/*
 * Values taken from a typical MPICH configure with gfortran.
 */
#define FORTRAN_CHARACTER_SIZE        1
#define FORTRAN_LOGICAL_SIZE          4
#define FORTRAN_INTEGER_SIZE          4
#define FORTRAN_REAL_SIZE             4
#define FORTRAN_DOUBLE_PRECISION_SIZE 8

/* The literal values of .TRUE. and .FALSE. in Fortran */
#define F77_TRUE_VALUE  1
#define F77_FALSE_VALUE 0

static int errors = 0;

static void info_set_int(MPI_Info info, const char *key, int val)
{
    char str[20];
    snprintf(str, sizeof(str), "%d", val);
    MPI_Info_set(info, key, str);
}

static void check_type_size(MPI_Datatype type, const char *name, int expected)
{
    int size = -1;

    MPI_Type_size(type, &size);
    if (expected != size) {
        fprintf(stderr, "MPI_Type_size(%s) returned %d, expected %d\n",
                name, size, expected);
        errors++;
    }
}

/*
 * A mpi_<type><n>_supported key describes the Fortran compiler, not a
 * guarantee the build can realize the type.  When such a type is requested
 * but the build has no underlying type of that size, MPI_Abi_set_fortran_info
 * still succeeds and MPI_Type_size must report MPI_UNDEFINED.  It must never
 * report any size other than the requested one -- a wrong non-undefined size
 * would be a real bug.
 */
static void check_type_size_best_effort(MPI_Datatype type, const char *name,
                                        int expected)
{
    int size = -1;

    MPI_Type_size(type, &size);
    if (MPI_UNDEFINED != size && expected != size) {
        fprintf(stderr, "MPI_Type_size(%s) returned %d, expected %d or "
                "MPI_UNDEFINED\n", name, size, expected);
        errors++;
    }
}

int main(int argc, char **argv)
{
    int mpi_errno;
    MPI_Info info;

    MPI_Init(&argc, &argv);

    /*
     * The ABI info/boolean calls report errors through the predefined
     * communicator error handler (they take no handle of their own).  This
     * test deliberately makes calls it expects to fail with MPI_ERR_ABI, so
     * switch to a returning handler rather than the fatal default.
     */
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    MPI_Comm_set_errhandler(MPI_COMM_SELF, MPI_ERRORS_RETURN);

    mpi_errno = MPI_Abi_get_fortran_info(&info);
    if (MPI_SUCCESS != mpi_errno) {
        fprintf(stderr, "MPI_Abi_get_fortran_info failed\n");
        MPI_Finalize();
        return 1;
    }

    if (MPI_INFO_NULL == info) {
        /*
         * The implementation does not know the Fortran properties -- this
         * is a --disable-mpi-fortran build.  Supply them and verify that
         * the Fortran datatypes report the requested sizes.
         */

        /*
         * An info object carrying none of the recognized keys conveys no
         * Fortran type information.  It must be rejected with MPI_ERR_INFO
         * and must not consume the single permitted call, so the real
         * configuration below still succeeds.
         */
        MPI_Info_create(&info);
        MPI_Info_set(info, "an_unrelated_key", "value");
        mpi_errno = MPI_Abi_set_fortran_info(info);
        if (MPI_ERR_INFO != mpi_errno) {
            fprintf(stderr, "MPI_Abi_set_fortran_info with no recognized keys "
                    "returned %d, expected MPI_ERR_INFO\n", mpi_errno);
            errors++;
        }
        MPI_Info_free(&info);

        MPI_Info_create(&info);
        info_set_int(info, "mpi_character_size", FORTRAN_CHARACTER_SIZE);
        info_set_int(info, "mpi_logical_size", FORTRAN_LOGICAL_SIZE);
        info_set_int(info, "mpi_integer_size", FORTRAN_INTEGER_SIZE);
        info_set_int(info, "mpi_real_size", FORTRAN_REAL_SIZE);
        info_set_int(info, "mpi_double_precision_size", FORTRAN_DOUBLE_PRECISION_SIZE);
        MPI_Info_set(info, "mpi_logical4_supported", "true");
        MPI_Info_set(info, "mpi_integer1_supported", "true");
        MPI_Info_set(info, "mpi_integer2_supported", "true");
        MPI_Info_set(info, "mpi_integer4_supported", "true");
        MPI_Info_set(info, "mpi_integer8_supported", "true");
        MPI_Info_set(info, "mpi_real4_supported", "true");
        MPI_Info_set(info, "mpi_real8_supported", "true");
        MPI_Info_set(info, "mpi_complex8_supported", "true");
        MPI_Info_set(info, "mpi_complex16_supported", "true");
        MPI_Info_set(info, "mpi_double_complex_supported", "true");
        /*
         * Request an optional type the build may be unable to provide (no
         * 16-byte floating-point type exists on some platforms).  The call
         * must still succeed; availability is probed below.
         */
        MPI_Info_set(info, "mpi_real16_supported", "true");

        mpi_errno = MPI_Abi_set_fortran_info(info);
        if (MPI_SUCCESS != mpi_errno) {
            fprintf(stderr, "MPI_Abi_set_fortran_info failed\n");
            MPI_Info_free(&info);
            MPI_Finalize();
            return 1;
        }
        MPI_Info_free(&info);

        /* The default Fortran types must now report the requested sizes. */
        check_type_size(MPI_CHARACTER, "MPI_CHARACTER", FORTRAN_CHARACTER_SIZE);
        check_type_size(MPI_LOGICAL, "MPI_LOGICAL", FORTRAN_LOGICAL_SIZE);
        check_type_size(MPI_INTEGER, "MPI_INTEGER", FORTRAN_INTEGER_SIZE);
        check_type_size(MPI_REAL, "MPI_REAL", FORTRAN_REAL_SIZE);
        check_type_size(MPI_DOUBLE_PRECISION, "MPI_DOUBLE_PRECISION",
                        FORTRAN_DOUBLE_PRECISION_SIZE);

        /*
         * COMPLEX is a pair of default REALs and DOUBLE COMPLEX a pair of
         * DOUBLE PRECISION values, so their sizes follow the default REAL and
         * DOUBLE PRECISION sizes supplied above.
         */
        check_type_size(MPI_COMPLEX, "MPI_COMPLEX", 2 * FORTRAN_REAL_SIZE);
        check_type_size(MPI_DOUBLE_COMPLEX, "MPI_DOUBLE_COMPLEX",
                        2 * FORTRAN_DOUBLE_PRECISION_SIZE);

        /* Optional fixed-size types marked supported report their size. */
        check_type_size(MPI_LOGICAL4, "MPI_LOGICAL4", 4);
        check_type_size(MPI_INTEGER1, "MPI_INTEGER1", 1);
        check_type_size(MPI_INTEGER2, "MPI_INTEGER2", 2);
        check_type_size(MPI_INTEGER4, "MPI_INTEGER4", 4);
        check_type_size(MPI_INTEGER8, "MPI_INTEGER8", 8);
        check_type_size(MPI_REAL4, "MPI_REAL4", 4);
        check_type_size(MPI_REAL8, "MPI_REAL8", 8);
        check_type_size(MPI_COMPLEX8, "MPI_COMPLEX8", 8);
        check_type_size(MPI_COMPLEX16, "MPI_COMPLEX16", 16);

        /*
         * mpi_real16_supported was requested, but the build may lack a
         * 16-byte float.  MPI_REAL16 must therefore be either 16 bytes (if
         * realizable) or MPI_UNDEFINED (if not) -- never a wrong size.
         */
        check_type_size_best_effort(MPI_REAL16, "MPI_REAL16", 16);

        /*
         * A second call must fail with MPI_ERR_ABI per the MPI-5.0
         * "only the first call affects the state of the library" rule.
         */
        MPI_Info_create(&info);
        info_set_int(info, "mpi_integer_size", FORTRAN_INTEGER_SIZE);
        mpi_errno = MPI_Abi_set_fortran_info(info);
        if (MPI_ERR_ABI != mpi_errno) {
            fprintf(stderr, "second MPI_Abi_set_fortran_info returned %d, "
                    "expected MPI_ERR_ABI\n", mpi_errno);
            errors++;
        }
        MPI_Info_free(&info);

        /*
         * Exercise the Fortran boolean registration path as well.  In a
         * --disable-mpi-fortran build the literal values are not known, so
         * this should succeed.
         */
        int32_t logical_true = F77_TRUE_VALUE;
        int32_t logical_false = F77_FALSE_VALUE;
        mpi_errno = MPI_Abi_set_fortran_booleans(FORTRAN_LOGICAL_SIZE,
                                                 &logical_true, &logical_false);
        if (MPI_SUCCESS != mpi_errno) {
            fprintf(stderr, "MPI_Abi_set_fortran_booleans failed\n");
            errors++;
        }
    } else {
        /*
         * Fortran-enabled build: the implementation already knows the
         * sizes.  Setting them must be rejected with MPI_ERR_ABI.
         */
        MPI_Info_free(&info);

        MPI_Info_create(&info);
        info_set_int(info, "mpi_integer_size", FORTRAN_INTEGER_SIZE);
        mpi_errno = MPI_Abi_set_fortran_info(info);
        if (MPI_ERR_ABI != mpi_errno) {
            fprintf(stderr, "MPI_Abi_set_fortran_info returned %d in a "
                    "Fortran-enabled build, expected MPI_ERR_ABI\n", mpi_errno);
            errors++;
        }
        MPI_Info_free(&info);
    }

    MPI_Finalize();

    if (0 == errors) {
        printf("All tests passed\n");
    }
    return (0 == errors) ? 0 : 1;
}
