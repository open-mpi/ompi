/*
 * Copyright (c) 2026      Jeff Squyres.  All rights reserved.
 * Copyright (c) 2026      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

/*
 * Unit tests for the ABI Fortran-boolean helpers in
 * ompi/mpi/c/ompi_abi_fortran.c: ompi_abi_get_fortran_booleans() and
 * ompi_abi_set_fortran_booleans(), which back the MPI_Abi_get_fortran_booleans
 * / MPI_Abi_set_fortran_booleans public ABI routines.
 *
 * These functions do not require MPI to be initialized: everything they
 * branch on is a compile-time constant or file-static state, so they can be
 * exercised directly.  The logic under test is the power-of-two / bit-index
 * argument guard (which previously carried an off-by-one, inverted-guard bug)
 * and the "call set at most once" contract mandated by MPI-5.0.
 */

#include "ompi_config.h"

#include <stdint.h>
#include <stdio.h>

#include "mpi.h"
#include "ompi/mpi/c/bindings.h"
#include "support.h"

int main(int argc, char *argv[])
{
    /* Initialize MPI for the ABI functions that require it */
    MPI_Init(&argc, &argv);

    /* Set error handler to return (not abort) so we can test error cases */
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

    /* 16-byte scratch buffers: large enough for the widest logical
     * (opal_int128_t) any build could store, so a get/set for a valid size
     * never overruns regardless of Fortran support or platform logical
     * sizes.  The implementation reads and writes these through
     * int8_t..int64_t (and opal_int128_t) pointers, so the storage must be
     * suitably aligned -- a plain char array would rely on lucky stack
     * alignment on strict-alignment platforms. */
    union abi_logical_scratch {
        unsigned char bytes[16];
        int64_t align8[2];
    };
    union abi_logical_scratch true_storage = { {0} };
    union abi_logical_scratch false_storage = { {0} };
    unsigned char *logical_true = true_storage.bytes;
    unsigned char *logical_false = false_storage.bytes;
    char msg[128];
    size_t i;

    test_init("ompi_abi_fortran_booleans");

    /*
     * ompi_abi_get_fortran_booleans() must reject any logical_size that is
     * not a power of two with MPI_ERR_ARG.  This guard runs before the
     * build-conditional (Fortran vs. no-Fortran) code, so the contract holds
     * in every build.  The get routine has no single-call latch, so we can
     * probe many sizes.
     */
    {
        const int not_pow2[] = { 0, 3, 5, 6, 7, 9, 12, 15, 100 };
        for (i = 0; i < sizeof(not_pow2) / sizeof(not_pow2[0]); i++) {
            int is_set = -1;
            int rc = MPI_Abi_get_fortran_booleans(not_pow2[i], logical_true,
                                                    logical_false, &is_set);
            if (MPI_ERR_ARG == rc) {
                test_success();
            } else {
                snprintf(msg, sizeof(msg),
                         "MPI_Abi_get_fortran_booleans(%d) expected MPI_ERR_ARG, got %d",
                         not_pow2[i], rc);
                test_failure(msg);
            }
        }
    }

    /*
     * Powers of two in the representable index range (1, 2, 4, 8, 16 map to
     * indices 0..4) must NOT be rejected as bad arguments.  Depending on the
     * build (Fortran support and the platform's logical sizes) *is_set may be
     * 0 or 1, but the return code must never be MPI_ERR_ARG.
     */
    {
        const int pow2_ok[] = { 1, 2, 4, 8, 16 };
        for (i = 0; i < sizeof(pow2_ok) / sizeof(pow2_ok[0]); i++) {
            int is_set = -1;
            int rc = MPI_Abi_get_fortran_booleans(pow2_ok[i], logical_true,
                                                    logical_false, &is_set);
            if (MPI_ERR_ARG != rc) {
                test_success();
            } else {
                snprintf(msg, sizeof(msg),
                         "MPI_Abi_get_fortran_booleans(%d) unexpectedly returned MPI_ERR_ARG",
                         pow2_ok[i]);
                test_failure(msg);
            }
        }
    }

    /*
     * ompi_abi_set_fortran_booleans() may configure the Fortran logicals at
     * most once per process (MPI-5.0): only the first call that actually
     * affects the library's state counts against the limit, and any later
     * call returns MPI_ERR_ABI.  A call that fails argument validation stores
     * nothing, so it must NOT consume the single allowed call -- a subsequent
     * valid call must still be able to succeed.  This block therefore runs
     * after the get probes above, since its successful call latches the
     * one-shot for the remainder of the process.
     */
    {
#if OMPI_HAVE_FORTRAN_LOGICAL
        /* This build knows the Fortran compiler's LOGICAL
         * representation, so the implementation owns this state and
         * rejects every registration attempt (the get path returns the
         * compiler-derived values and never consults a user
         * registration). */
        int rc = MPI_Abi_set_fortran_booleans(4, logical_true, logical_false);
        if (MPI_ERR_ABI == rc) {
            test_success();
        } else {
            snprintf(msg, sizeof(msg),
                     "MPI_Abi_set_fortran_booleans(4) with Fortran support expected "
                     "MPI_ERR_ABI, got %d", rc);
            test_failure(msg);
        }
#else
        /* A non-power-of-two size is rejected and must not trip the latch. */
        int rc = MPI_Abi_set_fortran_booleans(3, logical_true, logical_false);
        if (MPI_ERR_ARG == rc) {
            test_success();
        } else {
            snprintf(msg, sizeof(msg),
                     "MPI_Abi_set_fortran_booleans(3) expected MPI_ERR_ARG, got %d", rc);
            test_failure(msg);
        }

        /* The first valid call configures the logicals and succeeds. */
        rc = MPI_Abi_set_fortran_booleans(4, logical_true, logical_false);
        if (MPI_SUCCESS == rc) {
            test_success();
        } else {
            snprintf(msg, sizeof(msg),
                     "first valid MPI_Abi_set_fortran_booleans(4) expected MPI_SUCCESS, got %d",
                     rc);
            test_failure(msg);
        }

        /* A second call, after a successful one, must return MPI_ERR_ABI. */
        rc = MPI_Abi_set_fortran_booleans(4, logical_true, logical_false);
        if (MPI_ERR_ABI == rc) {
            test_success();
        } else {
            snprintf(msg, sizeof(msg),
                     "second MPI_Abi_set_fortran_booleans() expected MPI_ERR_ABI, got %d",
                     rc);
            test_failure(msg);
        }
#endif
    }

    MPI_Finalize();
    return test_finalize();
}
