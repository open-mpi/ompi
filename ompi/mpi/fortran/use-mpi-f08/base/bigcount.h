/*
 * Copyright (c) 2024      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

/*
 * Bigcount array conversion macros for Fortran templates.
 */

#define OMPI_FORTRAN_BIGCOUNT_ARRAY_SET(array, tmp_array, n) \
    do { \
        if (sizeof(*(array)) == sizeof(*(tmp_array))) { \
            (tmp_array) = (void *)(array); \
        } else { \
            (tmp_array) = malloc(sizeof(*tmp_array) * n); \
            for (int bigcount_array_i = 0; bigcount_array_i < n; ++bigcount_array_i) { \
                (tmp_array)[bigcount_array_i] = (array)[bigcount_array_i]; \
            } \
        } \
    } while (0)

#define OMPI_FORTRAN_BIGCOUNT_ARRAY_COPYOUT(array, tmp_array, n) \
    do { \
        if ((array) != (tmp_array) && NULL != (tmp_array)) { \
            for (int bigcount_array_i = 0; bigcount_array_i < n; ++bigcount_array_i) { \
                (array)[bigcount_array_i] = (tmp_array)[bigcount_array_i]; \
            } \
        } \
    } while (0)


#define OMPI_FORTRAN_BIGCOUNT_ARRAY_CLEANUP(array, tmp_array) \
    do { \
        if ((void *)(array) != (void *)(tmp_array) && NULL != (tmp_array)) { \
            free(tmp_array); \
            tmp_array = NULL; \
        } \
    } while (0)

/*
 * Settle who owns a converted array once a nonblocking call has returned,
 * leaving tmp_array holding exactly what the request should take over and
 * NULL otherwise.  A conversion that did not allocate left the user's own
 * array in tmp_array, and an error means no request was produced to hand
 * anything to; both of those are disowned here.
 *
 * The caller then passes the surviving tmp_array pointers to
 * ompi_coll_base_nbc_set_cleanup_v() or _w(), which frees them once the
 * collective is done with them.
 */
#define OMPI_FORTRAN_BIGCOUNT_ARRAY_CLEANUP_NONBLOCKING(array, tmp_array, c_ierr) \
    do { \
        if (MPI_SUCCESS != (c_ierr) || (void *)(array) == (void *)(tmp_array)) { \
            OMPI_FORTRAN_BIGCOUNT_ARRAY_CLEANUP((array), (tmp_array)); \
            (tmp_array) = NULL; \
        } \
    } while (0)
