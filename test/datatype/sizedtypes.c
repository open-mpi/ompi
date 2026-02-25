/*
 * Copyright (c) 2024      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Test that all optional Fortran predefined datatypes are always defined in
 * mpi.h (per MPI-5 section 20.4), and that MPI_Type_size returns
 * MPI_UNDEFINED for types that are not supported on the current platform.
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

#define CHECK_TYPE(type, name, expected)                                    \
    do {                                                                    \
        MPI_Type_size(type, &size);                                         \
        if (size != (expected)) {                                           \
            fprintf(stderr, "FAIL: MPI_Type_size(%s) = %d, expected %d\n", \
                    name, size, (expected));                                \
            errors++;                                                       \
        }                                                                   \
    } while (0)

int main(int argc, char *argv[])
{
    int size;
    int errors = 0;

    MPI_Init(&argc, &argv);

#if OMPI_HAVE_FORTRAN_LOGICAL1
    CHECK_TYPE(MPI_LOGICAL1,  "MPI_LOGICAL1",  OMPI_SIZEOF_FORTRAN_LOGICAL1);
#else
    CHECK_TYPE(MPI_LOGICAL1,  "MPI_LOGICAL1",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL2
    CHECK_TYPE(MPI_LOGICAL2,  "MPI_LOGICAL2",  OMPI_SIZEOF_FORTRAN_LOGICAL2);
#else
    CHECK_TYPE(MPI_LOGICAL2,  "MPI_LOGICAL2",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL4
    CHECK_TYPE(MPI_LOGICAL4,  "MPI_LOGICAL4",  OMPI_SIZEOF_FORTRAN_LOGICAL4);
#else
    CHECK_TYPE(MPI_LOGICAL4,  "MPI_LOGICAL4",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL8
    CHECK_TYPE(MPI_LOGICAL8,  "MPI_LOGICAL8",  OMPI_SIZEOF_FORTRAN_LOGICAL8);
#else
    CHECK_TYPE(MPI_LOGICAL8,  "MPI_LOGICAL8",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL16
    CHECK_TYPE(MPI_LOGICAL16, "MPI_LOGICAL16", OMPI_SIZEOF_FORTRAN_LOGICAL16);
#else
    CHECK_TYPE(MPI_LOGICAL16, "MPI_LOGICAL16", MPI_UNDEFINED);
#endif

#if OMPI_HAVE_FORTRAN_INTEGER1
    CHECK_TYPE(MPI_INTEGER1,  "MPI_INTEGER1",  OMPI_SIZEOF_FORTRAN_INTEGER1);
#else
    CHECK_TYPE(MPI_INTEGER1,  "MPI_INTEGER1",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
    CHECK_TYPE(MPI_INTEGER2,  "MPI_INTEGER2",  OMPI_SIZEOF_FORTRAN_INTEGER2);
#else
    CHECK_TYPE(MPI_INTEGER2,  "MPI_INTEGER2",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
    CHECK_TYPE(MPI_INTEGER4,  "MPI_INTEGER4",  OMPI_SIZEOF_FORTRAN_INTEGER4);
#else
    CHECK_TYPE(MPI_INTEGER4,  "MPI_INTEGER4",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
    CHECK_TYPE(MPI_INTEGER8,  "MPI_INTEGER8",  OMPI_SIZEOF_FORTRAN_INTEGER8);
#else
    CHECK_TYPE(MPI_INTEGER8,  "MPI_INTEGER8",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
    CHECK_TYPE(MPI_INTEGER16, "MPI_INTEGER16", OMPI_SIZEOF_FORTRAN_INTEGER16);
#else
    CHECK_TYPE(MPI_INTEGER16, "MPI_INTEGER16", MPI_UNDEFINED);
#endif

#if OMPI_HAVE_FORTRAN_REAL2
    CHECK_TYPE(MPI_REAL2,  "MPI_REAL2",  OMPI_SIZEOF_FORTRAN_REAL2);
#else
    CHECK_TYPE(MPI_REAL2,  "MPI_REAL2",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_REAL4
    CHECK_TYPE(MPI_REAL4,  "MPI_REAL4",  OMPI_SIZEOF_FORTRAN_REAL4);
#else
    CHECK_TYPE(MPI_REAL4,  "MPI_REAL4",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_REAL8
    CHECK_TYPE(MPI_REAL8,  "MPI_REAL8",  OMPI_SIZEOF_FORTRAN_REAL8);
#else
    CHECK_TYPE(MPI_REAL8,  "MPI_REAL8",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_REAL16
    CHECK_TYPE(MPI_REAL16, "MPI_REAL16", OMPI_SIZEOF_FORTRAN_REAL16);
#else
    CHECK_TYPE(MPI_REAL16, "MPI_REAL16", MPI_UNDEFINED);
#endif

#if OMPI_HAVE_FORTRAN_COMPLEX4
    CHECK_TYPE(MPI_COMPLEX4,  "MPI_COMPLEX4",  OMPI_SIZEOF_FORTRAN_COMPLEX4);
#else
    CHECK_TYPE(MPI_COMPLEX4,  "MPI_COMPLEX4",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX8
    CHECK_TYPE(MPI_COMPLEX8,  "MPI_COMPLEX8",  OMPI_SIZEOF_FORTRAN_COMPLEX8);
#else
    CHECK_TYPE(MPI_COMPLEX8,  "MPI_COMPLEX8",  MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX16
    CHECK_TYPE(MPI_COMPLEX16, "MPI_COMPLEX16", OMPI_SIZEOF_FORTRAN_COMPLEX16);
#else
    CHECK_TYPE(MPI_COMPLEX16, "MPI_COMPLEX16", MPI_UNDEFINED);
#endif
#if OMPI_HAVE_FORTRAN_COMPLEX32
    CHECK_TYPE(MPI_COMPLEX32, "MPI_COMPLEX32", OMPI_SIZEOF_FORTRAN_COMPLEX32);
#else
    CHECK_TYPE(MPI_COMPLEX32, "MPI_COMPLEX32", MPI_UNDEFINED);
#endif

#if OMPI_HAVE_FORTRAN_DOUBLE_COMPLEX
    CHECK_TYPE(MPI_DOUBLE_COMPLEX, "MPI_DOUBLE_COMPLEX", OMPI_SIZEOF_FORTRAN_DOUBLE_COMPLEX);
#else
    CHECK_TYPE(MPI_DOUBLE_COMPLEX, "MPI_DOUBLE_COMPLEX", MPI_UNDEFINED);
#endif

    MPI_Finalize();
    if (errors > 0) {
        fprintf(stderr, "%d error(s) found\n", errors);
        return 1;
    }
    return 0;
}
