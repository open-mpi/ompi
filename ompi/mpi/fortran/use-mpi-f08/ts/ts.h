/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2014      Argonne National Laboratory.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <ISO_Fortran_binding.h>

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"

extern int ompi_ts_create_datatype(CFI_cdesc_t *cdesc, int oldcount, MPI_Datatype oldtype, MPI_Datatype *newtype);

#define OMPI_CFI_2_C(x, count, type, datatype, rc)                      \
    do {                                                                \
        datatype = type;                                                \
        if (x->rank != 0 && !CFI_is_contiguous(x)) {                    \
            rc = ompi_ts_create_datatype(x, count, type, &datatype);    \
            if (MPI_SUCCESS != rc) {                                    \
                return;                                                 \
            } else {                                                    \
                count = 1;                                              \
            }                                                           \
        } else {                                                        \
            rc = MPI_SUCCESS;                                           \
        }                                                               \
    } while (0)

#define OMPI_CFI_IS_CONTIGUOUS(x)                                       \
    (0 == x->rank || CFI_is_contiguous(x))

#define OMPI_CFI_CHECK_CONTIGUOUS(x, rc)                                \
    do {                                                                \
        if (OMPI_CFI_IS_CONTIGUOUS(x)) {                                \
            rc = MPI_SUCCESS;                                           \
        } else {                                                        \
            rc = MPI_ERR_INTERN;                                        \
        }                                                               \
    } while (0)
    
