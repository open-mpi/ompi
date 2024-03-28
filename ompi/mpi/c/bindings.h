/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_C_BINDINGS_H
#define OMPI_C_BINDINGS_H

#include "ompi_config.h"
#include "mpi.h"
#include "ompi/datatype/ompi_datatype.h"

BEGIN_C_DECLS

/* If compiling in the profile directory, then we don't have weak
   symbols and therefore we need the defines to map from MPI->PMPI.
   NOTE: pragma weak stuff is handled on a file-by-file basis; it
   doesn't work to simply list all of the pragmas in a top-level
   header file. */

/* These macros have to be used to check the correctness of the datatype depending on the
 * operations that we have to do with them. They can be used on all functions, not only
 * on the top level MPI functions, as they does not trigger the error handler. Is the user
 * responsibility to do it.
 */
#define OMPI_CHECK_DATATYPE_FOR_SEND( RC, DDT, COUNT )                  \
    do {                                                                \
        /* (RC) = MPI_SUCCESS; */                                       \
        if( NULL == (DDT) || MPI_DATATYPE_NULL == (DDT) ) (RC) = MPI_ERR_TYPE; \
        else if( (COUNT) < 0 ) (RC) = MPI_ERR_COUNT;                    \
        else if( !opal_datatype_is_committed(&((DDT)->super)) ) (RC) = MPI_ERR_TYPE; \
        else if( !opal_datatype_is_valid(&((DDT)->super)) ) (RC) = MPI_ERR_TYPE;       \
    } while (0)

#define OMPI_CHECK_DATATYPE_FOR_RECV( RC, DDT, COUNT )                  \
    do {                                                                \
        /* (RC) = MPI_SUCCESS; */                                        \
        if( NULL == (DDT) || MPI_DATATYPE_NULL == (DDT) ) (RC) = MPI_ERR_TYPE; \
        else if( (COUNT) < 0 ) (RC) = MPI_ERR_COUNT;                    \
        else if( !opal_datatype_is_committed(&((DDT)->super)) ) (RC) = MPI_ERR_TYPE;   \
        /* XXX Fix flags else if( ompi_datatype_is_overlapped((DDT)) ) (RC) = MPI_ERR_TYPE; */ \
        else if( !opal_datatype_is_valid(&((DDT)->super)) ) (RC) = MPI_ERR_TYPE;       \
    } while (0)

#define OMPI_CHECK_DATATYPE_FOR_ONE_SIDED( RC, DDT, COUNT )                          \
    do {                                                                             \
        /*(RC) = MPI_SUCCESS; */                                                     \
        if( NULL == (DDT) || MPI_DATATYPE_NULL == (DDT) ) (RC) = MPI_ERR_TYPE;       \
        else if( (COUNT) < 0 ) (RC) = MPI_ERR_COUNT;                                 \
        else if( !opal_datatype_is_committed(&((DDT)->super)) ) (RC) = MPI_ERR_TYPE; \
        else if( opal_datatype_is_overlapped(&((DDT)->super)) ) (RC) = MPI_ERR_TYPE; \
        else if( !opal_datatype_is_valid(&((DDT)->super)) ) (RC) = MPI_ERR_TYPE;     \
    } while(0)

#define OMPI_CHECK_DATATYPE_FOR_VIEW( RC, DDT, COUNT )                  \
    do {                                                                \
        /* (RC) = MPI_SUCCESS; */                                        \
        if( NULL == (DDT) || MPI_DATATYPE_NULL == (DDT) ) (RC) = MPI_ERR_TYPE; \
        else if( (COUNT) < 0 ) (RC) = MPI_ERR_COUNT;                    \
        else if( !opal_datatype_is_committed(&((DDT)->super)) ) (RC) = MPI_ERR_TYPE;   \
        /* XXX Fix flags else if( ompi_datatype_is_overlapped((DDT)) ) (RC) = MPI_ERR_TYPE; */ \
        else if( !opal_datatype_is_valid(&((DDT)->super)) ) (RC) = MPI_ERR_TYPE;       \
        else if( !ompi_datatype_is_monotonic((DDT)) ) (RC) = MPI_ERR_TYPE;       \
    } while (0)


/* This macro has to be used to check the correctness of the user buffer depending on the datatype.
 * This macro expects that the DDT parameter is a valid pointer to an ompi datatype object.
 */
#define OMPI_CHECK_USER_BUFFER(RC, BUFFER, DDT, COUNT)                  \
    do {                                                                \
        if ( NULL == (BUFFER) && 0 < (COUNT) && MPI_SUCCESS == (RC) ) { \
            if ( (DDT)->super.flags & OPAL_DATATYPE_FLAG_PREDEFINED ) { \
                (RC) = MPI_ERR_BUFFER;                                  \
            } else {                                                    \
                size_t ompi_chk_usr_buf_size = 0;                       \
                ptrdiff_t ompi_chk_usr_buf_true_lb = 0;                 \
                ptrdiff_t ompi_chk_usr_buf_true_extended = 0;           \
                ompi_datatype_type_size((DDT), &ompi_chk_usr_buf_size); \
                ompi_datatype_get_true_extent((DDT),                    \
                                             &ompi_chk_usr_buf_true_lb, \
                                       &ompi_chk_usr_buf_true_extended);\
                if ( 0 < ompi_chk_usr_buf_size &&                       \
                    0 == ompi_chk_usr_buf_true_extended ) {             \
                    (RC) = MPI_ERR_BUFFER;                              \
                }                                                       \
            }                                                           \
        }                                                               \
    } while (0)

/* Handling of MPI_Count/MPI_Aint and int array conversion */
#define OMPI_BIGCOUNT_ARRAY_NAME(name) tmp_ ## name

#define OMPI_BIGCOUNT_ARRAY_COUNT_IN_DECL(name) int *OMPI_BIGCOUNT_ARRAY_NAME(name)
#define OMPI_BIGCOUNT_ARRAY_DISPL_IN_DECL(name) OMPI_BIGCOUNT_ARRAY_COUNT_IN_DECL(name)
#define OMPI_BIGCOUNT_ARRAY_COUNT_OUT_DECL(name) OMPI_BIGCOUNT_ARRAY_COUNT_IN_DECL(name)
#define OMPI_BIGCOUNT_ARRAY_DISPL_OUT_DECL(name) OMPI_BIGCOUNT_ARRAY_COUNT_IN_DECL(name)

#define OMPI_BIGCOUNT_ARRAY_COUNT_IN_PREPARE(name, size, i) \
    do { \
        if (sizeof(*name) == sizeof(*OMPI_BIGCOUNT_ARRAY_NAME(name))) { \
            OMPI_BIGCOUNT_ARRAY_NAME(name) = (int *) name; \
            break; \
        } \
        OMPI_BIGCOUNT_ARRAY_NAME(name) = malloc(size * sizeof(*OMPI_BIGCOUNT_ARRAY_NAME(name))); \
        for (i = 0; i < size; ++i) { \
            OMPI_BIGCOUNT_ARRAY_NAME(name)[i] = name[i]; \
        } \
    } while (0)
#define OMPI_BIGCOUNT_ARRAY_DISPL_IN_PREPARE(name, size, i) OMPI_BIGCOUNT_ARRAY_COUNT_IN_PREPARE(name, size, i)
#define OMPI_BIGCOUNT_ARRAY_COUNT_OUT_PREPARE(name, size) \
    do { \
        if (sizeof(*name) == sizeof(*OMPI_BIGCOUNT_ARRAY_NAME(name))) { \
            OMPI_BIGCOUNT_ARRAY_NAME(name) = (int *) name; \
            break; \
        } \
        OMPI_BIGCOUNT_ARRAY_NAME(name) = malloc(size * sizeof(*OMPI_BIGCOUNT_ARRAY_NAME(name))); \
    } while (0)
#define OMPI_BIGCOUNT_ARRAY_DISPL_OUT_PREPARE(name, size) OMPI_BIGCOUNT_ARRAY_COUNT_OUT_PREPARE(name, size)

#define OMPI_BIGCOUNT_ARRAY_COUNT_IN_POST(name) \
    do { \
        if (sizeof(*name) != sizeof(*OMPI_BIGCOUNT_ARRAY_NAME(name))) { \
            free(OMPI_BIGCOUNT_ARRAY_NAME(name)); \
        } \
    } while (0)
#define OMPI_BIGCOUNT_ARRAY_DISPL_IN_POST(name) OMPI_BIGCOUNT_ARRAY_COUNT_IN_POST(name)
#define OMPI_BIGCOUNT_ARRAY_COUNT_OUT_POST(name, size, i) \
    do { \
        if (sizeof(*name) == sizeof(*OMPI_BIGCOUNT_ARRAY_NAME(name))) { \
            break; \
        } \
        for (i = 0; i < size; ++i) { \
            name[i] = OMPI_BIGCOUNT_ARRAY_NAME(name)[i]; \
        } \
        free(OMPI_BIGCOUNT_ARRAY_NAME(name)); \
    } while (0)
#define OMPI_BIGCOUNT_ARRAY_DISPL_OUT_POST(name, size, i) OMPI_BIGCOUNT_ARRAY_COUNT_OUT_POST(name, size, i)

END_C_DECLS

#endif /* OMPI_C_BINDINGS_H */
