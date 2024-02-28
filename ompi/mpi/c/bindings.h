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

/*
 * The following OMPI_TEMP_ARRAY* functions are designed to convert count and
 * displacement arrays of type 'int *' to 'size_t *' and 'ptrdiff_t *'
 * respectively, since the backend interface is designed for size_t/ptrdiff_t
 * arrays only. In the case of bigcount (or largecount) *_c functions, these
 * macros will just copy the pointer from the input and skip the allocation.
 */
#define OMPI_TEMP_ARRAY_NAME_CONVERT(name) tmp_ ## name
#define OMPI_TEMP_ARRAYS_DECL(counts, displs) \
    size_t *OMPI_TEMP_ARRAY_NAME_CONVERT(counts) = NULL; \
    ptrdiff_t *OMPI_TEMP_ARRAY_NAME_CONVERT(displs) = NULL
#define OMPI_TEMP_ARRAY_DECL(counts) \
    size_t *OMPI_TEMP_ARRAY_NAME_CONVERT(counts) = NULL
/*
 * Prepare temporary count/displ array by allocating and copying values, if necessary
 */
#define OMPI_TEMP_ARRAYS_PREPARE(counts, displs, i, size) \
    do { \
        if (sizeof(*counts) == sizeof(*OMPI_TEMP_ARRAY_NAME_CONVERT(counts))) { \
            OMPI_TEMP_ARRAY_NAME_CONVERT(counts) = (void *) counts; \
            OMPI_TEMP_ARRAY_NAME_CONVERT(displs) = (void *) displs; \
        } else { \
            OMPI_TEMP_ARRAY_NAME_CONVERT(counts) = malloc(sizeof(*OMPI_TEMP_ARRAY_NAME_CONVERT(counts)) * size); \
            if (NULL == OMPI_TEMP_ARRAY_NAME_CONVERT(counts)) { \
                return OMPI_ERR_OUT_OF_RESOURCE; \
            } \
            OMPI_TEMP_ARRAY_NAME_CONVERT(displs) = malloc(sizeof(*OMPI_TEMP_ARRAY_NAME_CONVERT(displs)) * size); \
            if (NULL == OMPI_TEMP_ARRAY_NAME_CONVERT(displs)) { \
                return OMPI_ERR_OUT_OF_RESOURCE; \
            } \
            for (i = 0; i < size; i++) { \
                OMPI_TEMP_ARRAY_NAME_CONVERT(counts)[i] = counts[i]; \
                OMPI_TEMP_ARRAY_NAME_CONVERT(displs)[i] = displs[i]; \
            } \
        } \
    } while (0)
#define OMPI_TEMP_ARRAY_PREPARE(counts, i, size) \
    do { \
        if (sizeof(*counts) == sizeof(*OMPI_TEMP_ARRAY_NAME_CONVERT(counts))) { \
            OMPI_TEMP_ARRAY_NAME_CONVERT(counts) = (void *) counts; \
        } else { \
            OMPI_TEMP_ARRAY_NAME_CONVERT(counts) = malloc(sizeof(*OMPI_TEMP_ARRAY_NAME_CONVERT(counts)) * size); \
            if (NULL == OMPI_TEMP_ARRAY_NAME_CONVERT(counts)) { \
                return OMPI_ERR_OUT_OF_RESOURCE; \
            } \
            for (i = 0; i < size; i++) { \
                OMPI_TEMP_ARRAY_NAME_CONVERT(counts)[i] = counts[i]; \
            } \
        } \
    } while (0)
/*
 * Free temporary allocations for counts/displs, if necessary
 */
#define OMPI_TEMP_ARRAYS_CLEANUP(counts, displs) \
    do { \
        if ((void *) OMPI_TEMP_ARRAY_NAME_CONVERT(counts) != (void *) counts) { \
            free(OMPI_TEMP_ARRAY_NAME_CONVERT(counts)); \
            free(OMPI_TEMP_ARRAY_NAME_CONVERT(displs)); \
        } \
    } while (0)
#define OMPI_TEMP_ARRAY_CLEANUP(counts) \
    do { \
        if ((void *) OMPI_TEMP_ARRAY_NAME_CONVERT(counts) != (void *) counts) { \
            free(OMPI_TEMP_ARRAY_NAME_CONVERT(counts)); \
        } \
    } while (0)


END_C_DECLS

#endif /* OMPI_C_BINDINGS_H */
