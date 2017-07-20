/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>


#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <errno.h>
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <pmix_common.h>
#include <pmix_rename.h>

#include "src/mca/bfrops/bfrops.h"
#include "src/include/pmix_globals.h"

#define PMIX_EMBED_DATA_BUFFER(b, db)                   \
    do {                                                \
        (b)->base_ptr = (db)->base_ptr;                 \
        (b)->pack_ptr = (db)->pack_ptr;                 \
        (b)->unpack_ptr = (db)->unpack_ptr;             \
        (b)->bytes_allocated = (db)->bytes_allocated;   \
        (b)->bytes_used = (db)->bytes_used;             \
        (db)->base_ptr = NULL;                          \
        (db)->pack_ptr = NULL;                          \
        (db)->unpack_ptr = NULL;                        \
        (db)->bytes_allocated = 0;                      \
        (db)->bytes_used = 0;                           \
    } while (0)

#define PMIX_EXTRACT_DATA_BUFFER(b, db)                 \
    do {                                                \
        (db)->base_ptr = (b)->base_ptr;                 \
        (db)->pack_ptr = (b)->pack_ptr;                 \
        (db)->unpack_ptr = (b)->unpack_ptr;             \
        (db)->bytes_allocated = (b)->bytes_allocated;   \
        (db)->bytes_used = (b)->bytes_used;             \
        (b)->base_ptr = NULL;                           \
        (b)->pack_ptr = NULL;                           \
        (b)->unpack_ptr = NULL;                         \
        (b)->bytes_allocated = 0;                       \
        (b)->bytes_used = 0;                            \
    } while (0)

PMIX_EXPORT pmix_status_t PMIx_Data_pack(pmix_data_buffer_t *buffer,
                                         void *src, int32_t num_vals,
                                         pmix_data_type_t type)
{
    pmix_status_t rc;
    pmix_buffer_t buf;

    /* setup the host */
    PMIX_CONSTRUCT(&buf, pmix_buffer_t);

    /* embed the data buffer into a buffer */
    PMIX_EMBED_DATA_BUFFER(&buf, buffer);

    /* pack the value */
    PMIX_BFROPS_PACK(rc, pmix_globals.mypeer,
                     &buf, src, num_vals, type);

    /* extract the data buffer - the pointers may have changed */
    PMIX_EXTRACT_DATA_BUFFER(&buf, buffer);

    /* no need to cleanup as all storage was xfered */
    return rc;
}


PMIX_EXPORT pmix_status_t PMIx_Data_unpack(pmix_data_buffer_t *buffer, void *dest,
                                           int32_t *max_num_values,
                                           pmix_data_type_t type)
{
    pmix_status_t rc;
    pmix_buffer_t buf;

    /* setup the host */
    PMIX_CONSTRUCT(&buf, pmix_buffer_t);

    /* embed the data buffer into a buffer */
    PMIX_EMBED_DATA_BUFFER(&buf, buffer);

    /* unpack the value */
    PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                       &buf, dest, max_num_values, type);

    /* extract the data buffer - the pointers may have changed */
    PMIX_EXTRACT_DATA_BUFFER(&buf, buffer);

    /* no need to cleanup as all storage was xfered */
    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Data_copy(void **dest, void *src,
                                         pmix_data_type_t type)
{
    pmix_status_t rc;

    /* copy the value */
    PMIX_BFROPS_COPY(rc, pmix_globals.mypeer,
                     dest, src, type);

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Data_print(char **output, char *prefix,
                                          void *src, pmix_data_type_t type)
{
    pmix_status_t rc;

    /* print the value */
    PMIX_BFROPS_PRINT(rc, pmix_globals.mypeer,
                      output, prefix, src, type);

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Data_copy_payload(pmix_data_buffer_t *dest,
                                                 pmix_data_buffer_t *src)
{
    pmix_status_t rc;
    pmix_buffer_t buf1, buf2;

    /* setup the hosts */
    PMIX_CONSTRUCT(&buf1, pmix_buffer_t);
    PMIX_CONSTRUCT(&buf2, pmix_buffer_t);

    /* embed the data buffer into a buffer */
    PMIX_EMBED_DATA_BUFFER(&buf1, dest);
    PMIX_EMBED_DATA_BUFFER(&buf2, src);

    /* copy payload */
    PMIX_BFROPS_COPY_PAYLOAD(rc, pmix_globals.mypeer,
                             &buf1, &buf2);

    /* extract the dest data buffer - the pointers may have changed */
    PMIX_EXTRACT_DATA_BUFFER(&buf1, dest);
    PMIX_EXTRACT_DATA_BUFFER(&buf2, src);

    /* no need to cleanup as all storage was xfered */
    return rc;
}
