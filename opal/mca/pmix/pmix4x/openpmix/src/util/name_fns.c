/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "pmix_config.h"

#include <stdio.h>
#include <string.h>

#include "pmix_common.h"

#include "src/threads/tsd.h"
#include "src/include/pmix_globals.h"
#include "src/util/error.h"
#include "src/util/name_fns.h"
#include "src/util/printf.h"

#define PMIX_PRINT_NAME_ARGS_MAX_SIZE   300
#define PMIX_PRINT_NAME_ARG_NUM_BUFS    16

#define PMIX_SCHEMA_DELIMITER_CHAR      '.'
#define PMIX_SCHEMA_WILDCARD_CHAR       '*'
#define PMIX_SCHEMA_WILDCARD_STRING     "*"
#define PMIX_SCHEMA_INVALID_CHAR        '$'
#define PMIX_SCHEMA_INVALID_STRING      "$"

static bool fns_init=false;

static pmix_tsd_key_t print_args_tsd_key;
char* pmix_print_args_null = "NULL";
typedef struct {
    char *buffers[PMIX_PRINT_NAME_ARG_NUM_BUFS];
    int cntr;
} pmix_print_args_buffers_t;

static void
buffer_cleanup(void *value)
{
    int i;
    pmix_print_args_buffers_t *ptr;

    if (NULL != value) {
        ptr = (pmix_print_args_buffers_t*)value;
        for (i=0; i < PMIX_PRINT_NAME_ARG_NUM_BUFS; i++) {
            free(ptr->buffers[i]);
        }
        free (ptr);
    }
}

static pmix_print_args_buffers_t*
get_print_name_buffer(void)
{
    pmix_print_args_buffers_t *ptr;
    int ret, i;

    if (!fns_init) {
        /* setup the print_args function */
        if (PMIX_SUCCESS != (ret = pmix_tsd_key_create(&print_args_tsd_key, buffer_cleanup))) {
            PMIX_ERROR_LOG(ret);
            return NULL;
        }
        fns_init = true;
    }

    ret = pmix_tsd_getspecific(print_args_tsd_key, (void**)&ptr);
    if (PMIX_SUCCESS != ret) return NULL;

    if (NULL == ptr) {
        ptr = (pmix_print_args_buffers_t*)malloc(sizeof(pmix_print_args_buffers_t));
        for (i=0; i < PMIX_PRINT_NAME_ARG_NUM_BUFS; i++) {
            ptr->buffers[i] = (char *) malloc((PMIX_PRINT_NAME_ARGS_MAX_SIZE+1) * sizeof(char));
        }
        ptr->cntr = 0;
        ret = pmix_tsd_setspecific(print_args_tsd_key, (void*)ptr);
    }

    return (pmix_print_args_buffers_t*) ptr;
}

static char* print_args(char *ns, pmix_rank_t rnk)
{
    pmix_print_args_buffers_t *ptr;
    char *rank;
    int index;

    /* get the next buffer */
    ptr = get_print_name_buffer();
    if (NULL == ptr) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        return pmix_print_args_null;
    }

    /* protect against NULL nspace */
    if (NULL == ns) {
        index = ptr->cntr;
        snprintf(ptr->buffers[index], PMIX_PRINT_NAME_ARGS_MAX_SIZE, "[NO-NAME]");
        ptr->cntr++;
        if (PMIX_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
            ptr->cntr = 0;
        }
        return ptr->buffers[index];
    }

    rank = pmix_util_print_rank(rnk);

    index = ptr->cntr;
    snprintf(ptr->buffers[index],
             PMIX_PRINT_NAME_ARGS_MAX_SIZE,
             "[%s:%s]", ns, rank);
    ptr->cntr++;
    if (PMIX_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    return ptr->buffers[index];
}

char* pmix_util_print_name_args(const pmix_proc_t *name)
{
    if (NULL == name) {
        return print_args(NULL, PMIX_RANK_UNDEF);
    }

    return print_args((char*)name->nspace, name->rank);
}

char *pmix_util_print_pname_args(const pmix_name_t *name)
{
    if (NULL == name) {
        return print_args(NULL, PMIX_RANK_UNDEF);
    }

    return print_args((char*)name->nspace, name->rank);
}

char* pmix_util_print_rank(const pmix_rank_t vpid)
{
    pmix_print_args_buffers_t *ptr;
    int index;

    ptr = get_print_name_buffer();

    if (NULL == ptr) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        return pmix_print_args_null;
    }

    index = ptr->cntr;
    if (PMIX_RANK_UNDEF == vpid) {
        snprintf(ptr->buffers[index], PMIX_PRINT_NAME_ARGS_MAX_SIZE, "UNDEF");
    } else if (PMIX_RANK_WILDCARD == vpid) {
        snprintf(ptr->buffers[index], PMIX_PRINT_NAME_ARGS_MAX_SIZE, "WILDCARD");
    } else {
        snprintf(ptr->buffers[index],
                 PMIX_PRINT_NAME_ARGS_MAX_SIZE,
                 "%ld", (long)vpid);
    }
    ptr->cntr++;
    if (PMIX_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }
    return ptr->buffers[index];
}
