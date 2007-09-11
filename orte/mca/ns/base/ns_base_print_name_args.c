/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include <stdio.h>
#include <string.h>

#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/threads/tsd.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/ns/base/base.h"

#define ORTE_PRINT_NAME_ARGS_MAX_SIZE   20
#define ORTE_PRINT_NAME_ARG_NUM_BUFS    8

static opal_tsd_key_t print_args_tsd_key;
char* orte_print_args_null = "NULL";
typedef struct {
    char *buffers[ORTE_PRINT_NAME_ARG_NUM_BUFS];
    int cntr;
} orte_print_args_buffers_t;

static void
buffer_cleanup(void *value)
{
    int i;
    orte_print_args_buffers_t *ptr;
    
    if (NULL != value) {
        ptr = (orte_print_args_buffers_t*)value;
        for (i=0; i < ORTE_PRINT_NAME_ARG_NUM_BUFS; i++) {
            free(ptr->buffers[i]);
        }
    }
}

static orte_print_args_buffers_t*
get_print_name_buffer(void)
{
    orte_print_args_buffers_t *ptr;
    int ret, i;
    
    ret = opal_tsd_getspecific(print_args_tsd_key, (void**)&ptr);
    if (OPAL_SUCCESS != ret) return NULL;
    
    if (NULL == ptr) {
        ptr = (orte_print_args_buffers_t*)malloc(sizeof(orte_print_args_buffers_t));
        for (i=0; i < ORTE_PRINT_NAME_ARG_NUM_BUFS; i++) {
            ptr->buffers[i] = (char *) malloc((ORTE_PRINT_NAME_ARGS_MAX_SIZE+1) * sizeof(char));
        }
        ptr->cntr = 0;
        ret = opal_tsd_setspecific(print_args_tsd_key, (void*)ptr);
    }
    
    return (orte_print_args_buffers_t*) ptr;
}

char* orte_ns_base_print_name_args(const orte_process_name_t *name)
{
    orte_print_args_buffers_t *ptr = get_print_name_buffer();
    
    if (NULL == ptr) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return orte_print_args_null;
    }
    
    /* cycle around the ring */
    if (ORTE_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }
    
    if (NULL == name) {
        snprintf(ptr->buffers[ptr->cntr++], ORTE_PRINT_NAME_ARGS_MAX_SIZE, "[NO-NAME]");
    } else {
        snprintf(ptr->buffers[ptr->cntr++], ORTE_PRINT_NAME_ARGS_MAX_SIZE, "[%ld,%ld]", (long)name->jobid, (long)name->vpid);
    }
    return ptr->buffers[ptr->cntr-1];
}

int
orte_ns_base_init_print_args(void)
{
    return opal_tsd_key_create(&print_args_tsd_key, buffer_cleanup);
}
