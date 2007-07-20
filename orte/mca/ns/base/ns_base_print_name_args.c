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

static opal_tsd_key_t print_args_tsd_key;
char* orte_print_args_null = "NULL";

static void
buffer_cleanup(void *value)
{
    if (NULL != value) free(value);
}

static char*
get_print_name_buffer(void)
{
    void *buffer;
    int ret;
    
    ret = opal_tsd_getspecific(print_args_tsd_key, &buffer);
    if (OPAL_SUCCESS != ret) return NULL;
    
    if (NULL == buffer) {
        buffer = (void*) malloc((ORTE_PRINT_NAME_ARGS_MAX_SIZE+1) * sizeof(char));
        ret = opal_tsd_setspecific(print_args_tsd_key, buffer);
    }
    
    return (char*) buffer;
}

char* orte_ns_base_print_name_args(orte_process_name_t *name)
{
    char *print_name_buf = get_print_name_buffer();
    
    if (NULL == print_name_buf) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return orte_print_args_null;
    }
    
    if (NULL == name) {
        snprintf(print_name_buf, ORTE_PRINT_NAME_ARGS_MAX_SIZE, "[NO-NAME]");
    } else {
        snprintf(print_name_buf, ORTE_PRINT_NAME_ARGS_MAX_SIZE, "[%ld,%ld]", (long)name->jobid, (long)name->vpid);
    }
    return print_name_buf;
}

int
orte_ns_base_init_print_args(void)
{
    return opal_tsd_key_create(&print_args_tsd_key, buffer_cleanup);
}
