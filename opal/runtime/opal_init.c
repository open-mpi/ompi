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
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "opal_config.h"

#include "opal/util/malloc.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "opal/util/show_help.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/mca/installdirs/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/memcpy/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/timer/base/base.h"
#include "opal/mca/backtrace/base/base.h"
#include "opal/constants.h"
#include "opal/util/error.h"
#include "opal/util/stacktrace.h"
#include "opal/util/keyval_parse.h"

int opal_initialized = 0;

static const char *
opal_err2str(int errnum)
{
    const char *retval;

    switch (errnum) {
    case OPAL_SUCCESS:
        retval = "Success";
        break;
    case OPAL_ERROR:
        retval = "Error";
        break;
    case OPAL_ERR_OUT_OF_RESOURCE:
        retval = "Out of resource";
        break;
    case OPAL_ERR_TEMP_OUT_OF_RESOURCE:
        retval = "Temporarily out of resource";
        break;
    case OPAL_ERR_RESOURCE_BUSY:
        retval = "Resource busy";
        break;
    case OPAL_ERR_BAD_PARAM:
        retval = "Bad parameter";
        break;
    case OPAL_ERR_FATAL:
        retval = "Fatal";
        break;
    case OPAL_ERR_NOT_IMPLEMENTED:
        retval = "Not implemented";
        break;
    case OPAL_ERR_NOT_SUPPORTED:
        retval = "Not supported";
        break;
    case OPAL_ERR_INTERUPTED:
        retval = "Interupted";
        break;
    case OPAL_ERR_WOULD_BLOCK:
        retval = "Would block";
        break;
    case OPAL_ERR_IN_ERRNO:
        retval = "In errno";
        break;
    case OPAL_ERR_UNREACH:
        retval = "Unreachable";
        break;
    case OPAL_ERR_NOT_FOUND:
        retval = "Not found";
        break;
    case OPAL_EXISTS:
        retval = "Exists";
        break;
    case OPAL_ERR_TIMEOUT:
        retval = "Timeout";
        break;
    case OPAL_ERR_NOT_AVAILABLE:
        retval = "Not available";
        break;
    case OPAL_ERR_PERM:
        retval = "No permission";
        break;
    case OPAL_ERR_VALUE_OUT_OF_BOUNDS:
        retval = "Value out of bounds";
        break;
    case OPAL_ERR_FILE_READ_FAILURE:
        retval = "File read failure";
        break;
    case OPAL_ERR_FILE_WRITE_FAILURE:
        retval = "File write failure";
        break;
    case OPAL_ERR_FILE_OPEN_FAILURE:
        retval = "File open failure";
        break;
    default:
        retval = NULL;
    }

    return retval;
}


int
opal_init_util(void)
{
    int ret;
    char *error = NULL;

    if( ++opal_initialized != 1 ) {
        if( opal_initialized < 1 ) {
            return OPAL_ERROR;
        }
        return OPAL_SUCCESS;
    }
    /* initialize the memory allocator */
    opal_malloc_init();

    /* initialize the output system */
    opal_output_init();

    /* register handler for errnum -> string converstion */
    if (OPAL_SUCCESS != (ret = opal_error_register("OPAL",
            OPAL_ERR_BASE, OPAL_ERR_MAX, opal_err2str))) {
        error = "opal_error_register";
        goto return_error;
    }

    /* initialize install dirs code */
    if (OPAL_SUCCESS != (ret = opal_installdirs_base_open())) {
        fprintf(stderr, "opal_installdirs_base_open() failed -- process will likely abort (%s:%d, returned %d instead of OPAL_INIT)\n",
                __FILE__, __LINE__, ret);
        return ret;
    }

    /* init the trace function */
    opal_trace_init();

    /* keyval lex-based parser */
    if (OPAL_SUCCESS != (ret = opal_util_keyval_parse_init())) {
        error = "opal_util_keyval_parse_init";
        goto return_error;
    }

    /* Setup the parameter system */
    if (OPAL_SUCCESS != (ret = mca_base_param_init())) {
        error = "mca_base_param_init";
        goto return_error;
    }

    /* register params for opal */
    if (OPAL_SUCCESS != (ret = opal_register_params())) {
        error = "opal_register_params";
        goto return_error;
    }

    /* pretty-print stack handlers */
    if (OPAL_SUCCESS != (ret = opal_util_register_stackhandlers ())) {
        error = "util_register_stackhandlers() failed";
        goto return_error;
    }

    return OPAL_SUCCESS;

 return_error:
    opal_show_help( "help-opal-runtime",
                    "opal_init:startup:internal-failure", true,
                    error, ret );
    return ret;
}


int
opal_init(void)
{
    int ret;
    char *error = NULL;

    /* initialize util code */
    if (OPAL_SUCCESS != (ret = opal_init_util())) {
        return ret;
    }

    /* initialize the mca */
    if (OPAL_SUCCESS != (ret = mca_base_open())) {
        error = "mca_base_open";
        goto return_error;
    }

    /* open the processor affinity base */
    opal_paffinity_base_open();
    opal_paffinity_base_select();

    /* the memcpy component should be one of the first who get
     * loaded in order to make sure we ddo have all the available
     * versions of memcpy correctly configured.
     */
    if( OPAL_SUCCESS != (ret = opal_memcpy_base_open()) ) {
        error = "opal_memcpy_base_open";
        goto return_error;
    }

    /* open the memory manager components.  Memory hooks may be
       triggered before this (any time after mem_free_init(),
       actually).  This is a hook available for memory manager hooks
       without good initialization routine support */
    if (OPAL_SUCCESS != (ret = opal_memory_base_open())) {
        error = "opal_memory_base_open";
        goto return_error;
    }

    /* initialize the memory manager / tracker */
    if (OPAL_SUCCESS != opal_mem_hooks_init()) {
        error = "opal_mem_free_init";
        goto return_error;
    }

    if (OPAL_SUCCESS != (ret = opal_backtrace_base_open())) {
        error = "opal_backtrace_base_open";
        goto return_error;
    }

    if (OPAL_SUCCESS != (ret = opal_timer_base_open())) {
        error = "opal_timer_base_open";
        goto return_error;
    }

    return OPAL_SUCCESS;

 return_error:
    opal_show_help( "help-opal-runtime",
                    "opal_init:startup:internal-failure", true,
                    error, ret );
    return ret;
}

