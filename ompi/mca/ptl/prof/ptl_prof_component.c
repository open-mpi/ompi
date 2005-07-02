/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "ompi_config.h"
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_prof.h"

static int mca_ptl_prof_component_open_fn( void );
static int mca_ptl_prof_component_close_fn( void );
static struct mca_ptl_base_module_t** ptl_prof_component_init_fn(
       int *num_ptls,
       bool enable_progress_threads, bool enable_mpi_threads);
static int ptl_prof_component_control_fn( int param, void* value, size_t size );

mca_ptl_prof_module_1_0_0_t mca_ptl_prof_component = {
    {
        /* First, the mca_base_module_t struct containing meta information
           about the module itself */

        {
            /* Indicate that we are a pml v1.0.0 module (which also implies a
               specific MCA version) */

            MCA_PTL_BASE_VERSION_1_0_0,

            "prof", /* MCA module name */
            1,  /* MCA module major version */
            0,  /* MCA module minor version */
            0,  /* MCA module release version */
            mca_ptl_prof_component_open_fn,  /* module open */
            mca_ptl_prof_component_close_fn  /* module close */
        },

        /* Next the MCA v1.0.0 module meta data */

        {
            /* Whether the module is checkpointable or not */
            true
        },

        ptl_prof_component_init_fn,
        ptl_prof_component_control_fn,
        NULL,
    }
};

/**
 * This is the moment to grab all existing modules, and then replace their
 * functions with my own. In same time the ptl_stack will be initialized
 * with the pointer to a ptl automatically generate, which will contain
 * the correct pointers.
 */
static int ptl_prof_component_control_fn( int param, void* value, size_t size )
{
    /* check in mca_ptl_base_modules_initialized */
    return 0;
}

/* We have to create at least one PTL, just to allow the PML to call the control
 * function associated with this PTL.
 */
extern mca_ptl_prof_t mca_ptl_prof;
static struct mca_ptl_base_module_t** ptl_prof_component_init_fn(
       int *num_ptls,
       bool enable_progress_threads,
       bool enable_mpi_threads)
{
    mca_ptl_prof_t** ptl_array;

    *num_ptls = 1;
    ptl_array = (mca_ptl_prof_t**)malloc( (*num_ptls) * sizeof(mca_ptl_prof_t*) );
    ptl_array[0] = &mca_ptl_prof;
    mca_ptl_prof.super.ptl_component = (mca_ptl_base_component_t*)&mca_ptl_prof_component;
    mca_ptl_prof_component.prof_ptls = ptl_array;
    return (struct mca_ptl_base_module_t**)ptl_array;
}

static int mca_ptl_prof_component_open_fn( void )
{
    return OMPI_SUCCESS;
}

static int mca_ptl_prof_component_close_fn( void )
{
#if 0
    /* JMS This should only occur if this component was selected -- if
       it wasn't selected, it appears that the PTL base takes care of
       freeing this (ptl_base_select.c line 124) */
    if( NULL != mca_ptl_prof_component.prof_ptls ) {
        free( mca_ptl_prof_component.prof_ptls );
        mca_ptl_prof_component.prof_ptls = NULL;
    }
#endif
    return OMPI_SUCCESS;
}

