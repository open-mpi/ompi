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
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/shmem/shmem.h"
#include "opal/mca/shmem/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/shmem/base/static-components.h"

/**
 * globals
 */
OPAL_DECLSPEC int opal_shmem_base_output = -1;
bool opal_shmem_base_components_opened_valid = false;
opal_list_t opal_shmem_base_components_opened;
char *opal_shmem_base_runtime_query_hint = NULL;

/**
 * locals
 */
static int opal_shmem_base_verbose = 0;

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * Register some shmem-wide MCA params 
 */
int
opal_shmem_base_register_params(void)
{
    int var_id;

    opal_shmem_base_verbose = 0;
    (void) mca_base_var_register("opal", "shmem", "base", "verbose",
                                 "Verbosity level of the shmem framework",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_LOCAL, &opal_shmem_base_verbose);

    /* register an INTERNAL parameter used to provide a component selection
     * hint to the shmem framework.
     * we are using a nonstandard name here because shmem_RUNTIME_QUERY_hint
     * is for internal use only!
     * see odls_base_default_fns.c for more details.
     */
    opal_shmem_base_runtime_query_hint = NULL;
    var_id = mca_base_var_register("opal", "shmem", "base", "RUNTIME_QUERY_hint",
                                   "Internal OMPI parameter used to provide a "
                                   "component selection hint to the shmem "
                                   "framework.  The value of this parameter "
                                   "is the name of the component that is "
                                   "available, selectable, and meets our "
                                   "run-time behavior requirements.",
                                   MCA_BASE_VAR_TYPE_STRING, NULL, 0,
                                   MCA_BASE_VAR_FLAG_INTERNAL,
                                   OPAL_INFO_LVL_9,
                                   MCA_BASE_VAR_SCOPE_READONLY,
                                   &opal_shmem_base_runtime_query_hint);
    (void) mca_base_var_register_synonym(var_id, "opal", "shmem", NULL,
                                         "RUNTIME_QUERY_hint",
                                         MCA_BASE_VAR_SYN_FLAG_INTERNAL);

    return OPAL_SUCCESS;
}


/* ////////////////////////////////////////////////////////////////////////// */ 
/** 
 * Function for finding and opening either all MCA components, or the one 
 * that was specifically requested via a MCA parameter. 
 */
int
opal_shmem_base_open(void) 
{
    opal_shmem_base_components_opened_valid = false;

    if (0 != opal_shmem_base_verbose) {
        opal_shmem_base_output = opal_output_open(NULL);
    }
    else {
        opal_shmem_base_output = -1;
    }
    
    /* open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("shmem", opal_shmem_base_output,
                                 mca_shmem_base_static_components,
                                 &opal_shmem_base_components_opened, true)) {
        return OPAL_ERROR;
    }

    opal_shmem_base_components_opened_valid = true;

    /* all done */
    return OPAL_SUCCESS;
}

