/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
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
#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "ompi/mca/pml/base/static-components.h"

int mca_pml_base_progress(void) 
{
    return OMPI_SUCCESS;
}

#define xstringify(pml) #pml
#define stringify(pml) xstringify(pml)

/*
 * Global variables
 */
int mca_pml_base_output = 0;
mca_pml_base_module_t mca_pml = {
    NULL,                    /* pml_add_procs */
    NULL,                    /* pml_del_procs */
    NULL,                    /* pml_enable */
    mca_pml_base_progress,   /* pml_progress */
    NULL,                    /* pml_add_comm */
    NULL,                    /* pml_del_comm */
    NULL,                    /* pml_irecv_init */
    NULL,                    /* pml_irecv */
    NULL,                    /* pml_recv */
    NULL,                    /* pml_isend_init */
    NULL,                    /* pml_isend */
    NULL,                    /* pml_send */
    NULL,                    /* pml_iprobe */
    NULL,                    /* pml_probe */
    NULL,                    /* pml_start */
    NULL,                    /* pml_dump */
    0,                       /* pml_max_contextid */
    0                        /* pml_max_tag */
};

opal_list_t mca_pml_base_components_available;
mca_pml_base_component_t mca_pml_base_selected_component;
char *mca_pml_base_pml;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_pml_base_open(void)
{
    char* default_pml;

    /* Open up all available components */

    if (OMPI_SUCCESS != 
        mca_base_components_open("pml", 0, mca_pml_base_static_components, 
                                 &mca_pml_base_components_available,
                                 !MCA_pml_DIRECT_CALL)) {
        return OMPI_ERROR;
    }

    /* Set a sentinel in case we don't select any components (e.g.,
       ompi_info) */

    mca_pml_base_selected_component.pmlm_finalize = NULL;

    /**
     * Right now our selection of BTLs is completely broken. If we have
     * multiple PMLs we will open all BTLs several times, leading to
     * undefined behaviors. The simplest solution, at least until we
     * figure out the correct way to do it, is to force a default value
     * in the mca_pml_base_pml global.
     */
#if MCA_pml_DIRECT_CALL
    default_pml = stringify(MCA_pml_DIRECT_CALL_COMPONENT);
#else
    default_pml = "ob1";
#endif

    mca_base_param_lookup_string(
        mca_base_param_register_string("pml",
                                       NULL,
                                       NULL,
                                       NULL,
                                       default_pml),
        &mca_pml_base_pml);
    if( NULL == mca_pml_base_pml )
        mca_pml_base_pml = default_pml;

    return OMPI_SUCCESS;
}
