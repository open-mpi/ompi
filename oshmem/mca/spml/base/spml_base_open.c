/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "oshmem_config.h"
#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNIST_H */
#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"


#include "oshmem/constants.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/base.h"
#include "oshmem/mca/spml/base/spml_base_request.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "oshmem/mca/spml/base/static-components.h"
/* irit
int mca_spml_base_progress(void) 
{
    return OSHMEM_SUCCESS;
}
*/
#define xstringify(spml) #spml
#define stringify(spml) xstringify(spml)

/*
 * Global variables
 */
int mca_spml_base_output = 0;
int mca_spml_base_verbose = -1;
mca_spml_base_module_t mca_spml;


opal_list_t mca_spml_base_components_available;
mca_spml_base_component_t mca_spml_base_selected_component;
opal_pointer_array_t mca_spml_base_spml;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_spml_base_open(void)
{

    int value = -1;

/* TODO: Consider restoring FT (fault tolerance) */
/* Irit removed temporarily.
#if OPAL_ENABLE_FT == 1
    char* wrapper_spml = NULL;
#endif
Irit*/

    /*
     * Register some MCA parameters
     */
     /* Debugging/Verbose output */


    mca_base_param_reg_int_name("spml",
            "base_verbose",
            "Verbosity level of the SPML framework",
            false, false,
            0, &value);

    mca_spml_base_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_spml_base_output, value);

    /**
     * Construct the send and receive request queues. There are 2 reasons to do it
     * here. First, as they are globals it's better to construct them in one common
     * place. Second, in order to be able to allow the external debuggers to show
     * their content, they should get constructed as soon as possible once the MPI
     * process is started.
     */
    OBJ_CONSTRUCT(&mca_spml_base_put_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_spml_base_get_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_spml_base_spml, opal_pointer_array_t);

    /* Open up all available components */

    if (OSHMEM_SUCCESS !=
            mca_base_components_open("spml", mca_spml_base_output, mca_spml_base_static_components,
                &mca_spml_base_components_available,
               !MCA_oshmem_spml_DIRECT_CALL)) { /*TODO: Irit change to MCA_oshmem_spml_DIRECT_CALL*/
                SPML_ERROR("Spml failed to open base component\n");
        return OSHMEM_ERROR;
    }

    /* Set a sentinel in case we don't select any components (e.g.,
     *          ompi_info) */

    mca_spml_base_selected_component.spmlm_finalize = NULL;


    /**
     * Right now our selection of BTLs is completely broken. If we have
     * multiple SPMLs that use BTLs than we will open all BTLs several times, leading to
     * undefined behaviors. The simplest solution, at least until we
     * figure out the correct way to do it, is to force a default SPML that
     * uses BTLs and any other SPMLs that do not in the mca_spml_base_spml array.
     */

/*TODO: Irit change to MCA_oshmem_spml_DIRECT_CALL*/
#if MCA_oshmem_spml_DIRECT_CALL
    opal_pointer_array_add(&mca_spml_base_spml,
            stringify(MCA_oshmem_spml_DIRECT_CALL_COMPONENT));
#else
    {
        /* Specify a SPML as a parameter */
        char* default_spml = NULL;

        mca_base_param_reg_string_name("spml", NULL,
                "Specify a specific SPML to use",
                false, false, "", &default_spml);

        if( (0 == strlen(default_spml)) || (default_spml[0] == '^') ) {
#ifdef OSHMEM_HAS_IKRIT
            opal_pointer_array_add(&mca_spml_base_spml, strdup("ikrit"));
#else
            opal_pointer_array_add(&mca_spml_base_spml, strdup("yoda"));
#endif
        } else {
            opal_pointer_array_add(&mca_spml_base_spml, default_spml);
        }
    }
/*TODO: Consider restoring FT */
/*#if OPAL_ENABLE_FT == 1*/
    /*
     *Which SPML Wrapper component to use, if any
     *  - NULL or "" = No wrapper
     *  - ow. select that specific wrapper component
     */
/*  Irit removed temporarily
     mca_base_param_reg_string_name("spml", "wrapper",
            "Use a Wrapper component around the selected SPML component",
            false, false,
            NULL, &wrapper_spml);
    if( NULL != wrapper_spml ) {
        opal_pointer_array_add(&mca_spml_base_spml, wrapper_spml);
    }
#endif
Irit */
#endif

    return OSHMEM_SUCCESS;
}
