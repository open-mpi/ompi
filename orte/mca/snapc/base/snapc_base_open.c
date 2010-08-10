/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2008 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"

#include "orte/constants.h"
#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "orte/mca/snapc/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_snapc_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/*
 * Globals
 */
int  orte_snapc_base_output  = -1;
bool orte_snapc_base_is_tool = false;
orte_snapc_base_module_t orte_snapc = {
    NULL, /* snapc_init      */
    NULL, /* snapc_finalize  */
    NULL, /* setup_job       */
    NULL  /* release_job     */
};

opal_list_t orte_snapc_base_components_available;
orte_snapc_base_component_t orte_snapc_base_selected_component;
orte_snapc_coord_type_t orte_snapc_coord_type = ORTE_SNAPC_UNASSIGN_TYPE;

bool orte_snapc_base_store_only_one_seq = false;
bool   orte_snapc_base_has_recovered = false;

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int orte_snapc_base_open(void)
{
    int value = 0;
    char * str_value = NULL;

    OPAL_OUTPUT_VERBOSE((10, orte_snapc_base_output,
                         "snapc:base: open()"));

    orte_snapc_base_output = opal_output_open(NULL);

    /*
     * Reuse sequence numbers
     * This will create a directory and always use seq 0 for all checkpoints
     * This *should* also enforce a 2-phase commit protocol
     */
    mca_base_param_reg_int_name("snapc_base",
                                "only_one_seq",
                                "Only store the most recent checkpoint sequence. [Default = disabled]",
                                false, false,
                                0,
                                &value);
    orte_snapc_base_store_only_one_seq = OPAL_INT_TO_BOOL(value);

    OPAL_OUTPUT_VERBOSE((20, orte_snapc_base_output,
                         "snapc:base: open: base_only_one_seq    = %d",
                         orte_snapc_base_store_only_one_seq));


    /* Init the sequence (interval) number */
    orte_snapc_base_snapshot_seq_number = 0;

    /* 
     * Which SnapC component to open
     *  - NULL or "" = auto-select
     *  - "none" = Empty component
     *  - ow. select that specific component
     * Note: Set the default to NULL here so ompi_info will work correctly,
     *       The 'real' default is set in base_select.c
     */
    mca_base_param_reg_string_name("snapc", NULL,
                                   "Which SNAPC component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &str_value);
    if( NULL != str_value ) {
        free(str_value);
    }

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("snapc", 
                                 orte_snapc_base_output, 
                                 mca_snapc_base_static_components,
                                 &orte_snapc_base_components_available,
                                 true)) {
        return ORTE_ERROR;
    }

    /*
     * Open up the SStore framework
     */
    orte_sstore_base_open();

    return ORTE_SUCCESS;
}

#endif /* ORTE_DISABLE_FULL_SUPPORT */
