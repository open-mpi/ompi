/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */

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
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
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
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"

#include "orte/constants.h"
#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "orte/mca/snapc/base/static-components.h"

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

static int orte_snapc_base_register(int flags)
{
    /*
     * Reuse sequence numbers
     * This will create a directory and always use seq 0 for all checkpoints
     * This *should* also enforce a 2-phase commit protocol
     */
    orte_snapc_base_store_only_one_seq = false;
    (void) mca_base_var_register("orte", "snapc", "base", "only_one_seq",
                                 "Only store the most recent checkpoint sequence. [Default = disabled]",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &orte_snapc_base_store_only_one_seq);

    return ORTE_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int orte_snapc_base_open(void)
{
    (void) orte_snapc_base_register(0);

    OPAL_OUTPUT_VERBOSE((10, orte_snapc_base_output,
                         "snapc:base: open()"));

    orte_snapc_base_output = opal_output_open(NULL);

    OPAL_OUTPUT_VERBOSE((20, orte_snapc_base_output,
                         "snapc:base: open: base_only_one_seq    = %d",
                         orte_snapc_base_store_only_one_seq));


    /* Init the sequence (interval) number */
    orte_snapc_base_snapshot_seq_number = 0;

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
