/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"


#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"

mca_btl_active_message_callback_t mca_btl_base_active_message_trigger[MCA_BTL_TAG_MAX];

/*
 *  mca_btl_base_descriptor_t
 */

static void mca_btl_base_descriptor_constructor(mca_btl_base_descriptor_t* des)
{
    des->des_src = NULL;
    des->des_src_cnt = 0;
    des->des_dst = NULL;
    des->des_dst_cnt = 0;
    des->des_cbfunc = NULL;
    des->des_cbdata = NULL;
    des->des_flags = 0;
}

static void mca_btl_base_descriptor_destructor(mca_btl_base_descriptor_t* des)
{
}

OBJ_CLASS_INSTANCE(
    mca_btl_base_descriptor_t,
    opal_list_item_t,
    mca_btl_base_descriptor_constructor,
    mca_btl_base_descriptor_destructor);


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "ompi/mca/btl/base/static-components.h"
#include "btl_base_error.h"

/*
 * Global variables
 */
int mca_btl_base_output = -1;
char* mca_btl_base_include = NULL;
char* mca_btl_base_exclude = NULL;
int mca_btl_base_warn_component_unused = 1;
opal_list_t mca_btl_base_components_opened;
opal_list_t mca_btl_base_modules_initialized;
int mca_btl_base_already_opened = 0;
bool mca_btl_base_thread_multiple_override = false;

static int mca_btl_base_register(int flags)
{
    mca_btl_base_verbose = 0;
    (void) mca_base_var_register("ompi", "btl", "base", "verbose",
                                 "Verbosity level of the BTL framework",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                 MCA_BASE_VAR_FLAG_SETTABLE,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_LOCAL,
                                 &mca_btl_base_verbose);

    /* Override the per-BTL "don't run if THREAD_MULTIPLE selected"
       embargo? */
    mca_btl_base_thread_multiple_override = false;
    (void) mca_base_var_register("ompi", "btl", "base", "thread_multiple_override",
                                 "Enable BTLs that are not normally enabled when MPI_THREAD_MULTIPLE is enabled (THIS IS FOR DEVELOPERS ONLY!  SHOULD NOT BE USED BY END USERS!)",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
                                 MCA_BASE_VAR_FLAG_INTERNAL,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_btl_base_thread_multiple_override);

    (void) mca_base_var_register("ompi", "btl", "base", "include", NULL,
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_btl_base_include);
    (void) mca_base_var_register("ompi", "btl", "base", "exclude", NULL,
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_btl_base_exclude);
    (void) mca_base_var_register("ompi", "btl", "base", "warn_component_unused",
                                 "This parameter is used to turn on warning messages when certain NICs are not used",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_btl_base_warn_component_unused);

    return OMPI_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_btl_base_open(void)
{
    int i;
    if( ++mca_btl_base_already_opened > 1 ) return OMPI_SUCCESS;

    (void) mca_btl_base_register(0);

    /* Verbose output */
    mca_btl_base_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_btl_base_output, mca_btl_base_verbose);

    /* Open up all available components */
    
    if (OMPI_SUCCESS != 
        mca_base_components_open("btl", mca_btl_base_output, mca_btl_base_static_components,
                                 &mca_btl_base_components_opened, true)) {
        return OMPI_ERROR;
    }

  /* Initialize the list so that in mca_btl_base_close(), we can
     iterate over it (even if it's empty, as in the case of
     ompi_info) */

  OBJ_CONSTRUCT(&mca_btl_base_modules_initialized, opal_list_t);

  /* All done */
  return OMPI_SUCCESS;
}
