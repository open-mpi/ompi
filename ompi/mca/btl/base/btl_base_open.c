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
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
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
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"

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

static bool already_opened = false;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_btl_base_open(void)
{
    if (already_opened) return OMPI_SUCCESS;
    already_opened = true;

    mca_base_param_reg_int_name(
                                "btl", 
                                "base_debug", 
                                "If btl_base_debug is 1 standard debug is output, if > 1 verbose debug is output", 
                                false, false, 
                                0, 
                                &mca_btl_base_debug);

    if( mca_btl_base_debug > 0 ) {
        mca_btl_base_output = opal_output_open(NULL);
        opal_output_set_verbosity(mca_btl_base_output, mca_btl_base_debug);
    } else {
        mca_btl_base_output = -1;
    }

  /* Open up all available components */
    
  if (OMPI_SUCCESS != 
      mca_base_components_open("btl", 0, mca_btl_base_static_components, 
                               &mca_btl_base_components_opened, true)) {
    return OMPI_ERROR;
  }

  /* Initialize the list so that in mca_btl_base_close(), we can
     iterate over it (even if it's empty, as in the case of
     ompi_info) */

  OBJ_CONSTRUCT(&mca_btl_base_modules_initialized, opal_list_t);

  /* register parameters */
  mca_base_param_lookup_string(
      mca_base_param_register_string("btl","base","include",NULL,NULL), &mca_btl_base_include);
  mca_base_param_lookup_string(
      mca_base_param_register_string("btl","base","exclude",NULL,NULL), &mca_btl_base_exclude);
  mca_base_param_reg_int_name("btl", "base_warn_component_unused",
      "This parameter is used to turn on warning messages when certain NICs are not used",
      false, false, 1, &mca_btl_base_warn_component_unused);

  /* All done */
  return OMPI_SUCCESS;
}
