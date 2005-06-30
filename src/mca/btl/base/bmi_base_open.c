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
#include <stdio.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"
#include "mca/bmi/base/base.h"

/*
 *  mca_bmi_base_descriptor_t
 */

static void mca_bmi_base_descriptor_constructor(mca_bmi_base_descriptor_t* des)
{
    des->des_src = NULL;
    des->des_src_cnt = 0;
    des->des_dst = NULL;
    des->des_dst_cnt = 0;
    des->des_cbfunc = NULL;
    des->des_cbdata = NULL;
    des->des_flags = 0;
}

static void mca_bmi_base_descriptor_destructor(mca_bmi_base_descriptor_t* des)
{
}

OBJ_CLASS_INSTANCE(
    mca_bmi_base_descriptor_t,
    ompi_list_item_t,
    mca_bmi_base_descriptor_constructor,
    mca_bmi_base_descriptor_destructor);


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/bmi/base/static-components.h"


/*
 * Global variables
 */
int mca_bmi_base_output = -1;
char* mca_bmi_base_include = NULL;
char* mca_bmi_base_exclude = NULL;
ompi_list_t mca_bmi_base_components_opened;
ompi_list_t mca_bmi_base_modules_initialized;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_bmi_base_open(void)
{
  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("bmi", 0, mca_bmi_base_static_components, 
                               &mca_bmi_base_components_opened, true)) {
    return OMPI_ERROR;
  }

  /* Initialize the list so that in mca_bmi_base_close(), we can
     iterate over it (even if it's empty, as in the case of
     ompi_info) */

  OBJ_CONSTRUCT(&mca_bmi_base_modules_initialized, ompi_list_t);

  /* register parameters */
  mca_base_param_lookup_string(
      mca_base_param_register_string("bmi","base","include",NULL,NULL), &mca_bmi_base_include);
  mca_base_param_lookup_string(
      mca_base_param_register_string("bmi","base","exclude",NULL,NULL), &mca_bmi_base_exclude);

  /* All done */
  return OMPI_SUCCESS;
}
