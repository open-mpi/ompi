/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_header.h"
#include "mca/iof/base/iof_base_fragment.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/iof/base/static-components.h"


/*
 * Global variables
 */

mca_iof_base_t mca_iof_base;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_iof_base_open(void)
{
    int id;
    int int_value;
    char* str_value;

    /* Initialize globals */
    OBJ_CONSTRUCT(&mca_iof_base.iof_components_opened, ompi_list_t);
    OBJ_CONSTRUCT(&mca_iof_base.iof_endpoints, ompi_list_t);
    OBJ_CONSTRUCT(&mca_iof_base.iof_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_iof_base.iof_condition, ompi_condition_t);
    OBJ_CONSTRUCT(&mca_iof_base.iof_fragments, ompi_free_list_t);

    /* lookup common parameters */
    id = mca_base_param_register_int("iof","base","window_size",NULL,MCA_IOF_BASE_MSG_MAX << 1);
    mca_base_param_lookup_int(id,&int_value);
    mca_iof_base.iof_window_size = int_value;

    id = mca_base_param_register_string("iof","base","service",NULL,"0.0.0");
    mca_base_param_lookup_int(id,&str_value);
    mca_iof_base.iof_service = ompi_name_server.convert_string_to_process_name(str_value);
 
    /* initialize free list */
    ompi_free_list_init(
        &mca_iof_base.iof_fragments,
        sizeof(mca_iof_base_frag_t),
        OBJ_CLASS(mca_iof_base_frag_t),
        0,   /* number to initially allocate */
        -1,  /* maximum elements to allocate */
        32,  /* number per allocation */
        NULL); /* optional memory pool */


    /* Open up all available components */
    if (OMPI_SUCCESS != 
        mca_base_components_open("iof", 0, mca_iof_base_static_components, 
                                 &mca_iof_base.iof_components_opened)) {
      return OMPI_ERROR;
    }
  
    /* All done */
    return OMPI_SUCCESS;
}

