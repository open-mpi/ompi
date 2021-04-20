/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      IBM Corporation. All rights reserved.
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNIST_H */
#include "ompi/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"


#include "ompi/constants.h"
#include "ompi/mca/part/part.h"
#include "ompi/mca/part/base/base.h"
#include "ompi/mca/part/base/part_base_prequest.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "ompi/mca/part/base/static-components.h"

/*
 * This function allows modules to not declare a progress function,
 * by defaulting to doing nothing.
 */
int mca_part_base_progress(void)
{
    return OMPI_SUCCESS;
}

#define xstringify(part) #part
#define stringify(part) xstringify(part)

/*
 * Global variables
 */
mca_part_base_module_t mca_part = {
    .part_progress = mca_part_base_progress   /* part_progress */
};

mca_part_base_component_t mca_part_base_selected_component = {{0}};
opal_pointer_array_t mca_part_base_part = {{0}};

static int mca_part_base_register(mca_base_register_flag_t flags)
{
    return OMPI_SUCCESS;
}

int mca_part_base_finalize(void) {
  if (NULL != mca_part_base_selected_component.partm_finalize) {
    return mca_part_base_selected_component.partm_finalize();
  }
  return OMPI_SUCCESS;
}


static int mca_part_base_close(void)
{
    int i, j;

    /* unregister the progress function */
    if( NULL != mca_part.part_progress ) {
        opal_progress_unregister(mca_part.part_progress);
    }

    /* reset the progress function to do nothing */
    mca_part.part_progress = mca_part_base_progress;

    /* Free all the strings in the array of components */
    j = opal_pointer_array_get_size(&mca_part_base_part);
    for (i = 0; i < j; ++i) {
        char *str;
        str = (char*) opal_pointer_array_get_item(&mca_part_base_part, i);
        free(str);
    }
    OBJ_DESTRUCT(&mca_part_base_part);

    OBJ_DESTRUCT(&mca_part_base_psend_requests);
    OBJ_DESTRUCT(&mca_part_base_precv_requests);

    /* Close all remaining available components */
    return mca_base_framework_components_close(&ompi_part_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int mca_part_base_open(mca_base_open_flag_t flags)
{
    OBJ_CONSTRUCT(&mca_part_base_part, opal_pointer_array_t);

    
    OBJ_CONSTRUCT(&mca_part_base_psend_requests, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_part_base_precv_requests, opal_free_list_t);
    /* Open up all available components */

    if (OPAL_SUCCESS !=
        mca_base_framework_components_open(&ompi_part_base_framework, flags)) {
        return OMPI_ERROR;
    }

    /* Set a sentinel in case we don't select any components (e.g.,
       ompi_info) */

    mca_part_base_selected_component.partm_finalize = NULL;

    /* Currently this uses a default with no selection criteria as there is only 1 module. */
    opal_pointer_array_add(&mca_part_base_part, strdup("persist"));

    return OMPI_SUCCESS;
}

MCA_BASE_FRAMEWORK_DECLARE(ompi, part, "OMPI PART", mca_part_base_register,
                           mca_part_base_open, mca_part_base_close,
                           mca_part_base_static_components, 0);
