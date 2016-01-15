/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include <stdio.h>

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNIST_H */
#include "oshmem/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "oshmem/constants.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/base.h"
#include "oshmem/mca/spml/base/spml_base_request.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "oshmem/mca/spml/base/static-components.h"

#define xstringify(spml) #spml
#define stringify(spml) xstringify(spml)

/*
 * Global variables
 */
mca_spml_base_module_t mca_spml = {0};

mca_spml_base_component_t mca_spml_base_selected_component = {{0}};
opal_pointer_array_t mca_spml_base_spml = {{0}};


static int mca_spml_base_register(mca_base_register_flag_t flags)
{
    return OMPI_SUCCESS;
}

int mca_spml_base_finalize(void)
{
    if (NULL != mca_spml_base_selected_component.spmlm_finalize) {
        return mca_spml_base_selected_component.spmlm_finalize();
    }
    return OSHMEM_SUCCESS;
}

static int mca_spml_base_close(void)
{
    int i, j;

    /**
     * Destruct the send and receive queues. The opal_free_list_t destructor
     * will return the memory to the mpool, so this has to be done before the
     * mpool get released by the SPML close function.
     */
    OBJ_DESTRUCT(&mca_spml_base_put_requests);
    OBJ_DESTRUCT(&mca_spml_base_get_requests);

    /* Free all the strings in the array */
    j = opal_pointer_array_get_size(&mca_spml_base_spml);
    for (i = 0; i < j; i++) {
        char * tmp_val;
        tmp_val = (char *) opal_pointer_array_get_item(&mca_spml_base_spml, i);
        if (NULL == tmp_val) {
            continue;
        }
        free(tmp_val);
    }
    OBJ_DESTRUCT(&mca_spml_base_spml);

    /* Close all remaining available components */
    return mca_base_framework_components_close(&oshmem_spml_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int mca_spml_base_open(mca_base_open_flag_t flags)
{
    /**
     * Construct the send and receive request queues. There are 2 reasons to do it
     * here. First, as they are globals it's better to construct them in one common
     * place. Second, in order to be able to allow the external debuggers to show
     * their content, they should get constructed as soon as possible once the MPI
     * process is started.
     */
    OBJ_CONSTRUCT(&mca_spml_base_put_requests, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_spml_base_get_requests, opal_free_list_t);

    OBJ_CONSTRUCT(&mca_spml_base_spml, opal_pointer_array_t);

    oshmem_framework_open_output(&oshmem_spml_base_framework);

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_framework_components_open(&oshmem_spml_base_framework, flags)) {
        return OSHMEM_ERROR;
    }

    /* Set a sentinel in case we don't select any components (e.g.,
       ompi_info) */

    mca_spml_base_selected_component.spmlm_finalize = NULL;

    /**
     * Right now our selection of BTLs is completely broken. If we have
     * multiple SPMLs that use BTLs than we will open all BTLs several times, leading to
     * undefined behaviors. The simplest solution, at least until we
     * figure out the correct way to do it, is to force a default SPML that
     * uses BTLs and any other SPMLs that do not in the mca_spml_base_spml array.
     */

#if MCA_ompi_pml_DIRECT_CALL
    opal_pointer_array_add(&mca_spml_base_spml,
                           strdup(stringify(MCA_oshmem_spml_DIRECT_CALL_COMPONENT)));
#else
    {
        const char **default_spml = NULL;
        int var_id;

        var_id = mca_base_var_find("oshmem", "spml", NULL, NULL);
        mca_base_var_get_value(var_id, &default_spml, NULL, NULL);

        if( (NULL == default_spml || NULL == default_spml[0] ||
             0 == strlen(default_spml[0])) || (default_spml[0][0] == '^') ) {
            opal_pointer_array_add(&mca_spml_base_spml, strdup("ikrit"));
            opal_pointer_array_add(&mca_spml_base_spml, strdup("yoda"));
        } else {
            opal_pointer_array_add(&mca_spml_base_spml, strdup(default_spml[0]));
        }
    }
#endif

    return OSHMEM_SUCCESS;
}

MCA_BASE_FRAMEWORK_DECLARE(oshmem, spml,
                           "OSHMEM SPML",
                           mca_spml_base_register,
                           mca_spml_base_open,
                           mca_spml_base_close,
                           mca_spml_base_static_components,
                           MCA_BASE_FRAMEWORK_FLAG_DEFAULT);
