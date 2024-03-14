/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
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
 * Copyright (c) 2018-2022 IBM Corporation.  All rights reserved.
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
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
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"


#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#if MPI_VERSION >= 4
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#endif

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

/* not #if conditional on OPAL_ENABLE_FT_MPI for ABI */
int mca_pml_base_revoke_comm(ompi_communicator_t *comm, bool coll_only)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}

#define xstringify(pml) #pml
#define stringify(pml) xstringify(pml)

/*
 * Global variables
 */
mca_pml_base_module_t mca_pml = {
    NULL,                    /* pml_add_procs */
    NULL,                    /* pml_del_procs */
    NULL,                    /* pml_enable */
    mca_pml_base_progress,   /* pml_progress */
    NULL,                    /* pml_add_comm */
    NULL,                    /* pml_del_comm */
    mca_pml_base_revoke_comm,/* pml_revoke_comm */
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
    0,                       /* pml_max_tag */
    0,                       /* pml_flags */
    NULL                     /* pml_get_transports */
};

mca_pml_base_component_t mca_pml_base_selected_component = {{0}};
opal_pointer_array_t mca_pml_base_pml = {{0}};
char *ompi_pml_base_bsend_allocator_name = NULL;
bool ompi_pml_base_check_pml = true;

#if !MCA_ompi_pml_DIRECT_CALL
static char *ompi_pml_base_wrapper = NULL;
#endif

#if MPI_VERSION >= 4
#define OMPI_PML_BASE_WARN_DEP_CANCEL_SEND_DEFAULT OMPI_PML_BASE_WARN_DEP_CANCEL_SEND_ONCE
int ompi_pml_base_warn_dep_cancel_send_level = OMPI_PML_BASE_WARN_DEP_CANCEL_SEND_DEFAULT;
mca_base_var_enum_value_t ompi_pml_base_warn_dep_cancel_send_values[] = {
    {.value = OMPI_PML_BASE_WARN_DEP_CANCEL_SEND_ALWAYS, .string = "always"},
    {.value = OMPI_PML_BASE_WARN_DEP_CANCEL_SEND_ONCE,   .string = "once"},
    {.value = OMPI_PML_BASE_WARN_DEP_CANCEL_SEND_NEVER,  .string = "never"},
    {0, NULL}
};
#endif

static int mca_pml_base_register(mca_base_register_flag_t flags)
{
#if !MCA_ompi_pml_DIRECT_CALL
    int var_id;
#endif

#if MPI_VERSION >= 4
    mca_base_var_enum_t *ompi_pml_base_warn_dep_cancel_send_enum = NULL;
    int rc;
#endif

    ompi_pml_base_bsend_allocator_name = "basic";
    (void) mca_base_var_register("ompi", "pml", "base", "bsend_allocator", NULL,
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &ompi_pml_base_bsend_allocator_name);

#if !MCA_ompi_pml_DIRECT_CALL
    ompi_pml_base_wrapper = NULL;
    var_id = mca_base_var_register("ompi", "pml", "base", "wrapper",
                                   "Use a Wrapper component around the selected PML component",
                                   MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                   OPAL_INFO_LVL_9,
                                   MCA_BASE_VAR_SCOPE_READONLY,
                                   &ompi_pml_base_wrapper);
    (void) mca_base_var_register_synonym(var_id, "ompi", "pml", NULL, "wrapper", 0);
#endif

#if MPI_VERSION >= 4
    mca_base_var_enum_create("pml_base_deprecate_warnings", ompi_pml_base_warn_dep_cancel_send_values,
                             &ompi_pml_base_warn_dep_cancel_send_enum);
    rc = mca_base_var_register("ompi", "pml", "base", "warn_dep_cancel_send",
                               "How often to issue warnings for deprecated cancellation of send requests",
                               MCA_BASE_VAR_TYPE_INT, ompi_pml_base_warn_dep_cancel_send_enum, 0, 0,
                               OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                               &ompi_pml_base_warn_dep_cancel_send_level);
    OBJ_RELEASE(ompi_pml_base_warn_dep_cancel_send_enum);
    if (OPAL_ERR_VALUE_OUT_OF_BOUNDS == rc) {
        ompi_pml_base_warn_dep_cancel_send_level = OMPI_PML_BASE_WARN_DEP_CANCEL_SEND_DEFAULT;
        opal_output(0, "pml:base:register: Warning invalid deprecation warning value specified. Using default: %d",
                    ompi_pml_base_warn_dep_cancel_send_level);
    }
#endif

    ompi_pml_base_check_pml = true;
    (void) mca_base_var_register("ompi", "pml", "base", "check_pml",
                                 "Whether to check the pml selections to ensure they all match",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &ompi_pml_base_check_pml);

    return OMPI_SUCCESS;
}

static int mca_pml_base_close(void)
{
    int i, j;

    /* turn off the progress code for the pml */
    if( NULL != mca_pml.pml_progress ) {
        opal_progress_unregister(mca_pml.pml_progress);
    }

    /* Blatantly ignore the return code (what would we do to recover,
       anyway?  This module is going away, so errors don't matter
       anymore) */

    /**
     * Destruct the send and receive queues. The opal_free_list_t destructor
     * will return the memory to the mpool, so this has to be done before the
     * mpool get released by the PML close function.
     */
    OBJ_DESTRUCT(&mca_pml_base_send_requests);
    OBJ_DESTRUCT(&mca_pml_base_recv_requests);

    mca_pml.pml_progress = mca_pml_base_progress;

    /* Free all the strings in the array */
    j = opal_pointer_array_get_size(&mca_pml_base_pml);
    for (i = 0; i < j; ++i) {
        char *str;
        str = (char*) opal_pointer_array_get_item(&mca_pml_base_pml, i);
        free(str);
    }
    OBJ_DESTRUCT(&mca_pml_base_pml);

    /* Close all remaining available components */
    return mca_base_framework_components_close(&ompi_pml_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int mca_pml_base_open(mca_base_open_flag_t flags)
{
    /**
     * Construct the send and receive request queues. There are 2 reasons to do it
     * here. First, as they are globals it's better to construct them in one common
     * place. Second, in order to be able to allow the external debuggers to show
     * their content, they should get constructed as soon as possible once the MPI
     * process is started.
     */
    OBJ_CONSTRUCT(&mca_pml_base_send_requests, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_base_recv_requests, opal_free_list_t);

    OBJ_CONSTRUCT(&mca_pml_base_pml, opal_pointer_array_t);

    /* Open up all available components */

    if (OPAL_SUCCESS !=
        mca_base_framework_components_open(&ompi_pml_base_framework, flags)) {
        return OMPI_ERROR;
    }

    /* Set a sentinel in case we don't select any components (e.g.,
       ompi_info) */

    mca_pml_base_selected_component.pmlm_finalize = NULL;

    /**
     * Right now our selection of BTLs is completely broken. If we have
     * multiple PMLs that use BTLs than we will open all BTLs several times, leading to
     * undefined behaviors. The simplest solution, at least until we
     * figure out the correct way to do it, is to force a default PML that
     * uses BTLs and any other PMLs that do not in the mca_pml_base_pml array.
     */

#if MCA_ompi_pml_DIRECT_CALL
    opal_pointer_array_add(&mca_pml_base_pml,
                           strdup(stringify(MCA_ompi_pml_DIRECT_CALL_COMPONENT)));
#else
    {
        const char **default_pml = NULL;
        int var_id;

        var_id = mca_base_var_find("ompi", "pml", NULL, NULL);
        mca_base_var_get_value(var_id, &default_pml, NULL, NULL);

        if( (NULL == default_pml || NULL == default_pml[0] ||
             0 == strlen(default_pml[0])) || (default_pml[0][0] == '^') ) {
            opal_pointer_array_add(&mca_pml_base_pml, strdup("ob1"));
            opal_pointer_array_add(&mca_pml_base_pml, strdup("ucx"));
            opal_pointer_array_add(&mca_pml_base_pml, strdup("cm"));
        } else {
#if OPAL_ENABLE_DEBUG
            char **req_pml = opal_argv_split(default_pml[0], ',');
            if( NULL != req_pml[1] ) {
                opal_output(0, "Only one PML must be provided. Using %s PML (the"
                            " first on the MCA pml list)", req_pml[0]);
                opal_pointer_array_add(&mca_pml_base_pml, strdup(req_pml[0]));
            } else {
                opal_pointer_array_add(&mca_pml_base_pml, strdup(default_pml[0]));
            }
            opal_argv_free(req_pml);
#else
            opal_pointer_array_add(&mca_pml_base_pml, strdup(default_pml[0]));
#endif  /* OPAL_ENABLE_DEBUG */
        }
    }
#endif

    return OMPI_SUCCESS;

}

MCA_BASE_FRAMEWORK_DECLARE(ompi, pml, "OMPI PML", mca_pml_base_register,
                           mca_pml_base_open, mca_pml_base_close,
                           mca_pml_base_static_components, 0);
