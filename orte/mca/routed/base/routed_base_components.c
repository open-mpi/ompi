/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_bitmap.h"
#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/routed/routed.h"
#include "orte/mca/routed/base/base.h"


/* The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct. */
#include "orte/mca/routed/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_routed_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

static void construct(orte_routed_tree_t *rt)
{
    rt->vpid = ORTE_VPID_INVALID;
    OBJ_CONSTRUCT(&rt->relatives, opal_bitmap_t);
}
static void destruct(orte_routed_tree_t *rt)
{
    OBJ_DESTRUCT(&rt->relatives);
}
OBJ_CLASS_INSTANCE(orte_routed_tree_t, opal_list_item_t,
                   construct, destruct);

static void jfamconst(orte_routed_jobfam_t *ptr)
{
    ptr->route.jobid = ORTE_JOBID_INVALID;
    ptr->route.vpid = ORTE_VPID_INVALID;
    ptr->route.epoch = ORTE_EPOCH_MIN;
    ptr->hnp_uri = NULL;
}
static void jfamdest(orte_routed_jobfam_t *ptr)
{
    if (NULL != ptr->hnp_uri) {
        free(ptr->hnp_uri);
    }
}
OBJ_CLASS_INSTANCE(orte_routed_jobfam_t, opal_object_t,
                   jfamconst, jfamdest);

int orte_routed_base_output = -1;
orte_routed_module_t orte_routed = {0};
opal_list_t orte_routed_base_components;
opal_mutex_t orte_routed_base_lock;
opal_condition_t orte_routed_base_cond;
bool orte_routed_base_wait_sync;
opal_pointer_array_t orte_routed_jobfams;

static orte_routed_component_t *active_component = NULL;
static bool component_open_called = false;
static bool opened = false;
static bool selected = false;

int
orte_routed_base_open(void)
{
    int ret;
    orte_routed_jobfam_t *jfam;

    if (opened) {
        return ORTE_SUCCESS;
    }
    opened = true;
    
    /* setup the output stream */
    orte_routed_base_output = opal_output_open(NULL);
    OBJ_CONSTRUCT(&orte_routed_base_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_routed_base_cond, opal_condition_t);
    orte_routed_base_wait_sync = false;
    
    /* Initialize globals */
    OBJ_CONSTRUCT(&orte_routed_base_components, opal_list_t);
   
    /* Initialize storage of remote hnp uris */
    OBJ_CONSTRUCT(&orte_routed_jobfams, opal_pointer_array_t);
    opal_pointer_array_init(&orte_routed_jobfams, 8, INT_MAX, 8);
    /* prime it with our HNP uri */
    jfam = OBJ_NEW(orte_routed_jobfam_t);
    jfam->route.jobid = ORTE_PROC_MY_HNP->jobid;
    jfam->route.vpid = ORTE_PROC_MY_HNP->vpid;
    jfam->route.epoch = ORTE_PROC_MY_HNP->epoch;
    jfam->job_family = ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid);
    if (NULL != orte_process_info.my_hnp_uri) {
        jfam->hnp_uri = strdup(orte_process_info.my_hnp_uri);
    }
    opal_pointer_array_add(&orte_routed_jobfams, jfam);

    /* Open up all available components */
    ret = mca_base_components_open("routed",
                                   orte_routed_base_output,
                                   mca_routed_base_static_components, 
                                   &orte_routed_base_components,
                                   true);
    component_open_called = true;

    return ret;
}

int
orte_routed_base_select(void)
{
    int ret, exit_status = OPAL_SUCCESS;
    orte_routed_component_t *best_component = NULL;
    orte_routed_module_t *best_module = NULL;

    if (selected) {
        return ORTE_SUCCESS;
    }
    selected = true;
    
    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("routed", orte_routed_base_output,
                                        &orte_routed_base_components,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected */
        exit_status = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Save the winner */
    orte_routed = *best_module;
    active_component = best_component;

    /* initialize the selected component */
    opal_output_verbose(10, orte_routed_base_output,
                        "orte_routed_base_select: initializing selected component %s",
                        best_component->base_version.mca_component_name);
    if (ORTE_SUCCESS != (ret = orte_routed.initialize()) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}


int
orte_routed_base_close(void)
{
    int i;
    orte_routed_jobfam_t *jfam;

    /* finalize the selected component */
    if (NULL != orte_routed.finalize) {
        orte_routed.finalize();
    }
    
    /* shutdown any remaining opened components */
    if (component_open_called) {
        mca_base_components_close(orte_routed_base_output, 
                                  &orte_routed_base_components, NULL);
    }

    for (i=0; i < orte_routed_jobfams.size; i++) {
        if (NULL != (jfam = (orte_routed_jobfam_t*)opal_pointer_array_get_item(&orte_routed_jobfams, i))) {
            OBJ_RELEASE(jfam);
        }
    }
    OBJ_DESTRUCT(&orte_routed_jobfams);

    OBJ_DESTRUCT(&orte_routed_base_components);
    OBJ_DESTRUCT(&orte_routed_base_lock);
    OBJ_DESTRUCT(&orte_routed_base_cond);
    
    opened   = false;
    selected = false;

    return ORTE_SUCCESS;
}

void orte_routed_base_update_hnps(opal_buffer_t *buf)
{
    int n, rc;
    char *uri;
    orte_process_name_t name;
    orte_routed_jobfam_t *jfam;
    uint16_t jobfamily;

    n = 1;
    while (ORTE_SUCCESS == opal_dss.unpack(buf, &uri, &n, OPAL_STRING)) {
        /*extract the name */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(uri, &name, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(uri);
            n=1;
            continue;
        }
        jobfamily = ORTE_JOB_FAMILY(name.jobid);
        /* see if we already have this connection */
        for (n=0; n < orte_routed_jobfams.size; n++) {
            if (NULL == (jfam = (orte_routed_jobfam_t*)opal_pointer_array_get_item(&orte_routed_jobfams,n))) {
                continue;
            }
            if (jobfamily == jfam->job_family) {
                /* update uri */
                if (NULL != jfam->hnp_uri) {
                    free(jfam->hnp_uri);
                }
                jfam->hnp_uri = strdup(uri);
                OPAL_OUTPUT_VERBOSE((10, orte_routed_base_output,
                                     "%s adding remote HNP %s\n\t%s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name), uri));
                goto done;
            }
        }
        /* nope - create it */
        jfam = OBJ_NEW(orte_routed_jobfam_t);
        jfam->job_family = jobfamily;
        jfam->route.jobid = name.jobid;
        jfam->route.vpid = name.vpid;
        jfam->route.epoch = name.epoch;
        jfam->hnp_uri = strdup(uri);
    done:
        free(uri);
        n=1;
    }
}

#endif /* ORTE_DISABLE_FULL_SUPPORT */
