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
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"

#include "orte/mca/grpcomm/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/grpcomm/base/static-components.h"

/*
 * Global variables
 */
orte_grpcomm_base_t orte_grpcomm_base;

orte_grpcomm_API_module_t orte_grpcomm = {
    orte_grpcomm_API_xcast,
    orte_grpcomm_API_allgather
};

static bool recv_issued = false;

static int orte_grpcomm_base_close(void)
{
    orte_grpcomm_base_active_t *active;

    if (recv_issued) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_XCAST);
        recv_issued = false;
    }

    /* Close the active modules */
    OPAL_LIST_FOREACH(active, &orte_grpcomm_base.actives, orte_grpcomm_base_active_t) {
        if (NULL != active->module->finalize) {
            active->module->finalize();
        }
    }
    OPAL_LIST_DESTRUCT(&orte_grpcomm_base.actives);
    OPAL_LIST_DESTRUCT(&orte_grpcomm_base.ongoing);
    OBJ_DESTRUCT(&orte_grpcomm_base.sig_table);

    return mca_base_framework_components_close(&orte_grpcomm_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int orte_grpcomm_base_open(mca_base_open_flag_t flags)
{
    OBJ_CONSTRUCT(&orte_grpcomm_base.actives, opal_list_t);
    OBJ_CONSTRUCT(&orte_grpcomm_base.ongoing, opal_list_t);
    OBJ_CONSTRUCT(&orte_grpcomm_base.sig_table, opal_hash_table_t);
    opal_hash_table_init(&orte_grpcomm_base.sig_table, 128);

    return mca_base_framework_components_open(&orte_grpcomm_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, grpcomm, NULL, NULL, orte_grpcomm_base_open, orte_grpcomm_base_close,
                           mca_grpcomm_base_static_components, 0);

OBJ_CLASS_INSTANCE(orte_grpcomm_base_active_t,
                   opal_list_item_t,
                   NULL, NULL);

static void scon(orte_grpcomm_signature_t *p)
{
    p->signature = NULL;
    p->sz = 0;
    p->seq_num = 0;
}
static void sdes(orte_grpcomm_signature_t *p)
{
    if (NULL != p->signature) {
        free(p->signature);
    }
}
OBJ_CLASS_INSTANCE(orte_grpcomm_signature_t,
                   opal_object_t,
                   scon, sdes);

static void ccon(orte_grpcomm_coll_t *p)
{
    p->sig = NULL;
    OBJ_CONSTRUCT(&p->bucket, opal_buffer_t);
    p->dmns = NULL;
    p->ndmns = 0;
    p->nreported = 0;
    p->distance_mask_recv = NULL;
    p->cbfunc = NULL;
    p->cbdata = NULL;
    p->buffers = NULL;
}
static void cdes(orte_grpcomm_coll_t *p)
{
    if (NULL != p->sig) {
        OBJ_RELEASE(p->sig);
    }
    OBJ_DESTRUCT(&p->bucket);
    if (NULL != p->dmns) {
        free(p->dmns);
    }
    free(p->buffers);
    if (NULL != p->distance_mask_recv) {
        free(p->distance_mask_recv);
    }
}
OBJ_CLASS_INSTANCE(orte_grpcomm_coll_t,
                   opal_list_item_t,
                   ccon, cdes);
