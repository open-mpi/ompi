/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_bitmap.h"
#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/oob/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/oob/base/static-components.h"

/*
 * Global variables
 */
orte_oob_base_t orte_oob_base;

static int orte_oob_base_register(mca_base_register_flag_t flags)
{
    (void)mca_base_var_register("orte", "oob", "base", "enable_module_progress_threads",
                                "Whether to independently progress OOB messages for each interface",
                                MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                OPAL_INFO_LVL_9,
                                MCA_BASE_VAR_SCOPE_READONLY,
                                &orte_oob_base.use_module_threads);

    return ORTE_SUCCESS;
}

static int orte_oob_base_close(void)
{
    mca_oob_base_component_t *component;
    mca_base_component_list_item_t *cli;
    opal_object_t *value;
    uint64_t key;
    void *node;
    int rc;

    /* shutdown all active transports */
    while (NULL != (cli = (mca_base_component_list_item_t *) opal_list_remove_first (&orte_oob_base.actives))) {
        component = (mca_oob_base_component_t*)cli->cli_component;
        if (NULL != component->shutdown) {
            component->shutdown();
        }
        OBJ_RELEASE(cli);
    }

    /* destruct our internal lists */
    OBJ_DESTRUCT(&orte_oob_base.actives);

    /* release all peers from the hash table */
    rc = opal_hash_table_get_first_key_uint64 (&orte_oob_base.peers, &key,
                                               (void **) &value, &node);
    while (OPAL_SUCCESS == rc) {
        if (NULL != value) {
            OBJ_RELEASE(value);
        }
        rc = opal_hash_table_get_next_key_uint64 (&orte_oob_base.peers, &key,
                                                  (void **) &value, node, &node);
    }

    OBJ_DESTRUCT(&orte_oob_base.peers);

    return mca_base_framework_components_close(&orte_oob_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
static int orte_oob_base_open(mca_base_open_flag_t flags)
{
    /* setup globals */
    orte_oob_base.max_uri_length = -1;
    OBJ_CONSTRUCT(&orte_oob_base.peers, opal_hash_table_t);
    opal_hash_table_init(&orte_oob_base.peers, 128);
    OBJ_CONSTRUCT(&orte_oob_base.actives, opal_list_t);

     /* Open up all available components */
    return mca_base_framework_components_open(&orte_oob_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, oob, "Out-of-Band Messaging Subsystem",
                           orte_oob_base_register, orte_oob_base_open, orte_oob_base_close,
                           mca_oob_base_static_components, 0);


OBJ_CLASS_INSTANCE(orte_oob_send_t,
                   opal_object_t,
                   NULL, NULL);

static void pr_cons(orte_oob_base_peer_t *ptr)
{
    ptr->component = NULL;
    OBJ_CONSTRUCT(&ptr->addressable, opal_bitmap_t);
    opal_bitmap_init(&ptr->addressable, 8);
}
static void pr_des(orte_oob_base_peer_t *ptr)
{
    OBJ_DESTRUCT(&ptr->addressable);
}
OBJ_CLASS_INSTANCE(orte_oob_base_peer_t,
                   opal_object_t,
                   pr_cons, pr_des);

