/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/mca/event/event.h"

#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"
#endif
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "rml_oob.h"

static orte_rml_base_module_t* rml_oob_init(int* priority);
static int rml_oob_open(void);
static int rml_oob_close(void);
static orte_rml_base_module_t *open_conduit(opal_list_t *attributes);
static int query_transports(opal_list_t *providers);

/**
 * component definition
 */
orte_rml_component_t mca_rml_oob_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

    .rml_version = {
        ORTE_RML_BASE_VERSION_3_0_0,

        .mca_component_name = "oob",
        MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                              ORTE_RELEASE_VERSION),
        .mca_open_component = rml_oob_open,
        .mca_close_component = rml_oob_close,
    },
    .rml_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .open_conduit = open_conduit,
    .query_transports = query_transports
};

orte_rml_oob_module_t orte_rml_oob_module = {
    .api = {
        .finalize = orte_rml_oob_fini,

        .get_contact_info = orte_rml_oob_get_uri,
        .set_contact_info = orte_rml_oob_set_uri,

        .ping = orte_rml_oob_ping,

        .send_nb = orte_rml_oob_send_nb,
        .send_buffer_nb = orte_rml_oob_send_buffer_nb,

        .purge = orte_rml_oob_purge
    }
};

/* Local variables */
static bool init_done = false;

static int rml_oob_open(void)
{
    return ORTE_SUCCESS;
}


static int rml_oob_close(void)
{
    return ORTE_SUCCESS;
}

static orte_rml_base_module_t *open_conduit(opal_list_t *attributes)
{
    orte_rml_oob_module_t *mod;

    /* check the list of attributes to see if we should respond */

    /* we will provide this module, so allocate the space for it */
    mod = (orte_rml_oob_module_t*)calloc(1, sizeof(orte_rml_oob_module_t));
    /* copy the function pointers across */
    memcpy(&mod->api, orte_rml_oob_module, sizeof(&orte_rml_oob_module.api));
    /* setup the remaining data locations */
    OBJ_CONSTRUCT(&mod->queued_routing_messages, opal_list_t);
    mod->timer_event = NULL;
    /* the timeout struct was already zero'd */
    return mod;
}

static int query_transports(opal_list_t *providers)
{
    opal_value_t *val;

    /* we only support Ethernet */
    val = OBJ_NEW(opal_value_t);
    val->key = strdup(ORTE_TRANSPORT);
    val->type = OPAL_STRING;
    val->data.string = strdup("Ethernet");
    opal_list_append(providers, &val->super);

    return ORTE_SUCCESS;
}
