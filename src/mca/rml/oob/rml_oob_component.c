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

#include "orte_config.h"
#include "include/orte_constants.h"
#include "util/output.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/rml/base/base.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "rml_oob.h"
#include "mca/errmgr/errmgr.h"

static orte_rml_module_t* orte_rml_oob_init(int* priority);
static int orte_rml_oob_open(void);
static int orte_rml_oob_close(void);


/**
 * component definition
 */
orte_rml_component_t mca_rml_oob_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a rml v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RML_BASE_VERSION_1_0_0,

        "oob", /* MCA component name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
        orte_rml_oob_open,  /* component open */
        orte_rml_oob_close, /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },
      orte_rml_oob_init
};

orte_rml_module_t orte_rml_oob_module = {
    mca_oob_base_module_init,
    NULL,
    (orte_rml_module_get_uri_fn_t)mca_oob_get_contact_info,
    (orte_rml_module_set_uri_fn_t)mca_oob_set_contact_info,
    (orte_rml_module_parse_uris_fn_t)mca_oob_parse_contact_info,
    (orte_rml_module_ping_fn_t)mca_oob_ping,
    (orte_rml_module_send_fn_t)mca_oob_send,
    (orte_rml_module_send_nb_fn_t)mca_oob_send_nb,
    (orte_rml_module_send_buffer_fn_t)mca_oob_send_packed,
    (orte_rml_module_send_buffer_nb_fn_t)mca_oob_send_packed_nb,
    (orte_rml_module_recv_fn_t)mca_oob_recv,
    (orte_rml_module_recv_nb_fn_t)mca_oob_recv_nb,
    (orte_rml_module_recv_buffer_fn_t)mca_oob_recv_packed,
    (orte_rml_module_recv_buffer_nb_fn_t)mca_oob_recv_packed_nb,
    (orte_rml_module_recv_cancel_fn_t)mca_oob_recv_cancel,
    (orte_rml_module_barrier_fn_t)mca_oob_barrier,
    (orte_rml_module_xcast_fn_t)mca_oob_xcast
};


static orte_rml_module_t* orte_rml_oob_init(int* priority)
{
    if(mca_oob_base_init() != ORTE_SUCCESS)
        return NULL;
    *priority = 1;
    return &orte_rml_oob_module;
}


/*
 * initialize the underlying oob infrastructure so that all the
 * pointers in the RML struct can be valid.
 */
static int
orte_rml_oob_open(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = mca_oob_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return rc;
}


/*
 * shut down the OOB, since we started it.
 */
static int
orte_rml_oob_close(void)
{
    int rc;


    if (ORTE_SUCCESS != (rc = mca_oob_base_close())) {
        return rc;
    }

    return rc;
}
