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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
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

#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "rml_oob.h"

static orte_rml_module_t* rml_oob_init(int* priority);
static int rml_oob_open(void);
static int rml_oob_close(void);

/**
 * component definition
 */
orte_rml_component_t mca_rml_oob_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        ORTE_RML_BASE_VERSION_2_0_0,

        "oob", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        rml_oob_open,  /* component open */
        rml_oob_close, /* component close */
      },
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },
      rml_oob_init
};

orte_rml_oob_module_t orte_rml_oob_module = {
    {
        orte_rml_oob_init,
        orte_rml_oob_fini,

        orte_rml_oob_get_uri,
        orte_rml_oob_set_uri,

        orte_rml_oob_ping,

        orte_rml_oob_send_nb,
        orte_rml_oob_send_buffer_nb,

        orte_rml_oob_recv_nb,
        orte_rml_oob_recv_buffer_nb,

        orte_rml_oob_recv_cancel,

        orte_rml_oob_add_exception,
        orte_rml_oob_del_exception,

        orte_rml_oob_ft_event,
        
        orte_rml_oob_purge
    }
};

/* Local variables */
static bool init_done = false;

static int
rml_oob_open(void)
{
    return ORTE_SUCCESS;
}


static int
rml_oob_close(void)
{
    return ORTE_SUCCESS;
}

static orte_rml_module_t*
rml_oob_init(int* priority)
{
    if (init_done) {
        *priority = 1;
        return &orte_rml_oob_module.super;
    }
    
    *priority = 1;
    
    OBJ_CONSTRUCT(&orte_rml_oob_module.exceptions, opal_list_t);
    
    init_done = true;
    return &orte_rml_oob_module.super;
}

int
orte_rml_oob_init(void)
{
    /* enable the base receive to get updates on contact info */
    orte_rml_base_comm_start();
    
    return ORTE_SUCCESS;
}


int
orte_rml_oob_fini(void)
{
    opal_list_item_t *item;

    while (NULL != 
           (item = opal_list_remove_first(&orte_rml_oob_module.exceptions))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_rml_oob_module.exceptions);

    /* clear the base receive */
    orte_rml_base_comm_stop();
    
    return ORTE_SUCCESS;
}

#if OPAL_ENABLE_FT_CR == 1
int
orte_rml_oob_ft_event(int state) {
    int exit_status = ORTE_SUCCESS;
    int ret;

    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    if( ORTE_SUCCESS != 
        (ret = orte_oob.ft_event(state)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }


    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        (void) mca_base_framework_close(&orte_oob_base_framework);

        if (ORTE_SUCCESS != (ret = mca_base_framework_open(&orte_oob_base_framework, 0))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        if( ORTE_SUCCESS != (ret = mca_oob_base_select())) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

 cleanup:
    return exit_status;
}
#else
int
orte_rml_oob_ft_event(int state) {
    return ORTE_SUCCESS;
}
#endif
