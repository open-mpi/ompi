/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** 
 * @file
 *
 * Reliable Multicast Framework.
 */


#ifndef ORTE_MCA_RMCAST_H_
#define ORTE_MCA_RMCAST_H_

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/mca.h"
#include "opal/dss/dss_types.h"

#include "orte/mca/rmcast/rmcast_types.h"

BEGIN_C_DECLS


/* ******************************************************************** */


/**
 * Function prototype for callback from receiving multicast messages
 */
typedef void (*orte_rmcast_callback_fn_t)(int channel, opal_buffer_t *buf, void* cbdata);

/* initialize the selected module */
typedef int (*orte_rmcast_base_module_init_fn_t)(void);

/* finalize the selected module */
typedef void (*orte_rmcast_base_module_finalize_fn_t)(void);

/* send a buffered message across a multicast channel */
typedef int (*orte_rmcast_base_module_send_fn_t)(unsigned int channel,
                                                 orte_rmcast_tag_t tag,
                                                 opal_buffer_t *buf);

/* non-blocking send messages from a multicast channel */
typedef int (*orte_rmcast_base_module_send_nb_fn_t)(unsigned int channel,
                                                    orte_rmcast_tag_t tag,
                                                    opal_buffer_t *buf,
                                                    orte_rmcast_callback_fn_t cbfunc,
                                                    void *cbdata);

/* non-blocking receive messages from a multicast channel */
typedef int (*orte_rmcast_base_module_recv_nb_fn_t)(unsigned int channel,
                                                    orte_rmcast_flag_t flags,
                                                    orte_rmcast_tag_t tag,
                                                    orte_rmcast_callback_fn_t cbfunc, void *cbdata);

/* blocking receive from a multicast channel */
typedef int (*orte_rmcast_base_module_recv_fn_t)(unsigned int channel,
                                                 orte_rmcast_tag_t tag,
                                                 opal_buffer_t *buf);

/* cancel a receive */
typedef void (*orte_rmcast_base_module_cancel_recv_fn_t)(unsigned int channel,
                                                         orte_rmcast_tag_t tag);

/* get the next available channel */
typedef unsigned int (*orte_rmcast_base_module_get_rmcast_channel_fn_t)(char *name, uint8_t direction);

/*
 * rmcast component
 */
struct orte_rmcast_base_component_1_0_0_t {
    /** component version */
    mca_base_component_t version;
    /** component data */
    mca_base_component_data_t base_data;
};
/** Convenience typedef */
typedef struct orte_rmcast_base_component_1_0_0_t orte_rmcast_base_component_1_0_0_t;
/** Convenience typedef */
typedef orte_rmcast_base_component_1_0_0_t orte_rmcast_base_component_t;

/*
 * Component modules Ver 1.0
 */
struct orte_rmcast_base_module_t {
    orte_rmcast_base_module_init_fn_t                   init;
    orte_rmcast_base_module_finalize_fn_t               finalize;
    orte_rmcast_base_module_send_fn_t                   send;
    orte_rmcast_base_module_send_nb_fn_t                send_nb;
    orte_rmcast_base_module_recv_fn_t                   recv;
    orte_rmcast_base_module_recv_nb_fn_t                recv_nb;
    orte_rmcast_base_module_cancel_recv_fn_t            cancel_recv;
    orte_rmcast_base_module_get_rmcast_channel_fn_t     get_channel;
};
/** Convienence typedef */
typedef struct orte_rmcast_base_module_t orte_rmcast_module_t;

/** Interface for RMCAST communication */
ORTE_DECLSPEC extern orte_rmcast_module_t orte_rmcast;


/* ******************************************************************** */


/** Macro for use in components that are of type rmcast */
#define ORTE_RMCAST_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "rmcast", 1, 0, 0


/* ******************************************************************** */


END_C_DECLS

#endif
