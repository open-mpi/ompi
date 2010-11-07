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
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif

#include "opal/mca/mca.h"
#include "opal/dss/dss_types.h"

#include "orte/mca/rmcast/rmcast_types.h"

BEGIN_C_DECLS


/* ******************************************************************** */


/**
 * Function prototypes for callback from receiving multicast messages
 */
typedef void (*orte_rmcast_callback_buffer_fn_t)(int status,
                                                 orte_rmcast_channel_t channel,
                                                 orte_rmcast_seq_t seq_num,
                                                 orte_rmcast_tag_t tag,
                                                 orte_process_name_t *sender,
                                                 opal_buffer_t *buf, void* cbdata);

typedef void (*orte_rmcast_callback_fn_t)(int status,
                                          orte_rmcast_channel_t channel,
                                          orte_rmcast_seq_t seq_num,
                                          orte_rmcast_tag_t tag,
                                          orte_process_name_t *sender,
                                          struct iovec *msg, int count, void* cbdata);

/* initialize the selected module */
typedef int (*orte_rmcast_base_module_init_fn_t)(void);

/* finalize the selected module */
typedef void (*orte_rmcast_base_module_finalize_fn_t)(void);

/* send a buffered message across a multicast channel */
typedef int (*orte_rmcast_base_module_send_buffer_fn_t)(orte_rmcast_channel_t channel,
                                                        orte_rmcast_tag_t tag,
                                                        opal_buffer_t *buf);

/* non-blocking send messages from a multicast channel */
typedef int (*orte_rmcast_base_module_send_buffer_nb_fn_t)(orte_rmcast_channel_t channel,
                                                           orte_rmcast_tag_t tag,
                                                           opal_buffer_t *buf,
                                                           orte_rmcast_callback_buffer_fn_t cbfunc,
                                                           void *cbdata);

/* send iovec message across a multicast channel */
typedef int (*orte_rmcast_base_module_send_fn_t)(orte_rmcast_channel_t channel,
                                                 orte_rmcast_tag_t tag,
                                                 struct iovec *msg, int count);

/* non-blocking send iovec message across a multicast channel */
typedef int (*orte_rmcast_base_module_send_nb_fn_t)(orte_rmcast_channel_t channel,
                                                    orte_rmcast_tag_t tag,
                                                    struct iovec *msg, int count,
                                                    orte_rmcast_callback_fn_t cbfunc,
                                                    void *cbdata);

/* blocking buffer receive from a multicast channel */
typedef int (*orte_rmcast_base_module_recv_buffer_fn_t)(orte_process_name_t *sender,
                                                        orte_rmcast_channel_t channel,
                                                        orte_rmcast_tag_t tag,
                                                        orte_rmcast_seq_t *seq_num,
                                                        opal_buffer_t *buf);

/* non-blocking receive buffer messages from a multicast channel */
typedef int (*orte_rmcast_base_module_recv_buffer_nb_fn_t)(orte_rmcast_channel_t channel,
                                                           orte_rmcast_tag_t tag,
                                                           orte_rmcast_flag_t flags,
                                                           orte_rmcast_callback_buffer_fn_t cbfunc,
                                                           void *cbdata);

/* receive iovec messages from a multicast channel */
typedef int (*orte_rmcast_base_module_recv_fn_t)(orte_process_name_t *sender,
                                                 orte_rmcast_channel_t channel,
                                                 orte_rmcast_tag_t tag,
                                                 orte_rmcast_seq_t *seq_num,
                                                 struct iovec **msg, int *count);

/* non-blocking receive iovec messages from a multicast channel */
typedef int (*orte_rmcast_base_module_recv_nb_fn_t)(orte_rmcast_channel_t channel,
                                                    orte_rmcast_tag_t tag,
                                                    orte_rmcast_flag_t flags,
                                                    orte_rmcast_callback_fn_t cbfunc,
                                                    void *cbdata);

/* cancel a receive */
typedef void (*orte_rmcast_base_module_cancel_recv_fn_t)(orte_rmcast_channel_t channel,
                                                         orte_rmcast_tag_t tag);

/* open the specified channel */
typedef int (*orte_rmcast_base_module_open_channel_fn_t)(orte_rmcast_channel_t channel, char *name,
                                                         char *network, int port, char *interface, uint8_t direction);

/* close the channel */
typedef int (*orte_rmcast_base_module_close_channel_fn_t)(orte_rmcast_channel_t channel);

/* return my group's channels */
typedef int (*orte_rmcast_base_module_query_channel_fn_t)(orte_rmcast_channel_t *output,
                                                          orte_rmcast_channel_t *input);

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
    orte_rmcast_base_module_init_fn_t               init;
    orte_rmcast_base_module_finalize_fn_t           finalize;
    orte_rmcast_base_module_send_fn_t               send;
    orte_rmcast_base_module_send_nb_fn_t            send_nb;
    orte_rmcast_base_module_send_buffer_fn_t        send_buffer;
    orte_rmcast_base_module_send_buffer_nb_fn_t     send_buffer_nb;
    orte_rmcast_base_module_recv_fn_t               recv;
    orte_rmcast_base_module_recv_nb_fn_t            recv_nb;
    orte_rmcast_base_module_recv_buffer_fn_t        recv_buffer;
    orte_rmcast_base_module_recv_buffer_nb_fn_t     recv_buffer_nb;
    orte_rmcast_base_module_cancel_recv_fn_t        cancel_recv;
    orte_rmcast_base_module_open_channel_fn_t       open_channel;
    orte_rmcast_base_module_close_channel_fn_t      close_channel;
    orte_rmcast_base_module_query_channel_fn_t      query_channel;
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
