/**
 * copyright (c) 2014 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * This header defines  Quality of Service Interface for Runtime messaging
 */

/**
 * @file
 *
 * Quality of Service (QoS) Communication Interface
 *
 * The QoS layer is responsible for providing quality of service for
 * messages exchanged between two ORTE processes through the use of
 * channels.
 */
#ifndef MCA_QOS_H_
#define MCA_QOS_H_

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/qos/base/base.h"
#include "orte/mca/errmgr/errmgr.h"

BEGIN_C_DECLS
/* ******************************************************************** */
struct opal_buffer_t;
struct orte_process_name_t;


/* ******************************************************************** */
#define ORTE_QOS_INVALID_CHANNEL_NUM  0xFFFF
#define ORTE_QOS_MAX_COMPONENTS 5
typedef void (*orte_qos_callback_fn_t)(int status,
                                       int channel_num,
                                       struct orte_process_name_t* peer,
                                       void* cbdata);

typedef int (*mca_qos_base_component_start_fn_t)(void);
typedef void (*mca_qos_base_component_shutdown_fn_t)(void);

#if OPAL_ENABLE_FT_CR == 1
typedef int (*mca_qos_base_component_ft_event_fn_t)(int state);
#endif
ORTE_DECLSPEC void * orte_qos_create_channel (void *qos_mod, opal_list_t *qos_attributes, uint32_t channel_num);
ORTE_DECLSPEC int orte_qos_open_channel (void *qos_mod, void *qos_channel, opal_buffer_t * buffer);
ORTE_DECLSPEC int orte_qos_close_channel (void *qos_mod, void *qos_channel);
ORTE_DECLSPEC void orte_qos_init_recv_channel (void *qos_mod, void *qos_channel, opal_list_t *qos_attributes);
ORTE_DECLSPEC int orte_qos_cmp_channel (void *qos_mod, void *qos_channel, opal_list_t *qos_attributes);
ORTE_DECLSPEC int orte_qos_send_channel (void *qos_mod, void *qos_channel, orte_rml_send_t *msg);
ORTE_DECLSPEC int orte_qos_recv_channel (void *qos_mod, void *qos_channel, orte_rml_recv_t *msg);
/**
 * qos module (channel) create function
 * initialize type specific attributes of the channel.
 */
typedef void* (*orte_qos_base_module_create_fn_t) (opal_list_t *qos_attributes, uint32_t channel_num);

/**
 * qos module (channel) open function
 * this function is called when rml_open_channel is requested
 */
typedef int (*orte_qos_base_module_open_fn_t) (void *qos_channel,
                                                opal_buffer_t * buf);

/**
 * qos module (channel) send function
 * this function is called when rml_send_channel is requested
 */
typedef int (*orte_qos_base_module_send_fn_t) ( void * qos_channel,
                                                orte_rml_send_t *send);

/**
 * qos module (channel) recv function
 * this function is called when a message is received on a channel
 */
typedef int (*orte_qos_base_module_recv_fn_t) ( void * channel,
                                                orte_rml_recv_t *msg);
/**
 * qos module (channel) close function
 * this function is called when a message is received on a channel
 */

typedef int (*orte_qos_base_module_close_fn_t) ( void * channel);
/**
 * qos module (channel) init recv
 * this function is used to initialize a channel for receiving msgs (called in response to open_channel req from peer)
 */
typedef int (*orte_qos_base_module_init_recv_fn_t) (void * channel, opal_list_t * attributes);

/**
 * qos module (channel) compare functions
 * compares attributes of existing channel with the requested list of attributes
 */
typedef int (*orte_qos_base_module_cmp_fn_t) (void * channel, opal_list_t * attributes);

/**
 * qos module (channel) compare functions
 * compares attributes of existing channel with the requested list of attributes
 */
typedef void (*orte_qos_base_module_send_callback_fn_t) (orte_rml_send_t *msg);

/**
 *
 * the qos channel data structure
 */
typedef struct {
    orte_qos_base_module_create_fn_t             create;
    orte_qos_base_module_open_fn_t               open;
    orte_qos_base_module_send_fn_t               send;
    orte_qos_base_module_recv_fn_t               recv;
    orte_qos_base_module_close_fn_t              close;
    orte_qos_base_module_init_recv_fn_t          init_recv;
    orte_qos_base_module_cmp_fn_t                cmp;
    orte_qos_base_module_send_callback_fn_t      send_callback;
} orte_qos_module_t;

typedef enum {
    orte_qos_noop = 0,
    orte_qos_ack = 1,
    orte_qos_nack = 2,
    orte_qos_ack_nack_hybrid = 3,
    orte_qos_multipath = 4,
}orte_qos_type_t ;

typedef struct {
    mca_base_component_t                           qos_base;
    mca_qos_base_component_start_fn_t              start;
    mca_qos_base_component_shutdown_fn_t           shutdown;
    orte_qos_type_t                                type;
    orte_qos_module_t                              mod;
/*  mca_qos_base_componenet_open_channel_fn_t      open_channel;
    mca_qos_base_component_send_channel_nb_fn_t    send_channel;
    mca_qos_base_component_recv_channel_nb_fn_t    recv_channel;
    mca_qos_base_component_close_channel_fn_t      close_channel;*/
#if OPAL_ENABLE_FT_CR == 1
    mca_qos_base_component_ft_event_fn_t           ft_event;
#endif
} mca_qos_base_component_t;

/**
 * Macro for use in components that are of type oob
 */
#define MCA_QOS_BASE_VERSION_2_0_0 \
ORTE_MCA_BASE_VERSION_2_1_0 ("qos", 2, 0, 0)

END_C_DECLS

#endif
