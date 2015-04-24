/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * QoS Framework maintenence interface
 *
 *
 *
 */

#ifndef MCA_QOS_BASE_H
#define MCA_QOS_BASE_H

#include "orte_config.h"
#include "orte/mca/qos/qos.h"
#include "orte/mca/rml/base/base.h"
#include "opal/class/opal_list.h"


/*
 * MCA Framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_qos_base_framework;
/* select a component */
ORTE_DECLSPEC int orte_qos_base_select(void);

/* a global struct containing framework-level values */
typedef struct {
    opal_list_t open_channels;
    opal_pointer_array_t actives;
#if OPAL_ENABLE_TIMING
    bool timing;
#endif
} orte_qos_base_t;
ORTE_DECLSPEC extern orte_qos_base_t orte_qos_base;

#define ORTE_QOS_MAX_WINDOW_SIZE 1000

typedef struct orte_qos_base_channel {
    opal_list_item_t super;
    uint32_t channel_num;
    opal_list_t attributes;
} orte_qos_base_channel_t;
OBJ_CLASS_DECLARATION(orte_qos_base_channel_t);

/* common implementations */
ORTE_DECLSPEC void* orte_qos_get_module ( opal_list_t *qos_attributes);
int orte_qos_base_pack_attributes (opal_buffer_t * buffer, opal_list_t * qos_attributes);

#define ORTE_QOS_SEND_COMPLETE(m)                                       \
 do {                                                                   \
    orte_qos_module_t *mod;                                             \
    opal_output_verbose(5, orte_qos_base_framework.framework_output,    \
                        "%s-%s Send message complete at %s:%d",         \
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),             \
                        ORTE_NAME_PRINT(&((m)->dst)),                   \
                        __FILE__, __LINE__);                            \
    mod = (orte_qos_module_t*) m->channel->qos;                         \
    if (NULL != mod)                                                    \
        mod->send_callback(m);                                          \
    else                                                                \
        ORTE_RML_SEND_COMPLETE(m);                                      \
 } while(0);

END_C_DECLS

#endif /* MCA_QOS_BASE_H */
