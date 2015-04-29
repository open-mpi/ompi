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
 * QoS No-op Component interface
 *
 *
 *
 */

#ifndef MCA_QOS_NOOP_H
#define MCA_QOS_NOOP_H

#include "orte_config.h"
#include "orte/mca/qos/qos.h"
#include "orte/mca/qos/base/base.h"

BEGIN_C_DECLS


ORTE_MODULE_DECLSPEC extern orte_qos_component_t mca_qos_noop_component;

extern orte_qos_module_t  orte_qos_noop_module;

END_C_DECLS

#endif /* MCA_QOS_NOOP_H */
