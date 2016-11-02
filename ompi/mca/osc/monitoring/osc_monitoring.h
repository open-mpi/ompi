/*
 * Copyright (c) 2016-2017 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_H
#define MCA_OSC_MONITORING_H

BEGIN_C_DECLS

#include <ompi_config.h>
#include <ompi/mca/osc/osc.h>
#include <ompi/mca/common/monitoring/common_monitoring.h>

struct ompi_osc_monitoring_component_t {
    ompi_osc_base_component_t super;
    int priority;
};
typedef struct ompi_osc_monitoring_component_t ompi_osc_monitoring_component_t;

OMPI_DECLSPEC extern ompi_osc_monitoring_component_t mca_osc_monitoring_component;

END_C_DECLS

#endif  /* MCA_OSC_MONITORING_H */
