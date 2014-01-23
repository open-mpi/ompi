/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/hwloc/hwloc.h"
#include "opal/util/os_dirpath.h"

#include "orte/util/show_help.h"

#include "orte/mca/sensor/base/base.h"
#include "sensor_pwr.h"

/*
 * Local functions
 */

static int orte_sensor_pwr_open(void);
static int orte_sensor_pwr_close(void);
static int orte_sensor_pwr_query(mca_base_module_t **module, int *priority);
static int pwr_component_register(void);
static int check_cpu_type(void);

orte_sensor_pwr_component_t mca_sensor_pwr_component = {
    {
        {
            ORTE_SENSOR_BASE_VERSION_1_0_0,
            
            "pwr", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_sensor_pwr_open,  /* component open  */
            orte_sensor_pwr_close, /* component close */
            orte_sensor_pwr_query,  /* component query */
            pwr_component_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        "pwr"  // data being sensed
    }
};

/**
  * component open/close/init function
  */
static int orte_sensor_pwr_open(void)
{
    return ORTE_SUCCESS;
}

static int orte_sensor_pwr_query(mca_base_module_t **module, int *priority)
{
    /* we only handle certain cpu types as we have to know the binary
     * layout of the msr file
     */
    if (ORTE_SUCCESS != check_cpu_type()) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }

    *priority = 50;  /* ahead of heartbeat */
    *module = (mca_base_module_t *)&orte_sensor_pwr_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_sensor_pwr_close(void)
{
    return ORTE_SUCCESS;
}

static int pwr_component_register(void)
{
    mca_base_component_t *c = &mca_sensor_pwr_component.super.base_version;

     mca_sensor_pwr_component.test = false;
    (void) mca_base_component_var_register (c, "test",
                                            "Generate and pass test vector",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            & mca_sensor_pwr_component.test);
    return ORTE_SUCCESS;
}

/* list of supported chipsets */
#define CPU_SANDYBRIDGE		42
#define CPU_SANDYBRIDGE_EP	45
#define CPU_IVYBRIDGE		58
#define CPU_IVYBRIDGE_EP	62
#define CPU_HASWELL		60


/* go thru our topology and check the sockets
 * to see if they contain a match - at this time,
 * we don't support hetero sockets, so any mismatch
 * will disqualify us
 */ 
static int check_cpu_type(void)
{
    hwloc_obj_t obj;
    unsigned k;

    if (NULL == (obj = hwloc_get_obj_by_type(opal_hwloc_topology, HWLOC_OBJ_SOCKET, 0))) {
        /* there are no sockets identified in this machine */
        orte_show_help("help-orte-sensor-pwr.txt", "no-sockets", true);
        return ORTE_ERROR;
    }

    while (NULL != obj) {
        for (k=0; k < obj->infos_count; k++) {
            if (0 == strcmp(obj->infos[k].name, "model") &&
                NULL != obj->infos[k].value) {
                mca_sensor_pwr_component.model = strtoul(obj->infos[k].value, NULL, 10);
                
                switch (mca_sensor_pwr_component.model) {
		case CPU_SANDYBRIDGE:
                    opal_output_verbose(2, orte_sensor_base_framework.framework_output,
                                        "sensor:pwr Found Sandybridge CPU");
                    return ORTE_SUCCESS;
		case CPU_SANDYBRIDGE_EP:
                    opal_output_verbose(2, orte_sensor_base_framework.framework_output,
                                        "sensor:pwr Found Sandybridge-EP CPU");
                    return ORTE_SUCCESS;
		case CPU_IVYBRIDGE:
                    opal_output_verbose(2, orte_sensor_base_framework.framework_output,
                                        "sensor:pwr Found Ivybridge CPU");
                    return ORTE_SUCCESS;
		case CPU_IVYBRIDGE_EP:
                    opal_output_verbose(2, orte_sensor_base_framework.framework_output,
                                        "sensor:pwr Found Ivybridge-EP CPU");
                    return ORTE_SUCCESS;
		case CPU_HASWELL:
                    opal_output_verbose(2, orte_sensor_base_framework.framework_output,
                                        "sensor:pwr Found Haswell CPU");
                    return ORTE_SUCCESS;
		default:
                    orte_show_help("help-orte-sensor-pwr.txt", "unsupported-model",
                                   true, mca_sensor_pwr_component.model);
                    return ORTE_ERROR;
                }
            }
        }
        obj = obj->next_sibling;
    }
    orte_show_help("help-orte-sensor-pwr.txt", "no-topo-info",
                   true, mca_sensor_pwr_component.model);
    return ORTE_ERROR;
}
