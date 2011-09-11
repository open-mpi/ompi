/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/hwloc/base/static-components.h"


/*
 * Globals
 */
int opal_hwloc_base_output = -1;
opal_list_t opal_hwloc_components;
bool opal_hwloc_base_inited = false;
#if OPAL_HAVE_HWLOC
hwloc_topology_t opal_hwloc_topology=NULL;
#endif

int opal_hwloc_base_open(void)
{
    if (opal_hwloc_base_inited) {
        return OPAL_SUCCESS;
    }
    opal_hwloc_base_inited = true;

#if OPAL_HAVE_HWLOC
    {
        int value;
        opal_data_type_t tmp;

        /* Debugging / verbose output */
        mca_base_param_reg_int_name("hwloc", "base_verbose", 
                                    "Verbosity level of the hwloc framework",
                                    false, false,
                                    0, &value);
        if (0 != value) {
            opal_hwloc_base_output = opal_output_open(NULL);
        } else {
            opal_hwloc_base_output = -1;
        }

        /* to support tools such as ompi_info, add the components
         * to a list
         */
        OBJ_CONSTRUCT(&opal_hwloc_components, opal_list_t);
        if (OPAL_SUCCESS !=
            mca_base_components_open("hwloc", opal_hwloc_base_output,
                                     mca_hwloc_base_static_components,
                                     &opal_hwloc_components, true)) {
            return OPAL_ERROR;
        }

        /* declare the hwloc data types */
        tmp = OPAL_HWLOC_TOPO;
        if (OPAL_SUCCESS != (value = opal_dss.register_type(opal_hwloc_pack,
                                                            opal_hwloc_unpack,
                                                            (opal_dss_copy_fn_t)opal_hwloc_copy,
                                                            (opal_dss_compare_fn_t)opal_hwloc_compare,
                                                            (opal_dss_size_fn_t)opal_hwloc_size,
                                                            (opal_dss_print_fn_t)opal_hwloc_print,
                                                            (opal_dss_release_fn_t)opal_hwloc_release,
                                                            OPAL_DSS_STRUCTURED,
                                                            "OPAL_HWLOC_TOPO", &tmp))) {
            return value;
        }
    }
#endif

    return OPAL_SUCCESS;
}
