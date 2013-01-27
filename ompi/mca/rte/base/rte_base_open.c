/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "ompi/constants.h"

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "ompi/mca/rte/rte.h"
#include "ompi/mca/rte/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "ompi/mca/rte/base/static-components.h"


/*
 * Globals
 */
int ompi_rte_base_output = -1;
opal_list_t ompi_rte_components;
int ompi_rte_base_inited = 0;

int ompi_rte_base_open(void)
{
    int value, rc = OMPI_SUCCESS;

    if( ompi_rte_base_inited++ < 0 ) {
        return OMPI_SUCCESS;
    }

    /* Debugging / verbose output */
    mca_base_param_reg_int_name("rte", "base_verbose", 
                                "Verbosity level of the rte framework",
                                false, false,
                                0, &value);
    if (0 != value) {
        ompi_rte_base_output = opal_output_open(NULL);
    } else {
        ompi_rte_base_output = -1;
    }

    /* to support tools such as ompi_info, add the components
     * to a list
     */
    OBJ_CONSTRUCT(&ompi_rte_components, opal_list_t);
    if (OPAL_SUCCESS !=
        mca_base_components_open("rte", ompi_rte_base_output,
                                 mca_rte_base_static_components,
                                 &ompi_rte_components, true)) {
        return OMPI_ERROR;
    }

    return rc;
}


OBJ_CLASS_INSTANCE(ompi_namelist_t,
                   opal_list_item_t,
                   NULL, NULL);
