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

static int ompi_rte_base_verbose;

static int ompi_rte_base_register(int flags)
{
    /* Debugging / verbose output */
    ompi_rte_base_verbose = 0;
    (void) mca_base_var_register("ompi", "rte", "base", "verbose",
                                 "Verbosity level of the rte framework",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                 MCA_BASE_VAR_FLAG_SETTABLE,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_LOCAL,
                                 &ompi_rte_base_verbose);

    return OMPI_SUCCESS;
}

int ompi_rte_base_open(void)
{
    int rc = OMPI_SUCCESS;

    if( ompi_rte_base_inited++ < 0 ) {
        return OMPI_SUCCESS;
    }

    (void) ompi_rte_base_register(0);

    if (0 != ompi_rte_base_verbose) {
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
