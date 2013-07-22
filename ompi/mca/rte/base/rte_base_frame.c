/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
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


static int ompi_rte_base_close(void)
{
    return mca_base_framework_components_close(&ompi_rte_base_framework, NULL);
}

static int ompi_rte_base_open(mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return mca_base_framework_components_open(&ompi_rte_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(ompi, rte, "OMPI Run-Time Environment Interface", NULL,
                           ompi_rte_base_open, ompi_rte_base_close,
                           mca_rte_base_static_components, 0);


OBJ_CLASS_INSTANCE(ompi_namelist_t,
                   opal_list_item_t,
                   NULL, NULL);
