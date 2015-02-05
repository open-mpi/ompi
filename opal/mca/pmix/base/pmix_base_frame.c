/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "opal/mca/pmix/pmix.h"
#include "opal/mca/pmix/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * components and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "opal/mca/pmix/base/static-components.h"

/* Note that this initializer is important -- do not remove it!  See
   https://github.com/open-mpi/ompi/issues/375 for details. */
opal_pmix_base_module_t opal_pmix = { 0 };
bool opal_pmix_use_collective = false;
bool opal_pmix_base_allow_delayed_server = false;

static int opal_pmix_base_frame_register(mca_base_register_flag_t flags)
{
    return OPAL_SUCCESS;
}

static int opal_pmix_base_frame_close(void)
{
    int rc;
    
    rc = mca_base_framework_components_close(&opal_pmix_base_framework, NULL);
    /* reset the opal_pmix function pointers to NULL */
    memset(&opal_pmix, 0, sizeof(opal_pmix));
    return rc;
}

static int opal_pmix_base_frame_open(mca_base_open_flag_t flags)
{
    int rc;
    
    /* Open up all available components */
    rc = mca_base_framework_components_open(&opal_pmix_base_framework, flags);
    /* ensure the function pointers are NULL */
    memset(&opal_pmix, 0, sizeof(opal_pmix));
    return rc;
}

MCA_BASE_FRAMEWORK_DECLARE(opal, pmix, "OPAL PMI Client Framework",
                           opal_pmix_base_frame_register,
                           opal_pmix_base_frame_open,
                           opal_pmix_base_frame_close,
                           mca_pmix_base_static_components, 0);

OBJ_CLASS_INSTANCE(pmix_info_t,
                   opal_list_item_t,
                   NULL, NULL);

