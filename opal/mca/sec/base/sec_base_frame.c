/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
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
#include "opal/dss/dss_types.h"

#include "opal/mca/sec/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * secments and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "opal/mca/sec/base/static-components.h"

opal_sec_base_module_t opal_sec;

static int opal_sec_base_close(void)
{
    /* let the selected module finalize */
    if (NULL != opal_sec.finalize) {
            opal_sec.finalize();
    }

    return mca_base_framework_components_close(&opal_sec_base_framework, NULL);
}

static int opal_sec_base_open(mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return mca_base_framework_components_open(&opal_sec_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(opal, sec, NULL, NULL, opal_sec_base_open, opal_sec_base_close,
                           mca_sec_base_static_components, 0);
