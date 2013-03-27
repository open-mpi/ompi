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
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/output.h"

int opal_hwloc_base_close(void);

int opal_hwloc_base_close(void)
{
    if (!opal_hwloc_base_inited) {
        return OPAL_SUCCESS;
    }

#if OPAL_HAVE_HWLOC
    {
        int ret;

        /* no need to close the component as it was statically opened */

        /* for support of tools such as ompi_info */
        ret = mca_base_framework_components_close (&opal_hwloc_base_framework, NULL);
        if (OPAL_SUCCESS != ret) {
            return ret;
        }

        /* free memory */
        if (NULL != opal_hwloc_my_cpuset) {
            hwloc_bitmap_free(opal_hwloc_my_cpuset);
            opal_hwloc_my_cpuset = NULL;
        }
    }
#endif

    /* All done */
    opal_hwloc_base_inited = false;
    return OPAL_SUCCESS;
}
