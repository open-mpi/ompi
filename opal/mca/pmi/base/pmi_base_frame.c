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

#include "opal/mca/pmi/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * components and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "opal/mca/pmi/base/static-components.h"

opal_pmi_base_module_t opal_pmi;

static int opal_pmi_base_frame_close(void)
{
   /* let the active component to finalize, should it wish to do so */
    if (NULL != opal_pmi.finalize) {
        opal_pmi.finalize();
    }

    return mca_base_framework_components_close(&opal_pmi_base_framework, NULL);
}

static int opal_pmi_base_frame_open(mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return mca_base_framework_components_open(&opal_pmi_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(opal, pmi, NULL, NULL,
                           opal_pmi_base_frame_open,
                           opal_pmi_base_frame_close,
                           mca_pmi_base_static_components, 0);

