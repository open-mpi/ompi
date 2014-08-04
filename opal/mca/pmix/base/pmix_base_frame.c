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

#include "opal/mca/pmix/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * components and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "opal/mca/pmix/base/static-components.h"

opal_pmix_base_module_t opal_pmix;
bool opal_pmix_use_collective = false;

static int opal_pmix_base_frame_register(mca_base_register_flag_t flags)
{
    opal_pmix_use_collective = false;
    (void)mca_base_var_register("opal", "pmix", "base", "direct_modex",
                                "Default to direct modex (default: true)",
                                MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                OPAL_INFO_LVL_9,
                                MCA_BASE_VAR_SCOPE_READONLY,
                                &opal_pmix_use_collective);
    return OPAL_SUCCESS;
}

static int opal_pmix_base_frame_close(void)
{
   /* let the active component to finalize, should it wish to do so */
    if (NULL != opal_pmix.finalize) {
        opal_pmix.finalize();
    }

    return mca_base_framework_components_close(&opal_pmix_base_framework, NULL);
}

static int opal_pmix_base_frame_open(mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return mca_base_framework_components_open(&opal_pmix_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(opal, pmix, "OPAL PMI Client Framework",
                           opal_pmix_base_frame_register,
                           opal_pmix_base_frame_open,
                           opal_pmix_base_frame_close,
                           mca_pmix_base_static_components, 0);

OBJ_CLASS_INSTANCE(pmix_info_t,
                   opal_list_item_t,
                   NULL, NULL);

static void acon(opal_pmix_attr_t *p)
{
    p->attr = PMIX_ATTR_UNDEF;
    p->scope = PMIX_SCOPE_UNDEF;
    OBJ_CONSTRUCT(&p->value, opal_value_t);
}
static void ades(opal_pmix_attr_t *p)
{
    OBJ_DESTRUCT(&p->value);
}
OBJ_CLASS_INSTANCE(opal_pmix_attr_t,
                   opal_list_item_t,
                   acon, ades);
