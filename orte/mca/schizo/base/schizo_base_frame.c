/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/mca.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/schizo/base/base.h"
/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/schizo/base/static-components.h"

/*
 * Global variables
 */
orte_schizo_base_t orte_schizo_base;
orte_schizo_base_module_t orte_schizo = {
    orte_schizo_base_parse_cli,
    orte_schizo_base_parse_env,
    orte_schizo_base_setup_fork,
    orte_schizo_base_setup_child
};

static int orte_schizo_base_close(void)
{
    /* cleanup globals */
    OPAL_LIST_DESTRUCT(&orte_schizo_base.active_modules);

    return mca_base_framework_components_close(&orte_schizo_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int orte_schizo_base_open(mca_base_open_flag_t flags)
{
    int rc;

    /* init the globals */
    OBJ_CONSTRUCT(&orte_schizo_base.active_modules, opal_list_t);

    /* Open up all available components */
    rc = mca_base_framework_components_open(&orte_schizo_base_framework, flags);

    /* All done */
    return rc;
}

MCA_BASE_FRAMEWORK_DECLARE(orte, schizo, "ORTE Schizo Subsystem",
                           NULL, orte_schizo_base_open, orte_schizo_base_close,
                           mca_schizo_base_static_components, 0);

OBJ_CLASS_INSTANCE(orte_schizo_base_active_module_t,
                   opal_list_item_t,
                   NULL, NULL);

