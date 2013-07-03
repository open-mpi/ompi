/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/dss/dss_types.h"

#include "orte/mca/db/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * dbments and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "orte/mca/db/base/static-components.h"

orte_db_base_module_t orte_db;

static int
orte_db_base_close(void)
{
    if (NULL != orte_db.finalize) {
        orte_db.finalize();
    }

    return mca_base_framework_components_close(&orte_db_base_framework, NULL);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, db, NULL, NULL, NULL, orte_db_base_close,
                           mca_db_base_static_components, 0);
