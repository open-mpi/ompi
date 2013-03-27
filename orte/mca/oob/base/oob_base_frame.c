/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
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


#include "orte/mca/oob/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/oob/base/static-components.h"

/*
 * Global variables
 */
mca_oob_t mca_oob;

static int orte_oob_base_close(void)
{
    if (NULL != mca_oob.oob_fini) {
        mca_oob.oob_fini();
    }

    return mca_base_framework_components_close(&orte_oob_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
static int orte_oob_base_open(mca_base_open_flag_t flags)
{
     /* Open up all available components */
    return mca_base_framework_components_open(&orte_oob_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, oob, NULL, NULL, orte_oob_base_open, orte_oob_base_close,
                           mca_oob_base_static_components, 0);
