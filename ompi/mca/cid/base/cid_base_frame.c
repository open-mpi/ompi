/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNIST_H */
#include "ompi/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"


#include "ompi/constants.h"
#include "ompi/mca/cid/cid.h"
#include "ompi/mca/cid/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "ompi/mca/cid/base/static-components.h"

/*
 * Global variables
 */
ompi_cid_base_module_t *ompi_cid = NULL;

static int ompi_cid_base_close(void)
{
    /* give the selected module a chance to finalize */
    if (NULL != ompi_cid->finalize) {
        ompi_cid->finalize();
    }
    return mca_base_framework_components_close(&ompi_cid_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int ompi_cid_base_open(mca_base_open_flag_t flags)
{
    int rc;

    /* Open up all available components */
    rc = mca_base_framework_components_open(&ompi_cid_base_framework, flags);

    /* All done */
    return rc;
}

MCA_BASE_FRAMEWORK_DECLARE(ompi, cid, "OMPI CID", NULL,
                           ompi_cid_base_open, ompi_cid_base_close,
                           mca_cid_base_static_components, 0);
