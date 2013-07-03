/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"


#include "ompi/mca/pubsub/pubsub.h"
#include "ompi/mca/pubsub/base/base.h"

#include "ompi/mca/pubsub/base/static-components.h"

/*
 * Globals
 */
OMPI_DECLSPEC ompi_pubsub_base_module_t ompi_pubsub={
    NULL,
    ompi_pubsub_base_null_publish,
    ompi_pubsub_base_null_unpublish,
    ompi_pubsub_base_null_lookup,
    NULL
};

static int ompi_pubsub_base_close(void)
{
    /* Close the selected component */
    if( NULL != ompi_pubsub.finalize ) {
        ompi_pubsub.finalize();
    }

    return mca_base_framework_components_close(&ompi_pubsub_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
static int ompi_pubsub_base_open(mca_base_open_flag_t flags)
{
     /* Open up all available components */
    return mca_base_framework_components_open(&ompi_pubsub_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(ompi, pubsub, "OMPI Publish-Subscribe Subsystem", NULL,
                           ompi_pubsub_base_open, ompi_pubsub_base_close,
                           mca_pubsub_base_static_components, 0);
