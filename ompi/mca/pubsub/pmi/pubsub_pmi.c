/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"


#include "opal/mca/pmix/pmix.h"

#include "ompi/info/info.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/mca/pubsub/base/base.h"
#include "pubsub_pmi.h"

/*
 * Init the module
 */
static int init(void)
{    
    return OMPI_SUCCESS;
}

/*
 * publish the port_name for the specified service_name.
 */
static int publish ( const char *service_name, ompi_info_t *info, const char *port_name )
{
    return opal_pmix.publish(service_name,port_name);
}

static char* lookup ( const char *service_name, ompi_info_t *info )
{
    char *port=NULL;
    int rc = opal_pmix.lookup(service_name, &port);
    /* in error case port will be set to NULL
     * this is what our callers expect to see
     * In future maybe som error handling need?
     */
    if( rc != OPAL_SUCCESS ){
        // improove error processing
        return port; // NULL ?
    }
    return port;
}

/*
 * delete the entry */
static int unpublish ( const char *service_name, ompi_info_t *info )
{
    return opal_pmix.unpublish(service_name);
}


/*
 * finalize the module
 */
static int finalize(void)
{
    opal_pmix.finalize();
    return OMPI_SUCCESS;
}

/*
 * instantiate the module
 */
ompi_pubsub_base_module_t ompi_pubsub_pmi_module = {
    init,
    publish,
    unpublish,
    lookup,
    finalize
};
