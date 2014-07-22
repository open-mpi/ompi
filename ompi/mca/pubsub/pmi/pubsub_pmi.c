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
    // did the pmix.init in the component
    return OMPI_SUCCESS;
}

/*
 * publish the port_name for the specified service_name.
 */
static int publish(const char *service_name, ompi_info_t *info, const char *port_name)
{
    pmix_info_t *p;
    opal_list_t xfer;
    ompi_info_entry_t *ie;
    int rc;

    /* transfer the ompi_info_t data to an array of pmix_info_t structs */
    OBJ_CONSTRUCT(&xfer, opal_list_t);
    OPAL_LIST_FOREACH(ie, &info->super, ompi_info_entry_t) {
        p = OBJ_NEW(pmix_info_t);
        strncpy(p->key, ie->ie_key, PMIX_MAX_INFO_KEY);
        strncpy(p->value, ie->ie_value, PMIX_MAX_INFO_VAL);
        opal_list_append(&xfer, &p->super);
    }

    rc = opal_pmix.publish(service_name, &xfer, port_name);
    OPAL_LIST_DESTRUCT(&xfer);
    return rc;
}

static char* lookup(const char *service_name, ompi_info_t *info)
{
    char port[PMIX_MAX_VALLEN], *ret;
    pmix_info_t *p;
    opal_list_t xfer;
    ompi_info_entry_t *ie;
    int rc;

    /* transfer the ompi_info_t data to an array of pmix_info_t structs */
    OBJ_CONSTRUCT(&xfer, opal_list_t);
    OPAL_LIST_FOREACH(ie, &info->super, ompi_info_entry_t) {
        p = OBJ_NEW(pmix_info_t);
        strncpy(p->key, ie->ie_key, PMIX_MAX_INFO_KEY);
        strncpy(p->value, ie->ie_value, PMIX_MAX_INFO_VAL);
        opal_list_append(&xfer, &p->super);
    }
    rc = opal_pmix.lookup(service_name, &xfer, port, PMIX_MAX_VALLEN);
    OPAL_LIST_DESTRUCT(&xfer);

    /* in error case port will be set to NULL
     * this is what our callers expect to see
     * In future maybe some error handling need?
     */
    if( rc != OPAL_SUCCESS ){
        // improve error processing
        return NULL;
    }
    ret = strdup(port);
    return ret;
}

/*
 * delete the entry */
static int unpublish(const char *service_name, ompi_info_t *info)
{
    pmix_info_t *p;
    opal_list_t xfer;
    ompi_info_entry_t *ie;
    int rc;

    /* transfer the ompi_info_t data to an array of pmix_info_t structs */
    OBJ_CONSTRUCT(&xfer, opal_list_t);
    OPAL_LIST_FOREACH(ie, &info->super, ompi_info_entry_t) {
        p = OBJ_NEW(pmix_info_t);
        strncpy(p->key, ie->ie_key, PMIX_MAX_INFO_KEY);
        strncpy(p->value, ie->ie_value, PMIX_MAX_INFO_VAL);
        opal_list_append(&xfer, &p->super);
    }
    rc = opal_pmix.unpublish(service_name, &xfer);
    OPAL_LIST_DESTRUCT(&xfer);
    return rc;
}


/*
 * finalize the module
 */
static int finalize(void)
{
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
