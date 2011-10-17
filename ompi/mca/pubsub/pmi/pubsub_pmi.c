/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include <pmi.h>

#include "ompi/info/info.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "ompi/mca/pubsub/base/base.h"
#include "pubsub_pmi.h"

static char* pmi_error(int pmi_err);
#define ORTE_PMI_ERROR(pmi_err, pmi_func)                               \
    do {                                                                \
        opal_output(0, "%s[%s:%d:%s] %s: %s\n",                         \
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),                 \
                    __FILE__, __LINE__, __func__,                       \
                    pmi_func, pmi_error(pmi_err));                      \
    } while(0);

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
static int publish ( char *service_name, ompi_info_t *info, char *port_name )
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI_Publish_name(service_name, port_name))) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Publish_name");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static char* lookup ( char *service_name, ompi_info_t *info )
{
    char *port=NULL;
    int rc;

    if (PMI_SUCCESS != (rc = PMI_Lookup_name(service_name, port))) {
        ORTE_PMI_ERROR(rc, "PMI_Lookup_name");
        return NULL;
    }

    return port;
}

/*
 * delete the entry */
static int unpublish ( char *service_name, ompi_info_t *info )
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI_Unpublish_name(service_name))) {
        ORTE_PMI_ERROR(rc, "PMI_Unpublish_name");
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;;
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


/* useful util */
static char* pmi_error(int pmi_err)
{
    char * err_msg;

    switch(pmi_err) {
        case PMI_FAIL: err_msg = "Operation failed"; break;
        case PMI_ERR_INIT: err_msg = "PMI is not initialized"; break;
        case PMI_ERR_NOMEM: err_msg = "Input buffer not large enough"; break;
        case PMI_ERR_INVALID_ARG: err_msg = "Invalid argument"; break;
        case PMI_ERR_INVALID_KEY: err_msg = "Invalid key argument"; break;
        case PMI_ERR_INVALID_KEY_LENGTH: err_msg = "Invalid key length argument"; break;
        case PMI_ERR_INVALID_VAL: err_msg = "Invalid value argument"; break;
        case PMI_ERR_INVALID_VAL_LENGTH: err_msg = "Invalid value length argument"; break;
        case PMI_ERR_INVALID_LENGTH: err_msg = "Invalid length argument"; break;
        case PMI_ERR_INVALID_NUM_ARGS: err_msg = "Invalid number of arguments"; break;
        case PMI_ERR_INVALID_ARGS: err_msg = "Invalid args argument"; break;
        case PMI_ERR_INVALID_NUM_PARSED: err_msg = "Invalid num_parsed length argument"; break;
        case PMI_ERR_INVALID_KEYVALP: err_msg = "Invalid invalid keyvalp atgument"; break;
        case PMI_ERR_INVALID_SIZE: err_msg = "Invalid size argument"; break;
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
