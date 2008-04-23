/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "orte/constants.h"
#include "orte/util/proc_info.h"
#include "ras_ccp.h"


/* Import the Windows CCP API. */
#import "ccpapi.tlb" named_guids no_namespace raw_interfaces_only   \
    rename("SetEnvironmentVariable","SetEnvVar")                    \
    rename("GetJob", "GetSingleJob")                                \
    rename("AddJob", "AddSingleJob")


/*
 * Local variables
 */
static int param_priority;


/*
 * Local functions
 */
static int ras_ccp_open(void);
static orte_ras_base_module_t *ras_ccp_init(int*);


orte_ras_ccp_component_t mca_ras_ccp_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */

        {
            /* Indicate that we are a ras v1.3.0 component (which also
               implies a specific MCA version) */
            
            ORTE_RAS_BASE_VERSION_2_0_0,
            
            /* Component name and version */
            
            "ccp",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            
            ras_ccp_open,
            NULL
        },
        
        /* Next the MCA v1.0.0 component meta data */
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        ras_ccp_init
    }
};


static int ras_ccp_open(void)
{
    mca_base_param_reg_int(&mca_ras_ccp_component.super.ras_version,
                           "priority",
                           "Priority of the ccp ras component",
                           false, false, 13, 
                           &mca_ras_ccp_component.priority);
    
    return ORTE_SUCCESS;
}


static orte_ras_base_module_t *ras_ccp_init(int* priority)
{
    int rc;
    ICluster* pCluster = NULL;
    HRESULT hr = S_OK;

    /* CCP is not thread safe. Use the apartment model. */
    CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);

    /* Try to create the Cluster object. */
    hr = CoCreateInstance( __uuidof(Cluster),
                           NULL,
                           CLSCTX_INPROC_SERVER,
                           __uuidof(ICluster),
                           reinterpret_cast<void **> (&pCluster) );
    if (FAILED(hr)) {
        /* We are not Windows clusters, don't select us.*/
        return NULL;
    }

    /* if we are NOT an HNP, then don't select us */
    if (!orte_process_info.hnp) {
        pCluster->Release();
        return NULL;
    }

    /* We are Windows clusters and this is HNP. */
    pCluster->Release();  
    *priority = mca_ras_ccp_component.priority;
    return &orte_ras_ccp_module;
}
