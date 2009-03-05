/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/class/opal_hash_table.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "btl_pcie.h"
#include "btl_pcie_proc.h"

static void mca_btl_pcie_proc_construct(mca_btl_pcie_proc_t* proc);
static void mca_btl_pcie_proc_destruct(mca_btl_pcie_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_pcie_proc_t, 
        opal_list_item_t, mca_btl_pcie_proc_construct, 
        mca_btl_pcie_proc_destruct);

void mca_btl_pcie_proc_construct(mca_btl_pcie_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_addr_count = 0;
    proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, opal_mutex_t);
    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_pcie_component.pcie_lock);
    opal_list_append(&mca_btl_pcie_component.pcie_procs, &proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_pcie_component.pcie_lock);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_pcie_proc_destruct(mca_btl_pcie_proc_t* proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_pcie_component.pcie_lock);
    opal_list_remove_item(&mca_btl_pcie_component.pcie_procs, &proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_pcie_component.pcie_lock);

    OBJ_DESTRUCT(&proc->proc_lock);
}


/*
 * Look for an existing TEMPLATE process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_btl_pcie_proc_t* mca_btl_pcie_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_pcie_proc_t* pcie_proc;

    OPAL_THREAD_LOCK(&mca_btl_pcie_component.pcie_lock);

    for(pcie_proc = (mca_btl_pcie_proc_t*)
            opal_list_get_first(&mca_btl_pcie_component.pcie_procs);
            pcie_proc != (mca_btl_pcie_proc_t*)
            opal_list_get_end(&mca_btl_pcie_component.pcie_procs);
            pcie_proc  = (mca_btl_pcie_proc_t*)opal_list_get_next(pcie_proc)) {

        if(pcie_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_pcie_component.pcie_lock);
            return pcie_proc;
        }

    }

    OPAL_THREAD_UNLOCK(&mca_btl_pcie_component.pcie_lock);

    return NULL;
}


/*
 * Create a TEMPLATE process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_pcie_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_pcie_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

int  mca_btl_pcie_proc_create(ompi_proc_t* ompi_proc, 
                              mca_btl_pcie_module_t* pcie_btl, 
                              mca_btl_pcie_proc_t** ret_proc)
{
    mca_btl_pcie_proc_t* pcie_proc = NULL;
    char *rem_dev_name = NULL, *lcl_dev_name = NULL;
    char *rem_hostname = NULL;
    int rc, num_peers, i;
    size_t size;
    mca_btl_pcie_modex_info_t *modex_info;

    /* Check if already have proc structure for this ompi process */
    pcie_proc = mca_btl_pcie_proc_lookup_ompi(ompi_proc);

    if(pcie_proc != NULL) {
        /* Gotcha! */
        *ret_proc = pcie_proc;
        return OMPI_SUCCESS;
    }

    /* query for the peer's device name info */ 
    rc = ompi_modex_recv(&mca_btl_pcie_component.super.btl_version, 
                         ompi_proc, 
                         (void*)&modex_info,
                         &size);
    if (OMPI_SUCCESS != rc) {
        opal_output(mca_btl_base_output, "[%s:%d] ompi_modex_recv failed for peer %s",
		    __FILE__, __LINE__, orte_util_print_name_args(&ompi_proc->proc_name));
        OBJ_RELEASE(pcie_proc);
        *ret_proc = NULL;
        return OMPI_ERROR;
    }
    
    if (0 == size || 0 != size % sizeof(mca_btl_pcie_modex_info_t)) { 
        *ret_proc = NULL;
        return OMPI_SUCCESS;
    }

    num_peers = size / sizeof(mca_btl_pcie_modex_info_t);

    for (i = 0 ; i < num_peers ; ++i) {
        MCA_BTL_PCIE_MODEX_INFO_NTOH(modex_info[i]);
        rem_hostname = modex_info[i].hostname;
        rem_dev_name = modex_info[i].devicename;
        lcl_dev_name = ompi_btl_pcie_cfg_get_matching_device(rem_hostname,
                                                             rem_dev_name);
        if (NULL != lcl_dev_name && 
            0 == strcmp(lcl_dev_name, pcie_btl->lcl_dev_name)) {
            /* we have a match.  continue onward */
            break;
        }
    }
    /* make sure the local device names match */
    if(NULL == lcl_dev_name || 
       0 != strcmp(lcl_dev_name, pcie_btl->lcl_dev_name)){ 
        *ret_proc = NULL;
        return OMPI_SUCCESS;
    }

    BTL_VERBOSE(("Have matching devices: %s:%s <-> %s:%s",
	       orte_process_info.nodename,
	       pcie_btl->lcl_dev_name,
               rem_hostname,
	       rem_dev_name));

    pcie_proc = OBJ_NEW(mca_btl_pcie_proc_t);
    if(NULL == pcie_proc){ 
        *ret_proc = NULL;
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    pcie_proc->proc_ompi = ompi_proc;

    /* build a unique identifier (of arbitrary
     * size) to represent the proc */
    pcie_proc->proc_guid = ompi_proc->proc_name;

    /* Initialize number of peer */
    pcie_proc->proc_endpoint_count = 1;

    pcie_proc->endpoint_proc = OBJ_NEW(mca_btl_pcie_endpoint_t);
    if(NULL == pcie_proc->endpoint_proc) {
        free(rem_dev_name);
        *ret_proc = NULL;
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    pcie_proc->endpoint_proc->lcl_dev_name = lcl_dev_name;
    pcie_proc->endpoint_proc->rem_dev_name = rem_dev_name;
    pcie_proc->endpoint_proc->endpoint_proc = pcie_proc;
    pcie_proc->endpoint_proc->endpoint_btl = pcie_btl;
    
    if(OMPI_SUCCESS != mca_btl_pcie_endpoint_init(pcie_proc->endpoint_proc)) { 
        BTL_ERROR(("Error initializing the PCIE endpoint \n"));
        *ret_proc = NULL;
        return OMPI_ERROR;
    }
        
    *ret_proc = pcie_proc;
    return OMPI_SUCCESS;
}


