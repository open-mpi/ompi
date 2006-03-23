/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"

#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/udapl/mpool_udapl.h"
#include "btl_udapl.h"
#include "btl_udapl_frag.h"
#include "btl_udapl_endpoint.h" 
#include "btl_udapl_proc.h" 
#include "ompi/mca/btl/base/base.h" 
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/datatype/convertor.h" 
#include "btl_udapl_endpoint.h"
#include "orte/util/proc_info.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"


/*
 * Local functions
 */

static int mca_btl_udapl_finish_connect(mca_btl_udapl_module_t* btl,
                                        mca_btl_udapl_frag_t* frag,
                                        DAT_EP_HANDLE endpoint);

mca_btl_udapl_component_t mca_btl_udapl_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_0,

            "udapl", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_udapl_component_open,  /* component open */
            mca_btl_udapl_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
        },

        mca_btl_udapl_component_init,  
        mca_btl_udapl_component_progress,
    }
};


/**
  * Report a uDAPL error - for debugging
  */

void
mca_btl_udapl_error(DAT_RETURN ret, char* str)
{
    char* major;
    char* minor;

    /* don't output anything if debug is not set */
    if(0 == mca_btl_udapl_component.udapl_debug) {
        return;
    }

    if(DAT_SUCCESS != dat_strerror(ret,
            (const char**)&major, (const char**)&minor))
    {
        printf("dat_strerror failed! ret is %d\n", ret);
        exit(-1);
    }

    OPAL_OUTPUT((0, "ERROR: %s %s %s\n", str, major, minor));
}


/*
 * Utility routines for parameter registration
 */

static inline char* mca_btl_udapl_param_register_string(
                                                     const char* param_name, 
                                                     const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl","udapl",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_udapl_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("btl","udapl",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_udapl_component_open(void)
{  
    int param, value;

    OPAL_OUTPUT((0, "udapl_component_open\n"));

    /* initialize state */
    mca_btl_udapl_component.udapl_num_btls=0;
    mca_btl_udapl_component.udapl_btls=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_udapl_component.udapl_procs, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_udapl_component.udapl_lock, opal_mutex_t);

    /* register uDAPL component parameters */
    mca_btl_udapl_component.udapl_free_list_num =
        mca_btl_udapl_param_register_int("free_list_num", 8);
    mca_btl_udapl_component.udapl_free_list_max =
        mca_btl_udapl_param_register_int("free_list_max", -1);
    mca_btl_udapl_component.udapl_free_list_inc =
        mca_btl_udapl_param_register_int("free_list_inc", 8);
    mca_btl_udapl_component.udapl_debug = 
        mca_btl_udapl_param_register_int("debug", 1); 
    mca_btl_udapl_component.udapl_mpool_name =
        mca_btl_udapl_param_register_string("mpool", "udapl");
    mca_btl_udapl_component.udapl_max_btls = 
        mca_btl_udapl_param_register_int("max_modules", 4);
    mca_btl_udapl_component.udapl_evd_qlen =
        mca_btl_udapl_param_register_int("evd_qlen", 8);
    mca_btl_udapl_component.udapl_num_repost = 
        mca_btl_udapl_param_register_int("num_repost", 4);
    mca_btl_udapl_component.udapl_num_mru = 
        mca_btl_udapl_param_register_int("num_mru", 64);
    mca_btl_udapl_component.udapl_port_low = 
        mca_btl_udapl_param_register_int("port_low", 45000);
    mca_btl_udapl_component.udapl_port_high = 
        mca_btl_udapl_param_register_int("port_high", 47000);
    mca_btl_udapl_component.udapl_timeout = 
        mca_btl_udapl_param_register_int("timeout", 10000000);

    /* register uDAPL module parameters */
    mca_btl_udapl_module.super.btl_exclusivity =
        mca_btl_udapl_param_register_int ("exclusivity", MCA_BTL_EXCLUSIVITY_DEFAULT - 10);
    mca_btl_udapl_module.super.btl_eager_limit = 
        mca_btl_udapl_param_register_int ("eager_limit", 32*1024);
    mca_btl_udapl_module.super.btl_min_send_size =
        mca_btl_udapl_param_register_int ("min_send_size", 32*1024);
    mca_btl_udapl_module.super.btl_max_send_size =
        mca_btl_udapl_param_register_int ("max_send_size", 64*1024);
    mca_btl_udapl_module.super.btl_min_rdma_size = 
        mca_btl_udapl_param_register_int("min_rdma_size", 512*1024); 
    mca_btl_udapl_module.super.btl_max_rdma_size = 
        mca_btl_udapl_param_register_int("max_rdma_size", 128*1024);
    mca_btl_udapl_module.super.btl_bandwidth  = 
        mca_btl_udapl_param_register_int("bandwidth", 225); 

    /* cmpute udapl_eager_frag_size and udapl_max_frag_size */
    mca_btl_udapl_component.udapl_eager_frag_size =
            mca_btl_udapl_module.super.btl_eager_limit;
    mca_btl_udapl_component.udapl_max_frag_size =
            mca_btl_udapl_module.super.btl_max_send_size;

    /* leave pinned option */
    value = 0;
    param = mca_base_param_find("mpi", NULL, "leave_pinned");
    mca_base_param_lookup_int(param, &value);
    mca_btl_udapl_component.leave_pinned = value;
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_udapl_component_close(void)
{
    OPAL_OUTPUT((0, "udapl_component_close\n"));

    /* TODO - what needs to be done here? */
    return OMPI_SUCCESS;
}


/*
 *  Register uDAPL component addressing information. The MCA framework
 *  will make this available to all peers.
 */

static int
mca_btl_udapl_modex_send(void)
{
    int         rc;
    size_t      i;
    size_t      size;
    mca_btl_udapl_addr_t *addrs = NULL;

    size = sizeof(mca_btl_udapl_addr_t) *
            mca_btl_udapl_component.udapl_num_btls;

    if (0 != size) {
        addrs = (mca_btl_udapl_addr_t *)malloc(size);
        if (NULL == addrs) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for (i = 0; i < mca_btl_udapl_component.udapl_num_btls; i++) {
            mca_btl_udapl_module_t *btl = mca_btl_udapl_component.udapl_btls[i];
            addrs[i] = btl->udapl_addr;
        }
    }
    rc = mca_pml_base_modex_send (&mca_btl_udapl_component.super.btl_version, addrs, size);
    if (NULL != addrs) {
        free (addrs);
    }
    return rc;
}


/*
 * Initialize the uDAPL component,
 * check how many interfaces are available and create a btl module for each.
 */

mca_btl_base_module_t **
mca_btl_udapl_component_init (int *num_btl_modules,
                           bool enable_progress_threads,
                           bool enable_mpi_threads)
{
    DAT_PROVIDER_INFO* datinfo;
    mca_btl_base_module_t **btls;
    mca_btl_udapl_module_t *btl;
    DAT_COUNT num_ias;
    int32_t i;

    OPAL_OUTPUT((0, "udapl_component_init\n"));

    /* enumerate uDAPL interfaces */
    datinfo = malloc(sizeof(DAT_PROVIDER_INFO) *
            mca_btl_udapl_component.udapl_max_btls);
    if(NULL == datinfo) {
        return NULL;
    }
    if(DAT_SUCCESS != dat_registry_list_providers(
            mca_btl_udapl_component.udapl_max_btls,
            (DAT_COUNT*)&num_ias, &datinfo)) {
        free(datinfo);
        return NULL;
    }

    /* allocate space for the each possible BTL */
    mca_btl_udapl_component.udapl_btls = (mca_btl_udapl_module_t **)
            malloc(num_ias * sizeof(mca_btl_udapl_module_t *));
    if(NULL == mca_btl_udapl_component.udapl_btls) {
        free(datinfo);
        return NULL;
    }

    /* create a BTL module for each interface */
    for(mca_btl_udapl_component.udapl_num_btls = i = 0; i < num_ias; i++) {
        OPAL_OUTPUT((0, "udapl creating btl for %s\n", datinfo[i].ia_name));

        btl = malloc(sizeof(mca_btl_udapl_module_t));
        if(NULL == btl) {
            free(datinfo);
            free(mca_btl_udapl_component.udapl_btls);
            return NULL;
        }

        /* copy default values into the new BTL */
        memcpy(btl, &mca_btl_udapl_module, sizeof(mca_btl_udapl_module_t));

        /* initialize this BTL */
        /* TODO - make use of the thread-safety info in datinfo also */
        if(OMPI_SUCCESS != mca_btl_udapl_init(datinfo[i].ia_name, btl)) {
            free(btl);
            continue;
        }

        /* successful btl creation */
        mca_btl_udapl_component.udapl_btls[i] = btl;
        if(++mca_btl_udapl_component.udapl_num_btls >=
                mca_btl_udapl_component.udapl_max_btls) {
            break;
        }
    }

    /* finished with datinfo */
    free(datinfo);

    /* Make sure we have some interfaces */
    if(0 == mca_btl_udapl_component.udapl_num_btls) {
        mca_btl_base_error_no_nics("uDAPL", "NIC");
        free(mca_btl_udapl_component.udapl_btls);
        return NULL;
    }

    /* publish uDAPL parameters with the MCA framework */
    if (OMPI_SUCCESS != mca_btl_udapl_modex_send()) {
        free(mca_btl_udapl_component.udapl_btls);
        return NULL;
    }

    /* return array of BTLs */
    btls = (mca_btl_base_module_t**) malloc(sizeof(mca_btl_base_module_t *) *
            mca_btl_udapl_component.udapl_num_btls);
    if (NULL == btls) {
        free(mca_btl_udapl_component.udapl_btls);
        return NULL;
    }

    memcpy(btls, mca_btl_udapl_component.udapl_btls,
           mca_btl_udapl_component.udapl_num_btls *
           sizeof(mca_btl_udapl_module_t *));
    *num_btl_modules = mca_btl_udapl_component.udapl_num_btls;
    return btls;
}


static int mca_btl_udapl_finish_connect(mca_btl_udapl_module_t* btl,
                                        mca_btl_udapl_frag_t* frag,
                                        DAT_EP_HANDLE endpoint)
{
    mca_btl_udapl_proc_t* proc;
    mca_btl_base_endpoint_t* ep;
    mca_btl_udapl_addr_t* addr;
    size_t i;

    /*addr = (mca_btl_udapl_addr_t*)frag->hdr;*/
    addr = (mca_btl_udapl_addr_t*)frag->segment.seg_addr.pval;

    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);
    for(proc = (mca_btl_udapl_proc_t*)
                opal_list_get_first(&mca_btl_udapl_component.udapl_procs);
            proc != (mca_btl_udapl_proc_t*)
                opal_list_get_end(&mca_btl_udapl_component.udapl_procs);
            proc  = (mca_btl_udapl_proc_t*)opal_list_get_next(proc)) {
    
        for(i = 0; i < proc->proc_endpoint_count; i++) {
            ep = proc->proc_endpoints[i];
    
            /* Does this endpoint match? */
            if(ep->endpoint_btl == btl &&
                    !memcmp(addr, &ep->endpoint_addr,
                        sizeof(mca_btl_udapl_addr_t))) {
                ep->endpoint_ep = endpoint;
                OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
                OPAL_OUTPUT((0, "btl_udapl matched endpoint! HAPPY DANCE!!!\n"));
                return OMPI_SUCCESS;
            }
        }
    }

    /* If this point is reached, no matching endpoint was found */
    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
    OPAL_OUTPUT((0, "btl_udapl ERROR could not match endpoint\n"));
    return OMPI_ERROR;
}


static int mca_btl_udapl_accept_connect(mca_btl_udapl_module_t* btl,
                                        DAT_CR_HANDLE cr_handle)
{
    mca_btl_udapl_frag_t* frag;
    DAT_EP_HANDLE endpoint;
    int rc;

    rc = dat_ep_create(btl->udapl_ia, btl->udapl_pz,
            btl->udapl_evd_dto, btl->udapl_evd_dto,
            btl->udapl_evd_conn, NULL, &endpoint);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_ep_create");
        return OMPI_ERROR;
    }

    rc = dat_cr_accept(cr_handle, endpoint, 0, NULL);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_cr_accept");
        return OMPI_ERROR;
    }

    /* Post a receive to get the address data */
    frag = (mca_btl_udapl_frag_t*)mca_btl_udapl_alloc(
            (mca_btl_base_module_t*)btl, sizeof(mca_btl_udapl_addr_t));

    frag->endpoint = NULL;
    frag->type = MCA_BTL_UDAPL_CONN_RECV;

    rc = dat_ep_post_recv(endpoint, 1, &frag->triplet,
            (DAT_DTO_COOKIE)(void*)frag, DAT_COMPLETION_DEFAULT_FLAG);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_ep_post_send");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


/*
 *  uDAPL component progress.
 */

int mca_btl_udapl_component_progress()
{
    mca_btl_udapl_module_t* btl;
    mca_btl_udapl_frag_t* frag;
    static int32_t inprogress = 0;
    DAT_EVENT event;
    int count = 0;
    size_t i;

    /* prevent deadlock - only one thread should be 'progressing' at a time */
    if(OPAL_THREAD_ADD32(&inprogress, 1) > 1) {
        OPAL_THREAD_ADD32(&inprogress, -1);
        return OMPI_SUCCESS;
    }

    /* check for work to do on each uDAPL btl */
    for(i = 0; i < mca_btl_udapl_component.udapl_num_btls; i++) {
        btl = mca_btl_udapl_component.udapl_btls[i];
        /* TODO - lock this properly */

        /* Check DTO EVD */
        while(DAT_SUCCESS ==
                dat_evd_dequeue(btl->udapl_evd_dto, &event)) {
            DAT_DTO_COMPLETION_EVENT_DATA* dto;

            switch(event.event_number) {
                case DAT_DTO_COMPLETION_EVENT:
                    OPAL_OUTPUT((0, "btl_udapl DTO completion\n"));
                    /* questions to answer:
                       should i use separate endpoints for eager/max frags?
                        i need to do this if i only want to post recv's for
                        the exact eager/max size, and uDAPL won't just pick
                        a large enough buffer

                        how about just worrying about eager frags for now?
                       */
                    dto = &event.event_data.dto_completion_event_data;
                    OPAL_OUTPUT((0, "DTO transferred %d bytes\n", dto->transfered_length));

                    /* Was the DTO successful? */
                    if(DAT_DTO_SUCCESS != dto->status) {
                        OPAL_OUTPUT((0,
                                "btl_udapl DTO error %d\n", dto->status));
                        break;
                    }

                    frag = dto->user_cookie.as_ptr;

                    switch(frag->type) {
                    case MCA_BTL_UDAPL_SEND:
                        /* TODO - write me */
                        break;
                    case MCA_BTL_UDAPL_CONN_SEND:
                        /* Set the endpoint state to connected */
                        OPAL_OUTPUT((0,
                                "btl_udapl SEND SIDE CONNECT COMPLETED!!\n"));
                        frag->endpoint->endpoint_state =
                                MCA_BTL_UDAPL_CONNECTED;

                        /* TODO - fire off any queued sends */

                        /* Retire the fragment */
                        MCA_BTL_UDAPL_FRAG_RETURN_EAGER(btl, frag);
                        break;
                    case MCA_BTL_UDAPL_CONN_RECV:
                        /* Just got the address data we need for completing
                           a new connection - match endpoints */
                        mca_btl_udapl_finish_connect(btl, frag, dto->ep_handle);
                        
                        /* Retire the fragment */
                        MCA_BTL_UDAPL_FRAG_RETURN_EAGER(btl, frag);
                        break;
#ifdef OMPI_ENABLE_DEBUG
                    default:
                        OPAL_OUTPUT((0, "WARNING unknown frag type: %d\n",
                                    frag->type));
#endif
                    }
                    count++;
                    break;
#ifdef OMPI_ENABLE_DEBUG
                default:
                    OPAL_OUTPUT((0, "WARNING unknown dto event: %d\n",
                            event.event_number));
#endif
            }
        }

        /* Check connection EVD */
        while(DAT_SUCCESS ==
                dat_evd_dequeue(btl->udapl_evd_conn, &event)) {

            switch(event.event_number) {
                case DAT_CONNECTION_REQUEST_EVENT:
                    /* Accept a new connection */
                    OPAL_OUTPUT((0, "btl_udapl accepting connection\n"));

                    mca_btl_udapl_accept_connect(btl,
                            event.event_data.cr_arrival_event_data.cr_handle);
                    count++;
                    break;
                case DAT_CONNECTION_EVENT_ESTABLISHED:
                    OPAL_OUTPUT((0, "btl_udapl connection established\n"));

                    /* Both the client and server side of a connection generate
                       this event */
                    /* Really shouldn't do anything here, as we won't have the
                       address data we need to match a uDAPL EP to a BTL EP.
                       Connections are finished when DTOs are completed for
                       the address transfer */
                    
                    count++;
                    break;
                case DAT_CONNECTION_EVENT_PEER_REJECTED:
                case DAT_CONNECTION_EVENT_NON_PEER_REJECTED:
                case DAT_CONNECTION_EVENT_ACCEPT_COMPLETION_ERROR:
                case DAT_CONNECTION_EVENT_DISCONNECTED:
                case DAT_CONNECTION_EVENT_BROKEN:
                case DAT_CONNECTION_EVENT_TIMED_OUT:
                    /* handle this case specially? if we have finite timeout,
                       we might want to try connecting again here. */
                case DAT_CONNECTION_EVENT_UNREACHABLE:
                    /* Need to set the BTL endpoint to MCA_BTL_UDAPL_FAILED
                       See dat_ep_connect documentation pdf pg 198 */
                    break;
#ifdef OMPI_ENABLE_DEBUG
                default:
                    OPAL_OUTPUT((0, "WARNING unknown conn event: %d\n",
                            event.event_number));
#endif
            }
        }

        /* Check async EVD */
        while(DAT_SUCCESS ==
                dat_evd_dequeue(btl->udapl_evd_async, &event)) {
            switch(event.event_number) {
                case DAT_ASYNC_ERROR_EVD_OVERFLOW:
                case DAT_ASYNC_ERROR_IA_CATASTROPHIC:
                case DAT_ASYNC_ERROR_EP_BROKEN:
                case DAT_ASYNC_ERROR_TIMED_OUT:
                case DAT_ASYNC_ERROR_PROVIDER_INTERNAL_ERROR:
                    break;
#ifdef OMPI_ENABLE_DEBUG
                default:
                    OPAL_OUTPUT((0, "WARNING unknown async event: %d\n",
                            event.event_number));
#endif
            }
        }
    }

    /* unlock and return */
    OPAL_THREAD_ADD32(&inprogress, -1);
    return count;
}

