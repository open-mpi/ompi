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
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 *
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
#include "ompi/runtime/params.h"

#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
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

#if OMPI_ENABLE_DEBUG
void
mca_btl_udapl_error(DAT_RETURN ret, char* str)
{
    char* major;
    char* minor;

    if(DAT_SUCCESS != dat_strerror(ret,
            (const char**)&major, (const char**)&minor))
    {
        printf("dat_strerror failed! ret is %d\n", ret);
        exit(-1);
    }

    OPAL_OUTPUT((0, "ERROR: %s %s %s\n", str, major, minor));
}
#endif


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
    mca_btl_udapl_component.udapl_mpool_name =
        mca_btl_udapl_param_register_string("mpool", "rdma");
    mca_btl_udapl_component.udapl_max_btls = 
        mca_btl_udapl_param_register_int("max_modules", 8);
    mca_btl_udapl_component.udapl_evd_qlen =
        mca_btl_udapl_param_register_int("evd_qlen", 32);
    mca_btl_udapl_component.udapl_num_recvs = 
        mca_btl_udapl_param_register_int("num_recvs", 8);
    mca_btl_udapl_component.udapl_num_sends = 
        mca_btl_udapl_param_register_int("num_sends", 8);
    mca_btl_udapl_component.udapl_timeout = 
        mca_btl_udapl_param_register_int("timeout", 10000000);

    /* register uDAPL module parameters */
    mca_btl_udapl_module.super.btl_exclusivity =
        mca_btl_udapl_param_register_int ("exclusivity",
                MCA_BTL_EXCLUSIVITY_DEFAULT - 10);
    mca_btl_udapl_module.super.btl_eager_limit = 
        mca_btl_udapl_param_register_int ("eager_limit", 32*1024);
    mca_btl_udapl_module.super.btl_min_send_size =
        mca_btl_udapl_param_register_int ("min_send_size", 16*1024);
    mca_btl_udapl_module.super.btl_max_send_size =
        mca_btl_udapl_param_register_int ("max_send_size", 64*1024);
    mca_btl_udapl_module.super.btl_min_rdma_size = 
        mca_btl_udapl_param_register_int("min_rdma_size", 512*1024); 
    mca_btl_udapl_module.super.btl_max_rdma_size = 
        mca_btl_udapl_param_register_int("max_rdma_size", 128*1024);
    mca_btl_udapl_module.super.btl_bandwidth  = 
        mca_btl_udapl_param_register_int("bandwidth", 225); 

    /* compute udapl_eager_frag_size and udapl_max_frag_size */
    mca_btl_udapl_component.udapl_eager_frag_size =
        mca_btl_udapl_module.super.btl_eager_limit;
    mca_btl_udapl_module.super.btl_eager_limit -=
        sizeof(mca_btl_udapl_footer_t);
    
    mca_btl_udapl_component.udapl_max_frag_size =
        mca_btl_udapl_module.super.btl_max_send_size;
    mca_btl_udapl_module.super.btl_max_send_size -=
        sizeof(mca_btl_udapl_footer_t);


    /* leave pinned option */
    mca_btl_udapl_component.leave_pinned = (int) ompi_mpi_leave_pinned;
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_udapl_component_close(void)
{
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
        addrs = (mca_btl_udapl_addr_t*)malloc(size);
        if (NULL == addrs) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for (i = 0; i < mca_btl_udapl_component.udapl_num_btls; i++) {
            mca_btl_udapl_module_t* btl = mca_btl_udapl_component.udapl_btls[i];
            addrs[i] = btl->udapl_addr;
        }
    }

    rc = mca_pml_base_modex_send(
            &mca_btl_udapl_component.super.btl_version, addrs, size);
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
    DAT_PROVIDER_INFO** datinfoptr;
    mca_btl_base_module_t **btls;
    mca_btl_udapl_module_t *btl;
    DAT_COUNT num_ias;
    int32_t i;

    /* enumerate uDAPL interfaces */
    /* Have to do weird pointer stuff to make uDAPL happy -
       just an array of DAT_PROVIDER_INFO isn't good enough. */
    datinfo = malloc(sizeof(DAT_PROVIDER_INFO) *
            mca_btl_udapl_component.udapl_max_btls);
    datinfoptr = malloc(sizeof(DAT_PROVIDER_INFO*) *
            mca_btl_udapl_component.udapl_max_btls);
    if(NULL == datinfo || NULL == datinfoptr) {
        return NULL;
    }

     for(i = 0; i < (int32_t)mca_btl_udapl_component.udapl_max_btls; i++) {
         datinfoptr[i] = &datinfo[i];
     }

    if(DAT_SUCCESS != dat_registry_list_providers(
            mca_btl_udapl_component.udapl_max_btls,
            (DAT_COUNT*)&num_ias, datinfoptr)) {
        free(datinfo);
        free(datinfoptr);
        return NULL;
    }

    free(datinfoptr);

    /* allocate space for the each possible BTL */
    mca_btl_udapl_component.udapl_btls = (mca_btl_udapl_module_t **)
            malloc(num_ias * sizeof(mca_btl_udapl_module_t *));
    if(NULL == mca_btl_udapl_component.udapl_btls) {
        free(datinfo);
        return NULL;
    }

    /* create a BTL module for each interface */
    for(mca_btl_udapl_component.udapl_num_btls = i = 0; i < num_ias; i++) {
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
        mca_btl_udapl_component.udapl_btls[mca_btl_udapl_component.udapl_num_btls] = btl;
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

    /* Post OOB receive */
    mca_btl_udapl_endpoint_post_oob_recv();

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


static int mca_btl_udapl_accept_connect(mca_btl_udapl_module_t* btl,
                                        DAT_CR_HANDLE cr_handle)
{
    DAT_EP_HANDLE endpoint;
    int rc;

    rc = dat_ep_create(btl->udapl_ia, btl->udapl_pz,
            btl->udapl_evd_dto, btl->udapl_evd_dto,
            btl->udapl_evd_conn, NULL, &endpoint);
    if(DAT_SUCCESS != rc) {
        MCA_BTL_UDAPL_ERROR(rc, "dat_ep_create");
        return OMPI_ERROR;
    }

    rc = dat_cr_accept(cr_handle, endpoint, 0, NULL);
    if(DAT_SUCCESS != rc) {
        MCA_BTL_UDAPL_ERROR(rc, "dat_cr_accept");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


static inline int mca_btl_udapl_sendrecv(mca_btl_udapl_module_t* btl,
        DAT_EP_HANDLE* endpoint)
{
    mca_btl_udapl_frag_t* frag;
    DAT_DTO_COOKIE cookie;
    static int32_t connection_seq = 1;
    int rc;

    /* Post a receive to get the peer's address data */
    frag = (mca_btl_udapl_frag_t*)mca_btl_udapl_alloc(
            (mca_btl_base_module_t*)btl, sizeof(mca_btl_udapl_addr_t) +
            sizeof(int32_t));
    cookie.as_ptr = frag;

    frag->type = MCA_BTL_UDAPL_CONN_RECV;

    rc = dat_ep_post_recv(endpoint, 1,
            &frag->triplet, cookie, DAT_COMPLETION_DEFAULT_FLAG);
    if(DAT_SUCCESS != rc) {
        MCA_BTL_UDAPL_ERROR(rc, "dat_ep_post_recv");
        return OMPI_ERROR;
    }


    /* Send our local address data over this EP */
    frag = (mca_btl_udapl_frag_t*)mca_btl_udapl_alloc(
            (mca_btl_base_module_t*)btl, sizeof(mca_btl_udapl_addr_t) +
            sizeof(int32_t));
    cookie.as_ptr = frag;

    memcpy(frag->segment.seg_addr.pval,
            &btl->udapl_addr, sizeof(mca_btl_udapl_addr_t));
    memcpy((char *)frag->segment.seg_addr.pval + sizeof(mca_btl_udapl_addr_t),
            &connection_seq, sizeof(int32_t));
    connection_seq++;

    frag->type = MCA_BTL_UDAPL_CONN_SEND;

    rc = dat_ep_post_send(endpoint, 1,
            &frag->triplet, cookie, DAT_COMPLETION_DEFAULT_FLAG);
    if(DAT_SUCCESS != rc) {
        MCA_BTL_UDAPL_ERROR(rc, "dat_ep_post_send");
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
    static int32_t inprogress = 0;
    DAT_EVENT event;
#if defined(__SVR4) && defined(__sun)
    DAT_COUNT nmore;  /* used by dat_evd_wait, see comment below */
#endif
    int count = 0;
    size_t i;

    /* prevent deadlock - only one thread should be 'progressing' at a time */
    if(OPAL_THREAD_ADD32(&inprogress, 1) > 1) {
        OPAL_THREAD_ADD32(&inprogress, -1);
        return OMPI_SUCCESS;
    }

    /* check for work to do on each uDAPL btl */
    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);
    for(i = 0; i < mca_btl_udapl_component.udapl_num_btls; i++) {
        btl = mca_btl_udapl_component.udapl_btls[i];

        /* Check DTO EVD */
        while(DAT_SUCCESS ==
                dat_evd_dequeue(btl->udapl_evd_dto, &event)) {
            DAT_DTO_COMPLETION_EVENT_DATA* dto;
            mca_btl_udapl_frag_t* frag;
        
            switch(event.event_number) {
                case DAT_DTO_COMPLETION_EVENT:
                    dto = &event.event_data.dto_completion_event_data;

                    frag = dto->user_cookie.as_ptr;
                    /* Was the DTO successful? */
                    if(DAT_DTO_SUCCESS != dto->status) {
                        OPAL_OUTPUT((0,
                                "btl_udapl ***** DTO error %d %d %d %p*****\n",
                                dto->status, frag->type, frag->size, dto->ep_handle));
                        break;
                    }

                    switch(frag->type) {
                    case MCA_BTL_UDAPL_SEND:
                    {
                        mca_btl_udapl_endpoint_t* endpoint = frag->endpoint;
                        /*OPAL_OUTPUT((0, "btl_udapl UDAPL_SEND %d",
                                    dto->transfered_length));*/

                        assert(frag->base.des_src == &frag->segment);
                        assert(frag->base.des_src_cnt == 1);
                        assert(frag->base.des_dst == NULL);
                        assert(frag->base.des_dst_cnt == 0);
                        assert(frag->type == MCA_BTL_UDAPL_SEND);

                        frag->base.des_cbfunc(&btl->super, frag->endpoint,
                                &frag->base, OMPI_SUCCESS);

                        if(frag->size ==
                                mca_btl_udapl_component.udapl_eager_frag_size) {
                            if(!opal_list_is_empty(
                                        &endpoint->endpoint_eager_frags)) {
                                DAT_DTO_COOKIE cookie;
                                
                                frag = (mca_btl_udapl_frag_t*)
                                    opal_list_remove_first(
                                            &endpoint->endpoint_eager_frags);
                    
                                assert(frag->triplet.segment_length ==
                                        frag->segment.seg_len +
                                        sizeof(mca_btl_udapl_footer_t));

                                cookie.as_ptr = frag;
                                dat_ep_post_send(endpoint->endpoint_eager,
                                        1, &frag->triplet, cookie,
                                        DAT_COMPLETION_DEFAULT_FLAG);
                            } else {
                                OPAL_THREAD_ADD32(
                                        &endpoint->endpoint_eager_sends, 1);
                            }
                        } else {
                            assert(frag->size ==
                                mca_btl_udapl_component.udapl_max_frag_size);
                            if(!opal_list_is_empty(
                                        &endpoint->endpoint_max_frags)) {
                                DAT_DTO_COOKIE cookie;
                               
                                frag = (mca_btl_udapl_frag_t*)
                                    opal_list_remove_first(
                                            &endpoint->endpoint_max_frags);
                                
                                assert(frag->triplet.segment_length ==
                                        frag->segment.seg_len +
                                        sizeof(mca_btl_udapl_footer_t));

                                cookie.as_ptr = frag;
                                dat_ep_post_send(endpoint->endpoint_max,
                                        1, &frag->triplet, cookie,
                                        DAT_COMPLETION_DEFAULT_FLAG);
                            } else {
                                OPAL_THREAD_ADD32(
                                        &endpoint->endpoint_max_sends, 1);
                            }
                        }

                        break;
                    }
                    case MCA_BTL_UDAPL_RECV:
                    {
                        mca_btl_base_recv_reg_t* reg;

                        assert(frag->base.des_dst == &frag->segment);
                        assert(frag->base.des_dst_cnt == 1);
                        assert(frag->base.des_src == NULL);
                        assert(frag->base.des_src_cnt == 0);
                        assert(frag->type == MCA_BTL_UDAPL_RECV);
                        assert(frag->triplet.virtual_address ==
                                (DAT_VADDR)frag->segment.seg_addr.pval);
                        assert(frag->triplet.segment_length == frag->size);
                        assert(frag->btl == btl);

			/* setup frag ftr location and do callback */
                        frag->segment.seg_len = dto->transfered_length -
                            sizeof(mca_btl_udapl_footer_t);
			frag->ftr = (mca_btl_udapl_footer_t *)
			    ((char *)frag->segment.seg_addr.pval + 
			     frag->segment.seg_len);
			reg = &btl->udapl_reg[frag->ftr->tag];
                        OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
                        reg->cbfunc(&btl->super,
                                frag->ftr->tag, &frag->base, reg->cbdata);
                        OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);

                        /* Repost the frag */
                        frag->ftr = frag->segment.seg_addr.pval;
                        frag->segment.seg_len =
                            frag->size - sizeof(mca_btl_udapl_footer_t);
                        frag->base.des_flags = 0;

                        if(frag->size ==
                              mca_btl_udapl_component.udapl_eager_frag_size) {
                            dat_ep_post_recv(frag->endpoint->endpoint_eager,
                                    1, &frag->triplet, dto->user_cookie,
                                    DAT_COMPLETION_DEFAULT_FLAG);
                        } else {
                            assert(frag->size ==
                                mca_btl_udapl_component.udapl_max_frag_size);
                            dat_ep_post_recv(frag->endpoint->endpoint_max,
                                    1, &frag->triplet, dto->user_cookie,
                                    DAT_COMPLETION_DEFAULT_FLAG);
                        }

                        break;
                    }
                    case MCA_BTL_UDAPL_CONN_RECV:
                        mca_btl_udapl_endpoint_finish_connect(btl,
                                frag->segment.seg_addr.pval,
                                (int32_t *)((char *)frag->segment.seg_addr.pval  +
                                    sizeof(mca_btl_udapl_addr_t)),
                                event.event_data.connect_event_data.ep_handle);
                        /* No break - fall through to free */
                    case MCA_BTL_UDAPL_CONN_SEND:
                        frag->segment.seg_len =
                                mca_btl_udapl_module.super.btl_eager_limit;
                        mca_btl_udapl_free((mca_btl_base_module_t*)btl,
                                (mca_btl_base_descriptor_t*)frag);
                        break;
                    default:
                        OPAL_OUTPUT((0, "WARNING unknown frag type: %d\n",
                                    frag->type));
                    }
                    count++;
                    break;
                default:
                    OPAL_OUTPUT((0, "WARNING unknown dto event: %d\n",
                            event.event_number));
            }
        }

        /* Check connection EVD */
        while(DAT_SUCCESS ==
#if defined(__SVR4) && defined(__sun)
	    /* There is a bug is Solaris udapl implementation
	     * such that dat_evd_dequeue does not dequeue
	     * DAT_CONNECTION_REQUEST_EVENT. Workaround is to use
	     * wait. This should be removed when fix available.
	     */
	        dat_evd_wait(btl->udapl_evd_conn, 0, 1, &event, &nmore)) {
#else
                dat_evd_dequeue(btl->udapl_evd_conn, &event)) {
#endif
            switch(event.event_number) {
                case DAT_CONNECTION_REQUEST_EVENT:
                    /* Accept a new connection */
                    mca_btl_udapl_accept_connect(btl,
                            event.event_data.cr_arrival_event_data.cr_handle);
                    count++;
                    break;
                case DAT_CONNECTION_EVENT_ESTABLISHED:
                    /* Both the client and server side of a connection generate
                       this event */

                    mca_btl_udapl_sendrecv(btl,
                            event.event_data.connect_event_data.ep_handle);
                    
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
                default:
                    OPAL_OUTPUT((0, "WARNING unknown conn event: %d\n",
                            event.event_number));
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
                default:
                    OPAL_OUTPUT((0, "WARNING unknown async event: %d\n",
                            event.event_number));
            }
        }
    }

    /* unlock and return */
    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
    OPAL_THREAD_ADD32(&inprogress, -1);
    return count;
}

