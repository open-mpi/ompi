/*
 * Copyright (c) 2007-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/runtime/opal_progress.h"
#include "opal/dss/dss.h"
#include "opal/util/alfg.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "ompi/mca/rte/rte.h"
#include "common_ofacm_xoob.h"
#include "opal/class/opal_hash_table.h"
#include "base.h"
#include "connect.h"
#include "opal/constants.h"

#define SIZE_OF3(A, B, C) (sizeof(A) + sizeof(B) + sizeof(C))
#define BASE_TO_XOOB(context)  (opal_common_ofacm_xoob_local_connection_context_t *)context
#define XOOB_TO_BASE(xcontext) (opal_common_ofacm_base_local_connection_context_t *)xcontext

static void xoob_component_register(void);
static int xoob_component_query(opal_common_ofacm_base_dev_desc_t *dev, 
                               opal_common_ofacm_base_module_t **cpc);
static int xoob_component_finalize(void);

static int xoob_module_start_connect
            (opal_common_ofacm_base_local_connection_context_t *context);

static void xoob_ib_address_constructor(ofacm_ib_address_t *ib_addr);
static void xoob_ib_address_destructor(ofacm_ib_address_t *ib_addr);

OBJ_CLASS_INSTANCE(ofacm_ib_address_t,
                   opal_list_item_t,
                   xoob_ib_address_constructor,
                   xoob_ib_address_destructor);
/*
 * The "component" struct -- the top-level function pointers for the
 * xoob connection scheme.
 */
opal_common_ofacm_base_component_t opal_common_ofacm_xoob = {
    "xoob",
    /* Register */
    xoob_component_register,
    /* Init */
    NULL,
    /* Query */
    xoob_component_query,
    /* Finalize */
    xoob_component_finalize,
};

typedef enum {
    ENDPOINT_XOOB_CONNECT_REQUEST,
    ENDPOINT_XOOB_CONNECT_RESPONSE,
    ENDPOINT_XOOB_CONNECT_XRC_REQUEST,
    ENDPOINT_XOOB_CONNECT_XRC_RESPONSE,
    ENDPOINT_XOOB_CONNECT_XRC_NR_RESPONSE /* The xrc recv qp already was destroyed */
} connect_message_type_t;

static int xoob_priority = 60;
static bool rml_recv_posted = false;
static opal_rng_buff_t rand_buff;

#define XOOB_SET_REMOTE_INFO(EP, INFO)                                    \
do {                                                                      \
    /* copy the rem_info stuff */                                         \
    EP.rem_lid       = INFO.rem_lid;                                      \
    EP.rem_subnet_id = INFO.rem_subnet_id;                                \
    EP.rem_mtu       = INFO.rem_mtu;                                      \
    EP.rem_index     = INFO.rem_index;                                    \
    memcpy((void*)EP.rem_qps, (void*)INFO.rem_qps,                        \
            sizeof(mca_btl_openib_rem_qp_info_t));                        \
    /* copy the rem_info stuff */                                         \
    memcpy((void*)EP.rem_srqs, (void*)INFO.rem_srqs,                      \
            sizeof(mca_btl_openib_rem_srq_info_t) *                       \
            mca_btl_openib_component.num_xrc_qps);                        \
} while (0)

/* Constructor destructor for xoob context. */
static void xoob_local_context_constructor
    (opal_common_ofacm_xoob_local_connection_context_t *context)
{
    context->addr = NULL;
    context->xrc_recv_psn = 0;
}

static void xoob_local_context_destructor
    (opal_common_ofacm_xoob_local_connection_context_t *context)
{
    if(NULL != context->addr) {
        OBJ_RELEASE(context->addr);
    }
}

OBJ_CLASS_INSTANCE(opal_common_ofacm_xoob_local_connection_context_t,
                   opal_common_ofacm_base_local_connection_context_t,
                   xoob_local_context_constructor,
                   xoob_local_context_destructor);

static void xoob_pending_context_constructor(pending_context_t *pcontext)
{
    pcontext->xcontext = NULL;
}

static void xoob_pending_context_destructor(pending_context_t *pcontext)
{
    /* I have nothing to do !*/
}

static void xoob_pending_context_init(pending_context_t *pcontext, 
        opal_common_ofacm_xoob_local_connection_context_t *xcontext)
{
    pcontext->xcontext = xcontext;
}

OBJ_CLASS_INSTANCE(pending_context_t,
                   opal_list_item_t,
                   xoob_pending_context_constructor,
                   xoob_pending_context_destructor);

static void xoob_ib_address_constructor(ofacm_ib_address_t *ib_addr)
{
    ib_addr->key = NULL;
    ib_addr->subnet_id = 0;
    ib_addr->lid = 0;
    ib_addr->status = XOOB_ADDR_CLOSED;
    ib_addr->qps = NULL;
    OBJ_CONSTRUCT(&ib_addr->addr_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ib_addr->pending_contexts, opal_list_t);
}

static void xoob_ib_address_destructor(ofacm_ib_address_t *ib_addr)
{
    if(NULL != ib_addr->qps && NULL != ib_addr->qps[0].lcl_qp) {
        if(ibv_destroy_qp(ib_addr->qps[0].lcl_qp)) {
            OFACM_ERROR(("Failed to destroy QP:%d\n", 0));
        }
    }
    if (NULL != ib_addr->key) {
        free(ib_addr->key);
    }
    OBJ_DESTRUCT(&ib_addr->addr_lock);
    OBJ_DESTRUCT(&ib_addr->pending_contexts);
}

static int xoob_ib_address_init(ofacm_ib_address_t *ib_addr, uint16_t lid, uint64_t s_id, ompi_jobid_t ep_jobid)
{
    ib_addr->key = malloc(SIZE_OF3(s_id, lid, ep_jobid));
    if (NULL == ib_addr->key) {
        OFACM_ERROR(("Failed to allocate memory for key\n"));
        return OPAL_ERROR;
    }
    memset(ib_addr->key, 0, SIZE_OF3(s_id, lid, ep_jobid));
    /* creating the key = lid + s_id + ep_jobid */
    memcpy(ib_addr->key, &lid, sizeof(lid));
    memcpy((void*)((char*)ib_addr->key + sizeof(lid)), &s_id, sizeof(s_id));
    memcpy((void*)((char*)ib_addr->key + sizeof(lid) + sizeof(s_id)),
            &ep_jobid, sizeof(ep_jobid));
    /* caching lid and subnet id */
    ib_addr->subnet_id = s_id;
    ib_addr->lid = lid;

    return OPAL_SUCCESS;
}

/* Create new entry in hash table for subnet_id and lid,
 * update the context pointer.
 * Before call to this function you need to protect with
 */
static ofacm_ib_address_t* xoob_ib_address_add_new (opal_common_ofacm_xoob_module_t *xcpc,
        uint16_t lid, uint64_t s_id, ompi_jobid_t ep_jobid)
{
    void *tmp;
    int ret;
    struct ofacm_ib_address_t *ib_addr = OBJ_NEW(ofacm_ib_address_t);

    ret = xoob_ib_address_init(ib_addr, lid, s_id, ep_jobid);
    if (OPAL_SUCCESS != ret ) {
        OFACM_ERROR(("XRC Internal error. Failed to init ib_addr\n"));
        OBJ_DESTRUCT(ib_addr);
        return NULL;
    }
    /* is it already in the table ?*/
    if (OPAL_SUCCESS != opal_hash_table_get_value_ptr(&xcpc->ib_addr_table,
                ib_addr->key,
                SIZE_OF3(s_id, lid, ep_jobid), &tmp)) {
        /* It is new one, lets put it on the table */
        ret = opal_hash_table_set_value_ptr(&xcpc->ib_addr_table,
                ib_addr->key, SIZE_OF3(s_id, lid, ep_jobid), (void*)ib_addr);
        if (OPAL_SUCCESS != ret) {
            OFACM_ERROR(("XRC Internal error."
                        " Failed to add element to ib_addr_table\n"));
            OBJ_DESTRUCT(ib_addr);
            return NULL;
        }
    } else {
        /* so we have this one in the table, just return the pointer */
        OBJ_DESTRUCT(ib_addr);
        ib_addr = (ofacm_ib_address_t *)tmp;
        OBJ_RETAIN(ib_addr);
        assert(lid == ib_addr->lid && s_id == ib_addr->subnet_id);
    }

    /* update the context with pointer to ib address */
    return ib_addr;
}

static void xoob_connection_complete(opal_common_ofacm_xoob_local_connection_context_t *xcontext)
{
    bool master = false;
    pending_context_t *pcon;
    opal_common_ofacm_base_local_connection_context_t *con;
    opal_common_ofacm_base_local_connection_context_t *context =
                                            XOOB_TO_BASE(xcontext);

    OFACM_VERBOSE(("Now we are CONNECTED"));
    OPAL_THREAD_LOCK(&xcontext->addr->addr_lock);
    if (XOOB_ADDR_CONNECTED == xcontext->addr->status) {
        /* We are not xrc master */
        /* set our qp pointer to master qp */
        master = false;
    } else {
        /* I'm master of XRC */
        xcontext->addr->status = XOOB_ADDR_CONNECTED;
        master = true;
    }

    /* The status was moved down to cpc */
    context->state = MCA_COMMON_OFACM_CONNECTED;

    while(master && !opal_list_is_empty(&xcontext->addr->pending_contexts)) {
        pcon = (pending_context_t *)opal_list_remove_first(&xcontext->addr->pending_contexts);
        con = XOOB_TO_BASE(pcon->xcontext);
        OBJ_RELEASE(pcon);
        if (OPAL_SUCCESS != 
                xoob_module_start_connect(con)) {
            OFACM_ERROR(("Failed to connect pending endpoint\n"));
        }
    }
    OPAL_THREAD_UNLOCK(&xcontext->addr->addr_lock);

    context->connect_cb(context->user_context);
}

static int xoob_init_rem_info_alloc_qp(opal_common_ofacm_base_remote_connection_context_t *rem_info)
{
    rem_info->rem_qps = (opal_common_ofacm_base_rem_qp_info_t *)
        malloc(sizeof(opal_common_ofacm_base_rem_qp_info_t));
    if (NULL == rem_info->rem_qps) {
        OFACM_ERROR(("Failed to allocate memory for remote QP data\n"));
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int xoob_init_rem_info_alloc_srq(opal_common_ofacm_base_remote_connection_context_t *rem_info, uint8_t num_srqs)
{
    rem_info->rem_srqs = (opal_common_ofacm_base_rem_srq_info_t*)
        calloc(num_srqs, sizeof(opal_common_ofacm_base_rem_srq_info_t));
    if (NULL == rem_info->rem_srqs) {
        OFACM_ERROR(("Failed to allocate memory for remote SRQ data\n"));
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

/* Free remote information structs */
static void xoob_free_rem_info(opal_common_ofacm_base_remote_connection_context_t *rem_info)
{
    if (NULL != rem_info->rem_qps) {
        free(rem_info->rem_qps);
    }
    if (NULL != rem_info->rem_srqs) {
        free(rem_info->rem_srqs);
    }
}

static int xoob_set_remote_info(opal_common_ofacm_xoob_local_connection_context_t *xcontext,
                           opal_common_ofacm_base_remote_connection_context_t *remote_info)
{
    opal_common_ofacm_base_local_connection_context_t *context = XOOB_TO_BASE(xcontext);
    
    /* If we got qp information  - copy it */
    if (NULL != remote_info->rem_qps) {
        xoob_init_rem_info_alloc_qp(&context->remote_info);
        memcpy(context->remote_info.rem_qps, 
               remote_info->rem_qps, 
               sizeof(opal_common_ofacm_base_rem_qp_info_t)); 
    }

    if (NULL != remote_info->rem_srqs) {
        xoob_init_rem_info_alloc_srq(&context->remote_info, context->num_of_srqs);
        memcpy(context->remote_info.rem_srqs, remote_info->rem_srqs,
                sizeof(opal_common_ofacm_base_rem_srq_info_t)*context->num_of_srqs); 
    }

    context->remote_info.rem_lid = remote_info->rem_lid;
    context->remote_info.rem_subnet_id = remote_info->rem_subnet_id;
    context->remote_info.rem_mtu = remote_info->rem_mtu;
    context->remote_info.rem_index = remote_info->rem_index;
    
    OFACM_VERBOSE(("Setting QP info,  LID = %d", context->remote_info.rem_lid));
    return OPAL_SUCCESS;

}

static void xoob_report_error(opal_common_ofacm_xoob_local_connection_context_t *xcontext)
{
    if (NULL == xcontext || NULL == (XOOB_TO_BASE(xcontext))->error_cb) {
        /* The context is undefined and we can not print specific error */
        opal_show_help("help-mpi-common-ofacm-oob.txt",
                "ofacm oob fatal error", true,
                opal_proc_local_get()->proc_hostname,
                __FILE__, __LINE__);
        exit(1);
    }

    /* Other way, call to user error callback */
    (XOOB_TO_BASE(xcontext))->error_cb((XOOB_TO_BASE(xcontext))->user_context);
}

static int xoob_context_init(opal_common_ofacm_xoob_local_connection_context_t *xcontext,
                                               opal_common_ofacm_xoob_module_t *xcpc,
                                               opal_common_ofacm_base_context_connect_cb_fn_t connect_cb,
                                               opal_common_ofacm_base_context_error_cb_fn_t error_cb,
                                               opal_common_ofacm_base_context_prepare_recv_cb_fn_t prepare_recv_cb,
                                               opal_common_ofacm_base_proc_t *proc,
                                               opal_common_ofacm_base_qp_config_t *qp_config,
                                               struct ibv_pd *pd, uint64_t subnet_id, int cpc_type,
                                               uint16_t lid, uint16_t rem_lid, 
                                               int32_t user_context_index, void *user_context)
{
    int ret;
    opal_common_ofacm_base_local_connection_context_t *context =
        XOOB_TO_BASE(xcontext);
    opal_common_ofacm_base_module_t *cpc = 
        (opal_common_ofacm_base_module_t *)xcpc;
    
    /* Set IB address for this context */
    xcontext->addr = xoob_ib_address_add_new(xcpc, rem_lid, subnet_id, proc->proc_opal->proc_name.jobid);
    if (NULL == xcontext->addr) {
        OFACM_ERROR(("Failed to allocate or found xoob ib address"));
        return OPAL_ERROR;
    } 

    /* Allocate memory for QPs */
    if (NULL == xcontext->addr->qps) {
        xcontext->addr->qps =
            calloc(qp_config->num_qps, sizeof(opal_common_ofacm_base_qp_t));
        if(NULL == xcontext->addr->qps) {
            OFACM_ERROR(("Failed to allocate memory for qps"));
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    /* Update QP pointers */
    context->qps = xcontext->addr->qps;

    /* Init base context */
    ret = opal_common_ofacm_base_context_init(context, cpc, connect_cb, error_cb, 
            prepare_recv_cb, proc, qp_config,
            pd, subnet_id, cpc_type, lid, rem_lid, user_context_index, user_context);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    return OPAL_SUCCESS;
}

/* XOOB connection context init */
static opal_common_ofacm_base_local_connection_context_t*
        xoob_endpoint_init(opal_proc_t *proc,
                          opal_common_ofacm_base_qp_config_t *qp_config,
                          struct ibv_pd *pd, uint64_t subnet_id, int cpc_type,
                          uint16_t lid, uint16_t rem_lid, int32_t user_context_index, void *user_context,
                          opal_common_ofacm_base_module_t *cpc,
                          opal_common_ofacm_base_context_connect_cb_fn_t connect_cb,
                          opal_common_ofacm_base_context_error_cb_fn_t error_cb,
                          opal_common_ofacm_base_context_prepare_recv_cb_fn_t prepare_recv_cb)
{
    int ret;
    bool new_proc;
    opal_common_ofacm_xoob_local_connection_context_t *xcontext;
    opal_common_ofacm_base_proc_t *context_proc;
    opal_common_ofacm_xoob_module_t *xcpc = 
        (opal_common_ofacm_xoob_module_t *)cpc;

    xcontext = (opal_common_ofacm_xoob_local_connection_context_t*) 
        OBJ_NEW(opal_common_ofacm_xoob_local_connection_context_t);
    context_proc = opal_common_ofacm_base_find_proc(&opal_common_ofacm_xoob, proc);

    if (NULL == context_proc) {
        new_proc = true;
        /* constructing new proc */
        context_proc = (opal_common_ofacm_base_proc_t *) 
            OBJ_NEW(opal_common_ofacm_base_proc_t );
    } else {
        new_proc = false;
        OBJ_RETAIN(context_proc);
    }

    OFACM_VERBOSE(("Xoob endpoint init: cpc_type %d, rem_lid %d, my_lid %d, subnet id %d",
                cpc_type, rem_lid, lid, subnet_id));

    opal_common_ofacm_base_proc_setup(context_proc, XOOB_TO_BASE(xcontext), proc);
    ret = xoob_context_init(xcontext, xcpc, connect_cb, error_cb, 
            prepare_recv_cb, context_proc, qp_config,
            pd, subnet_id, cpc_type, lid, rem_lid, user_context_index, user_context);
    if (OPAL_SUCCESS != ret) {
        OBJ_DESTRUCT(context_proc);
        OBJ_DESTRUCT(xcontext);
        return NULL;
    }
    if(new_proc) {
        opal_list_append(&opal_common_ofacm_xoob.all_procs, 
                (opal_list_item_t *)context_proc);
    }

    return &xcontext->super;
}

static int xoob_endpoint_finalize
            (opal_common_ofacm_base_local_connection_context_t *context)
{
    opal_list_item_t *proc_item, *cntx_item, *cntx_item_next;
    opal_list_t *proc_list = &opal_common_ofacm_xoob.all_procs;
    opal_common_ofacm_xoob_local_connection_context_t *xcontext;

    /* Proc cleanup. We should find the context proc in all proc list and remove
     * from the proc list our context. After it we try to release the proc context */
    for (proc_item = opal_list_get_first(proc_list);
            proc_item != opal_list_get_end(proc_list);
            proc_item = opal_list_get_next(proc_item)) {
        if (context->proc == ((opal_common_ofacm_base_proc_t *)proc_item)){
            opal_common_ofacm_base_proc_t *proc = 
                (opal_common_ofacm_base_proc_t *)proc_item;
            opal_list_t *cntx_list = &proc->all_contexts;

            /* Remove the context from proc list */
            cntx_item = opal_list_get_first(cntx_list);
            while(cntx_item != opal_list_get_end(cntx_list)) {
                /* take the next before removing from the list */
                cntx_item_next = opal_list_get_next(cntx_item);
                if (context == (opal_common_ofacm_base_local_connection_context_t *)cntx_item) {
                    opal_list_remove_item(cntx_list, cntx_item);
                }
                cntx_item = cntx_item_next;
            }
            /* Remove our proc from all list */
            if (opal_list_is_empty(cntx_list)) {
                opal_list_remove_item(proc_list, (opal_list_item_t *)proc);
            }
            OBJ_RELEASE(proc);
        }
    }

    if (0 != context->xrc_recv_qp_num) {
        if(ibv_unreg_xrc_rcv_qp(context->init_attr[0].xrc_domain,
                    context->xrc_recv_qp_num)) {
            OFACM_ERROR(("Failed to unregister XRC recv QP:%d\n", context->xrc_recv_qp_num));
        }
    }

    xcontext = BASE_TO_XOOB(context);

    /* We done with proc release and now we way destroy the context */
    OBJ_DESTRUCT(xcontext);

    return OPAL_SUCCESS;
}

/*
 * Callback when we have finished RML sending the connect data to a
 * remote peer
 */
static void xoob_rml_send_cb(int status, opal_process_name_t* context,
                        opal_buffer_t* buffer, ompi_rml_tag_t tag,
                        void* cbdata)
{
    OBJ_RELEASE(buffer);
}

/* Receive connect information to remote context */
static int xoob_receive_connect_data(opal_common_ofacm_base_remote_connection_context_t *info, uint16_t *lid, int *cpc_type,
        uint8_t *message_type, opal_buffer_t* buffer)
{
    int cnt = 1, rc, srq;
    uint8_t num_srqs;

    /* Recv standart header */
    OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT8));
    rc = opal_dss.unpack(buffer, message_type, &cnt, OPAL_UINT8);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return OPAL_ERROR;
    }
    OFACM_VERBOSE(("Recv unpack Message type  = %d", *message_type));

    OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT64));
    rc = opal_dss.unpack(buffer, &info->rem_subnet_id, &cnt, OPAL_UINT64);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return OPAL_ERROR;
    }
    OFACM_VERBOSE(("Recv unpack sid  = %d", info->rem_subnet_id));

    OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
    rc = opal_dss.unpack(buffer, &info->rem_lid, &cnt, OPAL_UINT16);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return OPAL_ERROR;
    }
    OFACM_VERBOSE(("Recv unpack lid  = %d", info->rem_lid));

    OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_INT));
    rc = opal_dss.unpack(buffer, cpc_type, &cnt, OPAL_INT);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return OPAL_ERROR;
    }
    OFACM_VERBOSE(("Recv unpack cpc_type  = %d", *cpc_type));

    /* Till now we got the standart header, now we continue to recieve data for
     * different packet types
     */
    if (ENDPOINT_XOOB_CONNECT_REQUEST == *message_type ||
            ENDPOINT_XOOB_CONNECT_RESPONSE == *message_type) {
        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &info->rem_qps->rem_qp_num, &cnt,
                OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return OPAL_ERROR;
        }
        OFACM_VERBOSE(("Recv unpack remote qp  = %x", info->rem_qps->rem_qp_num));

        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &info->rem_qps->rem_psn, &cnt,
                OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return OPAL_ERROR;
        }
        OFACM_VERBOSE(("Recv unpack remote psn = %d", info->rem_qps->rem_psn));

        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &info->rem_mtu, &cnt, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return OPAL_ERROR;
        }
        OFACM_VERBOSE(("Recv unpack remote mtu = %d", info->rem_mtu));
    }

    if (ENDPOINT_XOOB_CONNECT_REQUEST == *message_type ||
            ENDPOINT_XOOB_CONNECT_XRC_REQUEST == *message_type) {
        /* unpack requested lid info */
        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
        rc = opal_dss.unpack(buffer, lid, &cnt, OPAL_UINT16);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return OPAL_ERROR;
        }
        OFACM_VERBOSE(("Recv unpack requested lid = %d", *lid));
    }

    /* Unpack requested recv qp number */
    if (ENDPOINT_XOOB_CONNECT_XRC_REQUEST == *message_type) {
        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        /* In XRC request case we will use rem_qp_num as container for requested qp number */
        rc = opal_dss.unpack(buffer, &info->rem_qps->rem_qp_num, &cnt,
                OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        OFACM_VERBOSE(("Recv unpack requested qp = %x", info->rem_qps->rem_qp_num));
    }

    if (ENDPOINT_XOOB_CONNECT_RESPONSE == *message_type ||
            ENDPOINT_XOOB_CONNECT_XRC_RESPONSE == *message_type) {
        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &info->rem_index, &cnt, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return OPAL_ERROR;
        }
        OFACM_VERBOSE(("Recv unpack remote index = %d", info->rem_index));

        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT8));
        rc = opal_dss.unpack(buffer, &num_srqs, &cnt, OPAL_UINT8);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return OPAL_ERROR;
        }
        OFACM_VERBOSE(("Recv unpack remote num of srqs  = %d", num_srqs));
        
        rc = xoob_init_rem_info_alloc_srq(info, num_srqs);
        if (OPAL_SUCCESS != rc) {
            return OPAL_ERROR;
        }
        for (srq = 0; srq < num_srqs; srq++) {
            OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT8));
            rc = opal_dss.unpack(buffer, &info->rem_srqs[srq].rem_srq_num, &cnt, OPAL_UINT32);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                return OPAL_ERROR;
            }
            OFACM_VERBOSE(("Recv unpack remote index srq num[%d]= %d", srq, info->rem_srqs[srq].rem_srq_num));
        }
    }
    return OPAL_SUCCESS;
}

/*
 * send connect information to remote context
 */
static int xoob_send_connect_data(opal_common_ofacm_xoob_local_connection_context_t* xcontext,
        uint8_t message_type)
{
    opal_buffer_t* buffer = OBJ_NEW(opal_buffer_t);
    int rc, srq;
    opal_common_ofacm_base_local_connection_context_t *context = XOOB_TO_BASE(xcontext);

    if (NULL == buffer) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Bulding standart header that we use in all messages:
     * - Message type,
     * - Our subnet id
     * - Our LID
     */
    /* pack the info in the send buffer */
    OFACM_VERBOSE(("Send pack Message type = %d", message_type));
    OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT8));
    rc = opal_dss.pack(buffer, &message_type, 1, OPAL_UINT8);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    OFACM_VERBOSE(("Send pack sid = %d", context->subnet_id));
    OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT64));
    rc = opal_dss.pack(buffer, &context->subnet_id, 1, OPAL_UINT64);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    OFACM_VERBOSE(("Send pack lid = %d", context->lid));
    OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
    rc = opal_dss.pack(buffer, &context->lid, 1, OPAL_UINT16);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    OFACM_VERBOSE(("Send pack cpc type = %d", context->cpc_type));
    OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_INT));
    rc = opal_dss.pack(buffer, &context->cpc_type, 1, OPAL_INT);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    /* Now we append to standart header additional information
     * that is required for full (open qp,etc..) connect request and response:
     * - qp_num of first qp
     * - psn of first qp
     * - MTU
     */
    if (ENDPOINT_XOOB_CONNECT_REQUEST == message_type ||
            ENDPOINT_XOOB_CONNECT_RESPONSE == message_type) {
        uint32_t psn, qp_num;

        if (ENDPOINT_XOOB_CONNECT_REQUEST == message_type) {
            qp_num = context->qps[0].lcl_qp->qp_num;
            psn = context->qps[0].lcl_psn;
        } else {
            qp_num = context->xrc_recv_qp_num;
            psn = xcontext->xrc_recv_psn;
        }
        /* stuff all the QP info into the buffer */
        /* we need to send only one QP */
        OFACM_VERBOSE(("Send pack qp num = %x", qp_num));
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &qp_num, 1, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        OFACM_VERBOSE(("Send pack lpsn = %d", psn));
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &psn, 1, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }

        OFACM_VERBOSE(("Send pack mtu = %d", context->attr[0].path_mtu));
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &context->attr[0].path_mtu, 1,
                OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
    }

    /* We append to header above additional information
     * that is required for full & XRC connect request:
     * - The lid ob btl on remote site that we want to connect
     */
    if (ENDPOINT_XOOB_CONNECT_REQUEST == message_type ||
            ENDPOINT_XOOB_CONNECT_XRC_REQUEST == message_type) {
        /* when we are sending request we add remote lid that we want to connect */

        OFACM_VERBOSE(("Send pack remote lid = %d", context->rem_lid));
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
        rc = opal_dss.pack(buffer, &context->rem_lid, 1, OPAL_UINT16);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
    }

    /* when we are sending xrc request we add remote
     * recv qp number that we want to connect. */
    if (ENDPOINT_XOOB_CONNECT_XRC_REQUEST == message_type) {
        OFACM_VERBOSE(("Send pack remote qp = %x", xcontext->addr->remote_xrc_rcv_qp_num));
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &xcontext->addr->remote_xrc_rcv_qp_num,
                1, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
    }
    /* We append to header above additional information
     * that is required for full & XRC connect response:
     * - index of our context
     * - array of xrc-srq numbers
     */
    if (ENDPOINT_XOOB_CONNECT_RESPONSE == message_type ||
            ENDPOINT_XOOB_CONNECT_XRC_RESPONSE == message_type) {
        /* we need to send the context index for immidate send */
        OFACM_VERBOSE(("Send pack index = %d", context->index));
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &context->index, 1, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }

        OFACM_VERBOSE(("Send pack number of srqs = %d", context->num_of_srqs));
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT8));
        rc = opal_dss.pack(buffer, &context->num_of_srqs, 1, OPAL_UINT8);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        /* on response we add all SRQ numbers */
        for (srq = 0; srq < context->num_of_srqs; srq++) {
            OFACM_VERBOSE(("Send pack srq[%d] num  = %d", srq, context->srq_num[srq]));
            OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
            rc = opal_dss.pack(buffer, &context->srq_num[srq],
                    1, OPAL_UINT32);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    /* send to remote endpoint */
    rc = ompi_rte_send_buffer_nb(&context->proc->proc_opal->proc_name,
            buffer, OMPI_RML_TAG_XOFACM,
            xoob_rml_send_cb, NULL);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    OFACM_VERBOSE(("Send QP Info, LID = %d, SUBNET = %d, Message type = %d",
                context->lid,
                context->subnet_id,
                message_type));

    return OPAL_SUCCESS;
}

/* Create XRC send qp */
static int xoob_send_qp_create 
           (opal_common_ofacm_xoob_local_connection_context_t* xcontext)
{
    struct ibv_qp *qp;
    struct ibv_qp_init_attr init_attr;
    struct ibv_qp_attr attr;
    int ret;
    size_t req_inline;
    uint32_t init_mask = 0;
    opal_common_ofacm_base_local_connection_context_t *context = XOOB_TO_BASE(xcontext);

    /* Prepare QP structs */
    memcpy(&init_attr, &context->init_attr[0], sizeof(init_attr));
    req_inline = init_attr.cap.max_inline_data;
    qp = ibv_create_qp(context->ib_pd, &init_attr);
    if (NULL == qp) {
        OFACM_ERROR(("Error creating QP, errno says: %s", strerror(errno)));
        return OPAL_ERROR;
    }

    context->qps[0].lcl_qp = qp;

    if (init_attr.cap.max_inline_data < req_inline) {
        context->qps[0].ib_inline_max = init_attr.cap.max_inline_data;
        opal_show_help("help-mpi-common-ofacm-base.txt",
                       "inline truncated", true, opal_proc_local_get()->proc_hostname,
                       req_inline, init_attr.cap.max_inline_data);
    } else {
        context->qps[0].ib_inline_max = req_inline;
    }

    memcpy(&attr, &context->attr[0], sizeof(attr));
    attr.qp_state = IBV_QPS_INIT;
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;
    init_mask = IBV_QP_STATE        |
                IBV_QP_PKEY_INDEX   |
                IBV_QP_PORT         |
                IBV_QP_ACCESS_FLAGS;

    /* applying user specified init mask */
    if (NULL != context->custom_init_attr_mask) {
        init_mask |= context->custom_init_attr_mask[0];
    }

    ret = ibv_modify_qp(qp, &attr, init_mask);
    if (ret) {
        OFACM_ERROR(("Error modifying QP[%x] to IBV_QPS_INIT errno says: %s [%d]",
                    qp->qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }

    /* Setup meta data on the context */
    context->qps[0].lcl_psn = opal_rand(&rand_buff) & 0xffffff;

    /* Now that all the qp's are created locally, post some receive
       buffers, setup credits, etc. */
    return context->prepare_recv_cb(context->user_context);
}

/* Send qp connect */
static int xoob_send_qp_connect(opal_common_ofacm_xoob_local_connection_context_t *xcontext)
{
    struct ibv_qp* qp;
    struct ibv_qp_attr attr;
    uint32_t psn, rtr_mask = 0, rts_mask = 0;
    int ret;
    opal_common_ofacm_base_local_connection_context_t *context = XOOB_TO_BASE(xcontext);
    enum ibv_mtu mtu = (context->attr[0].path_mtu < context->remote_info.rem_mtu) ?
        context->attr[0].path_mtu : context->remote_info.rem_mtu;

    OFACM_VERBOSE(("Connecting Send QP\n"));
    assert(NULL != context->qps);
    qp = context->qps[0].lcl_qp;
    psn = context->qps[0].lcl_psn;

    memset(&attr, 0, sizeof(attr));
    memcpy(&attr, context->attr, sizeof(struct ibv_qp_attr));
    attr.qp_state           = IBV_QPS_RTR;
    attr.path_mtu           = mtu;
    attr.dest_qp_num        = context->remote_info.rem_qps[0].rem_qp_num;
    attr.rq_psn             = context->remote_info.rem_qps[0].rem_psn;
    attr.ah_attr.dlid          = context->remote_info.rem_lid;
    attr.ah_attr.static_rate   = 0;
    rtr_mask = IBV_QP_STATE              |
               IBV_QP_AV                 |
               IBV_QP_PATH_MTU           |
               IBV_QP_DEST_QPN           |
               IBV_QP_RQ_PSN             |
               IBV_QP_MAX_DEST_RD_ATOMIC |
               IBV_QP_MIN_RNR_TIMER;

    /* applying user specified rtr mask */
    if (NULL != context->custom_rtr_attr_mask) {
        rtr_mask |= context->custom_rtr_attr_mask[0];
    }

    OFACM_VERBOSE(("Set MTU to IBV value %d (%s bytes)", attr.path_mtu,
                (attr.path_mtu == IBV_MTU_256) ? "256" :
                (attr.path_mtu == IBV_MTU_512) ? "512" :
                (attr.path_mtu == IBV_MTU_1024) ? "1024" :
                (attr.path_mtu == IBV_MTU_2048) ? "2048" :
                (attr.path_mtu == IBV_MTU_4096) ? "4096" :
                "unknown (!)"));

    ret = ibv_modify_qp(qp, &attr, rtr_mask);
    if (ret) {
        OFACM_ERROR(("Error modifying QP[%x] to IBV_QPS_RTR errno says: %s [%d]",
                    qp->qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }

    attr.qp_state       = IBV_QPS_RTS;
    attr.sq_psn         = context->qps[0].lcl_psn;
    /* applying user specified rts mask */
    rts_mask = IBV_QP_STATE              |
               IBV_QP_TIMEOUT            |
               IBV_QP_RETRY_CNT          |
               IBV_QP_RNR_RETRY          |
               IBV_QP_SQ_PSN             |
               IBV_QP_MAX_QP_RD_ATOMIC;

    /* applying user specified rts mask */
    
    if (NULL != context->custom_rts_attr_mask) {
        rts_mask |= context->custom_rts_attr_mask[0];
    }

    ret = ibv_modify_qp(qp, &attr, rts_mask);
    if (ret) {
        OFACM_ERROR(("Error modifying QP[%x] to IBV_QPS_RTS errno says: %s [%d]",
                    qp->qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

/* Recv qp create */
static int xoob_recv_qp_create(opal_common_ofacm_xoob_local_connection_context_t *xcontext,
        opal_common_ofacm_base_remote_connection_context_t *remote_info)
{
    struct ibv_qp_init_attr init_attr;
    struct ibv_qp_attr attr;
    int ret;
    uint32_t init_mask = 0, rtr_mask = 0;
    struct ibv_xrc_domain *xrc_domain;
    opal_common_ofacm_base_local_connection_context_t *context = XOOB_TO_BASE(xcontext);
    enum ibv_mtu mtu = (context->attr[0].path_mtu < remote_info->rem_mtu) ?
        context->attr[0].path_mtu : remote_info->rem_mtu;

    OFACM_VERBOSE(("Connecting Recv QP\n"));

    memcpy(&init_attr, &context->init_attr[0], sizeof(init_attr));
    xrc_domain = init_attr.xrc_domain;
    /* Only xrc_domain is required, all other are ignored */
    ret = ibv_create_xrc_rcv_qp(&init_attr, &context->xrc_recv_qp_num);
    if (ret) {
        OFACM_ERROR(("Error creating XRC recv QP[%x], errno says: %s [%d]",
                    context->xrc_recv_qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }

    memcpy(&attr, &context->attr[0], sizeof(attr));
    attr.qp_state = IBV_QPS_INIT;
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;
    init_mask = IBV_QP_STATE        |
                IBV_QP_PKEY_INDEX   |
                IBV_QP_PORT         |
                IBV_QP_ACCESS_FLAGS;

    /* applying user specified init mask */
    if (NULL != context->custom_init_attr_mask) {
        init_mask |= context->custom_init_attr_mask[0];
    }

    ret = ibv_modify_xrc_rcv_qp(xrc_domain, context->xrc_recv_qp_num,
            &attr, init_mask);
    if (ret) {
        OFACM_ERROR(("Error modifying XRC recv QP[%x] to IBV_QPS_INIT, errno says: %s [%d]",
                     context->xrc_recv_qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }

    memcpy(&attr, &context->attr[0], sizeof(attr));
    attr.qp_state              = IBV_QPS_RTR;
    attr.path_mtu              = mtu; 
    attr.dest_qp_num           = remote_info->rem_qps[0].rem_qp_num;
    attr.rq_psn                = remote_info->rem_qps[0].rem_psn;
    attr.ah_attr.dlid          = remote_info->rem_lid;
    attr.ah_attr.static_rate   = 0;
    rtr_mask = IBV_QP_STATE             |
               IBV_QP_AV                |
               IBV_QP_PATH_MTU          |
               IBV_QP_DEST_QPN          |
               IBV_QP_RQ_PSN            |
               IBV_QP_MAX_DEST_RD_ATOMIC|
               IBV_QP_MIN_RNR_TIMER;

    /* applying user specified rtr mask */
    if (NULL != context->custom_rtr_attr_mask) {
        rtr_mask |= context->custom_rtr_attr_mask[0];
    }

    ret = ibv_modify_xrc_rcv_qp(xrc_domain, context->xrc_recv_qp_num,
            &attr, rtr_mask);
    if (ret) {
        OFACM_ERROR(("Error modifying XRC recv QP[%x] to IBV_QPS_RTR, errno says: %s [%d]",
                    context->xrc_recv_qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

/* Recv qp connect */
static int xoob_recv_qp_connect(opal_common_ofacm_xoob_local_connection_context_t *xcontext, 
        opal_common_ofacm_base_remote_connection_context_t *rem_info)
{
    int ret;
    opal_common_ofacm_base_local_connection_context_t *context = XOOB_TO_BASE(xcontext);

    struct ibv_xrc_domain *xrc_domain = context->init_attr[0].xrc_domain;

    OFACM_VERBOSE(("Connecting Recv QP\n"));
    ret = ibv_reg_xrc_rcv_qp(xrc_domain, rem_info->rem_qps->rem_qp_num);
    if (ret) { /* failed to regester the qp, so it is already die and we should create new one */
       /* Return NOT READY !!!*/
        OFACM_ERROR(("Failed to register qp_num: %d , get error: %s (%d)\n. Replying with RNR",
                    rem_info->rem_qps->rem_qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    } else {
        /* save the qp number for unregister */
        context->xrc_recv_qp_num = rem_info->rem_qps->rem_qp_num;
        return OPAL_SUCCESS;
    }
}

/*
 * Reply to a `start - connect' message
 */
static int xoob_reply_first_connect(opal_common_ofacm_xoob_local_connection_context_t *xcontext,
        opal_common_ofacm_base_remote_connection_context_t *remote_info)
{
    int rc;
    opal_common_ofacm_base_local_connection_context_t *context = 
                                                       XOOB_TO_BASE(xcontext);

    OFACM_VERBOSE(("Initialized QPs, LID = %d", (XOOB_TO_BASE(xcontext))->lid));

    /* Create local QP's and post receive resources */
    if (OPAL_SUCCESS != (rc = xoob_recv_qp_create(xcontext, remote_info))) {
        return rc;
    }

    /* prepost data on receiver site */
    if (OPAL_SUCCESS != (rc = context->prepare_recv_cb(context->user_context))) {
        OFACM_ERROR(("Failed to post on XRC SRQs"));
        xoob_report_error(xcontext);
        return rc;
    }

    if (OPAL_SUCCESS !=
        (rc = xoob_send_connect_data(xcontext, ENDPOINT_XOOB_CONNECT_RESPONSE))) {
        OFACM_ERROR(("Error in send connect request error code is %d",
                   rc));
        return rc;
    }

    return OPAL_SUCCESS;
}

/* Find context for specific subnet/lid/message/cpc type */
static opal_common_ofacm_xoob_local_connection_context_t* xoob_find_context
                        (opal_process_name_t* process_name, uint64_t subnet_id, 
                         uint16_t lid, uint8_t message_type, int cpc_type)
{
    opal_common_ofacm_xoob_local_connection_context_t *xcontext = NULL;
    opal_common_ofacm_base_proc_t *context_proc = NULL;
    bool found = false;
    opal_list_t *all_procs = 
        &opal_common_ofacm_xoob.all_procs;

    OFACM_VERBOSE(("Searching for ep and proc with follow parameters:"
                "jobid %d, vpid %d, sid %d, lid %d, cpc type %d",
                process_name->jobid, process_name->vpid, subnet_id, lid, cpc_type));
    /* find ibproc */
    for (context_proc  = (opal_common_ofacm_base_proc_t*)opal_list_get_first(all_procs);
         context_proc != (opal_common_ofacm_base_proc_t*)opal_list_get_end(all_procs);
         context_proc  = (opal_common_ofacm_base_proc_t*)opal_list_get_next(context_proc)) {
        if (ompi_rte_compare_name_fields(OMPI_RTE_CMP_ALL,
                    &context_proc->proc_opal->proc_name, process_name) == OPAL_EQUAL) {
            found = true;
            break;
        }
    }

    /* we found our context_proc, lets find context now */
    if (found) {
        opal_list_t *context_list = &context_proc->all_contexts; 
        opal_common_ofacm_base_local_connection_context_t *context;
        for (context  = (opal_common_ofacm_base_local_connection_context_t *)
                         opal_list_get_first(context_list);
             context != (opal_common_ofacm_base_local_connection_context_t *)
                         opal_list_get_end(context_list);
             context = (opal_common_ofacm_base_local_connection_context_t *)
                         opal_list_get_next(context)) {
            /* we need to check different
             * lid for different message type */
            if (ENDPOINT_XOOB_CONNECT_RESPONSE == message_type ||
                    ENDPOINT_XOOB_CONNECT_XRC_RESPONSE == message_type) {
                /* response message */
                if (context->subnet_id == subnet_id &&
                        context->rem_lid == lid) {
                    xcontext = BASE_TO_XOOB(context);
                    break; /* Found one */
                }
            } else {
                /* request message */
                if (context->subnet_id == subnet_id &&
                        context->lid == lid) {
                    xcontext = BASE_TO_XOOB(context);
                    break; /* Found one */
                }
            }
        }
        if (NULL == xcontext) {
            OFACM_ERROR(("can't find suitable context for this peer\n"));
        }
    } else {
            OFACM_ERROR(("can't find suitable context for this peer\n"));
    }
    return xcontext;
}

/* In case if XRC recv qp was closed and sender still don't know about it
 * we need close the qp, reset the ib_adrr status to CLOSED and start everything
 * from scratch.
 */
static void xoob_restart_connect
            (opal_common_ofacm_xoob_local_connection_context_t *xcontext)
{
    opal_common_ofacm_base_local_connection_context_t *context = 
        XOOB_TO_BASE(xcontext);
    OFACM_VERBOSE(("Restarting the connection for the context"));
    OPAL_THREAD_LOCK(&xcontext->addr->addr_lock);
    switch (xcontext->addr->status) {
        case XOOB_ADDR_CONNECTED:
            /* so we have the send qp, we just need the recive site.
             * Send request for SRQ numbers */
            OFACM_VERBOSE(("Restart The IB addr: sid %d lid %d"
                        "in XOOB_ADDR_CONNECTED status,"
                        " Changing to XOOB_ADDR_CLOSED and starting from scratch\n",
                        context->subnet_id, context->lid));
            /* Switching back to closed and starting from scratch */
            xcontext->addr->status = XOOB_ADDR_CLOSED;
            /* destroy the qp */
            if(ibv_destroy_qp(context->qps[0].lcl_qp))
                OFACM_ERROR(("Failed to destroy QP"));
        case XOOB_ADDR_CLOSED:
        case XOOB_ADDR_CONNECTING:
            OFACM_VERBOSE(("Restart The IB addr: sid %d lid %d"
                        "in XOOB_ADDR_CONNECTING or XOOB_ADDR_CLOSED status,"
                        " starting from scratch\n",
                        context->subnet_id, context->lid));
            OPAL_THREAD_UNLOCK(&xcontext->addr->addr_lock);
            /* xoob_module_start_connect() should automaticly handle all other cases */
            if (OPAL_SUCCESS != xoob_module_start_connect(XOOB_TO_BASE(xcontext)))
                OFACM_ERROR(("Failed to restart connection from XOOB_ADDR_CONNECTING/CLOSED"));
            break;
        default :
            OFACM_ERROR(("Invalid context status %d", xcontext->addr->status));
            OPAL_THREAD_UNLOCK(&xcontext->addr->addr_lock);
    }
}

/*
 * Non blocking RML recv callback.  Read incoming QP and other info,
 * and if this endpoint is trying to connect, reply with our QP info,
 * otherwise try to modify QP's and establish reliable connection
 */
static void xoob_rml_recv_cb(int status, opal_process_name_t* process_name,
                        opal_buffer_t* buffer, ompi_rml_tag_t tag,
                        void* cbdata)
{
    int rc;
    uint8_t message_type;
    uint16_t requested_lid = 0;
    int cpc_type = -1;
    opal_common_ofacm_base_local_connection_context_t *context;
    opal_common_ofacm_xoob_local_connection_context_t *xcontext;
    opal_common_ofacm_base_remote_connection_context_t remote_info;

    /* Init remote info */
    memset(&remote_info, 0, 
            sizeof(opal_common_ofacm_base_remote_connection_context_t));

    if ( OPAL_SUCCESS != xoob_init_rem_info_alloc_qp(&remote_info)) {
        return;
    }

    /* Get data. */
    if ( OPAL_SUCCESS != 
            xoob_receive_connect_data(&remote_info, &requested_lid, &cpc_type, &message_type, buffer)) {
        OFACM_ERROR(("Failed to read data\n"));
        xoob_report_error(NULL);
        return;
    }

    /* Processing message */
    switch (message_type) {
        case ENDPOINT_XOOB_CONNECT_REQUEST:
            OFACM_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_REQUEST: lid %d, sid %d, rlid %d\n",
                        remote_info.rem_lid,
                        remote_info.rem_subnet_id,
                        requested_lid));
            xcontext = xoob_find_context(process_name,remote_info.rem_subnet_id,
                    requested_lid, message_type, cpc_type);
            if ( NULL == xcontext) {
                OFACM_ERROR(("Got ENDPOINT_XOOB_CONNECT_REQUEST."
                            " Failed to find context with subnet %d and LID %d",
                            remote_info.rem_subnet_id, requested_lid));
                xoob_free_rem_info(&remote_info);
                xoob_report_error(xcontext);
                return;
            }
            context = XOOB_TO_BASE(xcontext);
            OPAL_THREAD_LOCK(&context->context_lock);
            /* we should create qp and send the info + srq to requestor */
            rc = xoob_reply_first_connect(xcontext, &remote_info);
            if (OPAL_SUCCESS != rc) {
                OFACM_ERROR(("error in context reply start connect"));
                xoob_free_rem_info(&remote_info);
                xoob_report_error(xcontext);
                return;
            }
            /* enable pooling for this btl */
            OPAL_THREAD_UNLOCK(&context->context_lock);
            break;
        case ENDPOINT_XOOB_CONNECT_XRC_REQUEST:
            OFACM_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_XRC_REQUEST: lid %d, sid %d\n",
                        remote_info.rem_lid,
                        remote_info.rem_subnet_id));
            xcontext = xoob_find_context(process_name, remote_info.rem_subnet_id,
                    requested_lid, message_type, cpc_type);
            if (NULL == xcontext) {
                OFACM_ERROR(("Got ENDPOINT_XOOB_CONNECT_XRC_REQUEST."
                            " Failed to find context with subnet %d and LID %d",
                            remote_info.rem_subnet_id, requested_lid));
                xoob_free_rem_info(&remote_info);
                xoob_report_error(xcontext);
                return;
            }

            context = XOOB_TO_BASE(xcontext);

            if (OPAL_SUCCESS == xoob_recv_qp_connect(xcontext, &remote_info)) {
                if (OPAL_SUCCESS != context->prepare_recv_cb(context->user_context)) {
                    OFACM_ERROR(("Failed to post on XRC SRQs"));
                    xoob_free_rem_info(&remote_info);
                    xoob_report_error(xcontext);
                    return;
                }
                OPAL_THREAD_LOCK(&context->context_lock);
                rc = xoob_send_connect_data(xcontext, ENDPOINT_XOOB_CONNECT_XRC_RESPONSE);
                if (OPAL_SUCCESS != rc) {
                    OFACM_ERROR(("error in context reply start connect"));
                    xoob_free_rem_info(&remote_info);
                    xoob_report_error(xcontext);
                    return;
                }
                OPAL_THREAD_UNLOCK(&context->context_lock);
            } else {
                /* The XRC recv qp was destroyed */
                OPAL_THREAD_LOCK(&context->context_lock);
                rc = xoob_send_connect_data(xcontext, ENDPOINT_XOOB_CONNECT_XRC_NR_RESPONSE);
                if (OPAL_SUCCESS != rc) {
                    OFACM_ERROR(("error in context reply start connect"));
                    xoob_free_rem_info(&remote_info);
                    xoob_report_error(xcontext);
                    return;
                }
                OPAL_THREAD_UNLOCK(&context->context_lock);
            }
            break;
        case ENDPOINT_XOOB_CONNECT_RESPONSE:
            OFACM_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_RESPONSE: lid %d, sid %d\n",
                        remote_info.rem_lid,
                        remote_info.rem_subnet_id));
            xcontext = xoob_find_context(process_name, remote_info.rem_subnet_id,
                    remote_info.rem_lid, message_type, cpc_type);
            if (NULL == xcontext) {
                OFACM_ERROR(("Got ENDPOINT_XOOB_CONNECT_RESPONSE."
                            " Failed to find context with subnet %d and LID %d",
                            remote_info.rem_subnet_id, remote_info.rem_lid));
                xoob_free_rem_info(&remote_info);
                xoob_report_error(xcontext);
                return;
            }

            context = XOOB_TO_BASE(xcontext);
            OPAL_THREAD_LOCK(&context->context_lock);
            /* we got all the data srq. switch the context to connect mode */
            xoob_set_remote_info(xcontext, &remote_info);
            /* update ib_addr with remote qp number */
            xcontext->addr->remote_xrc_rcv_qp_num =
                remote_info.rem_qps->rem_qp_num;
            OFACM_VERBOSE(("rem_info: lid %d, sid %d ep %d %d",
                        remote_info.rem_lid,
                        remote_info.rem_subnet_id,
                        context->remote_info.rem_lid, 
                        context->remote_info.rem_subnet_id));
            if (OPAL_SUCCESS != xoob_send_qp_connect(xcontext)) {
                OFACM_ERROR(("Failed to connect context\n"));
                xoob_free_rem_info(&remote_info);
                xoob_report_error(xcontext);
                return;
            }
            xoob_connection_complete(xcontext);
            OPAL_THREAD_UNLOCK(&context->context_lock);
            break;
        case ENDPOINT_XOOB_CONNECT_XRC_RESPONSE:
            OFACM_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_XRC_RESPONSE: lid %d, sid %d\n",
                        remote_info.rem_lid,
                        remote_info.rem_subnet_id));
            xcontext = xoob_find_context(process_name, remote_info.rem_subnet_id,
                    remote_info.rem_lid, message_type, cpc_type);
            if ( NULL == xcontext) {
                OFACM_ERROR(("Got ENDPOINT_XOOB_CONNECT_XRC_RESPONSE."
                            " Failed to find context with subnet %d and LID %d",
                            remote_info.rem_subnet_id, remote_info.rem_lid));
                xoob_report_error(xcontext);
                return;
            }
            context = XOOB_TO_BASE(xcontext);
            OPAL_THREAD_LOCK(&context->context_lock);
            /* we got srq numbers on our request */
            xoob_set_remote_info(xcontext, &remote_info);
            xoob_connection_complete(xcontext);
            OPAL_THREAD_UNLOCK(&context->context_lock);
            break;
        case ENDPOINT_XOOB_CONNECT_XRC_NR_RESPONSE:
            /* The XRC recv site already was destroyed so we need
             * start to bringup the connection from scratch  */
            OFACM_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_XRC_NR_RESPONSE: lid %d, sid %d\n",
                        remote_info.rem_lid,
                        remote_info.rem_subnet_id));
            xcontext = xoob_find_context(process_name, remote_info.rem_subnet_id,
                    remote_info.rem_lid, message_type, cpc_type);
            if ( NULL == xcontext) {
                OFACM_ERROR(("Got ENDPOINT_XOOB_CONNECT_XRC_NR_RESPONSE."
                            " Failed to find context with subnet %d and LID %d",
                            remote_info.rem_subnet_id, remote_info.rem_lid));
                xoob_report_error(xcontext);
                return;
            }
            xoob_restart_connect(xcontext);
            break;
        default :
            OFACM_ERROR(("Invalid message type %d", message_type));
    }

    xoob_free_rem_info(&remote_info);
}

/*
 * XOOB interface functions
 */

/* Quere for the XOOB priority - will be highest in XRC case */
static int xoob_component_query(opal_common_ofacm_base_dev_desc_t *dev, 
                               opal_common_ofacm_base_module_t **cpc)
{
    opal_common_ofacm_xoob_module_t *xcpc; /* xoob cpc module */
    opal_common_ofacm_base_module_t *bcpc; /* base cpc module */

    if (xoob_priority > 100) {
        xoob_priority = 100;
    } else if (xoob_priority < -1) {
        xoob_priority = -1;
    }

    if (!(dev->capabilities & OPAL_COMMON_OFACM_XRC_ONLY)) {
        OFACM_VERBOSE(("openib BTL: xoob CPC only supported with XRC receive queues; skipped on device %s",
                            ibv_get_device_name(dev->ib_dev)));
        return OPAL_ERR_NOT_SUPPORTED;
    }

    xcpc = malloc(sizeof(opal_common_ofacm_xoob_module_t));
    if (NULL == xcpc) {
        OFACM_VERBOSE(("openib BTL: xoob CPC system error (malloc failed)"));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    bcpc = &xcpc->super;
    
    /* If this btl supports XOOB, then post the RML message.  But
       ensure to only post it *once*, because another btl may have
       come in before this and already posted it. */
    if (!rml_recv_posted) {
        ompi_rte_recv_buffer_nb(OMPI_NAME_WILDCARD, 
                                OMPI_RML_TAG_XOFACM,
                                OMPI_RML_PERSISTENT,
                                xoob_rml_recv_cb,
                                NULL);
        rml_recv_posted = true;
    }
        
    OBJ_CONSTRUCT(&opal_common_ofacm_xoob.all_procs, opal_list_t);
    bcpc->data.cbm_component = &opal_common_ofacm_xoob;
    bcpc->data.cbm_priority = xoob_priority;
    bcpc->data.cbm_modex_message = NULL;
    bcpc->data.cbm_modex_message_len = 0;

    bcpc->cbm_endpoint_init = xoob_endpoint_init;
    bcpc->cbm_start_connect = xoob_module_start_connect;
    bcpc->cbm_endpoint_finalize = xoob_endpoint_finalize;
    bcpc->cbm_finalize = NULL;
    bcpc->cbm_uses_cts = false;

    /* seed RNG */
    opal_srand(&rand_buff,(uint32_t)(getpid()));
    /* Build our hash table for subnetid-lid */
    OBJ_CONSTRUCT(&xcpc->ib_addr_table, opal_hash_table_t);

    assert(ompi_process_info.num_procs > 1);
    if(NULL == xcpc->ib_addr_table.ht_table) {
        if(OPAL_SUCCESS != opal_hash_table_init(
                    &xcpc->ib_addr_table, ompi_process_info.num_procs)) {
            OFACM_ERROR(("XRC internal error. Failed to allocate ib_table"));
            return OPAL_ERROR;
        }
    }

    *cpc = bcpc;

    OFACM_VERBOSE(("openib BTL: xoob CPC available for use on %s",
                ibv_get_device_name(dev->ib_dev)));

    return OPAL_SUCCESS;
}

/* Open - this functions sets up any xoob specific commandline params */
static void xoob_component_register(void)
{
    xoob_priority = 60;
    (void) mca_base_var_register("ompi", "common", "ofacm", "connect_xoob_priority",
                                 "The selection method priority for xoob",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &xoob_priority);
}

/*
 * Connect function.  Start initiation of connections to a remote
 * peer.  We send our Queue Pair information over the RML/OOB
 * communication mechanism.  On completion of our send, a send
 * completion handler is called.
 */
static int xoob_module_start_connect
            (opal_common_ofacm_base_local_connection_context_t *context)
{
    int rc = OPAL_SUCCESS;
    opal_common_ofacm_xoob_local_connection_context_t *xcontext =
        (opal_common_ofacm_xoob_local_connection_context_t *)context;
    pending_context_t *pcontext;

    OPAL_THREAD_LOCK(&xcontext->addr->addr_lock);
    switch (xcontext->addr->status) {
        case XOOB_ADDR_CLOSED:
            OFACM_VERBOSE(("The IB addr: sid %d lid %d"
                        "in XOOB_ADDR_CLOSED status,"
                        " sending ENDPOINT_XOOB_CONNECT_REQUEST\n",
                        xcontext->addr->subnet_id, xcontext->addr->lid));
            if (OPAL_SUCCESS != (rc = xoob_send_qp_create(xcontext))) {
                break;
            }

            /* Send connection info over to remote endpoint */
            xcontext->super.state = MCA_COMMON_OFACM_CONNECTING;
            xcontext->addr->status = XOOB_ADDR_CONNECTING;
            if (OPAL_SUCCESS !=
                    (rc = xoob_send_connect_data(xcontext, ENDPOINT_XOOB_CONNECT_REQUEST))) {
                OFACM_ERROR(("Error sending connect request, error code %d", rc));
            }
            break;
        case XOOB_ADDR_CONNECTING:
            OFACM_VERBOSE(("The IB addr: sid %d lid %d"
                        "in XOOB_ADDR_CONNECTING status,"
                        " Subscribing to this address\n",
                        xcontext->addr->subnet_id, xcontext->addr->lid));
            pcontext = OBJ_NEW(pending_context_t);
            xoob_pending_context_init(pcontext, xcontext);
            /* some body already connectng to this machine, lets wait */
            opal_list_append(&xcontext->addr->pending_contexts,
                    (opal_list_item_t *)pcontext);
            xcontext->super.state = MCA_COMMON_OFACM_CONNECTING;
            break;
        case XOOB_ADDR_CONNECTED:
            /* so we have the send qp, we just need the recive site.
             * Send request for SRQ numbers */
            OFACM_VERBOSE(("The IB addr: sid %d lid %d"
                        "in XOOB_ADDR_CONNECTED status,"
                        " sending ENDPOINT_XOOB_CONNECT_XRC_REQUEST\n",
                        context->subnet_id, context->lid));
            xcontext->super.state = MCA_COMMON_OFACM_CONNECTING;
            if (OPAL_SUCCESS !=
                    (rc = xoob_send_connect_data(xcontext, ENDPOINT_XOOB_CONNECT_XRC_REQUEST))) {
                OFACM_ERROR(("error sending xrc connect request, error code %d", rc));
            }
            break;
        default :
            OFACM_ERROR(("Invalid context status %d", xcontext->addr->status));

    }
    OPAL_THREAD_UNLOCK(&xcontext->addr->addr_lock);
    return rc;
}


/*
 * Finalize function.  Cleanup RML non-blocking receive.
 */
static int xoob_component_finalize(void)
{
    if (rml_recv_posted) {
        ompi_rte_recv_cancel(OMPI_NAME_WILDCARD, OMPI_RML_TAG_XOFACM);
        rml_recv_posted = false;
    }
    return OPAL_SUCCESS;
}
