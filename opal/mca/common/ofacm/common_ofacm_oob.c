/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2008-2012 Mellanox Technologies.  All rights reserved.
 *
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
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
#include "connect.h"
#include "base.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_object.h"
#include "opal/constants.h"

#include "opal_stdint.h"

#define MAX_LINE_LEN 80
#define NUM_OF_TOKENS 7


typedef enum {
    ENDPOINT_CONNECT_REQUEST,
    ENDPOINT_CONNECT_RESPONSE,
    ENDPOINT_CONNECT_ACK
} connect_message_type_t;

typedef struct port_to_switch_lids{
       uint16_t port_lid;
       uint16_t switch_lid;
       struct port_to_switch_lids* next;
} port_to_switch_lids;

typedef struct switch_to_switch_sl{
       uint16_t switch_lid;
       uint8_t service_level;
       struct switch_to_switch_sl* next;
} switch_to_switch_sl;

static int oob_priority = 50;
static bool rml_recv_posted = false;
static opal_rng_buff_t rand_buff;

static void oob_component_register(void);
static int oob_component_query(opal_common_ofacm_base_dev_desc_t *dev,
                               opal_common_ofacm_base_module_t **cpc);
static int oob_component_finalize(void);

static int oob_module_start_connect(opal_common_ofacm_base_local_connection_context_t* context);
static int reply_start_connect(opal_common_ofacm_base_local_connection_context_t* context,
                               opal_common_ofacm_base_remote_connection_context_t *remote_info);
static int set_remote_info(opal_common_ofacm_base_local_connection_context_t *context,
                           opal_common_ofacm_base_remote_connection_context_t *remote_info);
static int qp_connect_all(opal_common_ofacm_base_local_connection_context_t* context);
static int qp_create_all(opal_common_ofacm_base_local_connection_context_t* context);
static int qp_create_one(opal_common_ofacm_base_local_connection_context_t* context, int qp);
static int send_connect_data(opal_common_ofacm_base_local_connection_context_t* context,
                             uint8_t message_type);
static opal_common_ofacm_base_local_connection_context_t*
        oob_endpoint_init(opal_proc_t *proc,
                          opal_common_ofacm_base_qp_config_t *qp_config,
                          struct ibv_pd *pd, uint64_t subnet_id, int cpc_type,
                          uint16_t lid, uint16_t rem_lid,
                          int32_t user_context_index, void *user_context,
                          opal_common_ofacm_base_module_t *cpc,
                          opal_common_ofacm_base_context_connect_cb_fn_t connect_cb,
                          opal_common_ofacm_base_context_error_cb_fn_t error_cb,
                          opal_common_ofacm_base_context_prepare_recv_cb_fn_t prepare_recv_cb);
static int oob_endpoint_finalize(opal_common_ofacm_base_local_connection_context_t *context);

static void report_error(opal_common_ofacm_base_local_connection_context_t* context);

static void rml_send_cb(int status, opal_process_name_t* endpoint,
                        opal_buffer_t* buffer, ompi_rml_tag_t tag,
                        void* cbdata);
static void rml_recv_cb(int status, opal_process_name_t* process_name,
                        opal_buffer_t* buffer, ompi_rml_tag_t tag,
                        void* cbdata);

/* Build service level hashtables per port */
static int create_service_level_table_for_port(uint16_t lid,
                opal_hash_table_t* port_to_switch_hash_table,
                opal_hash_table_t* switch_to_switch_hash_table);

/* Pick the service level of path between to endpoints */
static int pick_service_level(uint16_t src_port_lid, uint16_t dst_port_lid,
                              uint8_t* service_level,
                              opal_hash_table_t* port_to_switch_hash_table,
                              opal_hash_table_t* switch_to_switch_hash_table);

/*
 * The "component" struct -- the top-level function pointers for the
 * oob connection scheme.
 */
opal_common_ofacm_base_component_t opal_common_ofacm_oob = {
    "oob",
    /* Register */
    oob_component_register,
    /* Init */
    NULL,
    /* Query */
    oob_component_query,
    /* Finalize */
    oob_component_finalize,
};

/* Open - this functions sets up any oob specific commandline params */
static void oob_component_register(void)
{
    oob_priority = 50;
    (void) mca_base_var_register("ompi", "common", "ofacm", "connect_oob_priority",
                                 "The selection method priority for oob",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &oob_priority);
}

/*
 * Init function.  Post non-blocking RML receive to accept incoming
 * connection requests.
 */
static int oob_component_query(opal_common_ofacm_base_dev_desc_t *dev,
                               opal_common_ofacm_base_module_t **cpc)
{
    if (oob_priority > 100) {
        oob_priority = 100;
    } else if (oob_priority < -1) {
        oob_priority = -1;
    }

    /* If we have the transport_type member, check to ensure we're on
       IB (this CPC will not work with iWarp).  If we do not have the
       transport_type member, then we must be < OFED v1.2, and
       therefore we must be IB. */
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
    if (IBV_TRANSPORT_IB != dev->ib_dev->transport_type) {
        OFACM_VERBOSE(("OFACM: oob CPC only supported on InfiniBand; skipped on device %s",
                            ibv_get_device_name(dev->ib_dev)));
        return OPAL_ERR_NOT_SUPPORTED;
    }
#endif

    if (dev->capabilities & OPAL_COMMON_OFACM_XRC_ONLY) {
        OFACM_VERBOSE(("OFACM: oob CPC not supported with XRC receive queues, please try xoob CPC; skipped"));
        return OPAL_ERR_NOT_SUPPORTED;
    }
    /* If this btl supports OOB, then post the RML message.  But
       ensure to only post it *once*, because another btl may have
       come in before this and already posted it. */
    if (!rml_recv_posted) {
        ompi_rte_recv_buffer_nb(OMPI_NAME_WILDCARD,
                                OMPI_RML_TAG_OFACM,
                                OMPI_RML_PERSISTENT,
                                rml_recv_cb,
                                NULL);
        rml_recv_posted = true;
    }

    *cpc = malloc(sizeof(opal_common_ofacm_base_module_t));
    if (NULL == *cpc) {
        ompi_rte_recv_cancel(OMPI_NAME_WILDCARD, OMPI_RML_TAG_OFACM);
        rml_recv_posted = false;
        OFACM_VERBOSE(("openib BTL: oob CPC system error (malloc failed)"));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    /* Init global list of all connection contexts */
    OBJ_CONSTRUCT(&opal_common_ofacm_oob.all_procs, opal_list_t);
    (*cpc)->data.cbm_component = &opal_common_ofacm_oob;
    (*cpc)->data.cbm_priority = oob_priority;
    (*cpc)->data.cbm_modex_message = NULL;
    (*cpc)->data.cbm_modex_message_len = 0;

    (*cpc)->cbm_endpoint_init = oob_endpoint_init;
    (*cpc)->cbm_start_connect = oob_module_start_connect;
    (*cpc)->cbm_endpoint_finalize = oob_endpoint_finalize;
    (*cpc)->cbm_finalize = NULL;
    (*cpc)->cbm_uses_cts = false;

    /* seed RNG */
    opal_srand(&rand_buff,(uint32_t) getpid());
    OFACM_VERBOSE(("openib BTL: oob CPC available for use on %s",
                        ibv_get_device_name(dev->ib_dev)));
    return OPAL_SUCCESS;
}

static opal_common_ofacm_base_proc_t* find_proc(opal_proc_t *proc)
{
    opal_common_ofacm_base_proc_t *ret = NULL;
    opal_list_item_t *item;
    opal_list_t *list = &opal_common_ofacm_oob.all_procs;

    for (item = opal_list_get_first(list);
            item != opal_list_get_end(list);
            item = opal_list_get_next(item)) {
        if (proc == ((opal_common_ofacm_base_proc_t *)item)->proc_opal){
            ret = (opal_common_ofacm_base_proc_t *)item;
        }
    }
    return ret;
}

/* OOB connection context init */
static opal_common_ofacm_base_local_connection_context_t*
        oob_endpoint_init(opal_proc_t *proc,
                          opal_common_ofacm_base_qp_config_t *qp_config,
                          struct ibv_pd *pd, uint64_t subnet_id, int cpc_type,
                          uint16_t lid, uint16_t rem_lid,
                          int32_t user_context_index, void *user_context,
                          opal_common_ofacm_base_module_t *cpc,
                          opal_common_ofacm_base_context_connect_cb_fn_t connect_cb,
                          opal_common_ofacm_base_context_error_cb_fn_t error_cb,
                          opal_common_ofacm_base_context_prepare_recv_cb_fn_t prepare_recv_cb)
{
    int ret;
    bool new_proc;
    opal_common_ofacm_base_local_connection_context_t *context;
    opal_common_ofacm_base_proc_t *context_proc;

    context = (opal_common_ofacm_base_local_connection_context_t*)
        OBJ_NEW(opal_common_ofacm_base_local_connection_context_t);
    context_proc = find_proc(proc);

    if (NULL == context_proc) {
        new_proc = true;
        /* constructing new proc */
        context_proc = (opal_common_ofacm_base_proc_t *)
            OBJ_NEW(opal_common_ofacm_base_proc_t );
    } else {
        new_proc = false;
        OBJ_RETAIN(context_proc);
    }

    opal_common_ofacm_base_proc_setup(context_proc, context, proc);
    ret = opal_common_ofacm_base_context_init(context, cpc, connect_cb, error_cb,
            prepare_recv_cb, context_proc, qp_config,
            pd, subnet_id, cpc_type, lid, rem_lid, user_context_index, user_context);
    if (OPAL_SUCCESS != ret) {
        OBJ_DESTRUCT(context_proc);
        OBJ_DESTRUCT(context);
        return NULL;
    }

    if (new_proc) {
        opal_list_append(&opal_common_ofacm_oob.all_procs, (opal_list_item_t *)context_proc);
    }

    return context;
}

/* OOB connection context finalization */
static int oob_endpoint_finalize
            (opal_common_ofacm_base_local_connection_context_t *context)
{
    opal_list_item_t *proc_item, *cntx_item, *cntx_item_next;
    bool found = false;
    bool pfound = false;
    int qp;
    opal_list_t *proc_list = &opal_common_ofacm_oob.all_procs;

    /* Proc cleanup. We should find the context proc in all proc list and remove
     * from the proc list our context. After it we try to release the proc context */
    for (proc_item = opal_list_get_first(proc_list);
            proc_item != opal_list_get_end(proc_list);
            proc_item = opal_list_get_next(proc_item)) {
        if (context->proc == ((opal_common_ofacm_base_proc_t *)proc_item)){
            opal_common_ofacm_base_proc_t *proc =
                (opal_common_ofacm_base_proc_t *)proc_item;
            opal_list_t *cntx_list = &proc->all_contexts;
            pfound = true;

            /* Remove the context from proc list */
            cntx_item = opal_list_get_first(cntx_list);
            while(cntx_item != opal_list_get_end(cntx_list)) {
                /* take the next before removing from the list */
                cntx_item_next = opal_list_get_next(cntx_item);
                if (context == (opal_common_ofacm_base_local_connection_context_t *)cntx_item) {
                    found = true;
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

    /* Release QPs */
    for (qp = 0; qp < context->num_of_qps; qp++) {
        if(NULL != context->qps[qp].lcl_qp) {
            if(ibv_destroy_qp(context->qps[qp].lcl_qp)) {
                OFACM_ERROR(("Failed to destroy QP:%d\n", qp));
            }
        }
    }

    assert(true == found);
    assert(true == pfound);

    /* We done with proc release and now we way destroy the context */
    OBJ_RELEASE(context);

    return OPAL_SUCCESS;
}

/*
 * Connect function.  Start initiation of connections to a remote
 * peer.  We send our Queue Pair information over the RML/OOB
 * communication mechanism.  On completion of our send, a send
 * completion handler is called.
 */
static int oob_module_start_connect(opal_common_ofacm_base_local_connection_context_t *context)
{
    int rc;

    if (OPAL_SUCCESS != (rc = qp_create_all(context))) {
        return rc;
    }

    /* Send connection info over to remote endpoint */
    context->state = MCA_COMMON_OFACM_CONNECTING;
    if (OPAL_SUCCESS !=
        (rc = send_connect_data(context, ENDPOINT_CONNECT_REQUEST))) {
        OFACM_ERROR(("error sending connect request, error code %d", rc));
        return rc;
    }

    return OPAL_SUCCESS;
}

/*
 * Component finalize function.  Cleanup RML non-blocking receive.
 */
static int oob_component_finalize(void)
{
    if (rml_recv_posted) {
        ompi_rte_recv_cancel(OMPI_NAME_WILDCARD, OMPI_RML_TAG_OFACM);
        rml_recv_posted = false;
    }

    return OPAL_SUCCESS;
}

/**************************************************************************/

/*
 * Reply to a `start - connect' message
 */
static int reply_start_connect(opal_common_ofacm_base_local_connection_context_t* context,
                               opal_common_ofacm_base_remote_connection_context_t *remote_info)
{
    int rc;

    OFACM_VERBOSE(("Initialized QPs, LID = %d", context->lid));

    /* Create local QP's and post receive resources */
    if (OPAL_SUCCESS != (rc = qp_create_all(context))) {
        return rc;
    }

    /* Set the remote side info */
    set_remote_info(context, remote_info);

    /* Connect to remote endpoint qp's */
    if (OPAL_SUCCESS != (rc = qp_connect_all(context))) {
        return rc;
    }

    /* Send connection info over to remote endpoint */
    context->state = MCA_COMMON_OFACM_CONNECT_ACK;
    if (OPAL_SUCCESS !=
        (rc = send_connect_data(context, ENDPOINT_CONNECT_RESPONSE))) {
        OFACM_ERROR(("error in endpoint send connect request error code is %d",
                   rc));
        return rc;
    }
    return OPAL_SUCCESS;
}


static int set_remote_info(opal_common_ofacm_base_local_connection_context_t *context,
                           opal_common_ofacm_base_remote_connection_context_t *remote_info)
{
    /* copy the remote_info stuff */
    memcpy(&context->remote_info,
           remote_info, sizeof(opal_common_ofacm_base_remote_connection_context_t ));

    OFACM_VERBOSE(("Setting QP info,  LID = %d", context->remote_info.rem_lid));
    return OPAL_SUCCESS;

}


/*
 * Connect the local ends of all qp's to the remote side
 */
static int qp_connect_all(opal_common_ofacm_base_local_connection_context_t* context)
{
    int i;
    uint8_t service_level = 0;
    uint32_t rtr_mask = 0, rts_mask = 0;
    int rc = OPAL_SUCCESS;

    static bool is_hash_table_initialized = false;
    static opal_hash_table_t switch_to_switch_hash_table;
    static opal_hash_table_t port_to_switch_hash_table;


    /* Create two hash tables for a given port in order to allow
     * an efficient search of service level on any route exiting
     * from it */
    if((NULL != opal_common_ofacm_three_dim_torus) &&
            (false == is_hash_table_initialized)){

        rc = create_service_level_table_for_port(context->lid, &port_to_switch_hash_table,
                &switch_to_switch_hash_table);
        if(OPAL_SUCCESS != rc){
            /* Failed to create service table for port */
            return OPAL_ERROR;
        }
        is_hash_table_initialized = true;
    }


    /* Pick the Service Level of each route from the table */
    if(is_hash_table_initialized){
        rc = pick_service_level(context->lid, context->remote_info.rem_lid, &service_level,
                &port_to_switch_hash_table, &switch_to_switch_hash_table);
        if(OPAL_SUCCESS != rc){
            /* Failed to retrieve service level on the route */
            return OPAL_ERROR;
        }
        /*printf("Debug: qp_connect_all: lid %hu rem lid %hu num_qps %d SL %c\n", context->lid,
                context->remote_info.rem_lid, context->num_of_qps, service_level);*/
    }


    for (i = 0; i < context->num_of_qps; i++) {
        struct ibv_qp_attr attr;
        struct ibv_qp* qp = context->qps[i].lcl_qp;
        enum ibv_mtu mtu = (context->attr[i].path_mtu < context->remote_info.rem_mtu) ?
            context->attr[i].path_mtu : context->remote_info.rem_mtu;

        memset(&attr, 0, sizeof(attr));
        memcpy(&attr, context->attr, sizeof(struct ibv_qp_attr));
        attr.qp_state           = IBV_QPS_RTR;
        attr.path_mtu           = mtu;
        attr.dest_qp_num        = context->remote_info.rem_qps[i].rem_qp_num;
        attr.rq_psn             = context->remote_info.rem_qps[i].rem_psn;
        attr.ah_attr.dlid       = context->remote_info.rem_lid;

        if(is_hash_table_initialized){
            attr.ah_attr.sl         = service_level;
        }
        /* JMS to be filled in later dynamically */
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
            rtr_mask |= context->custom_rtr_attr_mask[i];
        }

        OFACM_VERBOSE(("Set MTU to IBV value %d (%s bytes)", mtu,
                    (mtu == IBV_MTU_256) ? "256" :
                    (mtu == IBV_MTU_512) ? "512" :
                    (mtu == IBV_MTU_1024) ? "1024" :
                    (mtu == IBV_MTU_2048) ? "2048" :
                    (mtu == IBV_MTU_4096) ? "4096" :
                    "unknown (!)"));

        if (ibv_modify_qp(qp, &attr, rtr_mask)) {
            OFACM_ERROR(("Error modifing QP to RTR errno says %s",
                       strerror(errno)));
            return OPAL_ERROR;
        }
        attr.qp_state       = IBV_QPS_RTS;
        /* On PP QPs we have SW flow control, no need for rnr retries. Setting
         * it to zero helps to catch bugs */
        /*
        attr.rnr_retry      = BTL_OPENIB_QP_TYPE_PP(i) ? 0 :
            mca_btl_openib_component.ib_rnr_retry;
        */
        attr.sq_psn         = context->qps[i].lcl_psn;
        rts_mask = IBV_QP_STATE              |
                   IBV_QP_TIMEOUT            |
                   IBV_QP_RETRY_CNT          |
                   IBV_QP_RNR_RETRY          |
                   IBV_QP_SQ_PSN             |
                   IBV_QP_MAX_QP_RD_ATOMIC;

        /* applying user specified rts mask */
        if (NULL != context->custom_rts_attr_mask) {
            rts_mask |= context->custom_rts_attr_mask[i];
        }

        if (ibv_modify_qp(qp, &attr, rts_mask)) {
            OFACM_ERROR(("error modifying QP to RTS errno says %s",
                       strerror(errno)));
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}


/*
 * Create the local side of all the qp's.  The remote sides will be
 * connected later.
 */
static int qp_create_all(opal_common_ofacm_base_local_connection_context_t* context)
{
    int qp, rc;

    for (qp = 0; qp < context->num_of_qps; ++qp) {
        rc = qp_create_one(context, qp);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }
    /* Now that all the qp's are created locally, post some receive
       buffers, setup credits, etc. */
    return context->prepare_recv_cb(context->user_context);
}

/*
 * Create the local side of one qp.  The remote side will be connected
 * later.
 */
static int qp_create_one(opal_common_ofacm_base_local_connection_context_t *context, int qp)
{
    struct ibv_qp *my_qp;
    struct ibv_qp_init_attr init_attr;
    struct ibv_qp_attr attr;
    size_t req_inline = context->init_attr[qp].cap.max_inline_data;
    uint32_t init_mask = 0;

    /* Taking default init attributes from user */
    memcpy(&init_attr, &context->init_attr[qp], sizeof(init_attr));
    my_qp = ibv_create_qp(context->ib_pd, &init_attr);

    if (NULL == my_qp) {
        OFACM_ERROR(("error creating qp errno says %s", strerror(errno)));
        return OPAL_ERROR;
    }
    context->qps[qp].lcl_qp = my_qp;

    if (init_attr.cap.max_inline_data < req_inline) {
        context->qps[qp].ib_inline_max = init_attr.cap.max_inline_data;
        opal_show_help("help-mpi-common-ofacm-cpc-base.txt",
                       "inline truncated", true, opal_proc_local_get()->proc_hostname,
                       req_inline, init_attr.cap.max_inline_data);
    } else {
        context->qps[qp].ib_inline_max = req_inline;
    }

    /* Taking default attributes from user */
    memcpy(&attr, &context->attr[qp], sizeof(attr));
    attr.qp_state        = IBV_QPS_INIT;
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;
    init_mask = IBV_QP_STATE |
                IBV_QP_PKEY_INDEX |
                IBV_QP_PORT |
                IBV_QP_ACCESS_FLAGS;
    /* apply user specified init mask */
    if (NULL != context->custom_init_attr_mask) {
        init_mask |= context->custom_init_attr_mask[qp];
    }

    if (ibv_modify_qp(context->qps[qp].lcl_qp,
                      &attr, init_mask)) {
        OFACM_ERROR(("Error modifying qp to INIT errno says %s", strerror(errno)));
        return OPAL_ERROR;
    }

    /* Setup meta data on the endpoint */
    context->qps[qp].lcl_psn = opal_rand(&rand_buff) & 0xffffff;

    return OPAL_SUCCESS;
}


/*
 * RML send connect information to remote endpoint
 */
static int send_connect_data(opal_common_ofacm_base_local_connection_context_t* context,
                             uint8_t message_type)
{
    opal_buffer_t* buffer = OBJ_NEW(opal_buffer_t);
    int rc;

    if (NULL == buffer) {
         OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
         return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */
    OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT8));
    OFACM_VERBOSE(("type %d\n", message_type));
    rc = opal_dss.pack(buffer, &message_type, 1, OPAL_UINT8);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT64));
    rc = opal_dss.pack(buffer, &context->subnet_id, 1, OPAL_UINT64);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    if (message_type != ENDPOINT_CONNECT_REQUEST) {
        /* send the QP connect request info we respond to */
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer,
                           &context->remote_info.rem_qps[0].rem_qp_num, 1,
                           OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
        rc = opal_dss.pack(buffer, &context->remote_info.rem_lid, 1, OPAL_UINT16);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
    }

    if (message_type != ENDPOINT_CONNECT_ACK) {
        int qp;
        /* send CM type/family */
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_INT));
        rc = opal_dss.pack(buffer, &context->cpc_type, 1, OPAL_INT);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        /* Pasha: Send number of qp here. We don't must to send number of QPs here, BUT
         * recv side callback code is pretty complicated and I don't want to touch
         * it now. So best work around on this stage is send another 1byte with number of
         * qps.
         */
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT8));
        rc = opal_dss.pack(buffer, &context->num_of_qps, 1, OPAL_UINT8);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        /* stuff all the QP info into the buffer */
        for (qp = 0; qp < context->num_of_qps; qp++) {
            OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
            rc = opal_dss.pack(buffer, &context->qps[qp].lcl_qp->qp_num,
                               1, OPAL_UINT32);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
            OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
            rc = opal_dss.pack(buffer, &context->qps[qp].lcl_psn, 1,
                               OPAL_UINT32);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
        }

        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
        rc = opal_dss.pack(buffer, &context->lid, 1, OPAL_UINT16);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &context->attr[0].path_mtu, 1,
                OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        OFACM_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &context->index, 1, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
    }

    /* send to remote endpoint */
    rc = ompi_rte_send_buffer_nb((orte_process_name_t*)&context->proc->proc_opal->proc_name,
                                 buffer, OMPI_RML_TAG_OFACM,
                                 rml_send_cb, NULL);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }
    OFACM_VERBOSE(("Sent QP Info, LID = %d, SUBNET = %lx\n",
                 context->lid,
                 context->subnet_id));

    return OPAL_SUCCESS;
}

static void report_error(opal_common_ofacm_base_local_connection_context_t* context)
{
    if (NULL == context || NULL == context->error_cb) {
        /* The context is undefined and we can not print specific error */
        opal_show_help("help-mpi-common-ofacm-oob.txt",
                "ofacm oob fatal error", true,
                opal_proc_local_get()->proc_hostname,
                __FILE__, __LINE__);
        exit(1);
    }

    /* Other way, call to user error callback */
    context->error_cb(context->user_context);
}

/*
 * Callback when we have finished RML sending the connect data to a
 * remote peer
 */
static void rml_send_cb(int status, opal_process_name_t* endpoint,
                        opal_buffer_t* buffer, ompi_rml_tag_t tag,
                        void* cbdata)
{
    OBJ_RELEASE(buffer);
}


/*
 * Non blocking RML recv callback.  Read incoming QP and other info,
 * and if this endpoint is trying to connect, reply with our QP info,
 * otherwise try to modify QP's and establish reliable connection
 */
static void rml_recv_cb(int status, opal_process_name_t* process_name,
                        opal_buffer_t* buffer, ompi_rml_tag_t tag,
                        void* cbdata)
{
    int context_state;
    int rc;
    uint32_t lcl_qp = 0;
    uint16_t lcl_lid = 0;
    int32_t cnt = 1;
    opal_common_ofacm_base_remote_connection_context_t remote_info;
    opal_common_ofacm_base_local_connection_context_t *l_context;
    opal_common_ofacm_base_proc_t *proc;
    uint8_t message_type, num_qps;
    int cpc_type;
    opal_list_t *procs_list = &opal_common_ofacm_oob.all_procs;
    opal_list_t *context_list;
    bool master;

    /* start by unpacking data first so we know who is knocking at
       our door */
    OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT8));
    rc = opal_dss.unpack(buffer, &message_type, &cnt, OPAL_UINT8);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        report_error(NULL);
        return;
    }

    OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT64));
    rc = opal_dss.unpack(buffer, &remote_info.rem_subnet_id, &cnt, OPAL_UINT64);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        report_error(NULL);
        return;
    }

    if (ENDPOINT_CONNECT_REQUEST != message_type) {
        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &lcl_qp, &cnt, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            report_error(NULL);
            return;
        }
        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
        rc = opal_dss.unpack(buffer, &lcl_lid, &cnt, OPAL_UINT16);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            report_error(NULL);
            return;
        }
    }

    if (ENDPOINT_CONNECT_ACK != message_type) {
        int qp;

        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_INT));
        rc = opal_dss.unpack(buffer, &cpc_type, &cnt, OPAL_INT);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            report_error(NULL);
            return;
        }
        /* Pasha: Reading number of qps, in original code we tool it from
         * btl component. In future we may change order of operations here. We may start
         * lookup for connection descriptor after receiving subnet_id and lid. But in order
         * to do it here I need totally to rewrite the recv callback...next time ;)
         */
        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT8));
        rc = opal_dss.unpack(buffer, &num_qps, &cnt, OPAL_UINT8);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            report_error(NULL);
            return;
        }
        /* get ready for the data */
        opal_common_ofacm_base_remote_context_init(&remote_info,
                num_qps, 0);

        /* unpack all the qp info */
        for (qp = 0; qp < num_qps; ++qp) {
            OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
            rc = opal_dss.unpack(buffer, &remote_info.rem_qps[qp].rem_qp_num, &cnt,
                                 OPAL_UINT32);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                report_error(NULL);
                return;
            }
            OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
            rc = opal_dss.unpack(buffer, &remote_info.rem_qps[qp].rem_psn, &cnt,
                                 OPAL_UINT32);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                report_error(NULL);
                return;
            }
        }

        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
        rc = opal_dss.unpack(buffer, &remote_info.rem_lid, &cnt, OPAL_UINT16);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            report_error(NULL);
            return;
        }

        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &remote_info.rem_mtu, &cnt, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            report_error(NULL);
            return;
        }

        OFACM_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &remote_info.rem_index, &cnt, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            report_error(NULL);
            return;
        }
    }

    OFACM_VERBOSE(("Received QP Info,  LID = %d, SUBNET = %lx, CPC_TYPE = %d",
                 remote_info.rem_lid,
                 remote_info.rem_subnet_id,
                 cpc_type));

    master = ompi_rte_compare_name_fields(OMPI_RTE_CMP_ALL, OMPI_PROC_MY_NAME,
                                    process_name) >= 0 ? true : false;
    for (proc = (opal_common_ofacm_base_proc_t *)opal_list_get_first(procs_list);
            proc != (opal_common_ofacm_base_proc_t *)opal_list_get_end(procs_list);
            proc = (opal_common_ofacm_base_proc_t *)opal_list_get_next(proc)){
        bool found = false;
        if (opal_compare_proc(proc->proc_opal->proc_name,
                              *process_name) != OPAL_EQUAL) {
            continue;
        }
        context_list = &proc->all_contexts;
        if (ENDPOINT_CONNECT_REQUEST != message_type) {
            /* This is a reply message. Try to get the endpoint
               instance the reply belongs to */
            for (l_context = (opal_common_ofacm_base_local_connection_context_t *)opal_list_get_first(context_list);
                    l_context != (opal_common_ofacm_base_local_connection_context_t *)opal_list_get_end(context_list);
                    l_context = (opal_common_ofacm_base_local_connection_context_t *)opal_list_get_next(l_context)) {
                if (l_context->qps[0].lcl_qp != NULL &&
                    lcl_lid == l_context->lid &&
                    lcl_qp == l_context->qps[0].lcl_qp->qp_num &&
                    remote_info.rem_subnet_id == l_context->subnet_id) {
                    found = true;
                    break;
                }
            }
        } else {
            /* This is new connection request. If this is master try
               to find endpoint in a connecting state. If this is
               slave try to find  endpoint in closed state and
               initiate connection back */
            opal_common_ofacm_base_local_connection_context_t *context_found = NULL;
            for (l_context = (opal_common_ofacm_base_local_connection_context_t *)opal_list_get_first(context_list);
                    l_context != (opal_common_ofacm_base_local_connection_context_t *)opal_list_get_end(context_list);
                    l_context = (opal_common_ofacm_base_local_connection_context_t *)opal_list_get_next(l_context)) {
                if (l_context->subnet_id != remote_info.rem_subnet_id ||
                    l_context->cpc_type != cpc_type ||
                   (l_context->state != MCA_COMMON_OFACM_CONNECTING
                    && l_context->state != MCA_COMMON_OFACM_CLOSED))
                    continue;
                found = true;
                context_found = l_context;
                if ((master &&
                     MCA_COMMON_OFACM_CONNECTING == l_context->state) ||
                    (!master &&
                     MCA_COMMON_OFACM_CLOSED == l_context->state))
                    break; /* Found one. No point to continue */
            }
            l_context = context_found;

            /* if this is slave and there is no endpoints in closed
               state then all connection are already in progress so
               just ignore this connection request */
            if (found && !master &&
                MCA_COMMON_OFACM_CLOSED != l_context->state) {
                return;
            }
        }

        if (!found) {
            OFACM_ERROR(("can't find suitable endpoint for this peer\n"));
            report_error(NULL);
            return;
        }

        OPAL_THREAD_LOCK(&l_context->context_lock);
        context_state = l_context->state;

        /* Update status */
        switch (context_state) {
        case MCA_COMMON_OFACM_CLOSED:
            /* We had this connection closed before.  The endpoint is
               trying to connect. Move the status of this connection
               to CONNECTING, and then reply with our QP
               information */
            if (master) {
                rc = reply_start_connect(l_context, &remote_info);
            } else {
                rc = oob_module_start_connect(l_context);
            }

            if (OPAL_SUCCESS != rc) {
                OFACM_ERROR(("error in endpoint reply start connect"));
                report_error(l_context);
                break;
            }

            /* As long as we expect a message from the peer (in order
               to setup the connection) let the event engine pool the
               RML events. Note: we increment it once peer active
               connection. */
            opal_progress_event_users_increment();
            break;

        case MCA_COMMON_OFACM_CONNECTING:
            /* preparing remote info for this context */
            opal_common_ofacm_base_remote_context_init(&l_context->remote_info,
                    l_context->num_of_qps, 0);
            /* need to check status here */
            set_remote_info(l_context, &remote_info);
            if (OPAL_SUCCESS != (rc = qp_connect_all(l_context))) {
                OFACM_ERROR(("endpoint connect error: %d", rc));
                report_error(l_context);
                break;
            }

            if (master) {
                l_context->state = MCA_COMMON_OFACM_WAITING_ACK;

                /* Send him an ACK */
                send_connect_data(l_context, ENDPOINT_CONNECT_RESPONSE);
            } else {
                send_connect_data(l_context, ENDPOINT_CONNECT_ACK);
                /* Tell main BTL that we're done */
                l_context->state = MCA_COMMON_OFACM_CONNECTED;
                l_context->connect_cb(l_context->user_context);
             }
            break;

        case MCA_COMMON_OFACM_WAITING_ACK:
            /* Tell main BTL that we're done */
            l_context->state = MCA_COMMON_OFACM_CONNECTED;
            l_context->connect_cb(l_context->user_context);
            break;

        case MCA_COMMON_OFACM_CONNECT_ACK:
            send_connect_data(l_context, ENDPOINT_CONNECT_ACK);
            /* Tell main BTL that we're done */
            l_context->state = MCA_COMMON_OFACM_CONNECTED;
            l_context->connect_cb(l_context->user_context);
            break;

        case MCA_COMMON_OFACM_CONNECTED:
            break;

        default :
            OFACM_ERROR(("Invalid endpoint state %d", context_state));
            report_error(l_context);
        }
        OPAL_THREAD_UNLOCK(&l_context->context_lock);
        break;
    }
}

/*
 * Get the service level on the route between
 * source port LID and destination port LID.
 * @Param src_port_lid - LID of the source port.
 * @Param dst_port_lid - LID of destination port.
 * @Param service_level - Returned value.
 * The service level on the route between source port
 * to destination port.
 * @return - Error Code. Non Zero value on error.
 */
static int pick_service_level(uint16_t src_port_lid, uint16_t dst_port_lid, uint8_t* service_level,
                                       opal_hash_table_t* port_to_switch_hash_table, opal_hash_table_t* switch_to_switch_hash_table)
{
       uint8_t* sl;
       uint16_t* dst_switch_lid;
       void* p_src_switch_lid = NULL;
       void* p_dst_switch_lid = NULL;
       void* p_service_level = NULL;
       int rc = OPAL_SUCCESS;

       /* Get the switch LID connected tothe source HCA LID */
       rc = opal_hash_table_get_value_ptr(port_to_switch_hash_table, &src_port_lid, sizeof(uint16_t), &p_src_switch_lid);
       if(OPAL_SUCCESS != rc){
               /* Could not find source port LID */
               rc = OPAL_ERROR;
               return rc;
       }


       /* Get the switch LID connected to the destination HCA LID */
       rc = opal_hash_table_get_value_ptr(port_to_switch_hash_table, &dst_port_lid, sizeof(uint16_t), &p_dst_switch_lid);
       if(OPAL_SUCCESS != rc){
               /* Could not find destination port LID */
               rc = OPAL_ERROR;
               return rc;
       }
       dst_switch_lid = (uint16_t*)p_dst_switch_lid;


       /* Get the service level of the route beween the source HCA LID and destination HCA LID */
       rc = opal_hash_table_get_value_ptr(switch_to_switch_hash_table, dst_switch_lid, sizeof(uint16_t), &p_service_level);
       if(OPAL_SUCCESS != rc){
               /* Could not find destination switch LID in hashtable*/
               rc = OPAL_ERROR;
               return rc;
       }
       sl = (uint8_t*)p_service_level;
       *service_level = *sl;

       return rc;
}


/*
 * Get the size of the port to switch hashtable from a file.

 * @Params fp - Descriptor of the input file.
 * @Param hash_table_size - Pointer to the size of
 *                    the port to switch hashtable.
 * @param head - pointer to a linked list containing
 *           the pairs to be stored in the hashtable.
 * @return - Error code. Non zero value for failure.
 */
static int get_port_to_switch_hashtable_data_from_file(FILE* fp, int* hash_table_size, port_to_switch_lids** head)
{
    int i;
    char c;
    int num_items;
    int rc = OPAL_SUCCESS;
    int ret = OPAL_SUCCESS;

    uint64_t guid;
    uint16_t port_lid;
    uint16_t switch_lid;
    uint16_t mtu, rate, lmc; /* TODO: Check binary representation */
    int port_number;

    port_to_switch_lids* item = NULL;
    port_to_switch_lids* p_head = *head;
    port_to_switch_lids* p_next_item = NULL;

    char str[MAX_LINE_LEN] = "\0";
    char input_str[NUM_OF_TOKENS][MAX_LINE_LEN] = {"\0"};
    char expected_str[NUM_OF_TOKENS][MAX_LINE_LEN] = {"\0"};


    c = fgetc(fp);
    fseek(fp, -1, SEEK_CUR);

    /* Init expected input strings */
    strcpy(expected_str[0], "Channel");
    strcpy(expected_str[1], "Adapter");
    strcpy(expected_str[2], "base");
    strcpy(expected_str[3], "LID");
    strcpy(expected_str[4], "LMC");
    strcpy(expected_str[5], "port");

    /* Create list */
    p_head = (port_to_switch_lids*)calloc(1, sizeof(port_to_switch_lids));
    if(NULL == p_head){
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        return rc;
    }
    *head = p_head;
    /* Pre-process the port-to-switch table */
    while(EOF != c)
    {
        ret = fscanf(fp, "%s %s %" PRIx64 " %c", input_str[0], input_str[1], &guid, &c);
        ret += fscanf(fp, "%s %s %hx %c", input_str[2], input_str[3], &port_lid, &c);
        ret += fscanf(fp, "%s %hu %c", input_str[4], &lmc, &c);
        ret += fscanf(fp, "%s %s %d", input_str[6], input_str[5], &port_number);


        if(14 != ret){
            rc = OPAL_ERR_FILE_READ_FAILURE;
            return rc;
        }

        for(i = 0; i < 6; i++)
        {
            /*if(strncmp(str, table_header, hash_table_header_size)){*/
            if(strcmp(input_str[i], expected_str[i])){
                /* Incorrect table header */
                rc = OPAL_ERROR;
                return rc;
            }
        }

        c = fgetc(fp);
        fgets(str, MAX_LINE_LEN, fp);
        if(strncmp(str, "# LID  : MTU : RATE", strlen(str) - 1)){
            /* Incorrect table header */
            rc = OPAL_ERROR;
            return rc;
        }

        c = fgetc(fp);
        fseek(fp, -1, SEEK_CUR);


        /* Read next line */
        fgets(str, MAX_LINE_LEN, fp);

        /* Update the port to switch hashtable size if read valid data */
        num_items = sscanf(str, "%hx %c %hu %c %hu", &switch_lid, &c, &mtu, &c, &rate);
        if(5 == num_items){
            (*hash_table_size)++;
        }
        else{
            /* Wrong file format */
            rc = OPAL_ERROR;
            return rc;
        }
        /* Store port LID and switch LID */
        item = calloc(1, sizeof(port_to_switch_lids));
        if(NULL == item){
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            return rc;
        }
        item->port_lid = port_lid;
        item->switch_lid = switch_lid;

        /* Insert the item to the head of the list */
        p_next_item = p_head->next;
        p_head->next = item;
        item->next = p_next_item;


        /* Get Next char */
        c = fgetc(fp);
        fseek(fp, -1, SEEK_CUR);
        }

        return rc;
    }

/*
 * Get from the input file the size of the
 * switch-to-switch hashtable dedicated for
 * the input switch LID.

 * @Params fp - Descriptor of the input file.
 * @Param switch_lid - the source switch local ID (LID).
 * @Param hash_table_size - Pointer to the hashtable size.
 *                          Value returned by this routine.
 * @Param head - pointer to a linked list containing the pairs
 *                      to be stored in the hashtable.
 * @return - Error code. Non zero value for failure.
 */
static int get_switch_to_switch_hashtable_size_from_file(FILE* fp, uint16_t switch_lid, int* hash_table_size, switch_to_switch_sl** head)
{
    int i;
    char c;
    int num_items;

    int port;
    uint64_t guid;
    uint16_t source_lid;
    uint16_t dest_lid;

    int rc = OPAL_SUCCESS;
    int ret = OPAL_SUCCESS;
    uint8_t service_level;

    switch_to_switch_sl* item = NULL;
    switch_to_switch_sl* p_head = NULL;
    switch_to_switch_sl* p_next_item = NULL;

    int table_offset = 0;
    int offset_in_table = 0;

    char str[MAX_LINE_LEN] = "\0";
    char input_str[NUM_OF_TOKENS][MAX_LINE_LEN] = {"\0"};
    char expected_str[NUM_OF_TOKENS][MAX_LINE_LEN] = {"\0"};


    /* Init expected strings */
    strcpy(expected_str[0], "Switch");
    strcpy(expected_str[1], "base");
    strcpy(expected_str[2], "LID");
    strcpy(expected_str[3], "port");


    /* Allocate empty list */
    p_head = (switch_to_switch_sl*)calloc(1, sizeof(switch_to_switch_sl));
    if(NULL == p_head){
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        return rc;
    }
    *head = p_head;

    c = fgetc(fp);
    fseek(fp, -1, SEEK_CUR);

    /* Read info */
    while(EOF != c){

        /* Go over the switch-to-switch routing tables until the requested
         * table dedicated for the input switch_lid is found */
        ret = fscanf(fp, "%s %" PRIx64 " %c", input_str[0], &guid, &c);
        ret += fscanf(fp, "%s %s %hx %c", input_str[1], input_str[2], &source_lid, &c);
        ret += fscanf(fp, "%s %s %d", input_str[4], input_str[3], &port);
        c = fgetc(fp);

        if(10 != ret)
        {
            rc = OPAL_ERR_FILE_READ_FAILURE;
            return rc;
        }

        for(i = 0; i < 4; i++){
            /* Validate the table header correctness */
            if(strncmp(input_str[i], expected_str[i], strlen(input_str[i]))){
                /* Incorrect table header */
                rc = OPAL_ERROR;
                return rc;
            }
        }

        /* Get next line acording to the currect structure of the file */
        fgets(str, MAX_LINE_LEN, fp);
        if(strncmp(str, "# LID  : SL : MTU : RATE", strlen(str) - 1)){
            rc = OPAL_ERROR;
            return rc;
        }

        /* Test if this is the requested table,
         * dedicated for the input source switch lid */
        if(source_lid != switch_lid){
            /* Skip to next table */

            while(EOF != c)
            {
                offset_in_table = ftell(fp);
                fgets(str, MAX_LINE_LEN, fp);
                if(!strncmp(str, "Switch", strlen("Switch"))){
                    /* Found new table found - start over */
                    fseek(fp, offset_in_table, SEEK_SET);
                    break;
                }
                /* Receive next charecter */
                c = fgetc(fp);
                fseek(fp, -1, SEEK_CUR);
            }
            if(EOF == c){
                /* End-Of-File was met without
                 * finding the required routing table*/
                rc = OPAL_ERROR;
            }
        }
        else{
            /* The right table was found */
            while(EOF != c){

                fgets(str, MAX_LINE_LEN, fp);

                /* Test if a new table was found */
                if(!strncmp(str, "Switch", strlen("Switch"))){
                    /* Quit the search - table was fully read */
                    return rc;
                }
                /* Still in the required switch route table */
                else{
                    /* Check correcness of the data and update table size */
                    num_items = sscanf(str, "%hx %c %c", &dest_lid, &c, &service_level);
                    if(3 != num_items){
                        /* Failed to read input data / wrong input formate */
                        rc = OPAL_ERROR;
                        return rc;
                    }
                    (*hash_table_size)++;

                    /* Add the data to the list*/
                    item = (switch_to_switch_sl*)calloc(1, sizeof(switch_to_switch_sl));
                    if(NULL == item){
                        rc = OPAL_ERR_OUT_OF_RESOURCE;
                        return rc;
                    }
                    item->switch_lid = dest_lid;
                    item->service_level = service_level;

                    p_next_item = p_head->next;
                    p_head->next = item;
                    item->next = p_next_item;
                }
                /* Get next charecter */
                c = fgetc(fp);
                fseek(fp, -1, SEEK_CUR);
            }
            /* Set file descriptor to the beginning
             * of the required table table */
            fseek(fp, table_offset, SEEK_SET);
        }
    }
    return rc;
}

/*
 * Set port to switch hashtable according to data read from an input file.
 * The hashtable Key is the port local ID (uint16_t).
 * The hashtable Value is the local ID (uint16_t) of the switch connected to the port in the fabric.
 *
 * @Param hashtable - the hashtable to set.
 * @Param hashtable_size - the number of hashtable elements.
 * @Param head - Pointer to a linked list containing
 *                 the pairs two be stored in the hashtable.
 * @return - Error code. Non Zero value on error.
 */
static int set_port_to_switch_hash_table(opal_hash_table_t* hashtable, size_t hashtable_size, port_to_switch_lids** p_head)
{
    int ret;
    uint16_t key;
    uint16_t* value = NULL;
    unsigned int i;
    int rc = OPAL_SUCCESS;

    port_to_switch_lids* head = NULL;
    port_to_switch_lids* p_item = NULL;
    port_to_switch_lids* p_item_next = NULL;


    if((NULL == p_head) || (NULL == *p_head)){
        rc = OPAL_ERROR;
        return rc;
    }
    head = *p_head;

    for(i = 0; i < hashtable_size; i++){

        /* Read pairs of port-lid and witch-lid from
         * file and store them in the input hashtable */
        value = (uint16_t*)calloc(1, sizeof(uint16_t));
        if(NULL == value){
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            return rc;
        }

        /* Get next pair to store */
        p_item = head->next;
        if(NULL == p_item){
            rc = OPAL_ERROR;
            return rc;
        }
        key = p_item->port_lid;
        *value = p_item->switch_lid;
        /* Remove item from list */
        p_item_next = p_item->next;
        head->next = p_item_next;
        free(p_item);

        /* Set the port to switch LIDS hashtable */
        ret = opal_hash_table_set_value_ptr(hashtable, &key, sizeof(uint16_t), (void*)value);
        if(OPAL_SUCCESS != ret){
            OFACM_ERROR(("Failed to set port2switch hashtable\n"));
            rc = OPAL_ERROR;
            break;
        }
    }

    free(*p_head);
    *p_head = NULL;
    return rc;
}

/*
 * Set switch to switch hashtable according to data read from an input file.
 * The hashtable Key is a switch local ID (uint16_t).
 * The hashtable Value is the service level (uint8_t) of the route in the
 * fabric between local switch LID (represented by key) and remote switch LID.
 *
 * @Param hashtable - The hashtable to set.
 * @Param hashtable_size - The number of hashtable elements.
 * @Param head - Pointer to a list of all the data
 *                 pair to be inserted into the hashtable.
 * @return - Error code. Non Zero value on error.
 */
static int set_switch_to_switch_hash_table(opal_hash_table_t* hashtable, size_t hashtable_size, switch_to_switch_sl** p_head)
{
    uint16_t key; /* switch lid */
    uint8_t* value = NULL;
    unsigned int i;
    int rc = OPAL_SUCCESS;
    int ret = OPAL_SUCCESS;

    switch_to_switch_sl* head = NULL;
    switch_to_switch_sl* item = NULL;
    switch_to_switch_sl* p_next_item = NULL;


    if((NULL == p_head) || (NULL == *p_head)){
        rc = OPAL_ERROR;
        return rc;
    }
    head = *p_head;

    /* Read pairs of remote switch (LID) and
     * route service level (SL) from file
     * and store the in the input hashtable */
    for(i = 0; i < hashtable_size; i++)
    {

        value = (uint8_t*)calloc(1, sizeof(uint8_t));
        if(NULL == value){
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            return rc;
        }

        /* Get data from list */
        item = head->next;
        if(NULL == item){
            rc = OPAL_ERROR;
            return rc;
        }
        key = item->switch_lid;
        *value = item->service_level;

        /* Remove data item from list */
        p_next_item = item->next;
        head->next = p_next_item;
        free(item);

        ret = opal_hash_table_set_value_ptr(hashtable, &key, sizeof(uint16_t), value);
        if(OPAL_SUCCESS != ret){
            OFACM_ERROR(("Failed to set sw2sw hashtable\n"));
            rc = OPAL_ERROR;
            break;
        }
    }

    free(*p_head);
    *p_head = NULL;
    return rc;
}

/*
 * An efficient method that allows to find the service level of any
 * any route from an input port to any other port in the fabric.
 *
 * Create two hashtables according to data read from an input file.
 * The first table maps any port LID in the fabric to the LID of
 * the switch it is connected to.
 * The second table is dedicated to the switch LID to which the
 * local port is connected.
 *
 * The table maps a remote switch LID to the service level
 * of the route between the table's LID and this remote LID.
 *
 * @Param lid - the local ID of the port.
 * @return - Error Code. Non Zero value in case of error.
 */
static int create_service_level_table_for_port(uint16_t lid, opal_hash_table_t* port_to_switch_hash_table,
                                               opal_hash_table_t* switch_to_switch_hash_table)
{
    FILE* fp = NULL;
    uint16_t* switch_lid;
    void* p_switch_lid = NULL;

    int rc = OPAL_SUCCESS;
    int ret = OPAL_SUCCESS;

    int file_name_len;
    char* switch_to_sl = NULL;

    int port_to_switch_hash_table_size = 0;
    int switch_to_switch_hash_table_size = 0;

    port_to_switch_lids* port_switch_lids = NULL;
    switch_to_switch_sl* switch_sl = NULL;



    /* Open input configuration file */
    fp = fopen(opal_common_ofacm_three_dim_torus, "rt");
    if(NULL == fp){
        /* File Opening failed */
        fprintf(stderr, "Failed to open the input file for the fabric's service level\n");
        rc = OPAL_ERR_FILE_OPEN_FAILURE;
        goto ERROR;
    }

    /* Get port-to-switch hashtable size */
    rc = get_port_to_switch_hashtable_data_from_file(fp, &port_to_switch_hash_table_size, &port_switch_lids);
    if(OPAL_SUCCESS != rc){
        goto ERROR;
    }
    fclose(fp);
    fp = NULL;

    /* Build and initialize the port-to-swich hashtable */
    OBJ_CONSTRUCT(port_to_switch_hash_table, opal_hash_table_t);
    opal_hash_table_init(port_to_switch_hash_table, port_to_switch_hash_table_size);

    /* Set the port-to-switch hashtable */
    rc = set_port_to_switch_hash_table(port_to_switch_hash_table, port_to_switch_hash_table_size, &port_switch_lids);
    if(OPAL_SUCCESS != rc){
        goto ERROR;
    }

    /* Get the LID of the switch connected to the port's LID */
    ret = opal_hash_table_get_value_ptr(port_to_switch_hash_table, &lid, sizeof(uint16_t), &p_switch_lid);
    if(OPAL_SUCCESS != ret){
        rc = OPAL_ERROR;
        goto ERROR;
    }


    /* Open the file containing the mapping from switch-to-switch route to service level */
    file_name_len = strlen(opal_common_ofacm_three_dim_torus);
    switch_to_sl = (char*)calloc(file_name_len + 7, sizeof(char));
    if(NULL == switch_to_sl){
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto ERROR;
    }
    /* Build the switch-to-switch file name based on the port-to-switch file name */
    strncpy(switch_to_sl, opal_common_ofacm_three_dim_torus,
            strlen(opal_common_ofacm_three_dim_torus) - strlen("peer-paths.dump") - 1);
    strcat(switch_to_sl, "-sw2sw-path-records.dump");

    /* Open path-to-SL file */
    fp = fopen(switch_to_sl, "rt");
    if(NULL == fp){
        /* File Opening failed */
        fprintf(stderr, "Failed to open the input file for the fabric's service level\n");
        rc = OPAL_ERR_FILE_OPEN_FAILURE;
        goto ERROR;
    }
    free(switch_to_sl);

    switch_lid = (uint16_t*)p_switch_lid;
    rc = get_switch_to_switch_hashtable_size_from_file(fp, *(uint16_t*)switch_lid,
                                    &switch_to_switch_hash_table_size, &switch_sl);
    if(OPAL_SUCCESS != rc){

        goto ERROR;
    }
    fclose(fp);
    fp = NULL;

    /* Build and initialize the switch-to-switch hashtable */
    OBJ_CONSTRUCT(switch_to_switch_hash_table, opal_hash_table_t);
    opal_hash_table_init(switch_to_switch_hash_table, switch_to_switch_hash_table_size);

    /* Set the switch-to-switch hashtable */
    rc = set_switch_to_switch_hash_table(switch_to_switch_hash_table,
            switch_to_switch_hash_table_size, &switch_sl);
    if(OPAL_SUCCESS != rc){
        goto ERROR;
    }


    /* Use: opal_hash_table_get_value_uint64 */
    return OPAL_SUCCESS;
ERROR:
    /* Close open files */
    if(NULL != fp){
        fclose(fp);
    }
    /* Release allocated resources */
    if(NULL != port_switch_lids){
        port_to_switch_lids* p_list = port_switch_lids;
        port_to_switch_lids* p_item = NULL;
        while(p_list->next != NULL){
            p_item = p_list->next;
            if(NULL != p_item){
                p_list->next = p_item->next;
                free(p_item);
             }
         }
         free(p_list);
     }
     if(NULL != switch_sl){
         switch_to_switch_sl* p_list = switch_sl;
         switch_to_switch_sl* p_item = NULL;
         while(p_list->next != NULL){
             p_item = p_list->next;
             if(NULL != p_item){
                 p_list->next = p_item->next;
                 free(p_item);
             }
         }
         free(p_list);
    }
    return rc;
}

