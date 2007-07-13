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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "opal/util/show_help.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"

#include "btl_udapl.h"
#include "btl_udapl_endpoint.h"
#include "btl_udapl_frag.h"
#include "btl_udapl_mca.h"
#include "btl_udapl_proc.h"
#include "ompi/datatype/convertor.h" 
#include "ompi/datatype/datatype.h" 
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/proc/proc.h"

static int udapl_reg_mr(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg);
static int udapl_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg);
static int mca_btl_udapl_set_peer_parameters(
        struct mca_btl_udapl_module_t* udapl_btl,
        size_t nprocs);

mca_btl_udapl_module_t mca_btl_udapl_module = {
    {
        &mca_btl_udapl_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* min rdma fragment size */
        0, /* max rdma fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        MCA_BTL_FLAGS_SEND,
        mca_btl_udapl_add_procs,
        mca_btl_udapl_del_procs,
        mca_btl_udapl_register, 
        mca_btl_udapl_finalize,
        mca_btl_udapl_alloc, 
        mca_btl_udapl_free, 
        mca_btl_udapl_prepare_src,
        mca_btl_udapl_prepare_dst,
        mca_btl_udapl_send,
        mca_btl_udapl_put,
        NULL, /* get */ 
        mca_btl_base_dump,
        NULL, /* mpool */
        NULL /* register error cb */ 
    }
};

static int udapl_reg_mr(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg)
{
    mca_btl_udapl_module_t *btl = (mca_btl_udapl_module_t*)reg_data;
    mca_btl_udapl_reg_t *udapl_reg = (mca_btl_udapl_reg_t*)reg;
    DAT_REGION_DESCRIPTION region;
    DAT_VLEN dat_size;
    DAT_VADDR dat_addr;
    int rc;

    region.for_va = base;
    udapl_reg->lmr_triplet.virtual_address = (DAT_VADDR)base;
    udapl_reg->lmr_triplet.segment_length = size;
    udapl_reg->lmr = NULL;

    rc = dat_lmr_create(btl->udapl_ia, DAT_MEM_TYPE_VIRTUAL, region, size,
            btl->udapl_pz, DAT_MEM_PRIV_ALL_FLAG, &udapl_reg->lmr,
            &udapl_reg->lmr_triplet.lmr_context, &udapl_reg->rmr_context,
            &dat_size, &dat_addr);

    if(rc != DAT_SUCCESS) {
        opal_show_help("help-mpi-btl-udapl.txt",
            "dat_lmr_create DAT_INSUFFICIENT_RESOURCES", true);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static int udapl_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg)
{
    mca_btl_udapl_reg_t *udapl_reg = (mca_btl_udapl_reg_t*)reg;
    int rc;

    if(udapl_reg->lmr != NULL) {
        rc = dat_lmr_free(udapl_reg->lmr);
        if(rc != DAT_SUCCESS) {
            char* major;
            char* minor;

            dat_strerror(rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_lmr_free",
                major, minor));
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

/**
 * Initialize module module resources.
 */

int
mca_btl_udapl_init(DAT_NAME_PTR ia_name, mca_btl_udapl_module_t* btl)
{
    mca_mpool_base_resources_t res;
    DAT_CONN_QUAL port;
    DAT_RETURN rc;

    /* open the uDAPL interface */
    btl->udapl_evd_async = DAT_HANDLE_NULL;
    rc = dat_ia_open(ia_name, btl->udapl_async_evd_qlen,
            &btl->udapl_evd_async, &btl->udapl_ia);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);

        opal_show_help("help-mpi-btl-udapl.txt",
            "dat_ia_open fail", true, ia_name, major, minor);

        return OMPI_ERROR;
    }

    /* create a protection zone */
    rc = dat_pz_create(btl->udapl_ia, &btl->udapl_pz);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_pz_create",
            major, minor));
        goto failure;
    }

    /* query to get address information */
    rc = dat_ia_query(btl->udapl_ia, &btl->udapl_evd_async,
            DAT_IA_ALL, &(btl->udapl_ia_attr), 0, NULL);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ia_query",
            major, minor));
        goto failure;
    }

    memcpy(&btl->udapl_addr.addr, (btl->udapl_ia_attr).ia_address_ptr,
        sizeof(DAT_SOCK_ADDR));

    /* check evd qlen against adapter max */
    if (btl->udapl_dto_evd_qlen > (btl->udapl_ia_attr).max_evd_qlen) {
        opal_show_help("help-mpi-btl-udapl.txt",
            "evd_qlen adapter max", 
            true,
            "btl_udapl_dto_evd_qlen",
            btl->udapl_dto_evd_qlen,
            (btl->udapl_ia_attr).max_evd_qlen);        
        btl->udapl_dto_evd_qlen = btl->udapl_ia_attr.max_evd_qlen;
    }
    if (btl->udapl_conn_evd_qlen > (btl->udapl_ia_attr).max_evd_qlen) {
        opal_show_help("help-mpi-btl-udapl.txt",
            "evd_qlen adapter max", 
            true,
            "btl_udapl_conn_evd_qlen",
            btl->udapl_conn_evd_qlen,
            (btl->udapl_ia_attr).max_evd_qlen);        
        btl->udapl_conn_evd_qlen = btl->udapl_ia_attr.max_evd_qlen;
    }

    /* set up evd's */
    rc = dat_evd_create(btl->udapl_ia,
        btl->udapl_dto_evd_qlen, DAT_HANDLE_NULL,
        DAT_EVD_DTO_FLAG | DAT_EVD_RMR_BIND_FLAG, &btl->udapl_evd_dto);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_create (dto)",
            major, minor));
        goto failure;
    }

    rc = dat_evd_create(btl->udapl_ia,
            btl->udapl_conn_evd_qlen, DAT_HANDLE_NULL,
            DAT_EVD_CR_FLAG | DAT_EVD_CONNECTION_FLAG, &btl->udapl_evd_conn);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_create (conn)",
            major, minor));
        goto failure;
    }

    /* create our public service point */
    rc = dat_psp_create_any(btl->udapl_ia, &port, btl->udapl_evd_conn,
        DAT_PSP_CONSUMER_FLAG, &btl->udapl_psp);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_psp_create_any",
            major, minor));
        goto failure;
    }

    /* establish endpoint parameters */
    rc = mca_btl_udapl_endpoint_get_params(btl, &(btl->udapl_ep_param));
    if(OMPI_SUCCESS != rc) { 
        /* by not erroring out here we can try to continue with
         * the default endpoint parameter values
         */
        opal_show_help("help-mpi-btl-udapl.txt",
            "use default endpoint params", 
            true);
    }

    /* Save the port with the address information */
    /* TODO - since we're doing the hack below, do we need our own port? */
    btl->udapl_addr.port = port;

    /* Using dat_ep_query to obtain the remote port would be ideal but
     * since the current udapl implementations don't seem to support
     * this we store the port in udapl_addr and explictly exchange the
     * information later.
     */
    ((struct sockaddr_in*)&btl->udapl_addr.addr)->sin_port = htons(port);

    /* initialize the memory pool */
    res.reg_data = btl;
    res.sizeof_reg = sizeof(mca_btl_udapl_reg_t);
    res.register_mem = udapl_reg_mr;
    res.deregister_mem = udapl_dereg_mr;
    btl->super.btl_mpool = mca_mpool_base_module_create(
            mca_btl_udapl_component.udapl_mpool_name, &btl->super, &res);

    /* initialize objects */
    OBJ_CONSTRUCT(&btl->udapl_frag_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_max, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_user, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_control, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_lock, opal_mutex_t);
    
     /* check buffer alignment against dat library */
    if (mca_btl_udapl_component.udapl_buffer_alignment !=
        DAT_OPTIMAL_ALIGNMENT) {

        opal_show_help("help-mpi-btl-udapl.txt",
            "optimal buffer alignment mismatch", 
            true,
            DAT_OPTIMAL_ALIGNMENT,
            mca_btl_udapl_component.udapl_buffer_alignment,
            DAT_OPTIMAL_ALIGNMENT);
    }

    /* initialize free lists */
    ompi_free_list_init_ex(&btl->udapl_frag_eager,
        sizeof(mca_btl_udapl_frag_eager_t) +
            mca_btl_udapl_component.udapl_eager_frag_size,
        sizeof(mca_btl_udapl_frag_t),
        mca_btl_udapl_component.udapl_buffer_alignment,
        OBJ_CLASS(mca_btl_udapl_frag_eager_t),
        mca_btl_udapl_component.udapl_free_list_num,
        mca_btl_udapl_component.udapl_free_list_max,
        mca_btl_udapl_component.udapl_free_list_inc,
        btl->super.btl_mpool);

    ompi_free_list_init_ex(&btl->udapl_frag_max,
        sizeof(mca_btl_udapl_frag_max_t) +
            mca_btl_udapl_component.udapl_max_frag_size,
        sizeof(mca_btl_udapl_frag_t),
        mca_btl_udapl_component.udapl_buffer_alignment,
        OBJ_CLASS(mca_btl_udapl_frag_max_t),
        mca_btl_udapl_component.udapl_free_list_num,
        mca_btl_udapl_component.udapl_free_list_max,
        mca_btl_udapl_component.udapl_free_list_inc,
        btl->super.btl_mpool);

    ompi_free_list_init_ex(&btl->udapl_frag_user,
        sizeof(mca_btl_udapl_frag_user_t),
        sizeof(mca_btl_udapl_frag_t),
        mca_btl_udapl_component.udapl_buffer_alignment,
        OBJ_CLASS(mca_btl_udapl_frag_user_t),
        mca_btl_udapl_component.udapl_free_list_num,
        mca_btl_udapl_component.udapl_free_list_max,
        mca_btl_udapl_component.udapl_free_list_inc,
        NULL);

    ompi_free_list_init_ex(&btl->udapl_frag_control,
        sizeof(mca_btl_udapl_frag_eager_t) +
        mca_btl_udapl_component.udapl_eager_frag_size,
        sizeof(mca_btl_udapl_frag_t),
        mca_btl_udapl_component.udapl_buffer_alignment,
        OBJ_CLASS(mca_btl_udapl_frag_eager_t),
        mca_btl_udapl_component.udapl_free_list_num,
        -1,
        mca_btl_udapl_component.udapl_free_list_inc,
        btl->super.btl_mpool);

    /* initialize eager rdma buffer info */
    orte_pointer_array_init(&btl->udapl_eager_rdma_endpoints, 
        mca_btl_udapl_component.udapl_max_eager_rdma_peers,
        mca_btl_udapl_component.udapl_max_eager_rdma_peers, 
        0);
    btl->udapl_eager_rdma_endpoint_count = 0;
    OBJ_CONSTRUCT(&btl->udapl_eager_rdma_lock, opal_mutex_t);

    /* initialize miscellaneous variables */
    btl->udapl_async_events = 0;
    btl->udapl_connect_inprogress = 0;
    btl->udapl_num_peers = 0;

    /* TODO - Set up SRQ when it is supported */
    return OMPI_SUCCESS;

failure:
    dat_ia_close(btl->udapl_ia, DAT_CLOSE_ABRUPT_FLAG);
    return OMPI_ERROR;
}

/*
 * Cleanup/release module resources.
 */

int mca_btl_udapl_finalize(struct mca_btl_base_module_t* base_btl)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*) base_btl; 
    int32_t i;
    
    /*
     * Cleaning up the endpoints here because mca_btl_udapl_del_procs
     * is never called by upper layers.
     * Note: this is only looking at those endpoints which are available
     * off of the btl module rdma list. 
     */
    for (i=0; i < udapl_btl->udapl_eager_rdma_endpoint_count; i++) {
        mca_btl_udapl_endpoint_t* endpoint =
            orte_pointer_array_get_item(udapl_btl->udapl_eager_rdma_endpoints,
                i);

        OBJ_DESTRUCT(endpoint);
    }

    /* release uDAPL resources */
    dat_evd_free(udapl_btl->udapl_evd_dto);
    dat_evd_free(udapl_btl->udapl_evd_conn);
    dat_pz_free(udapl_btl->udapl_pz);
    dat_ia_close(udapl_btl->udapl_ia, DAT_CLOSE_GRACEFUL_FLAG);

    /* destroy objects */
    OBJ_DESTRUCT(&udapl_btl->udapl_lock);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_eager);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_max);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_user);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_control);
    OBJ_DESTRUCT(&udapl_btl->udapl_eager_rdma_lock);
    
    free(udapl_btl);
    return OMPI_SUCCESS;
}


/*
 * Adjust parameters that are dependent on the number of peers.
 *
 * @param udapl_btl (IN)      BTL module
 * @param nprocs (IN)         number of processes handed into
 *                                mca_btl_udapl_add_procs()
 * @return                    OMPI_SUCCESS or error status on failure
 */

static int mca_btl_udapl_set_peer_parameters(
    struct mca_btl_udapl_module_t* udapl_btl,
    size_t nprocs) 
{
    int rc = OMPI_SUCCESS;
    DAT_RETURN dat_rc = DAT_SUCCESS;
    uint potential_udapl_timeout;
    int first_time_sizing = (udapl_btl->udapl_num_peers == 0 ? 1 : 0);
    DAT_EVD_PARAM evd_param;
    
    /* nprocs includes self so subtract 1 */
    udapl_btl->udapl_num_peers += nprocs - 1; 

    /* resize dto_evd_qlen if not already at its max */
    if (udapl_btl->udapl_dto_evd_qlen !=
        udapl_btl->udapl_ia_attr.max_evd_qlen) {

        int potential_dto_evd_qlen;
        int max_connection_dto_events;
        int eager_connection_dto_events;

        /* eager connection dto events already factored into
         * max_recv/request_dtos but need to calculate max connection dtos;
         * see mca_btl_udapl_get_params() for max_recv/request_dtos 
         */
        eager_connection_dto_events = udapl_btl->udapl_max_recv_dtos +
            udapl_btl->udapl_max_request_dtos;
        max_connection_dto_events = mca_btl_udapl_component.udapl_num_recvs +
            mca_btl_udapl_component.udapl_num_sends +
            (mca_btl_udapl_component.udapl_num_recvs /
                mca_btl_udapl_component.udapl_sr_win) + 1;
        potential_dto_evd_qlen = udapl_btl->udapl_num_peers *
            (eager_connection_dto_events + max_connection_dto_events);
        
        /* here we use what the library calculates as the
         * potential_dto_evd_qlen unless the user has set
         */
        if (first_time_sizing) { 
            if (udapl_btl->udapl_dto_evd_qlen < potential_dto_evd_qlen) {
                if (MCA_BTL_UDAPL_DTO_EVD_QLEN_DEFAULT !=
                    udapl_btl->udapl_dto_evd_qlen) {

                    /* user modified so warn */
                    opal_show_help("help-mpi-btl-udapl.txt",
                        "evd_qlen too low", 
                        true,
                        "btl_udapl_dto_evd_qlen",
                        udapl_btl->udapl_dto_evd_qlen,
                        "btl_udapl_dto_evd_qlen",                        
                        potential_dto_evd_qlen);
                } else {
                    udapl_btl->udapl_dto_evd_qlen = potential_dto_evd_qlen;
                }
            }
        } else {
            /* since this is not the first time attempting to resize the
             * evd queue length just use the potential value; this may not
             * be the best solution
             */
            udapl_btl->udapl_dto_evd_qlen = potential_dto_evd_qlen;
        }

        udapl_btl->udapl_dto_evd_qlen = ((udapl_btl->udapl_dto_evd_qlen >
            udapl_btl->udapl_ia_attr.max_evd_qlen) ?
            udapl_btl->udapl_ia_attr.max_evd_qlen :
            udapl_btl->udapl_dto_evd_qlen);
            
        /* OFED stack does not return DAT_INVALID_STATE when
         * the new qlen is less than current value so here we find
         * current value and if greater than what we intend to set
         * it to skip the resize. 
         */
        dat_rc = dat_evd_query(udapl_btl->udapl_evd_dto,
            DAT_EVD_FIELD_EVD_QLEN, &evd_param);
        if(DAT_SUCCESS != dat_rc) {
            char* major;
            char* minor;

            dat_strerror(dat_rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_query",
                major, minor));
        }

        if (udapl_btl->udapl_dto_evd_qlen > evd_param.evd_qlen) {
            /* resize dto event dispatcher queue length */
            dat_rc = dat_evd_resize(udapl_btl->udapl_evd_dto,
                udapl_btl->udapl_dto_evd_qlen);
            if(DAT_SUCCESS != dat_rc) {
                char* major;
                char* minor;

                dat_strerror(dat_rc, (const char**)&major,
                    (const char**)&minor);
                BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_resize",
                    major, minor));
                rc = OMPI_ERR_OUT_OF_RESOURCE;
            } 
        }
    }

    /* resize connection evd qlen */
    if (udapl_btl->udapl_conn_evd_qlen !=
        udapl_btl->udapl_ia_attr.max_evd_qlen) {

        int potential_conn_evd_qlen = 2 * udapl_btl->udapl_num_peers;

        if (first_time_sizing) { 
            if (udapl_btl->udapl_conn_evd_qlen < potential_conn_evd_qlen) {
                if (MCA_BTL_UDAPL_CONN_EVD_QLEN_DEFAULT !=
                    udapl_btl->udapl_conn_evd_qlen) {

                    /* user modified so warn */
                    opal_show_help("help-mpi-btl-udapl.txt",
                        "evd_qlen too low", 
                        true,
                        "btl_udapl_conn_evd_qlen",
                        udapl_btl->udapl_conn_evd_qlen,
                        "btl_udapl_conn_evd_qlen",
                        potential_conn_evd_qlen);
                } else {
                    udapl_btl->udapl_conn_evd_qlen = potential_conn_evd_qlen;
                }
            }
        } else {
            /* since this is not the first time attempting to resize the
             * evd queue length just use the potential value; this may not
             * be the best solution
             */
            udapl_btl->udapl_conn_evd_qlen = potential_conn_evd_qlen;
        }

        udapl_btl->udapl_conn_evd_qlen = ((udapl_btl->udapl_conn_evd_qlen >
            udapl_btl->udapl_ia_attr.max_evd_qlen) ?
            udapl_btl->udapl_ia_attr.max_evd_qlen :
            udapl_btl->udapl_conn_evd_qlen);
        
        /* OFED stack does not return DAT_INVALID_STATE when
         * the new qlen is less than current value so here we find
         * current value and if greater than what we intend to set
         * it to skip the resize. 
         */
        dat_rc = dat_evd_query(udapl_btl->udapl_evd_conn,
            DAT_EVD_FIELD_EVD_QLEN, &evd_param);
        if(DAT_SUCCESS != dat_rc) {
            char* major;
            char* minor;

            dat_strerror(dat_rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_query",
                major, minor));
        }

        if (udapl_btl->udapl_conn_evd_qlen > evd_param.evd_qlen) {
            /* resize conn evd queue length */
            dat_rc = dat_evd_resize(udapl_btl->udapl_evd_conn,
                udapl_btl->udapl_conn_evd_qlen);
            if(DAT_SUCCESS != dat_rc) {
                char* major;
                char* minor;

                dat_strerror(dat_rc, (const char**)&major,
                    (const char**)&minor);
                BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_resize",
                    major, minor));
                rc = OMPI_ERR_OUT_OF_RESOURCE;
            } 
        }
    }
    
    /* adjust connection timeout value, calculated in microseconds */
    potential_udapl_timeout = MCA_BTL_UDAPL_CONN_TIMEOUT_INC *
        udapl_btl->udapl_num_peers;
    
    if (mca_btl_udapl_component.udapl_timeout <
        potential_udapl_timeout) {

        if (MCA_BTL_UDAPL_CONN_TIMEOUT_DEFAULT !=
            mca_btl_udapl_component.udapl_timeout) {

            /* user modified so warn */
            opal_show_help("help-mpi-btl-udapl.txt",
                "connection timeout low", 
                true,
                "btl_udapl_timeout",
                mca_btl_udapl_component.udapl_timeout,
                "btl_udapl_timeout",                
                potential_udapl_timeout);         
        } else {
            mca_btl_udapl_component.udapl_timeout =
                potential_udapl_timeout;
        }
    }
    mca_btl_udapl_component.udapl_timeout =
        ((mca_btl_udapl_component.udapl_timeout >
            MCA_BTL_UDAPL_CONN_TIMEOUT_MAX) ?
            MCA_BTL_UDAPL_CONN_TIMEOUT_MAX :
            mca_btl_udapl_component.udapl_timeout);

    return rc;
}

/*
 *
 */

int mca_btl_udapl_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*)btl;
    int i, rc;

    for(i = 0; i < (int) nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_udapl_proc_t* udapl_proc;
        mca_btl_base_endpoint_t* udapl_endpoint;

        if(ompi_proc == ompi_proc_local()) 
            continue;

        if(NULL == (udapl_proc = mca_btl_udapl_proc_create(ompi_proc))) {
            continue;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this BTL instance to the proc.
         */

        OPAL_THREAD_LOCK(&udapl_proc->proc_lock);

        /* The btl_proc datastructure is shared by all uDAPL BTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        udapl_endpoint = OBJ_NEW(mca_btl_udapl_endpoint_t);
        if(NULL == udapl_endpoint) {
            OPAL_THREAD_UNLOCK(&udapl_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        udapl_endpoint->endpoint_btl = udapl_btl;
        rc = mca_btl_udapl_proc_insert(udapl_proc, udapl_endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(udapl_endpoint);
            OPAL_THREAD_UNLOCK(&udapl_proc->proc_lock);
            continue;
        }

        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&udapl_proc->proc_lock);
        peers[i] = udapl_endpoint;
    }

    /* resize based on number of processes */
    if (OMPI_SUCCESS !=
        mca_btl_udapl_set_peer_parameters(udapl_btl, nprocs)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}


int mca_btl_udapl_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_btl_base_endpoint_t ** peers)
{
    /* TODO */
    return OMPI_SUCCESS;
}


/**
 * Register callback function to support send/recv semantics
 */

int mca_btl_udapl_register(
                        struct mca_btl_base_module_t* btl, 
                        mca_btl_base_tag_t tag, 
                        mca_btl_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*) btl; 
    udapl_btl->udapl_reg[tag].cbfunc = cbfunc; 
    udapl_btl->udapl_reg[tag].cbdata = cbdata; 

    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_udapl_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*) btl; 
    mca_btl_udapl_frag_t* frag;
    int rc;
    int pad = 0;
    
    /* compute pad as needed */
    MCA_BTL_UDAPL_FRAG_CALC_ALIGNMENT_PAD(pad,
        (size + sizeof(mca_btl_udapl_footer_t)));

    if((size + pad) <= btl->btl_eager_limit) { 
        MCA_BTL_UDAPL_FRAG_ALLOC_EAGER(udapl_btl, frag, rc); 
    } else if(size <= btl->btl_max_send_size) {
        MCA_BTL_UDAPL_FRAG_ALLOC_MAX(udapl_btl, frag, rc); 
    } else {
        return NULL;
    }

    frag->segment.seg_len = size;

    /* Set up the LMR triplet from the frag segment.
     * Note: The triplet.segment_len is set to what is required for
     * actually sending the fragment, if later it is determined
     * that rdma can be used to transfer the fragment the
     * triplet.segment_len will have to change.
     */
    frag->triplet.virtual_address = (DAT_VADDR)frag->segment.seg_addr.pval;
    frag->triplet.segment_length =
        frag->segment.seg_len + sizeof(mca_btl_udapl_footer_t);
    assert(frag->triplet.lmr_context ==
        frag->registration->lmr_triplet.lmr_context);
    
    frag->btl = udapl_btl;
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;

    return &frag->base;
}


/**
 * Return a segment
 */

int mca_btl_udapl_free(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_descriptor_t* des) 
{
    mca_btl_udapl_frag_t* frag = (mca_btl_udapl_frag_t*)des;

    if(0 == frag->size) {
        if (NULL != frag->registration) {
            btl->btl_mpool->mpool_deregister(btl->btl_mpool, 
                                         &(frag->registration->base));
            frag->registration = NULL;
        }
        MCA_BTL_UDAPL_FRAG_RETURN_USER(btl, frag); 
    } else if(frag->size == mca_btl_udapl_component.udapl_eager_frag_size) {
        MCA_BTL_UDAPL_FRAG_RETURN_EAGER(btl, frag); 
    } else if(frag->size == mca_btl_udapl_component.udapl_max_frag_size) {
        MCA_BTL_UDAPL_FRAG_RETURN_MAX(btl, frag); 
    } else {
        OPAL_OUTPUT((0, "[%s:%d] mca_btl_udapl_free: invalid descriptor\n", __FILE__,__LINE__));
        return OMPI_ERR_BAD_PARAM;
    }
    return OMPI_SUCCESS; 
}

/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
mca_btl_base_descriptor_t* mca_btl_udapl_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
)
{
    mca_btl_udapl_frag_t* frag = NULL;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;
    int pad = 0;

    /* compute pad as needed */
    MCA_BTL_UDAPL_FRAG_CALC_ALIGNMENT_PAD(pad,
        (max_data + reserve + sizeof(mca_btl_udapl_footer_t)));

    if(ompi_convertor_need_buffers(convertor) == false && 0 == reserve) {
        if(registration != NULL || max_data > btl->btl_max_send_size) {

            MCA_BTL_UDAPL_FRAG_ALLOC_USER(btl, frag, rc);
            if(NULL == frag){
                return NULL;
            }

            iov.iov_len = max_data;
            iov.iov_base = NULL;

            ompi_convertor_pack(convertor, &iov,
                &iov_count, &max_data );

            *size = max_data;
        
            if(NULL == registration) {
                rc = btl->btl_mpool->mpool_register(btl->btl_mpool, iov.iov_base,
                    max_data, 0,
                    &registration);

                if(rc != OMPI_SUCCESS) {
                    MCA_BTL_UDAPL_FRAG_RETURN_USER(btl,frag);
                    return NULL;
                }
                /* keep track of the registration we did */
                frag->registration = (mca_btl_udapl_reg_t*)registration;
            }

            frag->segment.seg_len = max_data;
            frag->segment.seg_addr.pval = iov.iov_base;
            frag->triplet.segment_length = max_data;
            frag->triplet.virtual_address = (DAT_VADDR)iov.iov_base;
            frag->triplet.lmr_context =
                ((mca_btl_udapl_reg_t*)registration)->lmr_triplet.lmr_context;

            /* initialize base descriptor */
            frag->base.des_src = &frag->segment;
            frag->base.des_src_cnt = 1;
            frag->base.des_dst = NULL;
            frag->base.des_dst_cnt = 0;
            frag->base.des_flags = 0;
            
            return &frag->base;
        }
    }

    if(max_data + pad + reserve <= btl->btl_eager_limit) {
        /* the data is small enough to fit in the eager frag and
         * memory is not prepinned */
        MCA_BTL_UDAPL_FRAG_ALLOC_EAGER(btl, frag, rc);
    }

    if(NULL == frag) {
        /* the data doesn't fit into eager frag or eager frag is
         * not available */
        MCA_BTL_UDAPL_FRAG_ALLOC_MAX(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        if(max_data + reserve > btl->btl_max_send_size) {
            max_data = btl->btl_max_send_size - reserve;
        }
    }
    
    iov.iov_len = max_data;
    iov.iov_base = (char *) frag->segment.seg_addr.pval + reserve;
    
    rc = ompi_convertor_pack(convertor,
        &iov, &iov_count, &max_data );
    if(rc < 0) {
        MCA_BTL_UDAPL_FRAG_RETURN_MAX(btl, frag);
        return NULL;
    }

    *size = max_data;

    /* setup lengths and addresses to send out data */
    frag->segment.seg_len = max_data + reserve;
    frag->triplet.segment_length =
        max_data + reserve + sizeof(mca_btl_udapl_footer_t);
    frag->triplet.virtual_address = (DAT_VADDR)frag->segment.seg_addr.pval;

    /* initialize base descriptor */
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;

    return &frag->base;
}


/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contiguous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT)
 */
mca_btl_base_descriptor_t* mca_btl_udapl_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_udapl_frag_t* frag;
    ptrdiff_t lb;
    int rc;

    MCA_BTL_UDAPL_FRAG_ALLOC_USER(btl, frag, rc);
    if(NULL == frag) {
        return NULL;
    }

    ompi_ddt_type_lb(convertor->pDesc, &lb);
    frag->segment.seg_len = *size;
    frag->segment.seg_addr.pval = convertor->pBaseBuf + lb + convertor->bConverted;

    if(NULL == registration) {
        /* didn't get a memory registration passed in, so must
         * register the region now
         */ 
        rc = btl->btl_mpool->mpool_register(btl->btl_mpool,
                                   frag->segment.seg_addr.pval,
                                   frag->segment.seg_len,
                                   0,
                                   &registration);
        if(OMPI_SUCCESS != rc || NULL == registration) {
            MCA_BTL_UDAPL_FRAG_RETURN_USER(btl,frag);
            return NULL;
        }
        frag->registration = (mca_btl_udapl_reg_t*)registration;        
    }

    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = 0;

    frag->segment.seg_key.key32[0] =
        ((mca_btl_udapl_reg_t*)registration)->rmr_context;
    
    return &frag->base;
}

/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 * @param tag (IN)         The tag value used to notify the peer.
 */

int mca_btl_udapl_send( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* des, 
    mca_btl_base_tag_t tag)
   
{
    mca_btl_udapl_frag_t* frag = (mca_btl_udapl_frag_t*)des;

    frag->endpoint = endpoint;
    frag->ftr = (mca_btl_udapl_footer_t *)
        ((char *)frag->segment.seg_addr.pval + frag->segment.seg_len);
    frag->ftr->tag = tag;
    frag->type = MCA_BTL_UDAPL_SEND;

    /* TODO - will inlining this give worthwhile performance? */
    return mca_btl_udapl_endpoint_send(endpoint, frag);
}



/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

int mca_btl_udapl_put( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des)
{
    DAT_RMR_TRIPLET remote_buffer;
    DAT_DTO_COOKIE cookie;
    int rc = OMPI_SUCCESS;
    
    mca_btl_udapl_frag_t* frag = (mca_btl_udapl_frag_t*)des;
    mca_btl_base_segment_t *dst_segment = des->des_dst;

    frag->btl = (mca_btl_udapl_module_t *)btl;
    frag->endpoint = endpoint;
    frag->type = MCA_BTL_UDAPL_PUT;

    if(OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION], -1) < 0) {
        OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION], 1);
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        opal_list_append(&endpoint->endpoint_max_frags,
            (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        opal_progress();
    } else {
        frag->triplet.segment_length = frag->segment.seg_len;
        
        remote_buffer.rmr_context =
            (DAT_RMR_CONTEXT)dst_segment->seg_key.key32[0];
        remote_buffer.target_address =
            (DAT_VADDR)dst_segment->seg_addr.lval;
        remote_buffer.segment_length = dst_segment->seg_len;

        cookie.as_ptr = frag;
        
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        rc = dat_ep_post_rdma_write(endpoint->endpoint_max,
            1,
            &frag->triplet,
            cookie,
            &remote_buffer,
            DAT_COMPLETION_DEFAULT_FLAG);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        if(DAT_SUCCESS != rc) {
            char* major;
            char* minor;

            dat_strerror(rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_rdma_write",
                major, minor));
            rc = OMPI_ERROR;
        }
    }
    
    return rc;
}



/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

int mca_btl_udapl_get( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des)
{
    OPAL_OUTPUT((0, "udapl_get\n"));
    return OMPI_ERR_NOT_IMPLEMENTED;
}

