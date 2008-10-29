/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <errno.h>
#include <string.h>
#include <infiniband/verbs.h>

#include "ompi_config.h"
#include "ompi/constants.h"
#include "opal/prefetch.h"
#include "orte/util/show_help.h"
#include "ompi/mca/btl/btl.h"
#include "opal/sys/timer.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/runtime/mpiruntime.h"

#include "orte/runtime/orte_globals.h"

#include "btl_ofud.h"
#include "btl_ofud_frag.h"
#include "btl_ofud_endpoint.h"


mca_btl_ud_component_t mca_btl_ofud_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */
        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "ofud", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_ud_component_open,  /* component open */
            mca_btl_ud_component_close  /* component close */
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        mca_btl_ud_component_init,
        mca_btl_ud_component_progress,
    }
};


/*
 * Profiling information
 */

#if MCA_BTL_UD_ENABLE_PROFILE
mca_btl_ud_profile_t mca_btl_ud_profile = {0};
#endif


/*
 * utility routines for parameter registration
 */

static inline void mca_btl_ud_param_reg_string(const char* param_name,
                                                    const char* param_desc,
                                                    const char* default_value,
                                                    char** out_value)
{
    mca_base_param_reg_string(&mca_btl_ofud_component.super.btl_version,
                              param_name, param_desc, false, false,
                              default_value, out_value);
}

static inline void mca_btl_ud_param_reg_int(const char* param_name,
                                                 const char* param_desc,
                                                 int default_value,
                                                 int* out_value)
{
    mca_base_param_reg_int(&mca_btl_ofud_component.super.btl_version,
                           param_name, param_desc, false, false,
                           default_value, out_value);
}


/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_ud_component_open(void)
{
    int val;
    
    /* initialize state */
    mca_btl_ofud_component.num_btls = 0;
    mca_btl_ofud_component.ud_btls = NULL;
    
    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_ofud_component.ud_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_ofud_component.ud_procs, opal_list_t);

    /* register IB component parameters */
    mca_btl_ud_param_reg_int("max_btls",
                             "Maximum number of HCAs/ports to use",
                             4, (int*)&mca_btl_ofud_component.max_btls);

    mca_btl_ud_param_reg_string("mpool", "Name of the memory pool to be used",
                                "rdma", &mca_btl_ofud_component.ud_mpool_name);

    mca_btl_ud_param_reg_int("ib_pkey_index", "IB pkey index",
                             0, (int*)&mca_btl_ofud_component.ib_pkey_ix);
    mca_btl_ud_param_reg_int("ib_qkey", "IB qkey",
                             0x01330133, (int*)&mca_btl_ofud_component.ib_qkey);
    mca_btl_ud_param_reg_int("ib_service_level", "IB service level",
                             0, (int*)&mca_btl_ofud_component.ib_service_level);
    mca_btl_ud_param_reg_int("ib_src_path_bits", "IB source path bits",
                             0, (int*)&mca_btl_ofud_component.ib_src_path_bits);

    mca_btl_ud_param_reg_int("sd_num", "maximum send descriptors to post",
                             128, (int*)&mca_btl_ofud_component.sd_num);

    mca_btl_ud_param_reg_int("rd_num", "number of receive buffers",
                             6000, (int*)&mca_btl_ofud_component.rd_num);
#if 0
    mca_btl_ud_param_reg_int("rd_num_init", "initial receive buffers",
                             3000, (int*)&mca_btl_ofud_component.rd_num_init);
    mca_btl_ud_param_reg_int("rd_num_max", "maximum receive buffers",
                             4500, (int*)&mca_btl_ofud_component.rd_num_max);
    mca_btl_ud_param_reg_int("rd_num_inc",
                             "number of buffers to post when rate is high",
                             25, (int*)&mca_btl_ofud_component.rd_num_inc);
#endif

    /* TODO - this assumes a 2k UD MTU - query/do something more intelligent */
    /*mca_btl_ud_param_reg_int("eager_limit", "eager send limit",
                             2048, &val); */
    mca_btl_ud_param_reg_int("min_send_size", "minimum send size",
                             2048, &val);
    mca_btl_ofud_module.super.btl_rndv_eager_limit = val;
    mca_btl_ud_param_reg_int("max_send_size", "maximum send size",
                             2048, &val);
    mca_btl_ofud_module.super.btl_eager_limit = val;
    mca_btl_ofud_module.super.btl_max_send_size = val;

    mca_btl_ud_param_reg_int("exclusivity", "BTL exclusivity",
                             MCA_BTL_EXCLUSIVITY_DEFAULT,
                             (int*)&mca_btl_ofud_module.super.btl_exclusivity);
    mca_btl_ud_param_reg_int("bandwidth",
                             "Approximate maximum bandwidth of interconnect",
                             800, (int*)&mca_btl_ofud_module.super.btl_bandwidth);
    
    mca_btl_ofud_module.super.btl_eager_limit -= sizeof(mca_btl_ud_header_t);
    mca_btl_ofud_module.super.btl_max_send_size -= sizeof(mca_btl_ud_header_t);

    return OMPI_SUCCESS;
}


/*
 * Component cleanup 
 */

int mca_btl_ud_component_close(void)
{
    OBJ_DESTRUCT(&mca_btl_ofud_component.ud_lock);
    OBJ_DESTRUCT(&mca_btl_ofud_component.ud_procs);

    /* Calculate and print profiling numbers */
    MCA_BTL_UD_SHOW_TIME(post_send);
    MCA_BTL_UD_SHOW_TIME(ibv_post_send);

    return OMPI_SUCCESS;
}


/*
 * Register UD address information.  The MCA framework
 * will make this available to all peers.
 */

static int mca_btl_ud_modex_send(void)
{
    int rc;
    size_t i;
    size_t size;
    mca_btl_ud_addr_t* addrs = NULL;

    size = mca_btl_ofud_component.num_btls * sizeof(mca_btl_ud_addr_t);
    if(size != 0) {
        addrs = (mca_btl_ud_addr_t*)malloc(size);
        if(NULL == addrs) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for(i = 0; i < mca_btl_ofud_component.num_btls; i++) {
            mca_btl_ud_module_t* btl = &mca_btl_ofud_component.ud_btls[i];
            addrs[i] = btl->addr;
    
            BTL_VERBOSE((0, "modex_send QP num %x, LID = %x",
              addrs[i].qp_num, addrs[i].lid));
        }
    }

    rc = ompi_modex_send(
            &mca_btl_ofud_component.super.btl_version, addrs, size);
    if(NULL != addrs) {
        free(addrs);
    }
    return rc;
}

/*
 *  UD component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) post OOB receive for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */

mca_btl_base_module_t** mca_btl_ud_component_init(int* num_btl_modules,
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    struct ibv_device **ib_devs;
    struct ibv_device* ib_dev;
    int32_t num_devs;
    mca_btl_base_module_t** btls;
    uint32_t i, j;
    opal_list_t btl_list;
    mca_btl_ud_module_t* ud_btl;
    mca_btl_base_selected_module_t* ib_selected;
    opal_list_item_t* item;
    unsigned short seedv[3];
    char* btl_str;
    char* tok;

    /* Currently refuse to run if MPI_THREAD_MULTIPLE is enabled */
    if (ompi_mpi_thread_multiple && !mca_btl_base_thread_multiple_override) {
        return NULL;
    }

    /* First, check if the UD BTL was specifically selected.
       If not, then short out right away. */
    mca_base_param_lookup_string(
            mca_base_param_find("btl", NULL, NULL), &btl_str);
    if(NULL == btl_str || '^' == btl_str[0]) {
        /* No string at all, or an exclusion string, bail out */
        return NULL;
    }

    /* Try to find a 'ofud' token */
    tok = strtok(btl_str, ",");
    while(tok) {
        if(!strcasecmp("ofud", tok)) {
            break;
        }
    }

    if(NULL == tok) {
        /* No valid 'ofud' token found; bail out */
        return NULL;
    }

    /* initialization */
    *num_btl_modules = 0;
    num_devs = 0;

    seedv[0] = ORTE_PROC_MY_NAME->vpid;
    seedv[1] = opal_sys_timer_get_cycles();
    seedv[2] = opal_sys_timer_get_cycles();
    seed48(seedv);

    ib_devs = ibv_get_device_list(&num_devs);

    if(0 == num_devs) {
        mca_btl_base_error_no_nics("OpenFabrics UD", "HCA");
        mca_btl_ud_modex_send();
        return NULL;
    }

    /** We must loop through all the hca id's, get their handles and
        for each hca we query the number of ports on the hca and set up
        a distinct btl module for each hca port */

    OBJ_CONSTRUCT(&btl_list, opal_list_t);

    for(i = 0; (int32_t)i < num_devs &&
            mca_btl_ofud_component.num_btls < mca_btl_ofud_component.max_btls;
            i++) {
        struct ibv_device_attr ib_dev_attr;
        struct ibv_context* ib_dev_context;

        ib_dev = ib_devs[i];

        ib_dev_context = ibv_open_device(ib_dev);
        if(!ib_dev_context) {
            BTL_ERROR(("error obtaining device context for %s: %s\n",
                        ibv_get_device_name(ib_dev), strerror(errno)));
            return NULL;
        }

        if(ibv_query_device(ib_dev_context, &ib_dev_attr)){
            BTL_ERROR(("error obtaining device attributes for %s: %s\n",
                        ibv_get_device_name(ib_dev), strerror(errno)));
            return NULL;
        }


        /* Note ports are 1 based hence j = 1 */
        for(j = 1; j <= ib_dev_attr.phys_port_cnt; j++) {
            struct ibv_port_attr ib_port_attr;

            if(ibv_query_port(ib_dev_context, (uint8_t)j, &ib_port_attr)) {
                BTL_ERROR(("error getting port attributes for device %s port %d: %s",
                          ibv_get_device_name(ib_dev), j, strerror(errno)));
                return NULL;
            }

            if(IBV_PORT_ACTIVE == ib_port_attr.state) {
                ud_btl =
                    (mca_btl_ud_module_t*)malloc(sizeof(mca_btl_ud_module_t));
                memcpy(ud_btl, &mca_btl_ofud_module, sizeof(mca_btl_ud_module_t));

                ib_selected = OBJ_NEW(mca_btl_base_selected_module_t);
                ib_selected->btl_module = (mca_btl_base_module_t*)ud_btl;

                ud_btl->ib_dev = ib_dev;
                ud_btl->ib_dev_context = ib_dev_context;
                ud_btl->ib_port_num = (uint8_t)j;
                ud_btl->addr.subnet = ib_port_attr.sm_lid;
                ud_btl->addr.lid = ib_port_attr.lid;

                opal_list_append(&btl_list, (opal_list_item_t*) ib_selected);
                if(++mca_btl_ofud_component.num_btls >=
                        mca_btl_ofud_component.max_btls)
                    break;
            }
        }
    }


    /* Allocate space for btl modules */
    mca_btl_ofud_component.ud_btls = (mca_btl_ud_module_t*)
        malloc(sizeof(mca_btl_ud_module_t) * mca_btl_ofud_component.num_btls);
    if(NULL == mca_btl_ofud_component.ud_btls) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    btls = (struct mca_btl_base_module_t**)
        malloc(mca_btl_ofud_component.num_btls * sizeof(mca_btl_ud_module_t*));
    if(NULL == btls) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }


    for(i = 0; i < mca_btl_ofud_component.num_btls; i++){
        item = opal_list_remove_first(&btl_list);
        ib_selected = (mca_btl_base_selected_module_t*)item;
        ud_btl = (mca_btl_ud_module_t*)ib_selected->btl_module;

        memcpy(&(mca_btl_ofud_component.ud_btls[i]),
                ud_btl, sizeof(mca_btl_ud_module_t));
        free(ib_selected);
        free(ud_btl);

        ud_btl = &mca_btl_ofud_component.ud_btls[i];

        /* Initialize module state */
        if(mca_btl_ud_module_init(ud_btl) != OMPI_SUCCESS) {
            mca_btl_ofud_component.num_btls--;
            i--;

            continue;
        }

        btls[i] = &ud_btl->super;
    }

    OBJ_DESTRUCT(&btl_list);
    mca_btl_ud_modex_send();
    
    /* Since not all modules may have initialized successfully, realloc
       to free space from failed modules */
    mca_btl_ofud_component.ud_btls = (mca_btl_ud_module_t*)
            realloc(mca_btl_ofud_component.ud_btls,
                sizeof(mca_btl_ud_module_t) * mca_btl_ofud_component.num_btls);
    btls = (struct mca_btl_base_module_t**)realloc(btls,
            mca_btl_ofud_component.num_btls * sizeof(mca_btl_ud_module_t*));

    *num_btl_modules = mca_btl_ofud_component.num_btls;

    ibv_free_device_list(ib_devs);
    return btls;
}


/*
 *  IB component progress.
 */

#define MCA_BTL_UD_NUM_WC       500

int mca_btl_ud_component_progress(void)
{
    uint32_t i;
    int count = 0, ne, j, btl_ownership;
    mca_btl_ud_frag_t* frag;
    struct ibv_recv_wr* bad_wr;
    struct ibv_recv_wr* head_wr;
    mca_btl_ud_module_t* ud_btl;
    mca_btl_active_message_callback_t* reg;
    struct ibv_wc* cwc;
    struct ibv_wc wc[MCA_BTL_UD_NUM_WC];

    /* Poll for completions */
    for(i = 0; i < mca_btl_ofud_component.num_btls; i++) {
        ud_btl = &mca_btl_ofud_component.ud_btls[i];

        ne = ibv_poll_cq(ud_btl->ib_cq, MCA_BTL_UD_NUM_WC, wc);
        if(OPAL_UNLIKELY(ne < 0)) {
            BTL_ERROR(("error polling CQ with %d: %s\n",
                    ne, strerror(errno)));
            return OMPI_ERROR;
        }

        head_wr = NULL;

        for(j = 0; j < ne; j++) {
            cwc = &wc[j];
            if(OPAL_UNLIKELY(cwc->status != IBV_WC_SUCCESS)) {
                BTL_ERROR(("error polling CQ with status %d for wr_id %llu opcode %d\n",
                           cwc->status, cwc->wr_id, cwc->opcode));
                return OMPI_ERROR;
            }

            frag = (mca_btl_ud_frag_t*)(unsigned long)cwc->wr_id;

            /* Handle work completions */
            switch(frag->type) {
            case MCA_BTL_UD_FRAG_SEND:
            case MCA_BTL_UD_FRAG_USER:
            {
                assert(cwc->opcode == IBV_WC_SEND);
                btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                frag->base.des_cbfunc(&ud_btl->super,
                        frag->endpoint, &frag->base, OMPI_SUCCESS);
                if( btl_ownership ) {
                    mca_btl_ud_free( &ud_btl->super, &frag->base );
                }

                /* Increment send counter, post if any sends are queued */
                OPAL_THREAD_ADD32(&ud_btl->sd_wqe, 1);
                if(OPAL_UNLIKELY(
                            !opal_list_is_empty(&ud_btl->pending_frags))) {
                    OPAL_THREAD_LOCK(&ud_btl->ud_lock);
                    frag = (mca_btl_ud_frag_t*)
                        opal_list_remove_first(&ud_btl->pending_frags);
                    OPAL_THREAD_UNLOCK(&ud_btl->ud_lock);

                    if(OPAL_LIKELY(NULL != frag)) {
                        mca_btl_ud_endpoint_post_send(ud_btl, frag);
                    }
                }

                continue;
            }
            case MCA_BTL_UD_FRAG_RECV:
                assert(cwc->opcode == IBV_WC_RECV);
                reg = mca_btl_base_active_message_trigger + frag->hdr->tag;

                frag->segment.seg_addr.pval = frag->hdr + 1;
                frag->segment.seg_len = cwc->byte_len -
                        sizeof(mca_btl_ud_header_t) -
                        sizeof(mca_btl_ud_ib_header_t);

                reg->cbfunc(&ud_btl->super,
                        frag->hdr->tag, &frag->base, reg->cbdata);

                /* Add recv to linked list for reposting */
                frag->wr_desc.rd_desc.next = head_wr;
                head_wr = &frag->wr_desc.rd_desc;
                continue;
            default:
                BTL_ERROR(("Unhandled completion opcode %d frag type %d",
                            cwc->opcode, frag->type));
                break;
            }
        }

        count += ne;

        /* Repost any recv buffers all at once */
        if(OPAL_LIKELY(head_wr)) {
            if(OPAL_UNLIKELY(ibv_post_recv(
                    ud_btl->ib_qp[0], head_wr, &bad_wr))) {
                BTL_ERROR(("error posting recv: %s\n", strerror(errno)));
                return OMPI_ERROR;
            }

            head_wr = NULL;
        }
    }

    return count;
}

