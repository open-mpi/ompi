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
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
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
#include "opal/sys/timer.h"

#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/base/base.h"
#include "btl_ud.h"
#include "btl_ud_frag.h"
#include "btl_ud_endpoint.h"
#include "ompi/mca/btl/base/base.h"


#include "ompi/datatype/convertor.h"
#include "ompi/mca/mpool/mpool.h"
#include <sysfs/libsysfs.h>
#include <infiniband/verbs.h>
#include <errno.h>
#include <string.h>   /* for strerror()*/

#include "ompi/mca/pml/base/pml_base_module_exchange.h"

mca_btl_ud_component_t mca_btl_ud_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_0,

            "ud", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_ud_component_open,  /* component open */
            mca_btl_ud_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
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

static inline void mca_btl_ud_param_register_string(
                                                        const char* param_name,
                                                        const char* param_desc,
                                                        const char* default_value,
                                                        char** out_value)
{
    mca_base_param_reg_string(&mca_btl_ud_component.super.btl_version,
                              param_name,
                              param_desc,
                              false,
                              false,
                              default_value,
                              out_value);
}

static inline void  mca_btl_ud_param_register_int(
                                                      const char* param_name,
                                                      const char* param_desc,
                                                      int default_value,
                                                      int* out_value)
{
    mca_base_param_reg_int(&mca_btl_ud_component.super.btl_version,
                           param_name,
                           param_desc,
                           false,
                           false,
                           default_value,
                           out_value);
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_ud_component_open(void)
{
    /* initialize state */
    mca_btl_ud_component.ib_num_btls=0;
    mca_btl_ud_component.ud_btls=NULL;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_ud_component.ib_procs, opal_list_t);

    /* register IB component parameters */
    mca_btl_ud_param_register_int ("max_btls", "maximum number of HCAs/ports to use",
                                      4, (int*)&mca_btl_ud_component.ib_max_btls);
    mca_btl_ud_param_register_int ("free_list_num", "intial size of free lists",
                                       8, &mca_btl_ud_component.ib_free_list_num);
    mca_btl_ud_param_register_int ("free_list_max", "maximum size of free lists",
                                       -1, &mca_btl_ud_component.ib_free_list_max);
    mca_btl_ud_param_register_int ("free_list_inc", "increment size of free lists",
                                       32, &mca_btl_ud_component.ib_free_list_inc);
    mca_btl_ud_param_register_string("mpool", "name of the memory pool to be used",
                                         "openib", &mca_btl_ud_component.ib_mpool_name);
    mca_btl_ud_param_register_int("reg_mru_len",  "length of the registration cache most recently used list",
                                      16, (int*) &mca_btl_ud_component.reg_mru_len);
    mca_btl_ud_param_register_int("use_srq", "if 1 use the IB shared receive queue to post receive descriptors",
                                      0, (int*) &mca_btl_ud_component.use_srq);
    mca_btl_ud_param_register_int("ib_cq_size", "size of the IB completion queue",
                                      2000, (int*) &mca_btl_ud_component.ib_cq_size);
    mca_btl_ud_param_register_int("ib_sg_list_size", "size of IB segment list",
                                      4, (int*) &mca_btl_ud_component.ib_sg_list_size);
    mca_btl_ud_param_register_int("ib_pkey_ix", "IB pkey index",
                                      0, (int*) &mca_btl_ud_component.ib_pkey_ix);
    mca_btl_ud_param_register_int("ib_qkey", "IB qkey",
                                      0x01330133, (int*) &mca_btl_ud_component.ib_qkey);
    mca_btl_ud_param_register_int("ib_psn", "IB Packet sequence starting number",
                                      0, (int*) &mca_btl_ud_component.ib_psn);
    mca_btl_ud_param_register_int("ib_service_level", "IB service level",
                                      0, (int*) &mca_btl_ud_component.ib_service_level);
    mca_btl_ud_param_register_int("ib_src_path_bits", "IB source path bits",
                                      0, (int*) &mca_btl_ud_component.ib_src_path_bits);
    mca_btl_ud_param_register_int ("exclusivity", "BTL exclusivity",
                                       MCA_BTL_EXCLUSIVITY_DEFAULT, (int*) &mca_btl_ud_module.super.btl_exclusivity);
    mca_btl_ud_param_register_int("sd_num", "maximum descriptors to post to a QP",
                                      16, (int*) &mca_btl_ud_component.sd_num);
    mca_btl_ud_param_register_int("rd_num", "number of receive descriptors to post to a QP",
                                      500, (int*) &mca_btl_ud_component.rd_num);
    mca_btl_ud_param_register_int("rd_low", "low water mark before reposting occurs",
                                      300,  (int*) &mca_btl_ud_component.rd_low);
    mca_btl_ud_param_register_int("srq_rd_max", "Max number of receive descriptors posted per SRQ.",
                                      1000, (int*) &mca_btl_ud_component.srq_rd_max);
    mca_btl_ud_param_register_int("srq_rd_per_peer", "Number of receive descriptors posted per peer. (SRQ)",
                                      16, (int*) &mca_btl_ud_component.srq_rd_per_peer);
    mca_btl_ud_param_register_int("srq_sd_max", "Maximum number of send descriptors posted. (SRQ)",
                                      8,  &mca_btl_ud_component.srq_sd_max);

    /* TODO - this assumes a 2k UD MTU - should query/do something more intelligent */
    mca_btl_ud_param_register_int ("eager_limit", "eager send limit",
            2047, (int*)&mca_btl_ud_module.super.btl_eager_limit);
    mca_btl_ud_param_register_int ("min_send_size", "minimum send size",
            2048, (int*)&mca_btl_ud_module.super.btl_min_send_size);
    mca_btl_ud_param_register_int ("max_send_size", "maximum send size",
            2048, (int*) &mca_btl_ud_module.super.btl_max_send_size);
    mca_btl_ud_param_register_int("bandwidth", "Approximate maximum bandwidth of interconnect",
            800, (int*) &mca_btl_ud_module.super.btl_bandwidth);

    mca_btl_ud_module.super.btl_eager_limit -= sizeof(mca_btl_ud_header_t);
    mca_btl_ud_module.super.btl_max_send_size -= sizeof(mca_btl_ud_header_t);
    mca_btl_ud_component.max_send_size = mca_btl_ud_module.super.btl_max_send_size;
    mca_btl_ud_component.eager_limit = mca_btl_ud_module.super.btl_eager_limit;

    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_ud_component_close(void)
{
    /* Calculate and print profiling numbers */
    MCA_BTL_UD_SHOW_TIME(post_send);
    MCA_BTL_UD_SHOW_TIME(endpoint_send_conn);
    MCA_BTL_UD_SHOW_TIME(ibv_post_send);
    MCA_BTL_UD_SHOW_TIME(full_send);

    return OMPI_SUCCESS;
}


/*
 *  Register UD port information. The MCA framework
 *  will make this available to all peers.
 */

static int
mca_btl_ud_modex_send(void)
{
    int         rc;
    size_t      i;
    size_t      size;
    mca_btl_ud_port_info_t *ports = NULL;

    size = mca_btl_ud_component.ib_num_btls * sizeof (mca_btl_ud_port_info_t);
    if (size != 0) {
        ports = (mca_btl_ud_port_info_t *)malloc (size);
        if (NULL == ports) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for (i = 0; i < mca_btl_ud_component.ib_num_btls; i++) {
            mca_btl_ud_module_t *btl = &mca_btl_ud_component.ud_btls[i];
            ports[i] = btl->port_info;
        }
    }
    rc = mca_pml_base_modex_send (&mca_btl_ud_component.super.btl_version, ports, size);
    if (NULL != ports) {
        free (ports);
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

mca_btl_base_module_t** mca_btl_ud_component_init(int *num_btl_modules,
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    struct ibv_device **ib_devs;
    struct ibv_device* ib_dev;
    int32_t num_devs;
    mca_btl_base_module_t** btls;
    uint32_t i,j, length;
    struct mca_mpool_base_resources_t mpool_resources;
    opal_list_t btl_list;
    mca_btl_ud_module_t* ud_btl;
    mca_btl_base_selected_module_t* ib_selected;
    opal_list_item_t* item;
    unsigned short seedv[3];

    /* initialization */
    *num_btl_modules = 0;
    num_devs = 0;

    seedv[0] = orte_process_info.my_name->vpid;
    seedv[1] = opal_sys_timer_get_cycles();
    seedv[2] = opal_sys_timer_get_cycles();
    seed48(seedv);

#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST
    ib_devs = ibv_get_device_list(&num_devs);
#else
    /* Determine the number of hca's available on the host */
    dev_list = ibv_get_devices();
    if (NULL == dev_list) {
        mca_btl_base_error_no_nics("OpenIB", "HCA");
        mca_btl_ud_component.ib_num_btls = 0;
        mca_btl_ud_modex_send();
        return NULL;
    }
    dlist_start(dev_list);

    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        num_devs++;
#endif

    if(0 == num_devs) {
        mca_btl_base_error_no_nics("OpenIB", "HCA");
        mca_btl_ud_modex_send();
        return NULL;
    }

#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST == 0
    /* Allocate space for the ib devices */
    ib_devs = (struct ibv_device**) malloc(num_devs * sizeof(struct ibv_dev*));
        if(NULL == ib_devs) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    dlist_start(dev_list);

    i = 0;
    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        ib_devs[i++] =  ib_dev;
#endif

    /** We must loop through all the hca id's, get there handles and
        for each hca we query the number of ports on the hca and set up
        a distinct btl module for each hca port */

    OBJ_CONSTRUCT(&btl_list, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_ud_component.ib_lock, opal_mutex_t);


    for(i = 0; (int32_t)i < num_devs
            && mca_btl_ud_component.ib_num_btls < mca_btl_ud_component.ib_max_btls; i++){
        struct ibv_device_attr ib_dev_attr;
        struct ibv_context* ib_dev_context;

        ib_dev = ib_devs[i];

        ib_dev_context = ibv_open_device(ib_dev);
        if(!ib_dev_context) {
            BTL_ERROR((" error obtaining device context for %s errno says %s\n", ibv_get_device_name(ib_dev), strerror(errno)));
            return NULL;
        }

        if(ibv_query_device(ib_dev_context, &ib_dev_attr)){
            BTL_ERROR(("error obtaining device attributes for %s errno says %s\n", ibv_get_device_name(ib_dev), strerror(errno)));
            return NULL;
        }


        /* Note ports are 1 based hence j = 1 */

        for(j = 1; j <= ib_dev_attr.phys_port_cnt; j++){
            struct ibv_port_attr* ib_port_attr;
            ib_port_attr = (struct ibv_port_attr*) malloc(sizeof(struct ibv_port_attr));
            if(ibv_query_port(ib_dev_context, (uint8_t) j, ib_port_attr)){
                BTL_ERROR(("error getting port attributes for device %s port number %d errno says %s",
                          ibv_get_device_name(ib_dev), j, strerror(errno)));
                return NULL;
            }

            if( IBV_PORT_ACTIVE == ib_port_attr->state ){
                ud_btl = (mca_btl_ud_module_t*) malloc(sizeof(mca_btl_ud_module_t));
                memcpy(ud_btl, &mca_btl_ud_module, sizeof(mca_btl_ud_module));

                ib_selected = OBJ_NEW(mca_btl_base_selected_module_t);
                ib_selected->btl_module = (mca_btl_base_module_t*) ud_btl;
                ud_btl->ib_dev = ib_dev;
                ud_btl->ib_dev_context = ib_dev_context;
                ud_btl->port_num = (uint8_t) j;
                ud_btl->ib_port_attr = ib_port_attr;
                ud_btl->port_info.subnet = ib_port_attr->sm_lid; /* store the sm_lid for multi-nic support */

                opal_list_append(&btl_list, (opal_list_item_t*) ib_selected);
                if(++mca_btl_ud_component.ib_num_btls >= mca_btl_ud_component.ib_max_btls)
                    break;
            }
            else{
                free(ib_port_attr);
            }
        }
    }


    /* Allocate space for btl modules */
    mca_btl_ud_component.ud_btls = (mca_btl_ud_module_t*)
        malloc(sizeof(mca_btl_ud_module_t) * mca_btl_ud_component.ib_num_btls);

    if(NULL == mca_btl_ud_component.ud_btls) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    btls = (struct mca_btl_base_module_t**)
        malloc(mca_btl_ud_component.ib_num_btls * sizeof(mca_btl_ud_module_t*));
    if(NULL == btls) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }


    for(i = 0; i < mca_btl_ud_component.ib_num_btls; i++){
        item = opal_list_remove_first(&btl_list);
        ib_selected = (mca_btl_base_selected_module_t*)item;
        ud_btl = (mca_btl_ud_module_t*) ib_selected->btl_module;
        memcpy(&(mca_btl_ud_component.ud_btls[i]), ud_btl , sizeof(mca_btl_ud_module_t));
        free(ib_selected);
        free(ud_btl);

        ud_btl = &mca_btl_ud_component.ud_btls[i];
        ud_btl->rd_num = mca_btl_ud_component.rd_num;
        ud_btl->rd_low = mca_btl_ud_component.rd_low;

        ud_btl->sd_wqe_lp = mca_btl_ud_component.sd_num;
        ud_btl->sd_wqe_hp = mca_btl_ud_component.sd_num;

        /* Initialize module state */
        OBJ_CONSTRUCT(&ud_btl->ib_lock, opal_mutex_t);
        OBJ_CONSTRUCT(&ud_btl->send_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&ud_btl->send_free_max, ompi_free_list_t);
        OBJ_CONSTRUCT(&ud_btl->send_free_frag, ompi_free_list_t);

        OBJ_CONSTRUCT(&ud_btl->recv_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&ud_btl->recv_free_max, ompi_free_list_t);

        OBJ_CONSTRUCT(&ud_btl->pending_frags_hp, opal_list_t);
        OBJ_CONSTRUCT(&ud_btl->pending_frags_lp, opal_list_t);

        if(mca_btl_ud_module_init(ud_btl) != OMPI_SUCCESS) {
#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST
	        ibv_free_device_list(ib_devs);
#else
	        free(ib_devs);
#endif
	        return NULL;
        }
	
        mpool_resources.ib_pd = ud_btl->ib_pd;

        /* initialize the memory pool using the hca */
        ud_btl->super.btl_mpool =
            mca_mpool_base_module_create(mca_btl_ud_component.ib_mpool_name,
                                         &ud_btl->super,
                                         &mpool_resources);

        if(NULL == ud_btl->super.btl_mpool) {
            BTL_ERROR(("error creating openib memory pool! aborting ud btl initialization"));
            return NULL;
        }

        /* Initialize pool of send fragments */
        length = sizeof(mca_btl_ud_frag_t) +
            sizeof(mca_btl_ud_header_t) +
            ud_btl->super.btl_eager_limit +
            2*MCA_BTL_IB_FRAG_ALIGN;

        ompi_free_list_init(&ud_btl->send_free_eager,
                            length,
                            OBJ_CLASS(mca_btl_ud_send_frag_eager_t),
                            mca_btl_ud_component.ib_free_list_num,
                            mca_btl_ud_component.ib_free_list_max,
                            mca_btl_ud_component.ib_free_list_inc,
                            ud_btl->super.btl_mpool);

        ompi_free_list_init(&ud_btl->recv_free_eager,
                            length + sizeof(mca_btl_ud_ib_header_t),
                            OBJ_CLASS(mca_btl_ud_recv_frag_eager_t),
                            mca_btl_ud_component.ib_free_list_num,
                            mca_btl_ud_component.ib_free_list_max,
                            mca_btl_ud_component.ib_free_list_inc,
                            ud_btl->super.btl_mpool);

        length = sizeof(mca_btl_ud_frag_t) +
            sizeof(mca_btl_ud_header_t) +
            ud_btl->super.btl_max_send_size +
            2*MCA_BTL_IB_FRAG_ALIGN;

        ompi_free_list_init(&ud_btl->send_free_max,
                            length,
                            OBJ_CLASS(mca_btl_ud_send_frag_max_t),
                            mca_btl_ud_component.ib_free_list_num,
                            mca_btl_ud_component.ib_free_list_max,
                            mca_btl_ud_component.ib_free_list_inc,
                            ud_btl->super.btl_mpool);

        /* Initialize pool of receive fragments */
        ompi_free_list_init (&ud_btl->recv_free_max,
                             length + sizeof(mca_btl_ud_ib_header_t),
                             OBJ_CLASS (mca_btl_ud_recv_frag_max_t),
                             mca_btl_ud_component.ib_free_list_num,
                             mca_btl_ud_component.ib_free_list_max,
                             mca_btl_ud_component.ib_free_list_inc,
                             ud_btl->super.btl_mpool);

        length = sizeof(mca_btl_ud_frag_t) +
            sizeof(mca_btl_ud_header_t)+
            2*MCA_BTL_IB_FRAG_ALIGN;

        ompi_free_list_init(&ud_btl->send_free_frag,
                            length,
                            OBJ_CLASS(mca_btl_ud_send_frag_frag_t),
                            mca_btl_ud_component.ib_free_list_num,
                            mca_btl_ud_component.ib_free_list_max,
                            mca_btl_ud_component.ib_free_list_inc,
                            ud_btl->super.btl_mpool);

        /* Post receive descriptors */
        do {
            int32_t i;
            int rc;
            struct ibv_recv_wr* bad_wr;

            for(i = 0; i < ud_btl->rd_num; i++) {
                mca_btl_ud_frag_t* frag;
                OMPI_FREE_LIST_WAIT(&ud_btl->recv_free_eager, frag, rc);
                frag->sg_entry.length = frag->size +
                    sizeof(mca_btl_ud_header_t) +
                    sizeof(mca_btl_ud_ib_header_t);
                if(ibv_post_recv(ud_btl->qp_hp,
                            &frag->wr_desc.rd_desc, &bad_wr)) {
                    BTL_ERROR(("error posting recv, errno %s\n",
                                strerror(errno)));
                    return NULL;
                }

                OMPI_FREE_LIST_WAIT(&ud_btl->recv_free_max, frag, rc);
                frag->sg_entry.length = frag->size +
                    sizeof(mca_btl_ud_header_t) +
                    sizeof(mca_btl_ud_ib_header_t);
                if(ibv_post_recv(ud_btl->qp_lp,
                            &frag->wr_desc.rd_desc, &bad_wr)) {
                    BTL_ERROR(("error posting recv, errno %s\n",
                                strerror(errno)));
                    return NULL;
                }
            }
        } while(0);


            /* TODO - Put this somewhere else or clean up our macros */
#if 0
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
            if(mca_btl_ud_component.use_srq) {
                MCA_BTL_UD_POST_SRR_HIGH(ud_btl, 1);
                MCA_BTL_UD_POST_SRR_LOW(ud_btl, 1);
            } else {
#endif
                MCA_BTL_UD_ENDPOINT_POST_RR_HIGH(ud_btl, 0);
                MCA_BTL_UD_ENDPOINT_POST_RR_LOW(ud_btl, 0);
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
            }
#endif
#endif
        btls[i] = &ud_btl->super;
    }

    /* Post OOB receive to support dynamic connection setup */
    mca_btl_ud_post_recv();
    mca_btl_ud_modex_send();

    *num_btl_modules = mca_btl_ud_component.ib_num_btls;
#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST
    ibv_free_device_list(ib_devs);
#else
    free(ib_devs);
#endif
    return btls;
}


static inline int mca_btl_ud_handle_incoming_hp(mca_btl_ud_module_t *,
        mca_btl_ud_frag_t *, size_t);

static inline int mca_btl_ud_handle_incoming_hp(
        mca_btl_ud_module_t *ud_btl,
        mca_btl_ud_frag_t *frag,
        size_t byte_len)
{
    struct ibv_recv_wr* bad_wr;

    /* advance the segment address past the header and adjust the length..*/
    frag->segment.seg_addr.pval = frag->hdr + 1;
    frag->segment.seg_len = byte_len -
        sizeof(mca_btl_ud_header_t) - sizeof(mca_btl_ud_ib_header_t);

    /* call registered callback */
    ud_btl->ib_reg[frag->hdr->tag].cbfunc(&ud_btl->super,
            frag->hdr->tag, &frag->base,
            ud_btl->ib_reg[frag->hdr->tag].cbdata);

    /* TODO - not resetting segment values here.. problem? */
    if(ibv_post_recv(ud_btl->qp_hp, &frag->wr_desc.rd_desc, &bad_wr)) {
        BTL_ERROR(("error posting recv, errno %s\n", strerror(errno)));
        return OMPI_ERROR;
    }

#if 0
    OMPI_FREE_LIST_RETURN(&(ud_btl->recv_free_eager),
            (opal_list_item_t*) frag);

    /* repost receive descriptors */
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    if(mca_btl_ud_component.use_srq) {
        OPAL_THREAD_ADD32((int32_t*) &ud_btl->srd_posted_hp, -1);
        MCA_BTL_UD_POST_SRR_HIGH(ud_btl, 0);
    } else {
#endif
        OPAL_THREAD_ADD32((int32_t*) &ud_btl->rd_posted_hp, -1);
        MCA_BTL_UD_ENDPOINT_POST_RR_HIGH(ud_btl, 0);

#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    }
#endif
#endif
    return OMPI_SUCCESS;
}

/*
 *  IB component progress.
 */

int mca_btl_ud_component_progress()
{
    uint32_t i;
    int count = 0,ne = 0, ret;
    mca_btl_ud_frag_t* frag;
    struct ibv_recv_wr* bad_wr;

    /* Poll for completions */
    for(i = 0; i < mca_btl_ud_component.ib_num_btls; i++) {
        struct ibv_wc wc;
        mca_btl_ud_module_t* ud_btl = &mca_btl_ud_component.ud_btls[i];

        ne=ibv_poll_cq(ud_btl->ib_cq_hp, 1, &wc);
        if(ne < 0 ) {
            BTL_ERROR(("error polling HP CQ with %d errno says %s\n",
                    ne, strerror(errno)));
            return OMPI_ERROR;
        }
        else if(1 == ne) {
            if(wc.status != IBV_WC_SUCCESS) {
                BTL_ERROR(("error polling HP CQ with status %d for wr_id %llu opcode %d\n",
                           wc.status, wc.wr_id, wc.opcode));
                return OMPI_ERROR;
            }

            /* Handle work completions */
            switch(wc.opcode) {
            case IBV_WC_SEND :
                frag = (mca_btl_ud_frag_t*)(unsigned long)wc.wr_id;

#if MCA_BTL_UD_ENABLE_PROFILE
                mca_btl_ud_profile.avg_full_send +=
                    opal_sys_timer_get_cycles() - frag->tm;
                mca_btl_ud_profile.cnt_full_send++;
#endif

                /* Process a completed send */
                frag->base.des_cbfunc(&ud_btl->super,
                        frag->endpoint, &frag->base, OMPI_SUCCESS);

                OPAL_THREAD_ADD32(&ud_btl->sd_wqe_hp, 1);
                if(!opal_list_is_empty(&ud_btl->pending_frags_hp)) {
                    frag = (mca_btl_ud_frag_t*)
                        opal_list_remove_first(&ud_btl->pending_frags_hp);
                    mca_btl_ud_endpoint_post_send(ud_btl, frag->endpoint, frag);
                }

                count++;
                break;

            case IBV_WC_RECV:
                /* Process a RECV */
                frag = (mca_btl_ud_frag_t*)(unsigned long) wc.wr_id;
                ret = mca_btl_ud_handle_incoming_hp(ud_btl, frag, wc.byte_len);

                if (ret != OMPI_SUCCESS)
                    return ret;
                count++;
                break;

            default:
                BTL_ERROR(("Unhandled work completion opcode is %d", wc.opcode));
                break;
            }
        }

        ne=ibv_poll_cq(ud_btl->ib_cq_lp, 1, &wc );
        if(ne < 0){
            BTL_ERROR(("error polling LP CQ with %d errno says %s",
                        ne, strerror(errno)));
            return OMPI_ERROR;
        }
        else if(1 == ne) {
            if(wc.status != IBV_WC_SUCCESS) {
                BTL_ERROR(("error polling LP CQ with status %d for wr_id %llu opcode %d",
                          wc.status, wc.wr_id, wc.opcode));
                return OMPI_ERROR;
            }

            /* Handle n/w completions */
            switch(wc.opcode) {
            case IBV_WC_SEND:
                frag = (mca_btl_ud_frag_t*) (unsigned long) wc.wr_id;

                /* Process a completed send - receiver must return tokens */
                frag->base.des_cbfunc(&ud_btl->super,
                        frag->endpoint, &frag->base, OMPI_SUCCESS);

                OPAL_THREAD_ADD32(&ud_btl->sd_wqe_lp, 1);
                if(!opal_list_is_empty(&ud_btl->pending_frags_lp)) {
                    frag = (mca_btl_ud_frag_t*)
                        opal_list_remove_first(&ud_btl->pending_frags_lp);
                    mca_btl_ud_endpoint_post_send(ud_btl, frag->endpoint, frag);
                }

                count++;
                break;

            case IBV_WC_RECV:
                /* Process a RECV */
                frag = (mca_btl_ud_frag_t*) (unsigned long) wc.wr_id;

                frag->segment.seg_addr.pval = frag->hdr + 1;
                frag->segment.seg_len =
                        wc.byte_len - sizeof(mca_btl_ud_header_t) -
                        sizeof(mca_btl_ud_ib_header_t);

                /* call registered callback */
                ud_btl->ib_reg[frag->hdr->tag].cbfunc(&ud_btl->super,
                        frag->hdr->tag, &frag->base,
                        ud_btl->ib_reg[frag->hdr->tag].cbdata);

                if(ibv_post_recv(ud_btl->qp_lp,
                            &frag->wr_desc.rd_desc, &bad_wr)) {
                    BTL_ERROR(("error posting recv, errno %s\n",
                                strerror(errno)));
                    return OMPI_ERROR;
                }
#if 0
                OMPI_FREE_LIST_RETURN(
                        &(ud_btl->recv_free_max), (opal_list_item_t*) frag);

#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                if(mca_btl_ud_component.use_srq) {
                    /* repost receive descriptors */
                    OPAL_THREAD_ADD32((int32_t*) &ud_btl->srd_posted_lp, -1);
                    MCA_BTL_UD_POST_SRR_LOW(ud_btl, 0);
                } else {
#endif
                    /* repost receive descriptors */
                    OPAL_THREAD_ADD32((int32_t*) &ud_btl->rd_posted_lp, -1);
                    MCA_BTL_UD_ENDPOINT_POST_RR_LOW(ud_btl, 0);

#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                }
#endif
#endif
                count++;
                break;

            default:
                BTL_ERROR(("Unhandled work completion opcode %d", wc.opcode));
                break;
            }
        }

    }

    return count;
}

