
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
/**
 * @file
 */
#ifndef MCA_PTL_UD_H
#define MCA_PTL_UD_H

/* Standard system includes */
#include <sys/types.h>
#include <string.h>

/* Open MPI includes */
#include "ompi/class/ompi_free_list.h"
#include "ompi/class/ompi_bitmap.h"
#include "orte/class/orte_pointer_array.h"
#include "opal/class/opal_value_array.h"
#include "opal/event/event.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "opal/util/output.h"
#include "opal/sys/timer.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/btl/base/btl_base_error.h"

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "btl_ud_endpoint.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define MCA_BTL_IB_LEAVE_PINNED 1

/**
 * UD Infiniband (IB) BTL component.
 */

struct mca_btl_ud_component_t {
    mca_btl_base_component_1_0_1_t super;  /**< base BTL component */

    uint32_t ib_max_btls;
    /**< maximum number of hcas available to the IB component */

    uint32_t ib_num_btls;
    /**< number of hcas available to the IB component */

    struct mca_btl_ud_module_t *ud_btls;
    /**< array of available PTLs */

    int ib_free_list_num;
    /**< initial size of free lists */

    int ib_free_list_max;
    /**< maximum size of free lists */

    int ib_free_list_inc;
    /**< number of elements to alloc when growing free lists */

    opal_list_t ib_procs;
    /**< list of ib proc structures */

    opal_mutex_t ib_lock;
    /**< lock for accessing module state */

    char* ib_mpool_name;
    /**< name of ib memory pool */

    int32_t sd_num; /**< maximum number of send descriptors to post to a QP */
    int32_t rd_num; /**< number of receive descriptors to post to each QP */

    int32_t srq_rd_max; /* maximum number of receive descriptors posted */
    int32_t srq_rd_per_peer; /* number of receive descriptors to post per log2(peers) in SRQ mode */
    int32_t srq_sd_max; /* maximum number of send descriptors posted */

    size_t eager_limit;
    size_t max_send_size;
    uint32_t reg_mru_len;
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    uint32_t use_srq;
#endif

    uint32_t ib_cq_size;   /**< Max outstanding CQE on the CQ */
    uint32_t ib_sg_list_size; /**< Max scatter/gather descriptor entries on the WQ*/
    uint32_t ib_pkey_ix;
    uint32_t ib_qkey;
    uint32_t ib_service_level;
    uint32_t ib_src_path_bits;

}; typedef struct mca_btl_ud_component_t mca_btl_ud_component_t;

extern mca_btl_ud_component_t mca_btl_ud_component;

typedef mca_btl_base_recv_reg_t mca_btl_ud_recv_reg_t;


/**
 * Profiling variables
 */

#if OMPI_ENABLE_DEBUG
#define MCA_BTL_UD_ENABLE_PROFILE 0
#else
#define MCA_BTL_UD_ENABLE_PROFILE 0
#endif

#if MCA_BTL_UD_ENABLE_PROFILE

#define MCA_BTL_UD_PROFILE_VAR(var) \
    opal_timer_t avg_ ## var; \
    opal_timer_t cnt_ ## var; \
    opal_timer_t tmp_ ## var

struct mca_btl_ud_profile_t
{
    MCA_BTL_UD_PROFILE_VAR(post_send);
    MCA_BTL_UD_PROFILE_VAR(endpoint_send_conn);
    MCA_BTL_UD_PROFILE_VAR(ibv_post_send);
    MCA_BTL_UD_PROFILE_VAR(full_send);
};

typedef struct mca_btl_ud_profile_t mca_btl_ud_profile_t;
extern mca_btl_ud_profile_t mca_btl_ud_profile;

#endif


/**
 * IB PTL Interface
 */
struct mca_btl_ud_module_t {
    mca_btl_base_module_t  super;  /**< base PTL interface */
    mca_btl_ud_recv_reg_t ib_reg[256];
    uint8_t port_num;           /**< ID of the PORT */
    struct ibv_device* ib_dev;  /* the ib device */
    struct ibv_context* ib_dev_context;
    struct ibv_pd* ib_pd;
    struct ibv_cq* ib_cq_hp;
    struct ibv_cq* ib_cq_lp;

    struct mca_btl_ud_addr_t addr;
    /**< local address information */

    ompi_free_list_t send_free_eager;
    /**< free list of eager buffer descriptors */

    ompi_free_list_t send_free_max;
    /**< free list of max buffer descriptors */

    ompi_free_list_t send_free_frag;
    /**< free list of frags only... used for pining memory */

    ompi_free_list_t recv_free_eager;
    /**< High priority free list of buffer descriptors */

    ompi_free_list_t recv_free_max;
    /**< Low priority free list of buffer descriptors */

    opal_list_t pending_frags_hp;
    /**< list of pending high priority frags */

    opal_list_t pending_frags_lp;
    /**< list of pending low priority frags */

    opal_mutex_t ib_lock;   /**< module level lock */

    size_t ib_inline_max;   /**< max size of inline send*/

#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    struct ibv_srq *srq_hp;
    struct ibv_srq *srq_lp;
    int32_t srd_posted_hp;
    int32_t srd_posted_lp;
#endif

    int32_t rd_num;

#if 0
    int32_t rd_posted_hp;   /**< number of high priority descriptors posted */
    int32_t rd_posted_lp;   /**< number of low priority descriptors posted */
#endif

    int32_t sd_wqe_hp;      /**< number of available send wqe entries */
    int32_t sd_wqe_lp;      /**< number of available send wqe entries */

    struct ibv_qp* qp_hp;
    struct ibv_qp* qp_lp;
    /* Local QP (Low and High) */
}; typedef struct mca_btl_ud_module_t mca_btl_ud_module_t;

struct mca_btl_ud_frag_t;
extern mca_btl_ud_module_t mca_btl_ud_module;

/**
 * Register IB component parameters with the MCA framework
 */
extern int mca_btl_ud_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
extern int mca_btl_ud_component_close(void);

/**
 * IB component initialization.
 *
 * @param num_btl_modules (OUT)           Number of BTLs returned in BTL array.
 * @param allow_multi_user_threads (OUT)  Flag indicating wether BTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       Flag indicating wether BTL uses threads (TRUE)
 *
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) publish BTL addressing info
 *
 */
extern mca_btl_base_module_t** mca_btl_ud_component_init(
    int *num_btl_modules,
    bool allow_multi_user_threads,
    bool have_hidden_threads
);


/**
 * IB component progress.
 */
extern int mca_btl_ud_component_progress(void);


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BTL of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */

int mca_btl_ud_register(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_tag_t tag,
    mca_btl_base_module_recv_cb_fn_t cbfunc,
    void* cbdata
);


/**
 * Cleanup any resources held by the BTL.
 *
 * @param btl  BTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */

extern int mca_btl_ud_finalize(
    struct mca_btl_base_module_t* btl
);


/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)
 * @param nprocs (IN)     Number of processes
 * @param procs (IN)      Set of processes
 * @param peers (OUT)     Set of (optional) peer addressing info.
 * @param peers (IN/OUT)  Set of processes that are reachable via this BTL.
 * @return     OMPI_SUCCESS or error status on failure.
 *
 */

extern int mca_btl_ud_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers,
    ompi_bitmap_t* reachable
);

/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param nproc (IN)   Number of processes.
 * @param procs (IN)   Set of processes.
 * @param peers (IN)   Set of peer data structures.
 * @return             Status indicating if cleanup was successful
 *
 */
extern int mca_btl_ud_del_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers
);


/**
 * PML->BTL Initiate a send of the specified size.
 *
 * @param btl (IN)               BTL instance
 * @param btl_base_peer (IN)     BTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via mca_btl_base_request_alloc_fn_t)
 * @param size (IN)              Number of bytes PML is requesting BTL to deliver
 * @param flags (IN)             Flags that should be passed to the peer via the message header.
 * @param request (OUT)          OMPI_SUCCESS if the BTL was able to queue one or more fragments
 */
extern int mca_btl_ud_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* btl_peer,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag
);


/**
 * Allocate a descriptor.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Requested descriptor size.
 */
extern mca_btl_base_descriptor_t* mca_btl_ud_alloc(
                                                       struct mca_btl_base_module_t* btl,
                                                       size_t size);


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)         BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */
extern int mca_btl_ud_free(
                               struct mca_btl_base_module_t* btl,
                               mca_btl_base_descriptor_t* des);


/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
mca_btl_base_descriptor_t* mca_btl_ud_prepare_src(
                                                      struct mca_btl_base_module_t* btl,
                                                      struct mca_btl_base_endpoint_t* peer,
                                                      mca_mpool_base_registration_t* registration,
                                                      struct ompi_convertor_t* convertor,
                                                      size_t reserve,
                                                      size_t* size
                                                      );


/**
 * Return a send fragment to the modules free list.
 *
 * @param btl (IN)   BTL instance
 * @param frag (IN)  IB send fragment
 *
 */
extern void mca_btl_ud_send_frag_return(
                                            struct mca_btl_base_module_t* btl,
                                            struct mca_btl_ud_frag_t*
                                            );


int mca_btl_ud_module_init(mca_btl_ud_module_t* ud_btl);

void mca_btl_ud_dump(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    int verbose);


/*
 * Profiling stuff
 */

#if MCA_BTL_UD_ENABLE_PROFILE

#define MCA_BTL_UD_START_TIME(var) \
    ((mca_btl_ud_profile.tmp_ ## var) = opal_sys_timer_get_cycles())
#define MCA_BTL_UD_END_TIME(var) \
do { \
    mca_btl_ud_profile.avg_ ## var += \
        opal_sys_timer_get_cycles() - mca_btl_ud_profile.tmp_ ## var; \
    mca_btl_ud_profile.cnt_ ## var++; \
} while(0)

#define MCA_BTL_UD_SHOW_TIME(var) \
    BTL_VERBOSE(("  " #var " avg %lu cnt %lu", \
        (mca_btl_ud_profile.avg_ ## var) / (mca_btl_ud_profile.cnt_ ## var), \
        mca_btl_ud_profile.cnt_ ## var));

#else
#define MCA_BTL_UD_START_TIME(var)
#define MCA_BTL_UD_END_TIME(var)
#define MCA_BTL_UD_SHOW_TIME(var)
#endif


/*
 * Post non-SRQ receive buffers
 */

#define MCA_BTL_UD_ENDPOINT_POST_RR_HIGH(ud_btl, \
                                             additional) \
{ \
   do { \
    OPAL_THREAD_LOCK(&ud_btl->ib_lock); \
    if(ud_btl->rd_posted_hp <= mca_btl_ud_component.rd_low+additional && \
       ud_btl->rd_posted_hp < ud_btl->rd_num) { \
        MCA_BTL_UD_ENDPOINT_POST_RR_SUB(ud_btl->rd_num -  \
                                            ud_btl->rd_posted_hp, \
                                            &ud_btl->recv_free_eager, \
                                            ud_btl->rd_posted_hp, \
                                            ud_btl->qp_hp); \
    } \
    OPAL_THREAD_UNLOCK(&ud_btl->ib_lock); \
   } while(0); \
}

#define MCA_BTL_UD_ENDPOINT_POST_RR_LOW(ud_btl, \
                                            additional) { \
    do { \
    OPAL_THREAD_LOCK(&ud_btl->ib_lock); \
    if(ud_btl->rd_posted_lp <= mca_btl_ud_component.rd_low+additional && \
       ud_btl->rd_posted_lp < ud_btl->rd_num){ \
       MCA_BTL_UD_ENDPOINT_POST_RR_SUB(ud_btl->rd_num - \
                                            ud_btl->rd_posted_lp,  \
                                            &ud_btl->recv_free_max, \
                                            ud_btl->rd_posted_lp, \
                                            ud_btl->qp_lp \
                                            ); } \
    OPAL_THREAD_UNLOCK(&ud_btl->ib_lock); \
    } while(0); \
}

#define MCA_BTL_UD_ENDPOINT_POST_RR_SUB(cnt, \
                                            frag_list, \
                                            rd_posted, \
                                            qp ) \
do { \
    int32_t i; \
    int rc; \
    int32_t num_post = cnt; \
    struct ibv_recv_wr* bad_wr; \
    for(i = 0; i < num_post; i++) { \
        opal_list_item_t* item; \
        mca_btl_ud_frag_t* frag; \
        OMPI_FREE_LIST_WAIT(frag_list, item, rc); \
        frag = (mca_btl_ud_frag_t*) item; \
        frag->sg_entry.length = frag->size + sizeof(mca_btl_ud_header_t) + sizeof(mca_btl_ud_ib_header_t); \
        if(ibv_post_recv(qp, \
            &frag->wr_desc.rd_desc, \
            &bad_wr)) { \
            BTL_ERROR(("error posting receive errno says %s\n", strerror(errno))); \
            return OMPI_ERROR; \
        }\
    }\
    OPAL_THREAD_ADD32(&(rd_posted), num_post); \
} while(0);

#if 0
#define MCA_BTL_UD_ENDPOINT_POST_RR_SUB(cnt, \
                                            frag_list, \
                                            rd_posted, \
                                            qp ) \
do { \
    int32_t i; \
    int rc; \
    int32_t num_post = cnt; \
    struct ibv_recv_wr* head_wr; \
    struct ibv_recv_wr* prev_wr; \
    opal_list_item_t* item; \
    mca_btl_ud_frag_t* frag; \
    OMPI_FREE_LIST_WAIT(frag_list, item, rc); \
    frag = (mca_btl_ud_frag_t*)item; \
    head_wr = &frag->wr_desc.rd_desc; \
    prev_wr = head_wr; \
    OPAL_OUTPUT((0, "posting %d recvs\n", num_post)); \
    for(i = 1; i < num_post; i++) { \
        OMPI_FREE_LIST_WAIT(frag_list, item, rc); \
        frag = (mca_btl_ud_frag_t*) item; \
        prev_wr->next = &frag->wr_desc.rd_desc; \
        prev_wr = prev_wr->next; \
    }\
    prev_wr->next = NULL; \
    if(ibv_post_recv(qp, head_wr, &prev_wr)) { \
        BTL_ERROR(("error posting receive errno says %s\n", strerror(errno))); \
        return OMPI_ERROR; \
    }\
    OPAL_THREAD_ADD32(&(rd_posted), num_post); \
} while(0);
#endif

#define BTL_OPENIB_INSERT_PENDING(frag, frag_list, tokens, lock) \
do{ \
     OPAL_THREAD_LOCK(&lock); \
     opal_list_append(&frag_list, (opal_list_item_t *)frag); \
     OPAL_THREAD_UNLOCK(&lock); \
     OPAL_THREAD_ADD32(&tokens, 1); \
 } while(0);


/*
 * Post SRQ receive buffers
 */

#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ

#define MCA_BTL_UD_POST_SRR_HIGH(ud_btl, additional) \
{ \
    do{ \
        OPAL_THREAD_LOCK(&ud_btl->ib_lock); \
        if(ud_btl->srd_posted_hp <= ud_btl->rd_low+additional && \
           ud_btl->srd_posted_hp < ud_btl->rd_num){ \
           MCA_BTL_UD_POST_SRR_SUB(ud_btl->rd_num -  \
                                      ud_btl->srd_posted_hp, \
                                      ud_btl, \
                                      &ud_btl->recv_free_eager, \
                                      &ud_btl->srd_posted_hp, \
                                      ud_btl->srq_hp); \
        } \
        OPAL_THREAD_UNLOCK(&ud_btl->ib_lock); \
    } while(0); \
}

#define MCA_BTL_UD_POST_SRR_LOW(ud_btl, additional) \
{ \
    do { \
    OPAL_THREAD_LOCK(&ud_btl->ib_lock); \
    if(ud_btl->srd_posted_lp <= ud_btl->rd_low+additional && \
       ud_btl->srd_posted_lp < ud_btl->rd_num){ \
        MCA_BTL_UD_POST_SRR_SUB(ud_btl->rd_num -  \
                                            ud_btl->srd_posted_lp, \
                                            ud_btl, \
                                            &ud_btl->recv_free_max, \
                                            &ud_btl->srd_posted_lp, \
                                            ud_btl->srq_lp); \
    } \
    OPAL_THREAD_UNLOCK(&ud_btl->ib_lock); \
    } while(0); \
}


#define MCA_BTL_UD_POST_SRR_SUB(cnt, \
                                   ud_btl, \
                                   frag_list, \
                                   srd_posted, \
                                   srq) \
{\
    do { \
    int32_t i; \
    int32_t num_post = cnt; \
    opal_list_item_t* item = NULL; \
    mca_btl_ud_frag_t* frag = NULL; \
    struct ibv_recv_wr *bad_wr; \
    int32_t rc; \
    for(i = 0; i < num_post; i++) { \
        OMPI_FREE_LIST_WAIT(frag_list, item, rc); \
        frag = (mca_btl_ud_frag_t*) item; \
        frag->sg_entry.length = frag->size + \
            ((unsigned char*) frag->segment.seg_addr.pval-  \
             (unsigned char*) frag->hdr);  \
       if(ibv_post_srq_recv(srq, &frag->wr_desc.rd_desc, &bad_wr)) { \
           BTL_ERROR(("error posting receive descriptors to shared receive queue: %s",\
                   strerror(errno))); \
           return OMPI_ERROR; \
       }\
    }\
    OPAL_THREAD_ADD32(srd_posted,  num_post); \
   } while(0);\
}

#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
