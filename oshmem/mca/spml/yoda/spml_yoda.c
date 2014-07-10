/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/util/show_help.h"
#include "orte/include/orte/types.h"
#include "orte/runtime/orte_globals.h"

#include "opal/datatype/opal_convertor.h"

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/btl/sm/btl_sm_frag.h"

#include "oshmem/proc/proc.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/mca/spml/spml.h"

#include "spml_yoda.h"
#include "spml_yoda_putreq.h"
#include "spml_yoda_getreq.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "oshmem/runtime/runtime.h"

/* Turn ON/OFF debug output from build (default 0) */
#ifndef SPML_YODA_DEBUG
#define SPML_YODA_DEBUG    0
#endif

mca_spml_yoda_module_t mca_spml_yoda = {
    {
        /* Init mca_spml_base_module_t */
        mca_spml_yoda_add_procs,
        mca_spml_yoda_del_procs,
        mca_spml_yoda_enable,
        mca_spml_yoda_register,
        mca_spml_yoda_deregister,
        mca_spml_base_oob_get_mkeys,
        mca_spml_yoda_put,
        mca_spml_yoda_put_nb,
        mca_spml_yoda_get,
        mca_spml_yoda_recv,
        mca_spml_yoda_send,
        mca_spml_base_wait,
        mca_spml_base_wait_nb,
        mca_spml_yoda_fence,

        (void *)&mca_spml_yoda
    }
};

static inline mca_bml_base_btl_t *get_next_btl(int dst, int *btl_id);

static inline void spml_yoda_prepare_for_get(void* buffer, size_t size, void* p_src, int dst, void* p_dst, void* p_getreq);

static int btl_name_to_id(char *btl_name)
{
    if (0 == strcmp(btl_name, "sm")) {
        return YODA_BTL_SM;
    } else if (0 == strcmp(btl_name, "openib")) {
        return YODA_BTL_OPENIB;
    } else if (0 == strcmp(btl_name, "self")) {
        return YODA_BTL_SELF;
    }
    return YODA_BTL_UNKNOWN;
}

static char *btl_type2str(int btl_type)
{
    switch (btl_type) {
    case YODA_BTL_UNKNOWN:
        return "unknown btl";
    case YODA_BTL_SELF:
        return "self";
    case YODA_BTL_OPENIB:
        return "openib";
    case YODA_BTL_SM:
        return "sm";
    }
    return "bad_btl_type";
}

static inline void calc_nfrags(mca_bml_base_btl_t* bml_btl,
                               size_t size,
                               unsigned int *frag_size,
                               int *nfrags,
                               int use_send)
{
    if (use_send) {
        *frag_size = bml_btl->btl->btl_max_send_size - SPML_YODA_SEND_CONTEXT_SIZE;
    }
    else {
        *frag_size = bml_btl->btl->btl_max_send_size;
    }
    *nfrags = 1 + (size - 1) / (*frag_size);
}

static int mca_spml_yoda_fence_internal(int puts_wait)
{
    int n_puts_wait;

    /* Waiting for certain number of puts : 'puts_wait'
     * if 'puts_wait' == 0 waiting for all puts ('n_active_puts')
     * if 'puts_wait' > 'n_active_puts' waiting for 'n_active_puts' */

    n_puts_wait = puts_wait > 0 ? mca_spml_yoda.n_active_puts - puts_wait : 0;

    if (n_puts_wait < 0) {
        n_puts_wait = 0;
    }

    while (n_puts_wait < mca_spml_yoda.n_active_puts) {
        oshmem_request_wait_any_completion();
    }
    return OSHMEM_SUCCESS;
}

static inline void mca_spml_yoda_bml_alloc( mca_bml_base_btl_t* bml_btl,
                                            mca_btl_base_descriptor_t** des,
                                            uint8_t order, size_t size, uint32_t flags,
                                            int use_send)
{
    bool is_done;
    bool is_fence_complete;

    is_done = false;
    is_fence_complete = false;

    if (use_send) {
        size = (0 == size ? size : size + SPML_YODA_SEND_CONTEXT_SIZE);
    }

    do {
        mca_bml_base_alloc(bml_btl,
                       des,
                       MCA_BTL_NO_ORDER,
                       size,
                       flags);

        if (OPAL_UNLIKELY(!(*des) || !(*des)->des_src ) && !is_fence_complete) {
            mca_spml_yoda_fence_internal(mca_spml_yoda.bml_alloc_threshold);

            is_fence_complete = true;
        } else {
            is_done = true;
        }

    } while (!is_done);
}

static inline void spml_yoda_prepare_for_put(void* buffer, size_t size, void* p_src, void* p_dst, int use_send)
{
    if (use_send) {
        memcpy((void*) buffer, &size, sizeof(size));
        memcpy((void*) ( ((char*) buffer) + sizeof(size)), &p_dst, sizeof(p_dst));
        memcpy((void*) ( ((char*) buffer) + sizeof(size) + sizeof(p_dst)), p_src, size);
    }
    else {
        memcpy((void*) (  (unsigned char*) buffer), p_src, size);
    }
}

static inline void spml_yoda_prepare_for_get_response(void* buffer, size_t size, void* p_src, void* p_dst, void* p_getreq, int use_send)
{
    if (use_send) {
        memcpy((void*) buffer, &size, sizeof(size));
        memcpy((void*) ( ((char*) buffer) + sizeof(size)), &p_dst, sizeof(p_dst));
        memcpy((void*) ( ((char*) buffer) + sizeof(size) + sizeof(p_dst)), p_src, size);
        memcpy((void*) ( ((char*) buffer) + sizeof(size) + sizeof(p_dst) + size), &p_getreq, sizeof(p_getreq));
    }
    else {
        memcpy((void*) (  (unsigned char*) buffer), p_src, size);
    }
}

static inline void spml_yoda_prepare_for_get(void* buffer, size_t size, void* p_src, int dst, void* p_dst, void* p_getreq)
{
    memcpy((void*) buffer, &p_src, sizeof(p_src));
    memcpy((void*) ( ((unsigned char*) buffer) + sizeof(p_src) ), &size, sizeof(size));
    memcpy((void*) ( ((unsigned char*) buffer) + sizeof(p_src) + sizeof(size) ), &dst, sizeof(dst));
    memcpy((void*) ( ((unsigned char*) buffer) + sizeof(p_src) + sizeof(size) + sizeof(dst)), &p_dst, sizeof(p_dst));
    memcpy((void*) ( ((unsigned char*) buffer) + sizeof(p_src) + sizeof(size) + sizeof(dst) + sizeof(p_dst)), &p_getreq, sizeof(p_getreq));
}

static void mca_yoda_put_callback(mca_btl_base_module_t* btl,
                                  mca_btl_base_tag_t tag,
                                  mca_btl_base_descriptor_t* des,
                                  void* cbdata )
{
    size_t* size;
    void** l_addr;

    size = (size_t *) des->des_dst->seg_addr.pval;
    l_addr = (void**) ( ((char*)size) + sizeof(*size));
    memcpy(*l_addr, ((char*)l_addr) + sizeof(*l_addr), *size);
}

static void mca_yoda_get_callback(mca_btl_base_module_t* btl,
                                  mca_btl_base_tag_t tag,
                                  mca_btl_base_descriptor_t* des,
                                  void* cbdata )
{
    void** p, ** p_src, **p_dst;
    size_t* size;
    int* dst;
    void** p_getreq;
    mca_btl_base_descriptor_t* des_loc;
    int rc;
    mca_bml_base_btl_t* bml_btl;
    mca_spml_yoda_rdma_frag_t* frag;
    int btl_id;
    mca_spml_yoda_put_request_t *putreq;

    rc = OSHMEM_SUCCESS;
    btl_id = 0;
    putreq = NULL;

    /* Unpack data */
    p = (void **)des->des_dst->seg_addr.pval;
    p_src = (void*) p;

    size = (size_t*)((char*)p_src + sizeof(*p_src) );
    dst = (int*)( (char*)size + sizeof(*size));
    p_dst = (void*) ((char*)dst + sizeof(*dst));
    p_getreq =(void**) ( (char*)p_dst + sizeof(*p_dst));

    /* Prepare put via send*/
    bml_btl = get_next_btl(*dst, &btl_id);

    putreq = mca_spml_yoda_putreq_alloc(*dst);
    frag = &putreq->put_frag;

    mca_spml_yoda_bml_alloc(bml_btl,
                            &des_loc,
                            MCA_BTL_NO_ORDER,
                            *size,
                            MCA_BTL_DES_SEND_ALWAYS_CALLBACK,
                            1);

    if (OPAL_UNLIKELY(!des_loc || !des_loc->des_src)) {
        SPML_ERROR("shmem OOM error need %d bytes", (int)*size);
        oshmem_shmem_abort(-1);
    }
    spml_yoda_prepare_for_get_response((void*)des_loc->des_src->seg_addr.pval, *size, (void*)*p_src, (void*) *p_dst,(void*)*p_getreq,1);

    frag->rdma_req = putreq;

    /* Initialize callback data for put*/
    des_loc->des_cbdata = frag;
    des_loc->des_cbfunc = mca_spml_yoda_put_completion;
    des_loc->des_src_cnt = 1;

    OPAL_THREAD_ADD32(&mca_spml_yoda.n_active_puts, 1);

    /* Put via send*/
    rc = mca_bml_base_send(bml_btl, des_loc, MCA_SPML_YODA_GET_RESPONSE);
    if (1 == rc) {
        rc = OSHMEM_SUCCESS;
    }

    if (OPAL_UNLIKELY(OSHMEM_SUCCESS != rc)) {
        if (OSHMEM_ERR_OUT_OF_RESOURCE == rc) {
            /* No free resources, Block on completion here */
            SPML_ERROR("shmem error: OSHMEM_ERR_OUT_OF_RESOURCE");
            oshmem_request_wait_completion(&putreq->req_put.req_base.req_oshmem);
        } else {
            SPML_ERROR("shmem error");
        }
        /* exit with errro */
        SPML_ERROR("shmem error: ret = %i, send_pe = %i, dest_pe = %i",
                   rc, oshmem_my_proc_id(), *dst);
        oshmem_shmem_abort(-1);
        rc = OSHMEM_ERROR;
    }
}

static void mca_yoda_get_response_callback(mca_btl_base_module_t* btl,
                                           mca_btl_base_tag_t tag,
                                           mca_btl_base_descriptor_t* des,
                                           void* cbdata )
{
    size_t* size;
    void** l_addr;
    mca_spml_yoda_get_request_t* getreq;

    /* unpacking data*/
    size = (size_t *) ( ((char*)des->des_dst->seg_addr.pval) );
    l_addr = (void**)( ((char*)size) + sizeof(*size));
    getreq = (mca_spml_yoda_get_request_t*)*(void**)((char*)l_addr + sizeof(*l_addr) + *size);

    /* Complete get request*/
    OPAL_THREAD_ADD32(&getreq->parent->active_count, -1);
    getreq->req_get.req_base.req_spml_complete = true;
    oshmem_request_complete(&getreq->req_get.req_base.req_oshmem, 1);
    oshmem_request_free((oshmem_request_t**) &getreq);

    memcpy(*l_addr, (char*)l_addr + sizeof(*l_addr), *size);
}

/**
 * note: we have to reg memory directly with btl because no proc will have a full btl list in proc_bml
 */
int mca_spml_yoda_deregister(sshmem_mkey_t *mkeys)
{
    int i;
    struct yoda_btl *ybtl;
    mca_spml_yoda_context_t* yoda_context;

    MCA_SPML_CALL(fence());
    mca_spml_yoda_wait_gets();

    if (!mkeys) {
        return OSHMEM_SUCCESS;
    }

    for (i = 0; i < mca_spml_yoda.n_btls; i++) {
        ybtl = &mca_spml_yoda.btl_type_map[i];
        yoda_context = (mca_spml_yoda_context_t*) mkeys[i].spml_context;
        if (NULL == yoda_context) {
            continue;
        }
        if (yoda_context->btl_src_descriptor) {
            ybtl->btl->btl_free(ybtl->btl, yoda_context->btl_src_descriptor);
            yoda_context->btl_src_descriptor = NULL;
        }
        if (yoda_context->registration) {
            ybtl->btl->btl_mpool->mpool_deregister(ybtl->btl->btl_mpool,
                                                   yoda_context->registration);
        }

    }
    free(mkeys);

    return OSHMEM_SUCCESS;
}

sshmem_mkey_t *mca_spml_yoda_register(void* addr,
                                        size_t size,
                                        uint64_t shmid,
                                        int *count)
{
    int i;
    mca_btl_base_descriptor_t* des = NULL;
    const opal_datatype_t *datatype = &opal_datatype_wchar;
    opal_convertor_t convertor;
    sshmem_mkey_t *mkeys;
    struct yoda_btl *ybtl;
    oshmem_proc_t *proc_self;
    mca_spml_yoda_context_t* yoda_context;
    struct iovec iov;
    uint32_t iov_count = 1;


    SPML_VERBOSE(10, "address %p len %llu", addr, (unsigned long long)size);
    *count = 0;
    /* make sure everything is initialized to 0 */
    mkeys = (sshmem_mkey_t *) calloc(1,
                                       mca_spml_yoda.n_btls * sizeof(*mkeys));
    if (!mkeys) {
        return NULL ;
    }

    proc_self = oshmem_proc_group_find(oshmem_group_all, oshmem_my_proc_id());
    /* create convertor */
    OBJ_CONSTRUCT(&convertor, opal_convertor_t);

    mca_bml.bml_register( MCA_SPML_YODA_PUT,
                          mca_yoda_put_callback,
                          NULL );
    mca_bml.bml_register( MCA_SPML_YODA_GET,
                          mca_yoda_get_callback,
                          NULL );
    mca_bml.bml_register( MCA_SPML_YODA_GET_RESPONSE,
                          mca_yoda_get_response_callback,
                          NULL );
    /* Register proc memory in every rdma BTL. */
    for (i = 0; i < mca_spml_yoda.n_btls; i++) {

        ybtl = &mca_spml_yoda.btl_type_map[i];
        mkeys[i].va_base = addr;
        mkeys[i].u.key = MAP_SEGMENT_SHM_INVALID;

        if (!ybtl->use_cnt) {
            SPML_VERBOSE(10,
                         "%s: present but not in use. SKIP registration",
                         btl_type2str(ybtl->btl_type));
            continue;
        }

        /* If we have shared memory just save its id*/
        if (YODA_BTL_SM == ybtl->btl_type
                && MAP_SEGMENT_SHM_INVALID != (int)shmid) {
            mkeys[i].u.key = shmid;
            mkeys[i].va_base = 0;
            continue;
        }

        yoda_context = calloc(1, sizeof(*yoda_context));
        mkeys[i].spml_context = yoda_context;

        yoda_context->registration = NULL;
        if (NULL != ybtl->btl->btl_prepare_src) {

            /* initialize convertor for source descriptor*/
            opal_convertor_copy_and_prepare_for_recv(proc_self->proc_convertor,
                                                     datatype,
                                                     size,
                                                     addr,
                                                     0,
                                                     &convertor);

            if (NULL != ybtl->btl->btl_mpool && NULL != ybtl->btl->btl_mpool->mpool_register) {
                iov.iov_len = size;
                iov.iov_base = NULL;

                opal_convertor_pack(&convertor, &iov, &iov_count, &size);
                ybtl->btl->btl_mpool->mpool_register(ybtl->btl->btl_mpool,
                                                     iov.iov_base, size, 0, &yoda_context->registration);
            }
            /* initialize convertor for source descriptor*/
            opal_convertor_copy_and_prepare_for_recv(proc_self->proc_convertor,
                                                     datatype,
                                                     size,
                                                     addr,
                                                     0,
                                                     &convertor);

            /* register source memory */
            des = ybtl->btl->btl_prepare_src(ybtl->btl,
                                             ybtl->bml_btl->btl_endpoint,
                                             yoda_context->registration,
                                             &convertor,
                                             MCA_BTL_NO_ORDER,
                                             0,
                                             &size,
                                             0);
            if (NULL == des) {
                SPML_ERROR("%s: failed to register source memory. ",
                           btl_type2str(ybtl->btl_type));
                /* FIXME some cleanup might be needed here
                 * yoda_context->btl_src_descriptor = NULL;
                 * OBJ_DESTRUCT(&convertor);
                 * *count = ???;
                 */
                return NULL;
            }

            yoda_context->btl_src_descriptor = des;
            mkeys[i].u.data = des->des_src;
            mkeys[i].len  = ybtl->btl->btl_seg_size;
        }

        SPML_VERBOSE(5,
                     "rank %d btl %s va_base: 0x%p len: %d key %llx size %llu",
                     oshmem_proc_local_proc->proc_name.vpid, btl_type2str(ybtl->btl_type),
                     mkeys[i].va_base, mkeys[i].len, (unsigned long long)mkeys[i].u.key, (unsigned long long)size);
    }
    OBJ_DESTRUCT(&convertor);
    *count = mca_spml_yoda.n_btls;
    return mkeys;
}

/*
 * For each proc setup a datastructure that indicates the BTLs
 * that can be used to reach the destination.
 */
static void mca_spml_yoda_error_handler(struct mca_btl_base_module_t* btl,
                                        int32_t flags,
                                        ompi_proc_t* errproc,
                                        char* btlinfo)
{
    oshmem_shmem_abort(-1);
}

/*  make global btl list&map */
static int create_btl_list(void)
{
    int btl_type;
    char *btl_name;
    int size;
    opal_list_item_t *item;
    mca_btl_base_selected_module_t *btl_sm;
    int i;

    size = opal_list_get_size(&mca_btl_base_modules_initialized);
    if (0 >= size) {
        SPML_ERROR("no btl(s) available");
        return OSHMEM_ERROR;
    }
    SPML_VERBOSE(50, "found %d capable btls", size);

    mca_spml_yoda.btl_type_map =
            (struct yoda_btl *) calloc(size, sizeof(struct yoda_btl));
    if (!mca_spml_yoda.btl_type_map)
        return OSHMEM_ERROR;

    mca_spml_yoda.n_btls = 0;
    for (i = 0, item = opal_list_get_first(&mca_btl_base_modules_initialized);
         item != opal_list_get_end(&mca_btl_base_modules_initialized);
         item = opal_list_get_next(item), i++) {

        btl_sm = (mca_btl_base_selected_module_t *) item;
        btl_name = btl_sm->btl_component->btl_version.mca_component_name;
        btl_type = btl_name_to_id(btl_name);

        SPML_VERBOSE(50, "found btl (%s) btl_type=%s", btl_name, btl_type2str(btl_type));

        /* Note: we setup bml_btl in create_btl_idx() */
        mca_spml_yoda.btl_type_map[mca_spml_yoda.n_btls].bml_btl = NULL;
        mca_spml_yoda.btl_type_map[mca_spml_yoda.n_btls].btl =
                btl_sm->btl_module;
        mca_spml_yoda.btl_type_map[mca_spml_yoda.n_btls].btl_type = btl_type;
        mca_spml_yoda.n_btls++;
    }

    if (0 == mca_spml_yoda.n_btls) {
        SPML_ERROR("can not find any suitable btl");
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}

static int _find_btl_id(mca_bml_base_btl_t *bml_btl)
{
    int i;

    for (i = 0; i < mca_spml_yoda.n_btls; i++) {
        if (mca_spml_yoda.btl_type_map[i].btl == bml_btl->btl)
            return i;
    }
    return -1;
}

/* for each proc create transport ids which are indexes into global
 * btl list&map
 */
static int create_btl_idx(int dst_pe)
{
    oshmem_proc_t *proc;
    int btl_id;
    mca_bml_base_endpoint_t* endpoint;
    mca_bml_base_btl_t* bml_btl = 0;
    int i, size;
    mca_bml_base_btl_array_t *btl_array;
    int shmem_index = -1;

    proc = oshmem_proc_group_find(oshmem_group_all, dst_pe);
    endpoint = (mca_bml_base_endpoint_t*) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
    assert(endpoint);
    size = mca_bml_base_btl_array_get_size(btl_array = &endpoint->btl_rdma);

    if (0 >= size) {
        /* Possibly this is SM BTL with KNEM disabled? Then we should use send based get/put */
        /*
           This hack is necessary for the case when KNEM is not available.
           In this case we still want to use send/recv of SM BTL for put and get
           but SM BTL is not in the rdma list anymore
        */
        size = mca_bml_base_btl_array_get_size(btl_array =
                &endpoint->btl_eager);
        if (0 < size) {
            /*Chose SHMEM capable btl from eager array. Not filter now: take the first
              (but could appear on demand).*/
            for (shmem_index = 0; shmem_index < size; shmem_index++) {
                bml_btl = mca_bml_base_btl_array_get_index(btl_array, shmem_index);
                _find_btl_id(bml_btl);
                size = 1;
                break;
            }
        }
        else {
            SPML_ERROR("no SHMEM capable transport for dest pe=%d", dst_pe);
            return OSHMEM_ERROR;
        }
    }

    proc->transport_ids = (char *) malloc(size * sizeof(char));
    if (!proc->transport_ids)
        return OSHMEM_ERROR;

    proc->num_transports = size;

    for (i = 0; i < size; i++) {
        bml_btl = mca_bml_base_btl_array_get_index(btl_array,
                                                   (shmem_index >= 0) ?
                                                       (shmem_index) : (i));
        btl_id = _find_btl_id(bml_btl);
        SPML_VERBOSE(50,
                     "dst_pe(%d) use btl (%s) btl_id=%d",
                     dst_pe, bml_btl->btl->btl_component->btl_version.mca_component_name, btl_id);
        if (0 > btl_id) {
            SPML_ERROR("unknown btl: dst_pe(%d) use btl (%s) btl_id=%d",
                       dst_pe, bml_btl->btl->btl_component->btl_version.mca_component_name, btl_id);
            return OSHMEM_ERROR;
        }
        proc->transport_ids[i] = btl_id;
        mca_spml_yoda.btl_type_map[btl_id].bml_btl = bml_btl;
        mca_spml_yoda.btl_type_map[btl_id].use_cnt++;
    }
    return OSHMEM_SUCCESS;
}

static int destroy_btl_list(void)
{
    if (mca_spml_yoda.btl_type_map) {
        free(mca_spml_yoda.btl_type_map);
    }

    return OSHMEM_SUCCESS;
}

static int destroy_btl_idx(int dst_pe)
{
    oshmem_proc_t *proc;

    proc = oshmem_proc_group_find(oshmem_group_all, dst_pe);
    if (proc->transport_ids) {
        free(proc->transport_ids);
    }

    return OSHMEM_SUCCESS;
}

int mca_spml_yoda_add_procs(oshmem_proc_t** procs, size_t nprocs)
{
    opal_bitmap_t reachable;
    int rc;
    size_t i;

    if (0 == nprocs) {
        return OSHMEM_SUCCESS;
    }

    OBJ_CONSTRUCT(&reachable, opal_bitmap_t);
    rc = opal_bitmap_init(&reachable, (int) nprocs);
    if (OSHMEM_SUCCESS != rc) {
        return rc;
    }

    rc = mca_bml.bml_register_error(mca_spml_yoda_error_handler);
    if (OMPI_SUCCESS != rc) {
        goto cleanup_and_return;
    }

    /* create btl index and map */
    rc = create_btl_list();
    if (OSHMEM_SUCCESS != rc) {
        goto cleanup_and_return;
    }

    for (i = 0; i < nprocs; i++) {
        rc = create_btl_idx(i);
        if (OSHMEM_SUCCESS != rc) {
            goto cleanup_and_return;
        }
    }

cleanup_and_return:
    OBJ_DESTRUCT(&reachable);

    return rc;
}

int mca_spml_yoda_del_procs(oshmem_proc_t** procs, size_t nprocs)
{
    size_t i;

    for (i = 0; i < nprocs; i++) {
        destroy_btl_idx(i);
    }
    destroy_btl_list();

    return OSHMEM_SUCCESS;
}

static inline mca_bml_base_btl_t *get_next_btl(int dst, int *btl_id)
{
    mca_bml_base_endpoint_t* endpoint;
    mca_bml_base_btl_t* bml_btl;
    oshmem_proc_t *proc;
    mca_bml_base_btl_array_t *btl_array = 0;
    int size = 0;
    int shmem_index = 0;

    /* get endpoint and btl */
    proc = oshmem_proc_group_all(dst);
    if (!proc) {
        SPML_ERROR("Can not find destination proc for pe=%d", dst);
        return NULL ;
    }

    endpoint = (mca_bml_base_endpoint_t*) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
    if (!endpoint) {
        SPML_ERROR("pe=%d proc has no endpoint", dst);
        return NULL ;
    }

    /* At the moment always return first transport */
    size = mca_bml_base_btl_array_get_size(btl_array = &endpoint->btl_rdma);

    if (0 >= size) {
        /* Possibly this is SM BTL with KNEM disabled? Then we should use send based get/put */
        /*
           This hack is necessary for the case when KNEM is not available.
           In this case we still want to use send/recv of SM BTL for put and get
           but SM BTL is not in the rdma list anymore
        */
        size = mca_bml_base_btl_array_get_size(btl_array =
                &endpoint->btl_eager);
        if (0 < size) {
            /*Chose SHMEM capable btl from eager array. Not filter now: take the first
              (but could appear on demand).*/
            for (shmem_index = 0; shmem_index < size; shmem_index++) {
                bml_btl = mca_bml_base_btl_array_get_index(btl_array, shmem_index);
                _find_btl_id(bml_btl);
                size = 1;
                break;
            }
        }
    }

    bml_btl = mca_bml_base_btl_array_get_index(btl_array, shmem_index);
    *btl_id = proc->transport_ids[0];

#if SPML_YODA_DEBUG == 1
    assert(*btl_id >= 0 && *btl_id < YODA_BTL_MAX);
    SPML_VERBOSE(100, "pe=%d reachable via btl %s %d", dst,
                 bml_btl->btl->btl_component->btl_version.mca_component_name, *btl_id);
#endif
    return bml_btl;
}


static inline int mca_spml_yoda_put_internal(void *dst_addr,
                                             size_t size,
                                             void *src_addr,
                                             int dst,
                                             int is_nb)
{
    int rc = OSHMEM_SUCCESS;
    mca_spml_yoda_put_request_t *putreq = NULL;
    mca_bml_base_btl_t* bml_btl;
    mca_btl_base_descriptor_t* des = NULL;
    mca_btl_base_segment_t* segment;
    mca_spml_yoda_rdma_frag_t* frag;
    int nfrags;
    int i;
    unsigned ncopied = 0;
    unsigned int frag_size = 0;
    char *p_src, *p_dst;
    void* rva;
    sshmem_mkey_t *r_mkey;
    int btl_id = 0;
    struct yoda_btl *ybtl;
    int put_via_send;

    /* If nothing to put its OK.*/
    if (0 >= size) {
        return OSHMEM_SUCCESS;
    }

    /* Find bml_btl and its global btl_id */
    bml_btl = get_next_btl(dst, &btl_id);
    if (!bml_btl) {
        SPML_ERROR("cannot reach %d pe: no appropriate btl found", oshmem_my_proc_id());
        oshmem_shmem_abort(-1);
    }
    /* Check if btl has PUT method. If it doesn't - use SEND*/
    put_via_send = !(bml_btl->btl->btl_flags & MCA_BTL_FLAGS_PUT);

    /* Get rkey of remote PE (dst proc) which must be on memheap*/
    r_mkey = mca_memheap.memheap_get_cached_mkey(dst,
                                                 dst_addr,
                                                 btl_id,
                                                 &rva);
    if (!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable",
                   dst, dst_addr);
        oshmem_shmem_abort(-1);
    }

#if SPML_YODA_DEBUG == 1
    SPML_VERBOSE(100, "put: pe:%d dst=%p <- src: %p sz=%d. dst_rva=%p, %s",
                 dst, dst_addr, src_addr, (int)size, (void *)rva, mca_spml_base_mkey2str(r_mkey));
#endif

    ybtl = &mca_spml_yoda.btl_type_map[btl_id];

    /* check if we doing put into shm attached segment and if so
     * just do memcpy
     */
    if ((YODA_BTL_SM == ybtl->btl_type)
            && mca_memheap_base_can_local_copy(r_mkey, dst_addr)) {
        memcpy((void *) (unsigned long) rva, src_addr, size);
        return OSHMEM_SUCCESS;
    }

    /* We support only blocking PUT now => we always need copy for src buffer*/
    calc_nfrags(bml_btl, size, &frag_size, &nfrags, put_via_send);

    p_src = (char*) src_addr;
    p_dst = (char*) (unsigned long) rva;
    for (i = 0; i < nfrags; i++) {
        /* Allocating send request from free list */
        putreq = mca_spml_yoda_putreq_alloc(dst);
        frag = &putreq->put_frag;
        ncopied = i < nfrags - 1 ? frag_size :(unsigned) ((char *) src_addr + size - p_src);

        /* Preparing source buffer */

        /* allocate buffer */
        mca_spml_yoda_bml_alloc(bml_btl,
                                &des,
                                MCA_BTL_NO_ORDER,
                                ncopied,
                                MCA_BTL_DES_SEND_ALWAYS_CALLBACK,
                                put_via_send);

        if (OPAL_UNLIKELY(!des || !des->des_src )) {
            SPML_ERROR("src=%p nfrags = %d frag_size=%d",
                       src_addr, nfrags, frag_size);
            SPML_ERROR("shmem OOM error need %d bytes", ncopied);
            opal_show_help("help-oshmem-spml-yoda.txt",
                           "internal_oom_error",
                           true,
                           "Put", ncopied, mca_spml_yoda.bml_alloc_threshold);
            oshmem_shmem_abort(-1);
        }

        /* copy data to allocated buffer*/
        segment = des->des_src;
        spml_yoda_prepare_for_put((void*)segment->seg_addr.pval, ncopied,
                                  (void*)p_src, (void*)p_dst, put_via_send);

        /* Preparing destination buffer */

        assert( NULL != r_mkey->u.data && 0 != r_mkey->len);

        memcpy(&frag->rdma_segs[0].base_seg,
                r_mkey->u.data,
                r_mkey->len);

        frag->rdma_segs[0].base_seg.seg_addr.lval = (uintptr_t) p_dst;
        frag->rdma_segs[0].base_seg.seg_len = (put_via_send ?
                                                   ncopied + SPML_YODA_SEND_CONTEXT_SIZE :
                                                   ncopied);
        des->des_dst = &frag->rdma_segs[0].base_seg;

        frag->rdma_req = putreq;

        /* initialize callback data for put*/
        des->des_cbdata = frag;
        des->des_cbfunc = mca_spml_yoda_put_completion;
        des->des_dst_cnt = 1;

        OPAL_THREAD_ADD32(&mca_spml_yoda.n_active_puts, 1);
        /* put the data to remote side */
        if (!put_via_send) {
            rc = mca_bml_base_put(bml_btl, des);
        } else {
            rc = mca_bml_base_send(bml_btl, des, MCA_SPML_YODA_PUT);
            if (1 == rc)
                rc = OSHMEM_SUCCESS;
        }

        if (OPAL_UNLIKELY(OSHMEM_SUCCESS != rc)) {
            if (OSHMEM_ERR_OUT_OF_RESOURCE == rc) {
                /* No free resources, Block on completion here */
                SPML_ERROR("shmem error: OSHMEM_ERR_OUT_OF_RESOURCE");
                oshmem_request_wait_completion(&putreq->req_put.req_base.req_oshmem);
            } else {
                SPML_ERROR("shmem error");
            }
            /* exit with errro */
            SPML_ERROR("shmem error: ret = %i, send_pe = %i, dest_pe = %i",
                       rc, oshmem_my_proc_id(), dst);
            oshmem_shmem_abort(-1);
            rc = OSHMEM_ERROR;
        }
        p_src += ncopied;
        p_dst += ncopied;
    }

    return rc;
}

int mca_spml_yoda_put(void *dst_addr, size_t size, void *src_addr, int dst)
{
    return mca_spml_yoda_put_internal(dst_addr, size, src_addr, dst, 0);
}

int mca_spml_yoda_put_nb(void* dst_addr,
                         size_t size,
                         void* src_addr,
                         int dst,
                         void **handle)
{
    UNREFERENCED_PARAMETER(handle);

    return mca_spml_yoda_put_internal(dst_addr, size, src_addr, dst, 1);
}

int mca_spml_yoda_fence(void)
{
    return mca_spml_yoda_fence_internal(0);
}

int mca_spml_yoda_wait_gets(void)
{

    while (0 < mca_spml_yoda.n_active_gets) {
        opal_progress();
    }
    return OSHMEM_SUCCESS;
}


int mca_spml_yoda_enable(bool enable)
{
    SPML_VERBOSE(50, "*** yoda ENABLED ****");
    if (false == enable) {
        return OSHMEM_SUCCESS;
    }

    OBJ_CONSTRUCT(&mca_spml_yoda.lock, opal_mutex_t);

    /**
     *If we get here this is the SPML who get selected for the run. We
     * should get ownership for the put and get requests list, and
     * initialize them with the size of our own requests.
     */

    ompi_free_list_init_new(&mca_spml_base_put_requests,
                            sizeof(mca_spml_yoda_put_request_t),
                            opal_cache_line_size,
                            OBJ_CLASS(mca_spml_yoda_put_request_t),
                            0,
                            opal_cache_line_size,
                            mca_spml_yoda.free_list_num,
                            mca_spml_yoda.free_list_max,
                            mca_spml_yoda.free_list_inc,
                            NULL );

    ompi_free_list_init_new(&mca_spml_base_get_requests,
                            sizeof(mca_spml_yoda_get_request_t),
                            opal_cache_line_size,
                            OBJ_CLASS(mca_spml_yoda_get_request_t),
                            0,
                            opal_cache_line_size,
                            mca_spml_yoda.free_list_num,
                            mca_spml_yoda.free_list_max,
                            mca_spml_yoda.free_list_inc,
                            NULL );

    mca_spml_yoda.enabled = true;

    /* The following line resolves the issue with BTL tcp and SPML yoda. In this case the
     * atomic_basic_lock(root_rank) function may behave as DoS attack on root_rank, since
     * all the procceses will do shmem_int_get from root_rank. These calls would go through
     * bml active messaging and will trigger replays in libevent on root rank. If the flag
     * OPAL_ENVLOOP_ONCE is not set then libevent will continously progress constantly
     * incoming events thus causing root_rank to stuck in libevent loop.
     */
    opal_progress_set_event_flag(OPAL_EVLOOP_NONBLOCK | OPAL_EVLOOP_ONCE);

#if OSHMEM_WAIT_COMPLETION_DEBUG == 1
    condition_dbg_init();
#endif

    return OSHMEM_SUCCESS;
}

/**
 * shmem_get reads data from a remote address
 * in the symmetric heap via RDMA READ.
 * Get operation:
 * 1. Get the rkey to the remote address.
 * 2. Allocate a get request.
 * 3. Allocated a temporary pre-registered buffer
 *    to copy the data to.
 * 4. Init the request descriptor with remote side
 *    data and local side data.
 * 5. Read the remote buffer to a pre-registered
 *    buffer on the local PE using RDMA READ.
 * 6. Copy the received data to dst_addr if an
 *    intermediate pre-register buffer was used.
 * 7. Clear the request and return.
 *
 * src_addr - address on remote pe.
 * size - the amount on bytes to be read.
 * dst_addr - address on the local pe.
 * src - the pe of remote process.
 */
int mca_spml_yoda_get(void* src_addr, size_t size, void* dst_addr, int src)
{
    int rc = OSHMEM_SUCCESS;
    sshmem_mkey_t *r_mkey, *l_mkey;
    void* rva;
    unsigned ncopied = 0;
    unsigned int frag_size = 0;
    char *p_src, *p_dst;
    int i;
    int nfrags;
    mca_bml_base_btl_t* bml_btl = NULL;
    mca_btl_base_segment_t* segment;
    mca_btl_base_descriptor_t* des = NULL;
    mca_spml_yoda_rdma_frag_t* frag = NULL;
    struct mca_spml_yoda_getreq_parent get_holder;
    struct yoda_btl *ybtl;
    int btl_id = 0;
    int get_via_send;
    const opal_datatype_t *datatype = &opal_datatype_wchar;
    opal_convertor_t convertor;
    oshmem_proc_t *proc_self;
    size_t prepare_size;
    mca_mpool_base_registration_t* registration;
    mca_spml_yoda_get_request_t* getreq = NULL;

    /*If nothing to get its OK.*/
    if (0 >= size) {
        return rc;
    }

    /* Find bml_btl and its global btl_id */
    bml_btl = get_next_btl(src, &btl_id);
    if (!bml_btl) {
        SPML_ERROR("cannot reach %d pe: no appropriate btl found", oshmem_my_proc_id());
        oshmem_shmem_abort(-1);
    }
    /* Check if btl has GET method. If it doesn't - use SEND*/
    get_via_send = ! ( (bml_btl->btl->btl_flags & (MCA_BTL_FLAGS_GET)) &&
                       (bml_btl->btl->btl_flags & (MCA_BTL_FLAGS_PUT)) );

    /* Get rkey of remote PE (src proc) which must be on memheap*/
    r_mkey = mca_memheap.memheap_get_cached_mkey(src,
                                                 src_addr,
                                                 btl_id,
                                                 &rva);
    if (!r_mkey) {
        SPML_ERROR("pe=%d: %p is not address of shared variable",
                   src, src_addr);
        oshmem_shmem_abort(-1);
    }
#if SPML_YODA_DEBUG == 1
    SPML_VERBOSE(100, "get: pe:%d src=%p -> dst: %p sz=%d. src_rva=%p, %s",
                 src, src_addr, dst_addr, (int)size, (void *)rva, mca_spml_base_mkey2str(r_mkey));
#endif

    ybtl = &mca_spml_yoda.btl_type_map[btl_id];

    nfrags = 1;

    /* check if we doing get into shm attached segment and if so
     * just do memcpy
     */
    if ((YODA_BTL_SM == ybtl->btl_type)
            && mca_memheap_base_can_local_copy(r_mkey, src_addr)) {
        memcpy(dst_addr, (void *) rva, size);
        /* must call progress here to avoid deadlock. Scenarion:
         * pe1 pols pe2 via shm get. pe2 tries to get static variable from node one, which goes to sm btl
         * In this case pe2 is stuck forever because pe1 never calls opal_progress.
         * May be we do not need to call progress on every get() here but rather once in a while.
         */
        opal_progress();
        return OSHMEM_SUCCESS;
    }

    l_mkey = mca_memheap.memheap_get_local_mkey(dst_addr,
                                                btl_id);
    /*
     * Need a copy if local memory has not been registered or
     * we make GET via SEND
     */
    frag_size = ncopied;
    if ((NULL == l_mkey) || get_via_send) {
        calc_nfrags(bml_btl, size, &frag_size, &nfrags, get_via_send);
    }

    p_src = (char*) (unsigned long) rva;
    p_dst = (char*) dst_addr;
    get_holder.active_count = 0;

    for (i = 0; i < nfrags; i++) {
        /**
         * Allocating a get request from a pre-allocated
         * and pre-registered free list.
         */
        getreq = mca_spml_yoda_getreq_alloc(src);
        assert(getreq);
        getreq->p_dst = NULL;
        frag = &getreq->get_frag;
        getreq->parent = &get_holder;

        ncopied = i < nfrags - 1 ? frag_size :(unsigned) ((char *) dst_addr + size - p_dst);
        frag->allocated = 0;
        /* Prepare destination descriptor*/
        assert(0 != r_mkey->len);
        memcpy(&frag->rdma_segs[0].base_seg,
                r_mkey->u.data,
                r_mkey->len);

        frag->rdma_segs[0].base_seg.seg_len = (get_via_send ? ncopied + SPML_YODA_SEND_CONTEXT_SIZE : ncopied);
        if (get_via_send) {
            frag->use_send = 1;
            frag->allocated = 1;
            /**
             * Allocate a temporary buffer on the local PE.
             * The local buffer will store the data read
             * from the remote address.
             */
            mca_spml_yoda_bml_alloc(bml_btl,
                                    &des,
                                    MCA_BTL_NO_ORDER,
                                    (int)frag_size,
                                    MCA_BTL_DES_SEND_ALWAYS_CALLBACK,
                                    get_via_send);
            if (OPAL_UNLIKELY(!des || !des->des_src)) {
                SPML_ERROR("shmem OOM error need %d bytes", ncopied);
                SPML_ERROR("src=%p nfrags = %d frag_size=%d",
                           src_addr, nfrags, frag_size);
                oshmem_shmem_abort(-1);
            }

            segment = des->des_src;
            spml_yoda_prepare_for_get((void*)segment->seg_addr.pval, ncopied, (void*)p_src, oshmem_my_proc_id(), (void*)p_dst, (void*) getreq);
            des->des_cbfunc = mca_spml_yoda_get_response_completion;

            OPAL_THREAD_ADD32(&mca_spml_yoda.n_active_gets, 1);
        }
        else {
            /*
             * Register src memory if do GET via GET
             */
            proc_self = oshmem_proc_group_find(oshmem_group_all, oshmem_my_proc_id());
            OBJ_CONSTRUCT(&convertor, opal_convertor_t);

            prepare_size = ncopied;
            opal_convertor_copy_and_prepare_for_recv(proc_self->proc_convertor,
                                                     datatype,
                                                     prepare_size,
                                                     p_dst,
                                                     0,
                                                     &convertor);

            registration = (NULL == l_mkey ? NULL : ((mca_spml_yoda_context_t*)l_mkey->spml_context)->registration);
            des = ybtl->btl->btl_prepare_dst(ybtl->btl,
                                             bml_btl->btl_endpoint,
                                             registration,
                                             &convertor,
                                             MCA_BTL_NO_ORDER,
                                             0,
                                             &prepare_size,
                                             0);
            if (NULL == des) {
                SPML_ERROR("%s: failed to register destination memory %p.",
                           btl_type2str(ybtl->btl_type), p_dst);
            }
            OBJ_DESTRUCT(&convertor);
            frag->rdma_segs[0].base_seg.seg_addr.lval = (uintptr_t) p_src;
            getreq->p_dst = (uint64_t*) p_dst;
            frag->size = ncopied;
            des->des_cbfunc = mca_spml_yoda_get_completion;
            des->des_src = &frag->rdma_segs[0].base_seg;

            OPAL_THREAD_ADD32(&mca_spml_yoda.n_active_gets, 1);
        }

        /**
         * Initialize the remote data fragment
         * with remote address data required for
         * executing RDMA READ from a remote buffer.
         */

        frag->rdma_req = getreq;

        /**
         * Init remote side descriptor.
         */
        des->des_src_cnt = 1;
        des->des_cbdata = frag;

        /**
         *  Do GET operation
         */
        if (get_via_send) {
            rc = mca_bml_base_send(bml_btl, des, MCA_SPML_YODA_GET);
            if (1 == rc)
                rc = OSHMEM_SUCCESS;
        } else {
            rc = mca_bml_base_get(bml_btl, des);
        }

        if (OPAL_UNLIKELY(OSHMEM_SUCCESS != rc)) {
            if (OSHMEM_ERR_OUT_OF_RESOURCE == rc) {
                /* No free resources, Block on completion here */
                oshmem_request_wait_completion(&getreq->req_get.req_base.req_oshmem);
                return OSHMEM_SUCCESS;
            } else {
                SPML_ERROR("oshmem_get: error %d", rc);
                oshmem_shmem_abort(-1);
                return rc;
            }
        }
        p_dst += ncopied;
        p_src += ncopied;
        OPAL_THREAD_ADD32(&get_holder.active_count, 1);
    }

    /* revisit if we really need this for self and sm */
    /* if (YODA_BTL_SELF == ybtl->btl_type) */
    opal_progress();

    /* Wait for completion on request */
    while (get_holder.active_count > 0)
        oshmem_request_wait_completion(&getreq->req_get.req_base.req_oshmem);

    return rc;
}

int mca_spml_yoda_send(void* buf,
                       size_t size,
                       int dst,
                       mca_spml_base_put_mode_t sendmode)
{
    int rc = OSHMEM_SUCCESS;

    rc = MCA_PML_CALL(send(buf,
                           size,
                           &(ompi_mpi_unsigned_char.dt),
                           dst,
                           0,
                           (mca_pml_base_send_mode_t)sendmode,
                           &(ompi_mpi_comm_world.comm)));

    return rc;
}

int mca_spml_yoda_recv(void* buf, size_t size, int src)
{
    int rc = OSHMEM_SUCCESS;

    rc = MCA_PML_CALL(recv(buf,
                           size,
                           &(ompi_mpi_unsigned_char.dt),
                           src,
                           0,
                           &(ompi_mpi_comm_world.comm),
                           NULL));

    return rc;
}

