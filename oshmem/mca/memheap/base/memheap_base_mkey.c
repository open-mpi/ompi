/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/util/output.h"
#include "opal/dss/dss.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"

#include "ompi/mca/bml/bml.h"
#include "ompi/mca/dpm/dpm.h"

#include "oshmem/proc/proc.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/mca/spml/spml.h"

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include <sys/ipc.h>
#include <sys/shm.h>

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
#include <infiniband/verbs.h>
#endif /* MPAGE_ENABLE */

/* Turn ON/OFF debug output from build (default 0) */
#ifndef MEMHEAP_BASE_DEBUG
#define MEMHEAP_BASE_DEBUG    0
#endif

#define MEMHEAP_RKEY_REQ            0xA1
#define MEMHEAP_RKEY_RESP           0xA2
#define MEMHEAP_RKEY_RESP_FAIL      0xA3

struct oob_comm {
    opal_mutex_t lck;
    opal_condition_t cond;
    mca_spml_mkey_t *mkeys;
    int mkeys_rcvd;
};

#define MEMHEAP_VERBOSE_FASTPATH(...)

static mca_memheap_map_t* memheap_map = NULL;

struct oob_comm memheap_oob;

/* pickup list of rkeys and remote va */
static int memheap_oob_get_mkeys(int pe,
                                 uint32_t va_seg_num,
                                 mca_spml_mkey_t *mkey);

static inline void* __seg2base_va(int seg)
{
    return memheap_map->mem_segs[seg].start;
}

static int __seg_cmp(const void *k, const void *v)
{
    uintptr_t va = (uintptr_t) k;
    map_segment_t *s = (map_segment_t *) v;

    if (va < (uintptr_t)s->start)
        return -1;
    if (va >= (uintptr_t)s->end)
        return 1;

    return 0;
}

static inline map_segment_t *__find_va(const void* va)
{
    map_segment_t *s;

    if (OPAL_LIKELY((uintptr_t)va >= (uintptr_t)memheap_map->mem_segs[HEAP_SEG_INDEX].start &&
                    (uintptr_t)va < (uintptr_t)memheap_map->mem_segs[HEAP_SEG_INDEX].end)) {
        s = &memheap_map->mem_segs[HEAP_SEG_INDEX];
    } else {
        s = bsearch(va,
                    &memheap_map->mem_segs[SYMB_SEG_INDEX],
                    memheap_map->n_segments - 1,
                    sizeof(*s),
                    __seg_cmp);
    }

#if MEMHEAP_BASE_DEBUG == 1
    if (s) {
        MEMHEAP_VERBOSE(5, "match seg#%02ld: 0x%llX - 0x%llX %llu bytes va=%p",
                s - memheap_map->mem_segs,
                (long long)s->start,
                (long long)s->end,
                (long long)(s->end - s->start),
                (void *)va);
    }
#endif
    return s;
}

static int do_mkey_req(opal_buffer_t *msg, int pe, int seg)
{
    uint8_t msg_type;
    oshmem_proc_t *proc;
    int i, n, tr_id;
    mca_spml_mkey_t *mkey;

    msg_type = MEMHEAP_RKEY_RESP;
    opal_dss.pack(msg, &msg_type, 1, OPAL_UINT8);

    /* go over all transports to remote pe and pack mkeys */
    n = oshmem_get_transport_count(pe);
    proc = oshmem_proc_group_find(oshmem_group_all, pe);
    opal_dss.pack(msg, &n, 1, OPAL_UINT32);
    MEMHEAP_VERBOSE(5, "found %d transports to %d", n, pe);
    for (i = 0; i < n; i++) {
        tr_id = proc->transport_ids[i];

        mkey = mca_memheap_base_get_mkey(__seg2base_va(seg), tr_id);
        if (!mkey) {
            MEMHEAP_ERROR("seg#%d tr_id: %d failed to find local mkey",
                          seg, tr_id);
            return OSHMEM_ERROR;
        }
        opal_dss.pack(msg, &tr_id, 1, OPAL_UINT32);
        opal_dss.pack(msg, &mkey->key, 1, OPAL_UINT64);
        opal_dss.pack(msg, &mkey->va_base, 1, OPAL_UINT64);

        if (NULL != MCA_SPML_CALL(get_remote_context_size)) {
            uint32_t context_size =
                    (mkey->spml_context == NULL ) ?
                            0 :
                            (uint32_t) MCA_SPML_CALL(get_remote_context_size(mkey->spml_context));
            opal_dss.pack(msg, &context_size, 1, OPAL_UINT32);
            if (0 != context_size) {
                opal_dss.pack(msg,
                              MCA_SPML_CALL(get_remote_context(mkey->spml_context)),
                              context_size,
                              OPAL_BYTE);
            }
        }

        MEMHEAP_VERBOSE(5,
                        "seg#%d tr_id: %d key %llx base_va %p",
                        seg, tr_id, (unsigned long long)mkey->key, mkey->va_base);
    }
    return OSHMEM_SUCCESS;
}

static void memheap_attach_segment(mca_spml_mkey_t *mkey, int tr_id)
{
    /* process special case when va was got using shmget(IPC_PRIVATE)
     * this case is notable for:
     * - key is set as (type|shmid);
     * - va_base is set as 0;
     */
    if (!mkey->va_base
            && ((int) MEMHEAP_SHM_GET_ID(mkey->key) != MEMHEAP_SHM_INVALID)) {
        MEMHEAP_VERBOSE(5,
                        "shared memory usage tr_id: %d key %llx base_va %p shmid 0x%X|0x%X",
                        tr_id,
                        (unsigned long long)mkey->key,
                        mkey->va_base,
                        MEMHEAP_SHM_GET_TYPE(mkey->key),
                        MEMHEAP_SHM_GET_ID(mkey->key));

        if (MEMHEAP_SHM_GET_TYPE(mkey->key) == MAP_SEGMENT_ALLOC_SHM) {
            mkey->va_base = shmat(MEMHEAP_SHM_GET_ID(mkey->key),
                                             0,
                                             0);
        } else if (MEMHEAP_SHM_GET_TYPE(mkey->key) == MAP_SEGMENT_ALLOC_IBV) {
#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
            openib_device_t *device = NULL;
            struct ibv_mr *ib_mr;
            void *addr;
            static int mr_count;

            int access_flag = IBV_ACCESS_LOCAL_WRITE |
            IBV_ACCESS_REMOTE_WRITE |
            IBV_ACCESS_REMOTE_READ |
            IBV_ACCESS_NO_RDMA;

            device = (openib_device_t *)memheap_map->mem_segs[HEAP_SEG_INDEX].context;
            assert(device);

            /* workaround mtt problem - request aligned addresses */
            ++mr_count;
            addr = (void *)(mca_memheap_base_start_address + mca_memheap_base_mr_interleave_factor*1024ULL*1024ULL*1024ULL*mr_count);
            ib_mr = ibv_reg_shared_mr(MEMHEAP_SHM_GET_ID(mkey->key),
                    device->ib_pd, addr, access_flag);
            if (NULL == ib_mr)
            {
                mkey->va_base = (void*)-1;
                MEMHEAP_ERROR("error to ibv_reg_shared_mr() errno says %d: %s",
                        errno, strerror(errno));
            }
            else
            {
                if (ib_mr->addr != addr) {
                    MEMHEAP_WARN("Failed to map shared region to address %p got addr %p. Try to increase 'memheap_mr_interleave_factor' from %d", addr, ib_mr->addr, mca_memheap_base_mr_interleave_factor);
                }

                opal_value_array_append_item(&device->ib_mr_array, &ib_mr);
                mkey->va_base = ib_mr->addr;
            }
#endif /* MPAGE_ENABLE */
        } else {
            MEMHEAP_ERROR("tr_id: %d key %llx attach failed: incorrect shmid 0x%X|0x%X",
                          tr_id, 
                          (unsigned long long)mkey->key,
                          MEMHEAP_SHM_GET_TYPE(mkey->key),
                          MEMHEAP_SHM_GET_ID(mkey->key));
            oshmem_shmem_abort(-1);
        }

        if ((void *) -1 == (void *) mkey->va_base) {
            MEMHEAP_ERROR("tr_id: %d key %llx attach failed: errno = %d",
                          tr_id, (unsigned long long)mkey->key, errno);
            oshmem_shmem_abort(-1);
        }
    }
}

static void do_mkey_resp(opal_buffer_t *msg)
{
    int32_t cnt;
    int32_t n;
    int32_t tr_id;
    int i;

    cnt = 1;
    opal_dss.unpack(msg, &n, &cnt, OPAL_UINT32);
    for (i = 0; i < n; i++) {
        opal_dss.unpack(msg, &tr_id, &cnt, OPAL_UINT32);
        opal_dss.unpack(msg, &memheap_oob.mkeys[tr_id].key, &cnt, OPAL_UINT64);
        opal_dss.unpack(msg,
                        &memheap_oob.mkeys[tr_id].va_base,
                        &cnt,
                        OPAL_UINT64);

        if (NULL != MCA_SPML_CALL(set_remote_context_size)) {
            int32_t context_size;
            opal_dss.unpack(msg, &context_size, &cnt, OPAL_UINT32);
            if (0 != context_size) {
                MCA_SPML_CALL(set_remote_context_size(&(memheap_oob.mkeys[tr_id].spml_context), context_size));
                void* context;
                context = calloc(1, context_size);
                opal_dss.unpack(msg, context, &context_size, OPAL_BYTE);
                MCA_SPML_CALL(set_remote_context(&(memheap_oob.mkeys[tr_id].spml_context),context));
            }
        }

        memheap_attach_segment(&memheap_oob.mkeys[tr_id], tr_id);

        MEMHEAP_VERBOSE(5,
                        "tr_id: %d key %llx base_va %p",
                        tr_id, (unsigned long long)memheap_oob.mkeys[tr_id].key, memheap_oob.mkeys[tr_id].va_base);
    }
}

static void memheap_buddy_rml_recv_cb(int status,
                                      orte_process_name_t* process_name,
                                      opal_buffer_t* buffer,
                                      orte_rml_tag_t tag,
                                      void* cbdata)
{
    MEMHEAP_VERBOSE(5,
                    "**** get request from %u:%d",
                    process_name->jobid, process_name->vpid);
    int32_t cnt = 1;
    int rc;
    opal_buffer_t *msg;
    uint8_t msg_type;
    uint32_t seg;

    MEMHEAP_VERBOSE(5, "unpacking %d of %d", cnt, OPAL_UINT8);
    rc = opal_dss.unpack(buffer, &msg_type, &cnt, OPAL_UINT8);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto send_fail;
    }

    switch (msg_type) {
    case MEMHEAP_RKEY_REQ:
        cnt = 1;
        rc = opal_dss.unpack(buffer, &seg, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            MEMHEAP_ERROR("bad RKEY_REQ msg");
            goto send_fail;
        }

        MEMHEAP_VERBOSE(5, "*** RKEY REQ");
        msg = OBJ_NEW(opal_buffer_t);
        if (!msg) {
            MEMHEAP_ERROR("failed to get msg buffer");
            ORTE_ERROR_LOG(rc);
            return;
        }

        if (OSHMEM_SUCCESS != do_mkey_req(msg, process_name->vpid, seg)) {
            OBJ_RELEASE(msg);
            goto send_fail;
        }

        rc = orte_rml.send_buffer_nb(process_name, msg, OMPI_RML_TAG_SHMEM, orte_rml_send_callback, NULL);

        if (0 > rc) {
            MEMHEAP_ERROR("FAILED to send rml message %d", rc);
            ORTE_ERROR_LOG(rc);
            goto send_fail;
        }
        break;

    case MEMHEAP_RKEY_RESP:
        MEMHEAP_VERBOSE(5, "*** RKEY RESP");
        OPAL_THREAD_LOCK(&memheap_oob.lck);
        do_mkey_resp(buffer);
        memheap_oob.mkeys_rcvd = MEMHEAP_RKEY_RESP;
        opal_condition_broadcast(&memheap_oob.cond);
        OPAL_THREAD_UNLOCK(&memheap_oob.lck);
        break;

    case MEMHEAP_RKEY_RESP_FAIL:
        MEMHEAP_VERBOSE(5, "*** RKEY RESP FAIL");
        memheap_oob.mkeys_rcvd = MEMHEAP_RKEY_RESP_FAIL;
        opal_condition_broadcast(&memheap_oob.cond);
        OPAL_THREAD_UNLOCK(&memheap_oob.lck);
        break;

    default:
        MEMHEAP_VERBOSE(5, "Unknown message type %x", msg_type);
        goto send_fail;
    }
    return;

    send_fail: msg = OBJ_NEW(opal_buffer_t);
    if (!msg) {
        MEMHEAP_ERROR("failed to get msg buffer");
        ORTE_ERROR_LOG(rc);
        return;
    }
    msg_type = MEMHEAP_RKEY_RESP_FAIL;
    opal_dss.pack(msg, &msg_type, 1, OPAL_UINT8);

    rc = orte_rml.send_buffer_nb(process_name, msg, OMPI_RML_TAG_SHMEM, orte_rml_send_callback, NULL);
    if (0 > rc) {
        MEMHEAP_ERROR("FAILED to send rml message %d", rc);
        ORTE_ERROR_LOG(rc);
    }

}

int memheap_oob_init(mca_memheap_map_t *map)
{
    int rc = OSHMEM_SUCCESS;

    memheap_map = map;

    OBJ_CONSTRUCT(&memheap_oob.lck, opal_mutex_t);
    OBJ_CONSTRUCT(&memheap_oob.cond, opal_condition_t);

    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                 OMPI_RML_TAG_SHMEM,
                                 ORTE_RML_PERSISTENT,
                                 memheap_buddy_rml_recv_cb,
                                 NULL );

    return rc;
}

void memheap_oob_destruct(void)
{
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, OMPI_RML_TAG_SHMEM);
    OBJ_DESTRUCT(&memheap_oob.lck);
    OBJ_DESTRUCT(&memheap_oob.cond);
}

static int memheap_oob_get_mkeys(int pe, uint32_t seg, mca_spml_mkey_t *mkeys)
{
    orte_process_name_t name;
    opal_buffer_t *msg;
    int rc;
    uint8_t cmd;
    int i;

    if (OSHMEM_SUCCESS == MCA_SPML_CALL(oob_get_mkeys(pe, seg, mkeys))) {
        for (i = 0; i < memheap_map->num_transports; i++) {
            mkeys[i].va_base = __seg2base_va(seg);
            MEMHEAP_VERBOSE(5,
                            "MKEY CALCULATED BY LOCAL SPML: pe: %d tr_id: %d key %llx base_va %p",
                            pe,
                            i,
                            (unsigned long long)mkeys[i].key,
                            mkeys[i].va_base);
        }
        return OSHMEM_SUCCESS;
    }

    OPAL_THREAD_LOCK(&memheap_oob.lck);

    memheap_oob.mkeys = mkeys;
    memheap_oob.mkeys_rcvd = 0;

    name.jobid = ORTE_PROC_MY_NAME->jobid;
    name.vpid = pe;

    msg = OBJ_NEW(opal_buffer_t);
    if (!msg) {
        OPAL_THREAD_UNLOCK(&memheap_oob.lck);
        MEMHEAP_ERROR("failed to get msg buffer");
        return OSHMEM_ERROR;
    }

    OPAL_THREAD_LOCK(&memheap_oob.lck);
    cmd = MEMHEAP_RKEY_REQ;
    opal_dss.pack(msg, &cmd, 1, OPAL_UINT8);
    opal_dss.pack(msg, &seg, 1, OPAL_UINT32);
    rc = orte_rml.send_buffer_nb(&name, msg, OMPI_RML_TAG_SHMEM, orte_rml_send_callback, NULL);
    if (0 > rc) {
        OBJ_RELEASE(msg);
        OPAL_THREAD_UNLOCK(&memheap_oob.lck);
        MEMHEAP_ERROR("FAILED to send rml message %d", rc);
        return OSHMEM_ERROR;
    }

    MEMHEAP_VERBOSE(5, "message sent: %d bytes!", rc);

    while (!memheap_oob.mkeys_rcvd) {
        opal_condition_wait(&memheap_oob.cond, &memheap_oob.lck);
    }

    if (MEMHEAP_RKEY_RESP == memheap_oob.mkeys_rcvd) {
        rc = OSHMEM_SUCCESS;
    } else {
        MEMHEAP_ERROR("failed to get rkey seg#%d pe=%d", seg, pe);
        rc = OSHMEM_ERROR;
    }

    OPAL_THREAD_UNLOCK(&memheap_oob.lck);
    return rc;
}

void mca_memheap_modex_recv_all(void)
{
    int i;
    int j;
    int nprocs, my_pe;
    oshmem_proc_t *proc;
    mca_spml_mkey_t *mkey;
    void* dummy_rva;

    if (!mca_memheap_base_key_exchange)
        return;

    /* init rkey cache */
    nprocs = oshmem_num_procs();
    my_pe = oshmem_my_proc_id();

    /* Note:
     * Doing exchange via rml till we figure out problem with grpcomm.modex and barrier
     */
    for (i = 0; i < nprocs; i++) {
        if (i == my_pe)
            continue;

        proc = oshmem_proc_group_find(oshmem_group_all, i);
        for (j = 0; j < memheap_map->n_segments; j++) {
            mkey =
                    mca_memheap_base_get_cached_mkey(i,
                                                     memheap_map->mem_segs[j].start,
                                                     proc->transport_ids[0],
                                                     &dummy_rva);
            if (!mkey) {
                MEMHEAP_ERROR("Failed to receive mkeys");
                oshmem_shmem_abort(-1);
            }
        }

    }

    /*
     * There is an issue with orte_grpcomm.barrier usage as
     * ess/pmi directs to use grpcomm/pmi in case slurm srun() call grpcomm/pmi calls PMI_Barrier() 
     * that is a function of external library.
     * There is no opal_progress() in such way. As a result slow PEs send a request (MEMHEAP_RKEY_REQ) to
     * fast PEs waiting on barrier and do not get a respond (MEMHEAP_RKEY_RESP).
     *
     * there are following ways to solve one:
     * 1. calculate requests from remote PEs and do ORTE_PROGRESSED_WAIT waiting for expected value;
     * 2. use shmem_barrier_all();
     * 3. rework pmi/barrier to use opal_progress();
     * 4. use orte_grpcomm.barrier carefully;
     * 
     * It seems there is no need to use orte_grpcomm.barrier here
     */

    if (memheap_map->mem_segs[HEAP_SEG_INDEX].shmid != MEMHEAP_SHM_INVALID) {
        /* unfortunately we must do barrier here to assure that everyone are attached to our segment
         * good thing that this code path only invoked on older linuxes (-mca shmalloc_use_hugepages 3|4)
         * try to minimize damage here by waiting 5 seconds and doing progress
         */
        shmem_barrier_all();
        /* keys exchanged, segments attached, now we can safely cleanup */
        if (memheap_map->mem_segs[HEAP_SEG_INDEX].type
                == MAP_SEGMENT_ALLOC_SHM) {
            shmctl(memheap_map->mem_segs[HEAP_SEG_INDEX].shmid,
                   IPC_RMID,
                   NULL );
        }
    }
}

static inline void* va2rva(void* va,
                              void* local_base,
                              void* remote_base)
{
    return (void*) (remote_base > local_base ? (uintptr_t)va + (remote_base - local_base) :
                    (uintptr_t)va - (local_base - remote_base));
}

mca_spml_mkey_t * mca_memheap_base_get_cached_mkey(int pe,
                                                   void* va,
                                                   int btl_id,
                                                   void** rva)
{
    map_segment_t *s;
    int rc;
    mca_spml_mkey_t *mkey;

    MEMHEAP_VERBOSE_FASTPATH(10, "rkey: pe=%d va=%p", pe, va);
    s = __find_va(va);
    if (NULL == s)
        return NULL ;

    if (!s->is_active)
        return NULL ;

    if (pe == oshmem_my_proc_id()) {
        *rva = va;
        MEMHEAP_VERBOSE_FASTPATH(10, "rkey: pe=%d va=%p -> (local) %lx %p", pe, va,
                s->mkeys[btl_id].key, *rva);
        return &s->mkeys[btl_id];
    }

    if (OPAL_LIKELY(s->mkeys_cache[pe])) {
        mkey = &s->mkeys_cache[pe][btl_id];
        *rva = va2rva(va, s->start, mkey->va_base);
        MEMHEAP_VERBOSE_FASTPATH(10, "rkey: pe=%d va=%p -> (cached) %lx %p", pe, (void *)va, mkey->key, (void *)*rva);
        return mkey;
    }

    s->mkeys_cache[pe] = (mca_spml_mkey_t *) calloc(memheap_map->num_transports,
                                                    sizeof(mca_spml_mkey_t));
    if (!s->mkeys_cache[pe])
        return NULL ;

    rc = memheap_oob_get_mkeys(pe,
                               s - memheap_map->mem_segs,
                               s->mkeys_cache[pe]);
    if (OSHMEM_SUCCESS != rc)
        return NULL ;

    mkey = &s->mkeys_cache[pe][btl_id];
    *rva = va2rva(va, s->start, mkey->va_base);

    MEMHEAP_VERBOSE_FASTPATH(5, "rkey: pe=%d va=%p -> (remote lookup) %lx %p", pe, (void *)va, mkey->key, (void *)*rva);
    return mkey;
}

mca_spml_mkey_t *mca_memheap_base_get_mkey(void* va, int tr_id)
{
    map_segment_t *s;

    s = __find_va(va);

    return ((s && s->is_active) ? &s->mkeys[tr_id] : NULL );
}

uint64_t mca_memheap_base_find_offset(int pe,
                                      int tr_id,
                                      void* va,
                                      void* rva)
{
    map_segment_t *s;

    s = __find_va(va);

    return ((s && s->is_active) ? (rva - s->mkeys_cache[pe][tr_id].va_base) : 0);
}

int mca_memheap_base_is_symmetric_addr(const void* va)
{
    return (__find_va(va) ? 1 : 0);
}

int mca_memheap_base_detect_addr_type(void* va)
{
    int addr_type = ADDR_INVALID;
    map_segment_t *s;

    s = __find_va(va);

    if (s) {
        if (s->type == MAP_SEGMENT_STATIC) {
            addr_type = ADDR_STATIC;
        } else if ((uintptr_t)va >= (uintptr_t) s->start
                   && (uintptr_t)va < (uintptr_t) (s->start + mca_memheap.memheap_size)) {
            addr_type = ADDR_USER;
        } else {
            assert( (uintptr_t)va >= (uintptr_t)(s->start + mca_memheap.memheap_size) && (uintptr_t)va < (uintptr_t)s->end);
            addr_type = ADDR_PRIVATE;
        }
    }

    return addr_type;
}
