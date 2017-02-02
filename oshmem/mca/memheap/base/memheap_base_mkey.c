/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "oshmem/util/oshmem_util.h"
#include "opal/dss/dss.h"

#include "oshmem/proc/proc.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/mca/spml/spml.h"

/* Turn ON/OFF debug output from build (default 0) */
#ifndef MEMHEAP_BASE_DEBUG
#define MEMHEAP_BASE_DEBUG    0
#endif

#define MEMHEAP_RKEY_REQ            0xA1
#define MEMHEAP_RKEY_RESP           0xA2
#define MEMHEAP_RKEY_RESP_FAIL      0xA3

#define MEMHEAP_MKEY_MAXSIZE   4096
#define MEMHEAP_RECV_REQS_MAX  16

typedef struct oob_comm_request {
    opal_list_item_t super;
    MPI_Request recv_req;
    char buf[MEMHEAP_MKEY_MAXSIZE];
} oob_comm_request_t;

struct oob_comm {
    opal_mutex_t lck;
    opal_condition_t cond;
    uint32_t segno;
    sshmem_mkey_t *mkeys;
    int mkeys_rcvd;
    oob_comm_request_t req_pool[MEMHEAP_RECV_REQS_MAX];
    opal_list_t req_list;
    int is_inited;
};

mca_memheap_map_t* memheap_map = NULL;

struct oob_comm memheap_oob = {{{0}}};

static int send_buffer(int pe, opal_buffer_t *msg);

static int oshmem_mkey_recv_cb(void);

/* pickup list of rkeys and remote va */
static int memheap_oob_get_mkeys(int pe,
                                 uint32_t va_seg_num,
                                 sshmem_mkey_t *mkey);

int mca_memheap_seg_cmp(const void *k, const void *v)
{
    uintptr_t va = (uintptr_t) k;
    map_segment_t *s = (map_segment_t *) v;

    if (va < (uintptr_t)s->super.va_base)
        return -1;
    if (va >= (uintptr_t)s->super.va_end)
        return 1;

    return 0;
}

static int pack_local_mkeys(opal_buffer_t *msg, int pe, int seg)
{
    int i, n;
    sshmem_mkey_t *mkey;

    /* go over all transports and pack mkeys */
    n = memheap_map->num_transports;
    opal_dss.pack(msg, &n, 1, OPAL_UINT32);
    MEMHEAP_VERBOSE(5, "found %d transports to %d", n, pe);
    for (i = 0; i < n; i++) {
        mkey = mca_memheap_base_get_mkey(mca_memheap_seg2base_va(seg), i);
        if (!mkey) {
            MEMHEAP_ERROR("seg#%d tr_id: %d failed to find local mkey",
                          seg, i);
            return OSHMEM_ERROR;
        }
        opal_dss.pack(msg, &i, 1, OPAL_UINT32);
        opal_dss.pack(msg, &mkey->va_base, 1, OPAL_UINT64);
        if (0 == mkey->va_base) {
            opal_dss.pack(msg, &mkey->u.key, 1, OPAL_UINT64);
        } else {
            opal_dss.pack(msg, &mkey->len, 1, OPAL_UINT16);
            if (0 < mkey->len) {
                opal_dss.pack(msg, mkey->u.data, mkey->len, OPAL_BYTE);
            }
        }
        MEMHEAP_VERBOSE(5,
                        "seg#%d tr_id: %d %s",
                        seg, i, mca_spml_base_mkey2str(mkey));
    }
    return OSHMEM_SUCCESS;
}

static void memheap_attach_segment(sshmem_mkey_t *mkey, int tr_id)
{
    /* process special case when va was got using sshmem
     * this case is notable for:
     * - key is set as (seg_id);
     * - va_base is set as 0;
     * - len is set as 0;
     */
    assert(mkey->va_base == 0);
    assert(mkey->len == 0);

    MEMHEAP_VERBOSE(5,
            "shared memory usage tr_id: %d va_base: 0x%p len: %d key %llx",
            tr_id,
            mkey->va_base, mkey->len, (unsigned long long)mkey->u.key);

    mca_sshmem_segment_attach(&(memheap_map->mem_segs[HEAP_SEG_INDEX]), mkey);

    if ((void *) -1 == (void *) mkey->va_base) {
        MEMHEAP_ERROR("tr_id: %d key %llx attach failed: errno = %d",
                tr_id, (unsigned long long)mkey->u.key, errno);
        oshmem_shmem_abort(-1);
    }
}


static void unpack_remote_mkeys(opal_buffer_t *msg, int remote_pe)
{
    int32_t cnt;
    int32_t n;
    int32_t tr_id;
    int i;
    ompi_proc_t *proc;

    proc = oshmem_proc_group_find(oshmem_group_all, remote_pe);
    cnt = 1;
    opal_dss.unpack(msg, &n, &cnt, OPAL_UINT32);
    for (i = 0; i < n; i++) {
        cnt = 1;
        opal_dss.unpack(msg, &tr_id, &cnt, OPAL_UINT32);
        cnt = 1;
        opal_dss.unpack(msg,
                        &memheap_oob.mkeys[tr_id].va_base,
                        &cnt,
                        OPAL_UINT64);

        if (0 == memheap_oob.mkeys[tr_id].va_base) {
            cnt = 1;
            opal_dss.unpack(msg, &memheap_oob.mkeys[tr_id].u.key, &cnt, OPAL_UINT64);
            if (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)) {
                memheap_attach_segment(&memheap_oob.mkeys[tr_id], tr_id);
            }
        } else {
            cnt = 1;
            opal_dss.unpack(msg, &memheap_oob.mkeys[tr_id].len, &cnt, OPAL_UINT16);
            if (0 < memheap_oob.mkeys[tr_id].len) {
                memheap_oob.mkeys[tr_id].u.data = malloc(memheap_oob.mkeys[tr_id].len);
                if (NULL == memheap_oob.mkeys[tr_id].u.data) {
                    MEMHEAP_ERROR("Failed allocate %d bytes", memheap_oob.mkeys[tr_id].len);
                    oshmem_shmem_abort(-1);
                }
                cnt = memheap_oob.mkeys[tr_id].len;
                opal_dss.unpack(msg, memheap_oob.mkeys[tr_id].u.data, &cnt, OPAL_BYTE);
            } else {
                memheap_oob.mkeys[tr_id].u.key = MAP_SEGMENT_SHM_INVALID;
            }
            MCA_SPML_CALL(rmkey_unpack(&memheap_oob.mkeys[tr_id], memheap_oob.segno, remote_pe, tr_id));
        }

        MEMHEAP_VERBOSE(5,
                        "tr_id: %d %s",
                        tr_id, mca_spml_base_mkey2str(&memheap_oob.mkeys[tr_id]));
    }
}

static void do_recv(int source_pe, opal_buffer_t* buffer)
{
    int32_t cnt = 1;
    int rc;
    opal_buffer_t *msg;
    uint8_t msg_type;
    uint32_t seg;

    MEMHEAP_VERBOSE(5, "unpacking %d of %d", cnt, OPAL_UINT8);
    rc = opal_dss.unpack(buffer, &msg_type, &cnt, OPAL_UINT8);
    if (OPAL_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto send_fail;
    }

    switch (msg_type) {
    case MEMHEAP_RKEY_REQ:
        cnt = 1;
        rc = opal_dss.unpack(buffer, &seg, &cnt, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
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

        msg_type = MEMHEAP_RKEY_RESP;
        opal_dss.pack(msg, &msg_type, 1, OPAL_UINT8);

        if (OSHMEM_SUCCESS != pack_local_mkeys(msg, source_pe, seg)) {
            OBJ_RELEASE(msg);
            goto send_fail;
        }

        rc = send_buffer(source_pe, msg);
        if (MPI_SUCCESS != rc) {
            MEMHEAP_ERROR("FAILED to send rml message %d", rc);
            ORTE_ERROR_LOG(rc);
            goto send_fail;
        }
        break;

    case MEMHEAP_RKEY_RESP:
        MEMHEAP_VERBOSE(5, "*** RKEY RESP");
        OPAL_THREAD_LOCK(&memheap_oob.lck);
        unpack_remote_mkeys(buffer, source_pe);
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

    rc = send_buffer(source_pe, msg);
    if (MPI_SUCCESS != rc) {
        MEMHEAP_ERROR("FAILED to send rml message %d", rc);
        ORTE_ERROR_LOG(rc);
    }

}

/**
 * simple/fast version of MPI_Test that
 * - only works with persistant request
 * - does not do any progress
 * - can be safely called from within opal_progress()
 */
static inline int my_MPI_Test(ompi_request_t ** rptr,
                              int *completed,
                              ompi_status_public_t * status)
{
    ompi_request_t *request = *rptr;

    assert(request->req_persistent);
    assert(request->req_state != OMPI_REQUEST_INACTIVE);

    if (request->req_complete) {
        int old_error;

        *completed = true;
        *status = request->req_status;
        old_error = status->MPI_ERROR;
        status->MPI_ERROR = old_error;

        request->req_state = OMPI_REQUEST_INACTIVE;
        return request->req_status.MPI_ERROR;
    }

    *completed = false;
    return OMPI_SUCCESS;
}

static int oshmem_mkey_recv_cb(void)
{
    MPI_Status status;
    int flag;
    int n;
    int rc;
    opal_buffer_t *msg;
    int32_t size;
    void *tmp_buf;
    oob_comm_request_t *r;

    n = 0;
    r = (oob_comm_request_t *)opal_list_get_first(&memheap_oob.req_list);
    assert(r);
    while(r != (oob_comm_request_t *)opal_list_get_end(&memheap_oob.req_list)) {
        my_MPI_Test(&r->recv_req, &flag, &status);
        if (OPAL_LIKELY(0 == flag)) {
            return n;
        }
        PMPI_Get_count(&status, MPI_BYTE, &size);
        MEMHEAP_VERBOSE(5, "OOB request from PE: %d, size %d", status.MPI_SOURCE, size);
        n++;
        opal_list_remove_first(&memheap_oob.req_list);

        /* to avoid deadlock we must start request
         * before processing it. Data are copied to
         * the tmp buffer
         */
        tmp_buf = malloc(size);
        if (NULL == tmp_buf) {
            MEMHEAP_ERROR("not enough memory");
            ORTE_ERROR_LOG(0);
            return n;
        } else {
		    memcpy(tmp_buf, (void*)&r->buf, size);
		    msg = OBJ_NEW(opal_buffer_t);
		    if (NULL == msg) {
		        MEMHEAP_ERROR("not enough memory");
		        ORTE_ERROR_LOG(0);
		        free(tmp_buf);
		        return n;
		    }
		    opal_dss.load(msg, (void*)tmp_buf, size);

            /*
             * send reply before posting the receive request again to limit the recursion size to
             * number of receive requests.
             * send can call opal_progress which calls this function again. If recv req is started
             * stack size will be proportional to number of job ranks.
             */
            do_recv(status.MPI_SOURCE, msg);
            OBJ_RELEASE(msg);
        }

        rc = PMPI_Start(&r->recv_req);
        if (MPI_SUCCESS != rc) {
            MEMHEAP_ERROR("Failed to post recv request %d", rc);
            ORTE_ERROR_LOG(rc);
            return n;
        }
        opal_list_append(&memheap_oob.req_list, &r->super);


        r = (oob_comm_request_t *)opal_list_get_first(&memheap_oob.req_list);
        assert(r);
    }

    return 1;
}

int memheap_oob_init(mca_memheap_map_t *map)
{
    int rc = OSHMEM_SUCCESS;
    int i;
    oob_comm_request_t *r;

    memheap_map = map;

    OBJ_CONSTRUCT(&memheap_oob.lck, opal_mutex_t);
    OBJ_CONSTRUCT(&memheap_oob.cond, opal_condition_t);
    OBJ_CONSTRUCT(&memheap_oob.req_list, opal_list_t);


    for (i = 0; i < MEMHEAP_RECV_REQS_MAX; i++) {
        r = &memheap_oob.req_pool[i];
        rc = PMPI_Recv_init(r->buf, sizeof(r->buf), MPI_BYTE,
                MPI_ANY_SOURCE, 0,
                oshmem_comm_world,
                &r->recv_req);
        if (MPI_SUCCESS != rc) {
            MEMHEAP_ERROR("Failed to created recv request %d", rc);
            return rc;
        }

        rc = PMPI_Start(&r->recv_req);
        if (MPI_SUCCESS != rc) {
            MEMHEAP_ERROR("Failed to post recv request %d", rc);
            return rc;
        }
        opal_list_append(&memheap_oob.req_list, &r->super);
    }

    opal_progress_register(oshmem_mkey_recv_cb);
    memheap_oob.is_inited = 1;

    return rc;
}

void memheap_oob_destruct(void)
{
    int i;
    oob_comm_request_t *r;

    if (!memheap_oob.is_inited) {
        return;
    }

    opal_progress_unregister(oshmem_mkey_recv_cb);

    for (i = 0; i < MEMHEAP_RECV_REQS_MAX; i++) {
        r = &memheap_oob.req_pool[i];
        PMPI_Cancel(&r->recv_req);
        PMPI_Request_free(&r->recv_req);
    }

    OBJ_DESTRUCT(&memheap_oob.req_list);
    OBJ_DESTRUCT(&memheap_oob.lck);
    OBJ_DESTRUCT(&memheap_oob.cond);
    memheap_oob.is_inited = 0;
}

static int send_buffer(int pe, opal_buffer_t *msg)
{
    void *buffer;
    int32_t size;
    int rc;

    opal_dss.unload(msg, &buffer, &size);
    rc = PMPI_Send(buffer, size, MPI_BYTE, pe, 0, oshmem_comm_world);
    free(buffer);
    OBJ_RELEASE(msg);

    MEMHEAP_VERBOSE(5, "message sent: dst=%d, rc=%d, %d bytes!", pe, rc, size);
    return rc;
}

static int memheap_oob_get_mkeys(int pe, uint32_t seg, sshmem_mkey_t *mkeys)
{
    opal_buffer_t *msg;
    uint8_t cmd;
    int i;
    int rc;

    if (OSHMEM_SUCCESS == MCA_SPML_CALL(oob_get_mkeys(pe, seg, mkeys))) {
        for (i = 0; i < memheap_map->num_transports; i++) {
            MEMHEAP_VERBOSE(5,
                            "MKEY CALCULATED BY LOCAL SPML: pe: %d tr_id: %d %s",
                            pe,
                            i,
                            mca_spml_base_mkey2str(&mkeys[i]));
        }
        return OSHMEM_SUCCESS;
    }

    OPAL_THREAD_LOCK(&memheap_oob.lck);

    memheap_oob.mkeys = mkeys;
    memheap_oob.segno = seg;
    memheap_oob.mkeys_rcvd = 0;

    msg = OBJ_NEW(opal_buffer_t);
    if (!msg) {
        OPAL_THREAD_UNLOCK(&memheap_oob.lck);
        MEMHEAP_ERROR("failed to get msg buffer");
        return OSHMEM_ERROR;
    }

    cmd = MEMHEAP_RKEY_REQ;
    opal_dss.pack(msg, &cmd, 1, OPAL_UINT8);
    opal_dss.pack(msg, &seg, 1, OPAL_UINT32);

    rc = send_buffer(pe, msg);
    if (MPI_SUCCESS != rc) {
        OPAL_THREAD_UNLOCK(&memheap_oob.lck);
        MEMHEAP_ERROR("FAILED to send rml message %d", rc);
        return OSHMEM_ERROR;
    }

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
    opal_buffer_t *msg = NULL;
    void *send_buffer = NULL;
    char *rcv_buffer = NULL;
    int size;
    int *rcv_size = NULL;
    int *rcv_n_transports = NULL;
    int *rcv_offsets = NULL;
    int rc = OSHMEM_SUCCESS;
    size_t buffer_size;

    if (!mca_memheap_base_key_exchange) {
        oshmem_shmem_barrier();
        return;
    }

    nprocs = oshmem_num_procs();
    my_pe = oshmem_my_proc_id();

    /* buffer allocation for num_transports
     * message sizes and offsets */

    rcv_size = (int *)malloc(nprocs * sizeof(int));
    if (NULL == rcv_size) {
        MEMHEAP_ERROR("failed to get rcv_size buffer");
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto exit_fatal;
    }

    rcv_offsets = (int *)malloc(nprocs * sizeof(int));
    if (NULL == rcv_offsets) {
        MEMHEAP_ERROR("failed to get rcv_offsets buffer");
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto exit_fatal;
    }

    rcv_n_transports = (int *)malloc(nprocs * sizeof(int));
    if (NULL == rcv_offsets) {
        MEMHEAP_ERROR("failed to get rcv_offsets buffer");
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto exit_fatal;
    }

    /* serialize our own mkeys */
    msg = OBJ_NEW(opal_buffer_t);
    if (NULL == msg) {
        MEMHEAP_ERROR("failed to get msg buffer");
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto exit_fatal;
    }

    for (j = 0; j < memheap_map->n_segments; j++) {
        pack_local_mkeys(msg, 0, j);
    }

    /* we assume here that int32_t returned by opal_dss.unload
     * is equal to size of int we use for MPI_Allgather, MPI_Allgatherv */

    assert(sizeof(int32_t) == sizeof(int));

    /* Do allgather */
    opal_dss.unload(msg, &send_buffer, &size);
    MEMHEAP_VERBOSE(1, "local keys packed into %d bytes, %d segments", size, memheap_map->n_segments);

    /* we need to send num_transports and message sizes separately
     * since message sizes depend on types of btl used */

    rc = oshmem_shmem_allgather(&memheap_map->num_transports, rcv_n_transports, sizeof(int));
    if (MPI_SUCCESS != rc) {
        MEMHEAP_ERROR("allgather failed");
        goto exit_fatal;
    }

    rc = oshmem_shmem_allgather(&size, rcv_size, sizeof(int));
    if (MPI_SUCCESS != rc) {
        MEMHEAP_ERROR("allgather failed");
        goto exit_fatal;
    }

    /* calculating offsets (displacements) for allgatherv */

    rcv_offsets[0] = 0;
    for (i = 1; i < nprocs; i++) {
        rcv_offsets[i] = rcv_offsets[i - 1] + rcv_size[i - 1];
    }

    buffer_size = rcv_offsets[nprocs - 1] + rcv_size[nprocs - 1];

    rcv_buffer = malloc (buffer_size);
    if (NULL == rcv_buffer) {
        MEMHEAP_ERROR("failed to allocate recieve buffer");
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto exit_fatal;
    }

    rc = oshmem_shmem_allgatherv(send_buffer, rcv_buffer, size, rcv_size, rcv_offsets);
    if (MPI_SUCCESS != rc) {
        free (rcv_buffer);
        MEMHEAP_ERROR("allgatherv failed");
        goto exit_fatal;
    }

    opal_dss.load(msg, rcv_buffer, buffer_size);

    /* deserialize mkeys */
    OPAL_THREAD_LOCK(&memheap_oob.lck);
    for (i = 0; i < nprocs; i++) {
        if (i == my_pe) {
            continue;
        }

        msg->unpack_ptr = (void *)((intptr_t) msg->base_ptr + rcv_offsets[i]);

        for (j = 0; j < memheap_map->n_segments; j++) {
            map_segment_t *s;

            s = &memheap_map->mem_segs[j];
            if (NULL != s->mkeys_cache[i]) {
                MEMHEAP_VERBOSE(10, "PE%d: segment%d already exists, mkey will be replaced", i, j);
            } else {
                s->mkeys_cache[i] = (sshmem_mkey_t *) calloc(rcv_n_transports[i],
                        sizeof(sshmem_mkey_t));
                if (NULL == s->mkeys_cache[i]) {
                    MEMHEAP_ERROR("PE%d: segment%d: Failed to allocate mkeys cache entry", i, j);
                    oshmem_shmem_abort(-1);
                }
            }
            memheap_oob.mkeys = s->mkeys_cache[i];
            memheap_oob.segno = j;
            unpack_remote_mkeys(msg, i);
        }
    }

    OPAL_THREAD_UNLOCK(&memheap_oob.lck);

exit_fatal:
    if (rcv_size) {
        free(rcv_size);
    }
    if (rcv_offsets) {
        free(rcv_offsets);
    }
    if (rcv_n_transports) {
        free(rcv_n_transports);
    }
    if (send_buffer) {
        free(send_buffer);
    }
    if (msg) {
        OBJ_RELEASE(msg);
    }

    /* This function requires abort in any error case */
    if (OSHMEM_SUCCESS != rc) {
        oshmem_shmem_abort(rc);
    }
}

sshmem_mkey_t * mca_memheap_base_get_cached_mkey_slow(map_segment_t *s,
                                                      int pe,
                                                      void* va,
                                                      int btl_id,
                                                      void** rva)
{
    int rc;
    sshmem_mkey_t *mkey;

    if (!memheap_oob.is_inited) {
        return NULL;
    }

    s->mkeys_cache[pe] = (sshmem_mkey_t *) calloc(memheap_map->num_transports,
                                                    sizeof(sshmem_mkey_t));
    if (!s->mkeys_cache[pe])
        return NULL ;

    rc = memheap_oob_get_mkeys(pe,
                               s - memheap_map->mem_segs,
                               s->mkeys_cache[pe]);
    if (OSHMEM_SUCCESS != rc)
        return NULL ;

    mkey = &s->mkeys_cache[pe][btl_id];
    *rva = memheap_va2rva(va, s->super.va_base, mkey->va_base);

    MEMHEAP_VERBOSE_FASTPATH(5, "rkey: pe=%d va=%p -> (remote lookup) %lx %p", pe, (void *)va, mkey->u.key, (void *)*rva);
    return mkey;
}

sshmem_mkey_t *mca_memheap_base_get_mkey(void* va, int tr_id)
{
    map_segment_t *s;

    s = memheap_find_va(va);

    return ((s && MAP_SEGMENT_IS_VALID(s)) ? &s->mkeys[tr_id] : NULL );
}


int mca_memheap_base_is_symmetric_addr(const void* va)
{
    return (memheap_find_va((void *)va) ? 1 : 0);
}

int mca_memheap_base_detect_addr_type(void* va)
{
    int addr_type = ADDR_INVALID;
    map_segment_t *s;

    s = memheap_find_va(va);

    if (s) {
        if (s->type == MAP_SEGMENT_STATIC) {
            addr_type = ADDR_STATIC;
        } else if ((uintptr_t)va >= (uintptr_t) s->super.va_base
                   && (uintptr_t)va < (uintptr_t) ((uintptr_t)s->super.va_base + mca_memheap.memheap_size)) {
            addr_type = ADDR_USER;
        } else {
            assert( (uintptr_t)va >= (uintptr_t) ((uintptr_t)s->super.va_base + mca_memheap.memheap_size) && (uintptr_t)va < (uintptr_t)s->super.va_end);
            addr_type = ADDR_PRIVATE;
        }
    }

    return addr_type;
}

void mkey_segment_init(mkey_segment_t *seg, sshmem_mkey_t *mkey, uint32_t segno)
{
    map_segment_t *s;

    if (segno >= MCA_MEMHEAP_SEG_COUNT) {
        return;
    }

    s = memheap_find_seg(segno);
    assert(NULL != s);

    seg->super.va_base = s->super.va_base;
    seg->super.va_end  = s->super.va_end;
    seg->rva_base      = mkey->va_base;
}

