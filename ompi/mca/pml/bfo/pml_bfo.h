/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 *  @file
 */

#ifndef MCA_PML_BFO_H
#define MCA_PML_BFO_H

#include "ompi_config.h"
#include "opal/class/opal_free_list.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/datatype/ompi_datatype.h"
#include "pml_bfo_hdr.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/proc/proc.h"
#include "opal/mca/allocator/base/base.h"

BEGIN_C_DECLS

/**
 * BFO PML module
 */

struct mca_pml_bfo_t {
    mca_pml_base_module_t super;

    int priority;
    int free_list_num;      /* initial size of free list */
    int free_list_max;      /* maximum size of free list */
    int free_list_inc;      /* number of elements to grow free list */
    unsigned int send_pipeline_depth;
    unsigned int recv_pipeline_depth;
    unsigned int rdma_put_retries_limit;
    int max_rdma_per_request;
    int max_send_per_range;
    bool leave_pinned;
    int leave_pinned_pipeline;

    /* lock queue access */
    opal_mutex_t lock;

    /* free lists */
    opal_free_list_t rdma_frags;
    opal_free_list_t recv_frags;
    opal_free_list_t pending_pckts;
    opal_free_list_t buffers;
    opal_free_list_t send_ranges;

    /* list of pending operations */
    opal_list_t pckt_pending;
    opal_list_t send_pending;
    opal_list_t recv_pending;
    opal_list_t rdma_pending;
    /* List of pending fragments without a matching communicator */
    opal_list_t non_existing_communicator_pending;
    bool enabled;
    char* allocator_name;
    mca_allocator_base_module_t* allocator;
    unsigned int unexpected_limit;
};
typedef struct mca_pml_bfo_t mca_pml_bfo_t;

extern mca_pml_bfo_t mca_pml_bfo;
extern int mca_pml_bfo_output;

/*
 * PML interface functions.
 */

extern int mca_pml_bfo_add_comm(
    struct ompi_communicator_t* comm
);

extern int mca_pml_bfo_del_comm(
    struct ompi_communicator_t* comm
);

extern int mca_pml_bfo_add_procs(
    struct ompi_proc_t **procs,
    size_t nprocs
);

extern int mca_pml_bfo_del_procs(
    struct ompi_proc_t **procs,
    size_t nprocs
);

extern int mca_pml_bfo_enable( bool enable );

extern int mca_pml_bfo_progress(void);

extern int mca_pml_bfo_iprobe( int dst,
                               int tag,
                               struct ompi_communicator_t* comm,
                               int *matched,
                               ompi_status_public_t* status );

extern int mca_pml_bfo_probe( int dst,
                              int tag,
                              struct ompi_communicator_t* comm,
                              ompi_status_public_t* status );

extern int mca_pml_bfo_improbe( int dst,
                               int tag,
                               struct ompi_communicator_t* comm,
                               int *matched,
                               struct ompi_message_t **message,
                               ompi_status_public_t* status );

extern int mca_pml_bfo_mprobe( int dst,
                              int tag,
                              struct ompi_communicator_t* comm,
                              struct ompi_message_t **message,
                              ompi_status_public_t* status );

extern int mca_pml_bfo_isend_init( void *buf,
                                   size_t count,
                                   ompi_datatype_t *datatype,
                                   int dst,
                                   int tag,
                                   mca_pml_base_send_mode_t mode,
                                   struct ompi_communicator_t* comm,
                                   struct ompi_request_t **request );

extern int mca_pml_bfo_isend( void *buf,
                              size_t count,
                              ompi_datatype_t *datatype,
                              int dst,
                              int tag,
                              mca_pml_base_send_mode_t mode,
                              struct ompi_communicator_t* comm,
                              struct ompi_request_t **request );

extern int mca_pml_bfo_send( void *buf,
                             size_t count,
                             ompi_datatype_t *datatype,
                             int dst,
                             int tag,
                             mca_pml_base_send_mode_t mode,
                             struct ompi_communicator_t* comm );

extern int mca_pml_bfo_irecv_init( void *buf,
                                   size_t count,
                                   ompi_datatype_t *datatype,
                                   int src,
                                   int tag,
                                   struct ompi_communicator_t* comm,
                                   struct ompi_request_t **request );

extern int mca_pml_bfo_irecv( void *buf,
                              size_t count,
                              ompi_datatype_t *datatype,
                              int src,
                              int tag,
                              struct ompi_communicator_t* comm,
                              struct ompi_request_t **request );

extern int mca_pml_bfo_recv( void *buf,
                             size_t count,
                             ompi_datatype_t *datatype,
                             int src,
                             int tag,
                             struct ompi_communicator_t* comm,
                             ompi_status_public_t* status );

extern int mca_pml_bfo_imrecv( void *buf,
                               size_t count,
                               ompi_datatype_t *datatype,
                               struct ompi_message_t **message,
                               struct ompi_request_t **request );

extern int mca_pml_bfo_mrecv( void *buf,
                              size_t count,
                              ompi_datatype_t *datatype,
                              struct ompi_message_t **message,
                              ompi_status_public_t* status );

extern int mca_pml_bfo_dump( struct ompi_communicator_t* comm,
                             int verbose );

extern int mca_pml_bfo_start( size_t count,
                              ompi_request_t** requests );

extern int mca_pml_bfo_ft_event( int state );

END_C_DECLS

struct mca_pml_bfo_pckt_pending_t {
    opal_free_list_item_t super;
    ompi_proc_t* proc;
    mca_pml_bfo_hdr_t hdr;
    struct mca_bml_base_btl_t *bml_btl;
    uint8_t order;
};
typedef struct mca_pml_bfo_pckt_pending_t mca_pml_bfo_pckt_pending_t;
OBJ_CLASS_DECLARATION(mca_pml_bfo_pckt_pending_t);

#define MCA_PML_BFO_PCKT_PENDING_ALLOC(pckt)                    \
do {                                                            \
    opal_free_list_item_t* item;                                \
    OPAL_FREE_LIST_WAIT(&mca_pml_bfo.pending_pckts, item);      \
    pckt = (mca_pml_bfo_pckt_pending_t*)item;                   \
} while (0)

#define MCA_PML_BFO_PCKT_PENDING_RETURN(pckt)                   \
do {                                                            \
    /* return packet */                                         \
    OPAL_FREE_LIST_RETURN(&mca_pml_bfo.pending_pckts,           \
        (opal_free_list_item_t*)pckt);                          \
} while(0)

#define MCA_PML_BFO_ADD_FIN_TO_PENDING(P, D, B, O, S)               \
    do {                                                            \
        mca_pml_bfo_pckt_pending_t *_pckt;                          \
                                                                    \
        MCA_PML_BFO_PCKT_PENDING_ALLOC(_pckt);                      \
        _pckt->hdr.hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_FIN;  \
        _pckt->hdr.hdr_fin.hdr_des = (D);                           \
        _pckt->hdr.hdr_fin.hdr_fail = (S);                          \
        _pckt->proc = (P);                                          \
        _pckt->bml_btl = (B);                                       \
        _pckt->order = (O);                                         \
        OPAL_THREAD_LOCK(&mca_pml_bfo.lock);                        \
        opal_list_append(&mca_pml_bfo.pckt_pending,                 \
                (opal_list_item_t*)_pckt);                          \
        OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);                      \
    } while(0)


int mca_pml_bfo_send_fin(ompi_proc_t* proc, mca_bml_base_btl_t* bml_btl,
#if PML_BFO
        opal_ptr_t hdr_des, uint8_t order, uint32_t status,
        uint16_t seq, uint8_t reqseq, uint16_t ctx, uint32_t src);
#else /* PML_BFO */
        opal_ptr_t hdr_des, uint8_t order, uint32_t status);
#endif /* PML_BFO */

/* This function tries to resend FIN/ACK packets from pckt_pending queue.
 * Packets are added to the queue when sending of FIN or ACK is failed due to
 * resource unavailability. bml_btl passed to the function doesn't represents
 * packet's destination, it represents BTL on which resource was freed, so only
 * this BTL should be considered for resending packets */
void mca_pml_bfo_process_pending_packets(mca_bml_base_btl_t* bml_btl);

/* This function retries failed PUT/GET operations on frag. When RDMA operation
 * cannot be accomplished for some reason, frag is put on the rdma_pending list.
 * Later the operation is retried. The destination of RDMA operation is stored
 * inside the frag structure */
void mca_pml_bfo_process_pending_rdma(void);

#define MCA_PML_BFO_PROGRESS_PENDING(bml_btl)                   \
    do {                                                        \
        if(opal_list_get_size(&mca_pml_bfo.pckt_pending))       \
            mca_pml_bfo_process_pending_packets(bml_btl);       \
        if(opal_list_get_size(&mca_pml_bfo.recv_pending))       \
            mca_pml_bfo_recv_request_process_pending();         \
        if(opal_list_get_size(&mca_pml_bfo.send_pending))       \
            mca_pml_bfo_send_request_process_pending(bml_btl);  \
        if(opal_list_get_size(&mca_pml_bfo.rdma_pending))       \
            mca_pml_bfo_process_pending_rdma();                 \
    } while (0)

/*
 * Compute the total number of bytes on supplied descriptor
 */
static inline int mca_pml_bfo_compute_segment_length (size_t seg_size, void *segments, size_t count,
                                                      size_t hdrlen) {
    size_t i, length;

    for (i = 0, length = -hdrlen ; i < count ; ++i) {
        mca_btl_base_segment_t *segment =
            (mca_btl_base_segment_t *)((char *) segments + i * seg_size);

        length += segment->seg_len;
    }

    return length;
}

static inline int mca_pml_bfo_compute_segment_length_base (mca_btl_base_segment_t *segments,
                                                           size_t count, size_t hdrlen) {
    size_t i, length;

    for (i = 0, length = -hdrlen ; i < count ; ++i) {
        length += segments[i].seg_len;
    }

    return length;
}

/* represent BTL chosen for sending request */
struct mca_pml_bfo_com_btl_t {
    mca_bml_base_btl_t *bml_btl;
    struct mca_mpool_base_registration_t* btl_reg;
    size_t length;
};
typedef struct mca_pml_bfo_com_btl_t mca_pml_bfo_com_btl_t;

int mca_pml_bfo_com_btl_comp(const void *v1, const void *v2);

/* Calculate what percentage of a message to send through each BTL according to
 * relative weight */
static inline void
mca_pml_bfo_calc_weighted_length( mca_pml_bfo_com_btl_t *btls, int num_btls, size_t size,
                                  double weight_total )
{
    int i;
    size_t length_left;

    /* shortcut for common case for only one BTL */
    if( OPAL_LIKELY(1 == num_btls) ) {
        btls[0].length = size;
        return;
    }

    /* sort BTLs according of their weights so BTLs with smaller weight will
     * not hijack all of the traffic */
    qsort( btls, num_btls, sizeof(mca_pml_bfo_com_btl_t),
           mca_pml_bfo_com_btl_comp );

    for(length_left = size, i = 0; i < num_btls; i++) {
        mca_bml_base_btl_t* bml_btl = btls[i].bml_btl;
        size_t length = 0;
        if( OPAL_UNLIKELY(0 != length_left) ) {
            length = (length_left > bml_btl->btl->btl_eager_limit)?
                ((size_t)(size * (bml_btl->btl_weight / weight_total))) :
                length_left;

            if(length > length_left)
                length = length_left;
            length_left -= length;
        }
        btls[i].length = length;
    }

    /* account for rounding errors */
    btls[0].length += length_left;
}

#endif
