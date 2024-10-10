/*
 * Copyright (c) 2024      Amazon.com, Inc. or its affiliates. All Rights Reserved.
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * This file contains the hierarchical implementations of alltoallv.
 *
 * mca_coll_han_alltoallv_using_smsc:
 * This algorithm relies on SMSC and specifically XPMEM because of
 * the need to direct-map the memory.
 *
 * Each rank on one host is assigned a single
 * partner on a remote host and vice versa.  Then the rank collects all
 * the data its partner will need to receive from its host, and sends it
 * in one large send, and likewise receives its data in one large recv,
 * then cycles to the next host.
 */

#include "coll_han.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_han_trigger.h"
#include "opal/mca/smsc/smsc.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "ompi/mca/osc/base/base.h"

/* Who is the given ranks partner during the exchange?
   This function will require rounds comm_size-many rounds, and your partner
   will select you in the same round which you select that partner. */
static inline int ring_partner_no_skip(int rank, int round, int comm_size) {
    /* make sure ring_partner is positive: make argument to modulo > 0 with +comm_size.*/
    return (comm_size + round - rank) % comm_size;
}

/* Who is the given ranks partner during the exchange?
   This function will require rounds comm_size-many rounds, and does
   self-exchange last. */
static inline int ring_partner(int rank, int round, int comm_size) {
    round = round % comm_size;
    if (round == comm_size - 1) {
        /* last round: self-exchange */
        return rank;
    }
    int self_round = (2*rank) % comm_size;
    if ( round < self_round )
        return ring_partner_no_skip(rank, round, comm_size);
    else
        return ring_partner_no_skip(rank, round+1, comm_size);
}

struct peer_data {
    const void *sbuf;           /* mmapped: ctx in map_ctx[1] */
    struct peer_counts *counts; /* mmapped: ctx in map_ctx[0] */
    opal_datatype_t *sendtype;  /* local. deserialized from tailer of map_ctx[0] */
    void *map_ctx[2];
};

struct peer_counts {
    size_t scount;
    size_t sdispl;
    size_t rcount;
    size_t rdispl;
};

struct gathered_data {
    size_t stype_serialized_length;
    ssize_t sbuf_span;
    ssize_t sbuf_gap;
    void *sbuf;
    void *serialization_buffer;
};

/** calculate the minumum true lower bound and maximum true upper bound for all
 * peers accessing a buffer.
 * Outputs: min_lb and max_lb contain signed offsets in bytes from buffer pointer.
 */
static void coll_han_alltoallv_calc_all_span(
                    int nranks,
                    opal_datatype_t *dtype,
                    ompi_count_array_t counts,
                    ompi_disp_array_t displs,
                    ssize_t *gap,
                    ssize_t *span ) {
    ptrdiff_t min_lb = PTRDIFF_MAX;
    ptrdiff_t max_ub = PTRDIFF_MIN;
    *gap = 0;
    *span = 0;

    ptrdiff_t dummy_lb;
    ptrdiff_t dtype_extent;
    opal_datatype_get_extent(dtype, &dummy_lb, &dtype_extent);

    for (int jrankw=0; jrankw<nranks; jrankw++) {
        ptrdiff_t displ;
        ptrdiff_t count;
        ssize_t lb, ub;

        count = ompi_count_array_get(counts,jrankw);
        displ = ompi_disp_array_get(displs,jrankw);
        if (count > 0) {
            ptrdiff_t one_gap;
            ptrdiff_t one_span = opal_datatype_span(dtype, count, &one_gap);
            lb = dtype_extent*displ + one_gap;
            ub = lb + one_span;
            min_lb = MIN( min_lb, lb );
            max_ub = MAX( max_ub, ub );
        }
    }
    if (max_ub > min_lb) {
        *gap = min_lb;
        *span = max_ub - min_lb;
    }
}

/* Serialize the datatype into the buffer and return buffer length.
   If buf is NULL, just return the length of the required buffer. */
static size_t ddt_pack_datatype(opal_datatype_t* type, uint8_t* buf)
{
    size_t length = sizeof(opal_datatype_t) - offsetof(opal_datatype_t, flags);
    size_t n_copy = length;
    bool count_only = buf == NULL;
    if (!count_only) {
        memcpy(buf, &type->flags, length);
    }
    buf += length;

    if( type->opt_desc.used ) {
        /* we are losing the non optimized description of the datatype,
         * but it is only useful for dumping the datatype description.
         */
        n_copy = (1 + type->opt_desc.used) * sizeof(dt_elem_desc_t);
        if (!count_only) {
            memcpy(buf, type->opt_desc.desc, n_copy);
            buf += n_copy;
        }
        length += n_copy;
    } else {
        n_copy = (1 + type->desc.used) * sizeof(dt_elem_desc_t);
        if (!count_only) {
            memcpy(buf, type->desc.desc, n_copy);
            buf += n_copy;
        }
        length += n_copy;
    }
    /* The following is not necessary and in fact, non-function in homogenous configurations.*/
    // if (type->ptypes) {
    //     n_copy = OPAL_DATATYPE_MAX_SUPPORTED * sizeof(size_t);
    // } else {
    //     n_copy = 0;
    // }
    // if (!count_only) {
    //     memcpy(buf, type->ptypes, n_copy);
    // }
    // length += n_copy;
    return length;
}

static size_t ddt_unpack_datatype(opal_datatype_t* type, uint8_t* buf)
{
    OBJ_CONSTRUCT(type, opal_datatype_t);
    size_t length = sizeof(opal_datatype_t) - offsetof(opal_datatype_t, flags);
    memcpy(&type->flags, buf, length);
    buf += length;
    size_t nbytes_copy = (1+type->opt_desc.used) * sizeof(dt_elem_desc_t);
    type->opt_desc.desc = (dt_elem_desc_t*)malloc(nbytes_copy);
    memcpy(type->opt_desc.desc, buf, nbytes_copy);
    length += nbytes_copy;
    type->desc = type->opt_desc;
    buf += nbytes_copy;
    type->ptypes = NULL;
    return length;
}

/* Simple implementation: send all buffers without packing, but still keeping a
   limited number in flight.

      Note: CMA on XPMEM-mapped buffers does not work.  If the low-level network
      provider attempts to use CMA to implement send/recv, then errors will
      occur!
*/
static inline int alltoallv_sendrecv_w_direct_for_debugging(
            void **send_from_addrs,
            size_t *send_counts,
            opal_datatype_t **send_types,
            int jrank_sendto,
            int ntypes_send,
            void **recv_to_addrs,
            size_t *recv_counts,
            opal_datatype_t **recv_types,
            int jrank_recvfrom,
            int ntypes_recv,
            struct ompi_communicator_t *comm) {


    const int MAX_BUF_COUNT=8;
    int nreqs = MAX_BUF_COUNT;
    ompi_request_t *requests[MAX_BUF_COUNT];
    const char* problem_hint;

    int jfirst_sendreq = nreqs/2 + nreqs%2;

    int jreq;
    ompi_datatype_t *ompi_dtype_from_opal;
    int rc = 0;
    int jloop;

    int jrecvs_posted = 0;
    int jrecvs_completed = 0;
    int jsends_posted = 0;
    int jsends_completed = 0;

    for (jloop = 0; ; jloop++) {
        int have_completion;

        if (jsends_completed == ntypes_send && jrecvs_completed == ntypes_recv )
            break;

        if (jloop < nreqs){
            jreq = jloop;
            have_completion = 0;
            requests[jreq] = MPI_REQUEST_NULL;
        } else {
            have_completion = 1;
            jreq = jloop%nreqs;
            if (requests[jreq] == MPI_REQUEST_NULL) {
                continue;
            }
            problem_hint = "waiting for request completion";
            rc = ompi_request_wait(&requests[jreq], MPI_STATUS_IGNORE);
            if (rc) break;
            requests[jreq] = MPI_REQUEST_NULL;
        }
        int ii_send_req = jreq >= jfirst_sendreq;
        if (have_completion) {
            if (ii_send_req)
                jsends_completed++;
            else
                jrecvs_completed++;
        }

        if (ii_send_req && jsends_posted < ntypes_send) {
            problem_hint = "creating or committing temporary datatype";
            rc = ompi_datatype_create_contiguous( 1, (ompi_datatype_t*)send_types[jsends_posted], &ompi_dtype_from_opal );
            if (rc) break;
            rc = ompi_datatype_commit(&ompi_dtype_from_opal);
            if (rc) break;
            problem_hint = "posting isend";
            rc = MCA_PML_CALL(isend
                (send_from_addrs[jsends_posted], (int)send_counts[jsends_posted], ompi_dtype_from_opal, jrank_sendto,
                MCA_COLL_BASE_TAG_ALLTOALLV, MCA_PML_BASE_SEND_STANDARD,
                comm, &requests[jreq]));
            if (rc) break;
            problem_hint = "destroying temporary datatype";
            rc = ompi_datatype_destroy( &ompi_dtype_from_opal );
            if (rc) break;

            jsends_posted++;
        }
        if (!ii_send_req && jrecvs_posted < ntypes_recv ) {
            problem_hint = "creating or committing temporary datatype";
            rc = ompi_datatype_create_contiguous( 1, (ompi_datatype_t*)recv_types[jrecvs_posted], &ompi_dtype_from_opal );
            if (rc) break;
            rc = ompi_datatype_commit(&ompi_dtype_from_opal);
            if (rc) break;
            problem_hint = "posting irecv";
            rc = MCA_PML_CALL(irecv
                (recv_to_addrs[jrecvs_posted], (int)recv_counts[jrecvs_posted], ompi_dtype_from_opal, jrank_recvfrom,
                MCA_COLL_BASE_TAG_ALLTOALLV,
                comm, &requests[jreq]));
            if (rc) break;
            problem_hint = "destroying temporary datatype";
            rc = ompi_datatype_destroy( &ompi_dtype_from_opal );
            if (rc) break;
            jrecvs_posted++;
        }
    }
    if (rc) {
        opal_output_verbose(1, mca_coll_han_component.han_output,
            "Failed in alltoallv_sendrecv_w_direct_for_debugging while %s: jloop=%d, rc=%d\n",
            problem_hint, jloop,rc);
    }
    return rc;
}

static int alltoallv_sendrecv_w(
            void **send_from_addrs,
            size_t *send_counts,
            opal_datatype_t **send_types,
            int jrank_sendto,
            int ntypes_send,
            void **recv_to_addrs,
            size_t *recv_counts,
            opal_datatype_t **recv_types,
            int jrank_recvfrom,
            int ntypes_recv,
            struct ompi_communicator_t *comm) {

    uint32_t iov_count = 1;
    struct iovec iov;



    const int MAX_BUF_COUNT=8;
    ompi_request_t *requests[MAX_BUF_COUNT];
    opal_free_list_item_t *buf_items[MAX_BUF_COUNT];

    size_t buf_len = mca_coll_han_component.han_packbuf_bytes;
    int nbufs = MAX_BUF_COUNT;
    for (int jbuf=0; jbuf<nbufs; jbuf++) {
        buf_items[jbuf] = opal_free_list_get(&mca_coll_han_component.pack_buffers);
        if (buf_items[jbuf] == NULL) {
            nbufs = jbuf;
            opal_output_verbose(30, mca_coll_han_component.han_output,
                "alltoallv_sendrecv_w: Number of buffers reduced to %d instead of %d.  "
                "Check mca parameter coll_han_packbuf_max_count (currently %ld).\n",
                nbufs, MAX_BUF_COUNT, mca_coll_han_component.han_packbuf_max_count);
            break;
        }
    }

    /* although we could feasibly disqualify the higher-level alltoallv
    algorithm for these conditions, they represent invalid user parameters, so
    let's fail and tell the user why.  */
    if (nbufs < 2) {
        opal_output_verbose(1, mca_coll_han_component.han_output,
                    "ERROR: Need at least 2 buffers from HAN pack buffers!  "
                    "Check mca parameter coll_han_packbuf_max_count (currently %ld)\n",
                    mca_coll_han_component.han_packbuf_max_count);
        return MPI_ERR_NO_MEM;
    }
    if (buf_len < 16) {
        opal_output_verbose(1, mca_coll_han_component.han_output,
                    "ERROR: Need a buffer that can hold at least 16 bytes!  "
                    "Check mca parameter coll_han_packbuf_bytes (currently %ld)\n",
                    mca_coll_han_component.han_packbuf_bytes);
        return MPI_ERR_NO_MEM;
    }

    size_t nreqs = nbufs;
    int jreq;
    int jfirst_sendreq = nbufs/2 + nbufs%2;

    size_t jloop = 0;
    size_t send_pack_bytes_remaining = 0;
    size_t recv_convertor_bytes_remaining = 0;
    int have_completion = 0;
    int jtype_send;
    int jtype_recv;
    size_t nbytes_pack;

    const int buf_header_len = sizeof(int);
    int nsend_req_pending = 0;
    opal_convertor_t send_convertor;
    opal_convertor_t recv_convertor;
    OBJ_CONSTRUCT( &send_convertor, opal_convertor_t );
    OBJ_CONSTRUCT( &recv_convertor, opal_convertor_t );

    /* In the main loop, we will be posting recvs ahead of our recv convertors.
       This gives us two options when it comes to the end of the transfer:
        1) We could always pre-post data, then once our last convertor finds
           there is no more data coming, we could cancel the recvs we posted
        2) We could make a first-pass and count total bytes to recv, and be
           careful not to post more than we know is coming.

        After realizing that the convertor may decide to not fully pack the
        buffer, path 1 was the only clear way forward (since it was no longer
        clear how many bytes would be arriving.)

        Note this is not necessary for Sends, because our convertor leads
        posting the sends rather than trails it.
    */
   int ii_more_recvs_to_post = 0;
   for (jtype_recv=0; jtype_recv<ntypes_recv; jtype_recv++) {
        opal_convertor_copy_and_prepare_for_recv(ompi_mpi_local_convertor,
            recv_types[jtype_recv],
            recv_counts[jtype_recv],
            recv_to_addrs[jtype_recv],
            0,
            &recv_convertor);
        opal_convertor_get_packed_size( &recv_convertor, &recv_convertor_bytes_remaining );
        if (recv_convertor_bytes_remaining) {
            ii_more_recvs_to_post = 1;
            break;
        }
   }

   recv_convertor_bytes_remaining = 0;
   send_pack_bytes_remaining = 0;

    /*
        Main loop.

        First make a pass through all the request buffers, posting sends and
        recvs.

        After the first pass, we reach steady state: we call MPI_Waitany for
        either a send or recv to complete, and then re-use that buffer and
        that request to re-post the next send/recv.

        We may exit the loop only after both sending and recving are done, which
        means we must handle cases were there is no more send work or no more
        recv work to do.
    */
    jtype_send = -1;
    jtype_recv = -1;
    int sequential_continues = 0;
    for (jloop=0; ; jloop++) {
        int ii_more_sends_to_post = jtype_send < ntypes_send || send_pack_bytes_remaining > 0;
        int ii_more_sends_to_complete = nsend_req_pending > 0;

        if ( !( ii_more_sends_to_post || ii_more_sends_to_complete ||
                ii_more_recvs_to_post ) ) {
            /* exit.  All done! */
            break;
        }

        if (jloop >= nreqs) {
            /* Common Case: */
            /* wait for the send or recv to complete */
            jreq = jloop%nreqs;
            if (requests[jreq] == MPI_REQUEST_NULL) {
                if (++sequential_continues > nbufs) {
                    opal_output_verbose(1, mca_coll_han_component.han_output,
                        "ERROR: no active requests to wait on! Loop=%ld: %d %d %d\n",
                        jloop,
                        ii_more_sends_to_post, ii_more_sends_to_complete,
                        ii_more_recvs_to_post );
                    return MPI_ERR_INTERN;
                }
                continue;
            }
            sequential_continues = 0;
            ompi_request_wait( &requests[jreq], MPI_STATUS_IGNORE );
            have_completion = 1;
            requests[jreq] = MPI_REQUEST_NULL;
        } else {
            /* priming the loop: post sends or recvs while have_completion=0.

            note: it is possible we have more buffers than data, so logic below
            makes sure we don't infinite loop or send empty messages */
            jreq = jloop;
            have_completion = 0;
        }
        int ii_send_req = jreq >= jfirst_sendreq;
        char *req_buf = buf_items[jreq]->ptr;

        if (ii_send_req) {

            if (have_completion) {
                /* send request has completed */
                nsend_req_pending--;
            }

            int buf_remain = buf_len - buf_header_len;
            while (buf_remain > 0 && (jtype_send < ntypes_send || send_pack_bytes_remaining > 0) ) {
                if (jtype_send < ntypes_send && send_pack_bytes_remaining==0) {
                    /* switch to next datatype and prepare convertor: */
                    jtype_send++;
                    if (jtype_send < ntypes_send) {
                        opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
                            send_types[jtype_send],
                            send_counts[jtype_send],
                            send_from_addrs[jtype_send],
                            0,
                            &send_convertor);
                        opal_convertor_get_packed_size( &send_convertor, &send_pack_bytes_remaining );
                    }
                }

                /* pack more data */
                if (send_pack_bytes_remaining > 0 && buf_remain > 0) {
                    /* pack into the buffer described by the iov */
                    /* iovec: set the destination of the copy */
                    size_t start_offset = buf_len - buf_remain;
                    iov.iov_base = (char*)(req_buf) + start_offset;
                    iov.iov_len = buf_remain;
                    iov_count = 1;
                    opal_convertor_pack(&send_convertor, &iov, &iov_count, &nbytes_pack);
                    if (!nbytes_pack) {
                        /* can happen when buffer is only a few bytes from full,
                        and convertor doesn't want to split a primitive data
                        type. */
                        break;
                    }
                    buf_remain -= nbytes_pack;
                    send_pack_bytes_remaining -= nbytes_pack;
                }
            }
            /* start the buffer with an integer describing how many bytes we
            packed, *including* the integer's size */
            int pack_bytes_len = buf_len - buf_remain;

            /* post send */
            if (pack_bytes_len > buf_header_len) {
                *((int*)req_buf) = pack_bytes_len;
                MCA_PML_CALL(isend
                    (req_buf, pack_bytes_len, MPI_PACKED, jrank_sendto,
                    MCA_COLL_BASE_TAG_ALLTOALLV, MCA_PML_BASE_SEND_STANDARD,
                    comm, &requests[jreq]));
                nsend_req_pending++;
            } else {
                requests[jreq] = MPI_REQUEST_NULL;
            }

        } else { /* recv request */
            if (have_completion) {
                /* unpack data */
                ssize_t buf_remain = *((int*)req_buf) - buf_header_len;
                size_t buf_converted = buf_header_len;
                while (true) {
                    if (jtype_recv < ntypes_recv && recv_convertor_bytes_remaining==0) {
                        /* switch to next datatype and prepare convertor: */
                        jtype_recv++;
                        if (jtype_recv < ntypes_recv) {
                            opal_convertor_copy_and_prepare_for_recv(ompi_mpi_local_convertor,
                                recv_types[jtype_recv],
                                recv_counts[jtype_recv],
                                recv_to_addrs[jtype_recv],
                                0,
                                &recv_convertor);
                            opal_convertor_get_packed_size( &recv_convertor, &recv_convertor_bytes_remaining );
                            if (recv_convertor_bytes_remaining == 0) {
                                continue;
                            }
                        }
                    }
                    if (jtype_recv == ntypes_recv && recv_convertor_bytes_remaining==0 ) {
                        /* _all_ recving work is done! */
                        ii_more_recvs_to_post = false;
                        buf_remain = 0;
                        for (int kreq=0; kreq<jfirst_sendreq; kreq++) {
                            if (requests[kreq] != MPI_REQUEST_NULL) {
                                ompi_request_cancel(requests[kreq]);
                                requests[kreq] = MPI_REQUEST_NULL;
                            }
                        }
                    }
                    if (buf_remain == 0) { break; }

                    /* unpack more data */
                    if (recv_convertor_bytes_remaining > 0) {
                        /* unpack from the buffer described by the iov */
                        iov.iov_base = (char*)(req_buf) + buf_converted;
                        iov.iov_len = buf_remain;
                        iov_count = 1;
                        opal_convertor_unpack(&recv_convertor, &iov, &iov_count, &nbytes_pack);

                        buf_remain -= nbytes_pack;
                        buf_converted += nbytes_pack;
                        recv_convertor_bytes_remaining -= nbytes_pack;
                    }
                }
            }

            if (ii_more_recvs_to_post) {
                /* post a new recv */
                MCA_PML_CALL(irecv
                    (req_buf, buf_len, MPI_PACKED, jrank_recvfrom,
                    MCA_COLL_BASE_TAG_ALLTOALLV,
                    comm, &requests[jreq]));
            } else {
                requests[jreq] = MPI_REQUEST_NULL;
            }
        }
    }

    OBJ_DESTRUCT(&send_convertor);
    OBJ_DESTRUCT(&recv_convertor);

    for (int jbuf=0; jbuf<nbufs; jbuf++) {
        opal_free_list_return(&mca_coll_han_component.pack_buffers, buf_items[jbuf]);
    }
    return 0;
}

static int decide_to_use_smsc_alg(
    int *use_smsc,
    const void *sbuf,
    ompi_count_array_t scounts,
    ompi_disp_array_t sdispls,
    struct ompi_datatype_t *sdtype,
    void* rbuf,
    ompi_count_array_t rcounts,
    ompi_disp_array_t rdispls,
    struct ompi_datatype_t *rdtype,
    struct ompi_communicator_t *comm)
{
    opal_convertor_t convertor;
    long long reduce_buf_input[3];
    long long reduce_buf_output[3];
    int bufs_on_device = 0;
    size_t avg_send_bytes = 0;
    size_t packed_size_bytes = 0;
    int need_bufs = 0;
    int rc;
    int comm_size = ompi_comm_size(comm);
    int comm_rank = ompi_comm_rank(comm);

    /*
    Perform an allreduce on all ranks to decide if this algorithm is worth
    using. There are four important things:

     1. sbuf == MPI_IN_PLACE.  This algorithm does not currently support the
        in-place operation.  (Future developers may note that the inter-node
        communications are ordered such that in-place could be supported, but
        additional ordering and/or buffering would be required to ensure local
        ranks do not overwrite buffers before sends are posted. However, for now
        we just bail out.)
     2. Device buffers.  XPMEM doesn't support GPU buffers, so we cannot proceed
        with this algorithm.
     3. Send size per rank.  This algorithm can pack small messages together,
        but this typically isn't helpful for large messages, and XPMEM-mapped
        memory cannot be registered with ibv_reg_mr.
     4. Contiguous buffers.  The exception to #2 above is if we can't post our
        sends/recvs in one large block to the NIC.  For these non-contiguous
        datatypes, our packing algorithm is better because (a) we re-use our
        buffers from a free-list which can remain registered to the NIC, and (b)
        we pipeline multiple sends at once (c) we can pack more data into each
        send than the network layer because we have mmap'ed all local ranks and
        understand their datatypes.

    The allreduce adds about 50 usec on 256 ranks across 4 nodes, but for large
    message sizes this is insignificant, and for small message sizes it is <1/4
    our execution time, which is <1/10 of the "basic" algorithm.
    */

   if (sbuf == MPI_IN_PLACE) {
        if (comm_rank == 0) {
            opal_output_verbose(10, mca_coll_han_component.han_output, "alltoallv: decide_to_use_smsc_alg: "
                "MPI_IN_PLACE operation prevents smsc_alg from being used.  "
                "Continue with SMSC? ==> no.\n");
        }
        *use_smsc = 0;
   }

    /* some magic in the count: if we pick 1, need_buffers() might not be
    accurate.  We could be precisely correct and compute need_buffers for every
    rank's count, but that could be a lot of iteration.  Just use 2 and assume
    some rank will be sending more than 1 item, and adjust packed size
    accordingly. */
    const int count_for_convertor = 2;

    OBJ_CONSTRUCT( &convertor, opal_convertor_t );
    rc = opal_convertor_copy_and_prepare_for_recv(ompi_mpi_local_convertor,
        &rdtype->super, count_for_convertor, rbuf, 0, &convertor);
    if (rc) goto cleanup1;
    bufs_on_device = opal_convertor_on_device(&convertor);
    need_bufs = opal_convertor_need_buffers(&convertor);
    rc = opal_convertor_cleanup(&convertor);
    if (rc) goto cleanup1;
    rc = opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
        &sdtype->super, count_for_convertor, sbuf, 0, &convertor);
    if (rc) goto cleanup1;
    bufs_on_device |= opal_convertor_on_device(&convertor);
    opal_convertor_get_packed_size(&convertor, &packed_size_bytes);
    for (int jrank=0; jrank<comm_size; jrank++) {
        avg_send_bytes += packed_size_bytes/count_for_convertor * ompi_count_array_get(scounts,jrank);
    }
    need_bufs |= opal_convertor_need_buffers(&convertor);

    rc = opal_convertor_cleanup(&convertor);
cleanup1:
    OBJ_DESTRUCT( &convertor );
    if (rc != OMPI_SUCCESS) { return rc;}

    avg_send_bytes = avg_send_bytes / comm_size;
    reduce_buf_input[0] = !!(bufs_on_device);
    reduce_buf_input[1] = avg_send_bytes;
    reduce_buf_input[2] = !!(need_bufs);
    rc =comm->c_coll->coll_allreduce(
        &reduce_buf_input, &reduce_buf_output, 3, MPI_LONG_LONG, &ompi_mpi_op_sum.op,
        comm, comm->c_coll->coll_allreduce_module );
    if (rc != OMPI_SUCCESS) {return rc;}

    if (reduce_buf_output[0] > 0) {
        *use_smsc = 0;
        /* can't proceed: at least one rank using GPU buffers. */
    } else if (reduce_buf_output[2] >= comm_size * mca_coll_han_component.han_alltoallv_smsc_noncontig_activation_limit) {
        /* always proceed: enough ranks have non-contiguous data that the pack-and-send method is fastest. */
        *use_smsc = 1;
    } else if (reduce_buf_output[1] >= comm_size * mca_coll_han_component.han_alltoallv_smsc_avg_send_limit) {
        *use_smsc = 0;
        /* don't proceed: messages are large and contiguous.  It is faster to fall back to basic alg. */
    } else {
        *use_smsc = 1;
    }

    if (comm_rank == 0) {
        opal_output_verbose(10, mca_coll_han_component.han_output, "alltoallv: decide_to_use_smsc_alg: "
            "Ranks with GPU buffers: %lld (limit is 0).  "
            "Average send_size: %.1f bytes (limit is %ld).  "
            "Fraction with non-contiguous buffers: %.3f (activation limit: %.3f).  "
            "Continue with SMSC? ==>%s.\n",
            reduce_buf_output[0],
            ((double)reduce_buf_output[1])/comm_size, mca_coll_han_component.han_alltoallv_smsc_avg_send_limit,
            ((double)reduce_buf_output[2])/comm_size, mca_coll_han_component.han_alltoallv_smsc_noncontig_activation_limit,
            (*use_smsc) ? "Yes" : "No");
    }
    return rc;
}

int mca_coll_han_alltoallv_using_smsc(
        const void *sbuf,
        ompi_count_array_t scounts,
        ompi_disp_array_t sdispls,
        struct ompi_datatype_t *sdtype,
        void* rbuf,
        ompi_count_array_t rcounts,
        ompi_disp_array_t rdispls,
        struct ompi_datatype_t *rdtype,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{
    int rc;
    void **send_from_addrs = NULL;
    void **recv_to_addrs = NULL;
    size_t *send_counts = NULL;
    size_t *recv_counts = NULL;
    opal_datatype_t **send_types = NULL;
    opal_datatype_t **recv_types = NULL;
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;

    OPAL_OUTPUT_VERBOSE((90, mca_coll_han_component.han_output,
                            "Entering mca_coll_han_alltoall_using_smsc\n"));

    if (!mca_smsc || !mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)) {
        /* Assume all hosts take this path together :-\ */
        opal_output_verbose(1, mca_coll_han_component.han_output, "in mca_coll_han_alltoallv_using_smsc, "
            "but MCA_SMSC_FEATURE_CAN_MAP not available.  Disqualifying this alg!\n");
        HAN_UNINSTALL_COLL_API(comm, han_module, alltoallv);
        return han_module->previous_alltoallv(sbuf, scounts, sdispls, sdtype, rbuf, rcounts, rdispls, rdtype,
                                             comm, han_module->previous_alltoallv_module);
    }

    /* Create the subcommunicators */
    if( OMPI_SUCCESS != mca_coll_han_comm_create_new(comm, han_module) ) {
        opal_output_verbose(1, mca_coll_han_component.han_output,
                             "han cannot handle alltoallv with this communicator. Fall back on another component\n");
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(comm, han_module);
        return han_module->previous_alltoallv(sbuf, scounts, sdispls, sdtype, rbuf, rcounts, rdispls, rdtype,
                                          comm, han_module->previous_alltoallv_module);
    }

    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced || !han_module->is_mapbycore){
        opal_output_verbose(1, mca_coll_han_component.han_output,
                             "han cannot handle alltoallv with this communicator (imbalance/!mapbycore).  "
                             "Fall back on another component\n");
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_UNINSTALL_COLL_API(comm, han_module, alltoallv);
        return han_module->previous_alltoallv(sbuf, scounts, sdispls, sdtype, rbuf, rcounts, rdispls, rdtype,
                                             comm, han_module->previous_alltoallv_module);
    }

    int w_size = ompi_comm_size(comm);

    int use_smsc;
    rc = decide_to_use_smsc_alg(&use_smsc,
        sbuf, scounts, sdispls, sdtype, rbuf, rcounts, rdispls, rdtype, comm);
    if (rc != 0) {
        opal_output_verbose(1, mca_coll_han_component.han_output,
            "decide_to_use_smsc_alg failed during execution! rc=%d\n", rc);
    }
    if (!use_smsc) {
        return han_module->previous_alltoallv(sbuf, scounts, sdispls, sdtype, rbuf, rcounts, rdispls, rdtype,
                                             comm, han_module->previous_alltoallv_module);
    }

    ompi_communicator_t *low_comm = han_module->sub_comm[INTRA_NODE];
    ompi_communicator_t *up_comm = han_module->sub_comm[INTER_NODE];

    /* information about sub-communicators */
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);
    int up_size = ompi_comm_size(up_comm);
    int up_rank = ompi_comm_rank(up_comm);

    struct gathered_data low_gather_in;
    struct gathered_data *low_gather_out;


    low_gather_in.stype_serialized_length = ddt_pack_datatype(&sdtype->super, NULL);
    uint8_t *serialization_buf;

    size_t serialization_buf_length = low_gather_in.stype_serialized_length
        + sizeof(struct peer_counts)*w_size;

    /* allocate data */
    serialization_buf = malloc(serialization_buf_length);
    low_gather_out = malloc(sizeof(*low_gather_out) * low_size);
    struct peer_data *peers = malloc(sizeof(*peers) * low_size);
    opal_datatype_t *peer_send_types = malloc(sizeof(*peer_send_types) * low_size);

    low_gather_in.serialization_buffer = serialization_buf;
    low_gather_in.sbuf = (void*)sbuf; // cast to discard the const

    ptrdiff_t r_extent, r_lb;
    rc = ompi_datatype_get_extent( rdtype, &r_lb, &r_extent);

    /* calculate the full gap and span of all accesses to our buffer: */
    coll_han_alltoallv_calc_all_span( w_size, &sdtype->super, scounts, sdispls,
                &low_gather_in.sbuf_gap, &low_gather_in.sbuf_span );

    /* pack the serialization buffer: first the array of counts */
    size_t buf_packed = 0;
    for (int jrankw=0; jrankw<w_size; jrankw++) {
        struct peer_counts *counts = (void*)(serialization_buf + buf_packed);
        counts->scount = ompi_count_array_get(scounts,jrankw);
        counts->rcount = ompi_count_array_get(rcounts,jrankw);

        counts->sdispl = ompi_disp_array_get(sdispls,jrankw);
        counts->rdispl = ompi_disp_array_get(rdispls,jrankw);
        buf_packed += sizeof(*counts);
    }
    /* pack the serialization buffer: next the send and recv dtypes */
    buf_packed += ddt_pack_datatype(&sdtype->super, serialization_buf + buf_packed);
    assert(buf_packed == serialization_buf_length);

    rc = low_comm->c_coll->coll_allgather(&low_gather_in, sizeof(low_gather_in), MPI_BYTE,
            low_gather_out, sizeof(low_gather_in), MPI_BYTE, low_comm,
            low_comm->c_coll->coll_allgather_module);
    if (rc != 0) {
        opal_output_verbose(1, mca_coll_han_component.han_output,
            "During mca_coll_han_alltoallv_using_smsc: Allgather failed with rc=%d\n",rc);
        goto cleanup;
    }

    /*
    In this loop we unpack the data received in allgather:
      - use SMSC to memory-map the serialization buffer
        - construct dtype objects from the serialization buffer
        - set pointers to read counts and displacements directly from serialization buffer
      - Memory-map the send-bufs.

    */
    for (int jrank=0; jrank<low_size; jrank++) {
        void *tmp_ptr;
        peers[jrank].map_ctx[0] = NULL;
        peers[jrank].map_ctx[1] = NULL;

        if (jrank == low_rank) {
            /* special case for ourself */
            peers[jrank].counts = (struct peer_counts *)serialization_buf;
            peers[jrank].sbuf   = sbuf;
            peers[jrank].sendtype = &sdtype->super;
            continue;
        }

        struct gathered_data *gathered = &low_gather_out[jrank];
        struct ompi_proc_t* ompi_proc = ompi_comm_peer_lookup(low_comm, jrank);
        mca_smsc_endpoint_t *smsc_ep;
        smsc_ep = mca_coll_han_get_smsc_endpoint(ompi_proc);

        uint8_t *peer_serialization_buf;
        size_t peer_serialization_buf_length;
        peer_serialization_buf_length = w_size * sizeof(struct peer_counts);
        peer_serialization_buf_length += gathered->stype_serialized_length;

        /* mmap the buffers */
        peers[jrank].map_ctx[0] = mca_smsc->map_peer_region(
                smsc_ep,
                MCA_RCACHE_FLAGS_PERSIST,
                gathered->serialization_buffer,
                peer_serialization_buf_length,
                (void**) &peer_serialization_buf );
        if (gathered->sbuf_span > 0) {
            peers[jrank].map_ctx[1] = mca_smsc->map_peer_region(
                    smsc_ep,
                    MCA_RCACHE_FLAGS_PERSIST,
                    (char*)gathered->sbuf + gathered->sbuf_gap,
                    gathered->sbuf_span,
                    &tmp_ptr );
            peers[jrank].sbuf = (char*)tmp_ptr - gathered->sbuf_gap;
        }

        /* point the counts pointer into the mmapped serialization buffer */
        peers[jrank].counts = (struct peer_counts*)peer_serialization_buf;
        peer_serialization_buf += w_size * sizeof(struct peer_counts);

        /* unpack the dtypes */
        peer_serialization_buf += ddt_unpack_datatype( &peer_send_types[jrank], peer_serialization_buf);
        peers[jrank].sendtype = &peer_send_types[jrank];
    }

    send_from_addrs = malloc(sizeof(*send_from_addrs)*low_size);
    recv_to_addrs = malloc(sizeof(*recv_to_addrs)*low_size);
    send_counts = malloc(sizeof(*send_counts)*low_size);
    recv_counts = malloc(sizeof(*recv_counts)*low_size);
    send_types = malloc(sizeof(*send_types)*low_size);
    recv_types = malloc(sizeof(*recv_types)*low_size);

    /****
     *  Main exchange loop
     ****/
    int nloops = up_size;
    for (int jloop=0; jloop<nloops; jloop++) {

        int up_partner = ring_partner(up_rank, jloop, up_size);
        int jrank_sendto = up_partner*low_size + low_rank;
        int jrank_recvfrom = jrank_sendto;

        for (int jlow=0; jlow<low_size; jlow++) {
            int remote_wrank = up_partner*low_size + jlow;
            ptrdiff_t peer_sextent;

            rc = opal_datatype_type_extent( peers[jlow].sendtype, &peer_sextent);
            if (rc != 0) {
                opal_output_verbose(1, mca_coll_han_component.han_output,
                    "opal_datatype_type_extent returned error code = %d during mca_coll_han_alltoallv_using_smsc!\n",rc);
                goto cleanup;
            }
            void *from_addr = (uint8_t*)peers[jlow].sbuf + peers[jlow].counts[jrank_sendto].sdispl*peer_sextent;

            send_from_addrs[jlow] = from_addr;
            send_counts[jlow] = peers[jlow].counts[jrank_sendto].scount;
            send_types[jlow] = peers[jlow].sendtype;


            recv_to_addrs[jlow] = (uint8_t*)rbuf + ompi_disp_array_get(rdispls,remote_wrank)*r_extent;
            recv_counts[jlow] = ompi_count_array_get(rcounts,remote_wrank);
            recv_types[jlow] = &(rdtype->super);
        }

        int ntypes_send = low_size;
        int ntypes_recv = low_size;

        rc = alltoallv_sendrecv_w(
            send_from_addrs, send_counts, send_types, jrank_sendto, ntypes_send,
            recv_to_addrs, recv_counts, recv_types, jrank_recvfrom, ntypes_recv,
            comm);
        if (rc != 0) {
            opal_output_verbose(1, mca_coll_han_component.han_output,
                "alltoallv_sendrecv_w returned error code = %d!\n",rc);
            goto cleanup;
        }


    }
    rc=0;

    cleanup:
    low_comm->c_coll->coll_barrier(low_comm, low_comm->c_coll->coll_barrier_module);

    if (send_from_addrs) {
        free(send_from_addrs);
        free(recv_to_addrs);
        free(send_counts);
        free(recv_counts);
        free(send_types);
        free(recv_types);
    }

    for (int jlow=0; jlow<low_size; jlow++) {
        if (jlow != low_rank) {
            OBJ_DESTRUCT(&peer_send_types[jlow]);
        }

        for (int jbuf=0; jbuf<2; jbuf++) {
            if (peers[jlow].map_ctx[jbuf]) {
                mca_smsc->unmap_peer_region(peers[jlow].map_ctx[jbuf]);
            }
        }
    }
    free(serialization_buf);
    free(low_gather_out);
    free(peers);
    free(peer_send_types);

    OPAL_OUTPUT_VERBOSE((40, mca_coll_han_component.han_output,
                "Alltoall Complete with %d\n",rc));
    return rc;

}
