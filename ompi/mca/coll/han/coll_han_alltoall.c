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
 * This file contains the hierarchical implementations of alltoall.
 *
 * mca_coll_han_alltoall_using_smsc:
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

int mca_coll_han_alltoall_using_smsc(
        const void *sbuf, size_t scount,
        struct ompi_datatype_t *sdtype,
        void* rbuf, size_t rcount,
        struct ompi_datatype_t *rdtype,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{

    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;

    OPAL_OUTPUT_VERBOSE((90, mca_coll_han_component.han_output,
                            "Entering mca_coll_han_alltoall_using_smsc\n"));

    if (!mca_smsc || !mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)) {
        /* Assume all hosts take this path together :-\ */
        opal_output_verbose(1, mca_coll_han_component.han_output, "in mca_coll_han_alltoall_using_smsc, "
            "but MCA_SMSC_FEATURE_CAN_MAP not available.  Disqualifying this alg!\n");
        HAN_UNINSTALL_COLL_API(comm, han_module, alltoall);
        return han_module->previous_alltoall(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                             comm, han_module->previous_alltoall_module);
    }

    /* Create the subcommunicators */
    if( OMPI_SUCCESS != mca_coll_han_comm_create_new(comm, han_module) ) {
        opal_output_verbose(1, mca_coll_han_component.han_output,
                             "han cannot handle alltoall with this communicator. Fall back on another component\n");
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(comm, han_module);
        return han_module->previous_alltoall(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                          comm, han_module->previous_alltoall_module);
    }

    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced || !han_module->is_mapbycore){
        opal_output_verbose(1, mca_coll_han_component.han_output,
                             "han cannot handle alltoall with this communicator (imbalance/!mapbycore).  "
                             "Fall back on another component\n");
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_UNINSTALL_COLL_API(comm, han_module, alltoall);
        return han_module->previous_alltoall(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                             comm, han_module->previous_alltoall_module);
    }

    int rc, send_needs_bounce, ii_push_data;
    size_t sndsize;
    MPI_Aint sextent, rextent, lb;
    char *send_bounce;
    opal_convertor_t convertor;
    size_t packed_size = 0, packed_size_tmp;
    int use_isend;
    void *gather_buf_in[4];
    int up_rank;

    ompi_communicator_t *low_comm = han_module->sub_comm[INTRA_NODE];
    ompi_communicator_t *up_comm = han_module->sub_comm[INTER_NODE];
    ompi_request_t **inter_recv_reqs;
    ompi_request_t **inter_send_reqs;

    rc = ompi_datatype_get_extent( sdtype, &lb, &sextent);
    rc = ompi_datatype_get_extent( rdtype, &lb, &rextent);
    opal_datatype_type_size( &sdtype->super, &sndsize);

    int w_rank = ompi_comm_rank(comm);
    int w_size = ompi_comm_size(comm);

    /* information about sub-communicators */
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);
    int up_size = ompi_comm_size(up_comm);

    int fanout = mca_coll_han_component.han_alltoall_pstages;
    if (!fanout) {
        fanout = 1;
    }
    if (fanout > up_size) { fanout = up_size; }

    OBJ_CONSTRUCT( &convertor, opal_convertor_t );


    send_needs_bounce = 0;
    /* get converter for copying to one of the leader ranks, and get packed size: */
    opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor, &sdtype->super, scount, sbuf, 0, &convertor);
    send_needs_bounce |= 0 != opal_convertor_on_device(&convertor);
    send_needs_bounce |= opal_convertor_need_buffers(&convertor);
    opal_convertor_cleanup(&convertor);

    opal_convertor_copy_and_prepare_for_recv(ompi_mpi_local_convertor, &rdtype->super, rcount, rbuf, 0, &convertor);
    send_needs_bounce |= 0 != opal_convertor_on_device(&convertor);
    send_needs_bounce |= opal_convertor_need_buffers(&convertor);
    opal_convertor_get_packed_size( &convertor, &packed_size );
    opal_convertor_cleanup(&convertor);

    /*
      Because push-mode needs extra synchronizations, we'd like to avoid it,
      however it might be necessary:

      If application buffer is non-contigious or non-homogenous, then we'll
      need to "push" the data so that the packing process properly knows the
      memory layout and types.

      If the application buffer is device memory, we'll also need to exchange
      in push mode so that the process which has device registrations can
      perform the reads.

      In both of these cases, we'll need to use the bounce buffer too.
    */
    ii_push_data = send_needs_bounce;

    /*
      If we have a fanout > 1, we'll need somewhere to put data for the next
      send while the previous send is still in-flight.  We'll need a dedicated
      bounce buffer for this, but it doesn't mean we have to use "push" mode.
    */
    send_needs_bounce |= fanout != 1;

    up_rank = w_rank / low_size;
    assert( w_rank % low_size == low_rank );
    int64_t send_bytes_per_fan = low_size * packed_size;
    inter_send_reqs = malloc(sizeof(*inter_send_reqs) * fanout);
    inter_recv_reqs = malloc(sizeof(*inter_recv_reqs) * up_size );
    char **low_bufs = malloc(low_size * sizeof(*low_bufs));
    void **sbuf_map_ctx = malloc(low_size * sizeof(&sbuf_map_ctx));

    const int nptrs_gather = 3;
    void **gather_buf_out = calloc(low_size*nptrs_gather, sizeof(void*));
    bool send_bounce_is_allocated = false;

    do {
start_allgather:
        if ( 0 == send_needs_bounce ) {
            send_bounce = (char*)rbuf + up_rank*send_bytes_per_fan;
        } else {
            if (!send_bounce_is_allocated) {
                send_bounce = malloc(send_bytes_per_fan * fanout);
                send_bounce_is_allocated = true;
            }
        }

        if (ii_push_data) {
            /* all ranks will push to the other ranks' bounce buffer */
            gather_buf_in[0] = send_bounce;
        } else {
            /* all ranks will pull from the other ranks' sbuf */
            gather_buf_in[0] = (void*)sbuf;
        }
        gather_buf_in[1] = (void*)(intptr_t)send_needs_bounce;
        gather_buf_in[2] = (void*)(intptr_t)ii_push_data;

        rc = low_comm->c_coll->coll_allgather(gather_buf_in, nptrs_gather, MPI_AINT,
                    gather_buf_out, nptrs_gather, MPI_AINT, low_comm,
                    low_comm->c_coll->coll_allgather_module);

        if (rc != 0) {
            OPAL_OUTPUT_VERBOSE((40, mca_coll_han_component.han_output,
            "Allgather failed with %d\n",rc));
            goto cleanup;
        }

        for (int jother=0; jother<low_size; jother++) {
            int other_push_data = (uintptr_t)gather_buf_out[nptrs_gather*jother + 2] & 0x1;
            if (ii_push_data != other_push_data) {
                ii_push_data = 1;
                goto start_allgather;
            }
            send_needs_bounce  |= (uintptr_t)gather_buf_out[nptrs_gather*jother + 1] & 0x1;
            ii_push_data       |= other_push_data;
        }
    } while (0);

    use_isend = fanout > 1 || ii_push_data;

    for (int jother=0; jother<low_size; jother++) {
        low_bufs[jother] = gather_buf_out[nptrs_gather*jother];
        if (jother == low_rank) {
            sbuf_map_ctx[low_rank] = NULL;
        } else {
            struct ompi_proc_t* ompi_proc = ompi_comm_peer_lookup(low_comm, jother);
            mca_smsc_endpoint_t *smsc_ep;
            smsc_ep = mca_coll_han_get_smsc_endpoint(ompi_proc);

            sbuf_map_ctx[jother] = mca_smsc->map_peer_region(
                smsc_ep,
                MCA_RCACHE_FLAGS_PERSIST,
                low_bufs[jother],
                sextent*w_size*scount,
                (void**) &low_bufs[jother] );
        }
    }

    for (int jslot=0; jslot < fanout; jslot++) {
        inter_send_reqs[jslot] = MPI_REQUEST_NULL;
    }


    /* pre-post all our receives.  We will be ready to receive all data regardless of fan-out.
       (This is not an in-place algorithm)*/
    int inter_recv_count = 0;
    for (int jround=0; jround<up_size; jround++) {
            int up_partner = ring_partner(up_rank, jround, up_size);
            int first_remote_wrank = up_partner*low_size;
            /* pre-post the receive for remote.  Receive directly into application buffer */
            char *recv_chunk = ((char*)rbuf) + rextent*rcount*first_remote_wrank;

            MCA_PML_CALL(irecv
                        (recv_chunk, rcount*low_size, rdtype, first_remote_wrank+low_rank,
                        MCA_COLL_BASE_TAG_ALLTOALL,
                        comm, &inter_recv_reqs[inter_recv_count++]));
    }

    /* outer loop: in typical "fanout=1 case" do 1 iter per rank in the upper comm (ie, 1 per host)*/
    int nloops = up_size + fanout*use_isend;
    for (int jloop=0; jloop<nloops; jloop++) {

        int up_partner = ring_partner(up_rank, jloop, up_size);
        int jfan_slot = jloop % fanout;

        /* complete previous inter-node send */
        int prev_slot = jloop - fanout;
        if (use_isend && prev_slot >= 0 ) {
            /* we cannot fill for our next send until the previous send using this buffer is completed. */
            ompi_request_wait(&inter_send_reqs[jfan_slot], MPI_STATUS_IGNORE);
            if (ii_push_data && jloop < up_size) {
                /*  barrier here so followers know all leaders have completed
                    previous isend for this slot, and may begin overwriting bounce slot. */
                low_comm->c_coll->coll_barrier(low_comm, low_comm->c_coll->coll_barrier_module);
            }
        }

        if (jloop < up_size) {
            /* For this upper-comm partner, we must provide our data to all the leaders. */

            int first_remote_wrank;
            first_remote_wrank = up_partner*low_size;

            assert(up_partner >= 0);
            assert(up_partner < up_size);
            assert(first_remote_wrank <= w_size - low_size );

            /* pack data into each of the leaders' buffers */
            for (int jlow=0; jlow<low_size; jlow++) {

                int remote_wrank = first_remote_wrank + jlow;
                struct iovec iov;
                uint32_t iov_count = 1;

                ptrdiff_t fan_slot_offset = send_bytes_per_fan*jfan_slot;
                ptrdiff_t rank_offset = packed_size*low_rank;
                void *from_addr, *to_addr;

                if (ii_push_data) {
                    /* push our own data for remote into local leader's send_buf  */
                    from_addr = (char*)sbuf + sextent*scount*remote_wrank;
                    to_addr = low_bufs[jlow] + fan_slot_offset + rank_offset;
                } else {
                    /* pull other ranks' data for remote into our own send_buf */
                    to_addr = send_bounce + fan_slot_offset + packed_size*jlow;
                    from_addr = low_bufs[jlow] + packed_size*(low_size*up_partner+low_rank);
                }
                /* prepare: set the source for the copy */
                opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor, &sdtype->super, scount,
                                    from_addr,
                                    0, &convertor);
                /* iovec: set the destination of the copy */
                iov.iov_base = to_addr;
                iov.iov_len = sextent*scount;

                /* pack the data directly into local leader's sendbuf */
                packed_size_tmp = packed_size;
                rc = opal_convertor_pack(&convertor, &iov, &iov_count, &packed_size_tmp);
                opal_convertor_cleanup(&convertor);

                if (1 != rc) {
                    opal_output_verbose(1, mca_coll_han_component.han_output,
                            "opal_convert_pack failed with %d\n",rc);
                    rc = MPI_ERR_TRUNCATE;
                    goto cleanup;
                }
                rc = MPI_SUCCESS;
            }

            if (ii_push_data) {
                /* barrier here so leaders know all followers have filled data,
                   and can issue send. */
                low_comm->c_coll->coll_barrier(low_comm, low_comm->c_coll->coll_barrier_module);
            }

            if (use_isend == 0) {
                MCA_PML_CALL(send
                        (send_bounce,
                        send_bytes_per_fan, MPI_PACKED, first_remote_wrank+low_rank,
                        MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
                        comm) );
            } else {
                /* send the data to our remote partner */
                MCA_PML_CALL(isend
                        (&send_bounce[send_bytes_per_fan*jfan_slot],
                        send_bytes_per_fan, MPI_PACKED, first_remote_wrank+low_rank,
                        MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
                        comm, &inter_send_reqs[jfan_slot]));
            }
        }
    }

    /* wait for all irecv to complete */
    ompi_request_wait_all(inter_recv_count, inter_recv_reqs, MPI_STATUS_IGNORE);

cleanup:
    for (int jlow=0; jlow<low_size; jlow++) {
        if (jlow != low_rank ) {
            mca_smsc->unmap_peer_region(sbuf_map_ctx[jlow]);
        }
    }
    OBJ_DESTRUCT(&convertor);
    if (send_bounce_is_allocated) free(send_bounce);
    free(inter_send_reqs);
    free(inter_recv_reqs);
    free(sbuf_map_ctx);
    free(low_bufs);
    free(gather_buf_out);

    OPAL_OUTPUT_VERBOSE((40, mca_coll_han_component.han_output,
                "Alltoall Complete with %d\n",rc));
    return rc;
}
