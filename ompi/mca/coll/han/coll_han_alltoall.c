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
 * This file contains all the hierarchical implementations of alltoall.
 *
 * There are two similar Alltoall algorithms.  Both algorithms use the same
 * communication pattern.  Each rank on one host is assigned a single
 * partner on a remote host and vice versa.  Then the rank collects all
 * the data its partner will need to receive from it's host, and sends it
 * in one large send, and likewise receives it's data in one large recv,
 * then cycles to the next host.
 *
 * The two algorithms are:
 * - mca_coll_han_alltoall_using_allgather: gathering data is done once
 *   and each rank has a copy of all local data.  Only recommended for
 *   small message sizes.
 * - mca_coll_han_alltoall_using_smsc: ranks use smsc module to
 *   direct-map local memory before copying into a packed send buffer.
 *   Currently only the XPMEM-based smsc module supports this operation.
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
static inline int ring_partner(int rank, int round, int comm_size) {
    /* make sure ring_partner is positive: make argument to modulo > 0 with +comm_size.*/
    return (comm_size + round - rank) % comm_size;
}

int mca_coll_han_alltoall_using_allgather(
        const void *sbuf, int scount,
        struct ompi_datatype_t *sdtype,
        void* rbuf, int rcount,
        struct ompi_datatype_t *rdtype,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{

    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;

    /* Create the subcommunicators */
    if( OMPI_SUCCESS != mca_coll_han_comm_create_new(comm, han_module) ) {
        opal_output_verbose(1, mca_coll_han_component.han_output,
                             "han cannot handle alltoall with this communicator. Fall back on another component\n");
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(han_module, comm);
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
        HAN_LOAD_FALLBACK_COLLECTIVE(han_module, comm, alltoall);
        return han_module->previous_alltoall(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                             comm, han_module->previous_alltoall_module);
    }

    int rc;
    size_t sndsize;
    MPI_Aint sextent, rextent, lb;
    char *send_bounce;
    char *gather_bounce;

    ompi_communicator_t *low_comm = han_module->sub_comm[INTRA_NODE];
    ompi_communicator_t *up_comm = han_module->sub_comm[INTER_NODE];
    ompi_request_t **inter_recv_reqs;
    ompi_request_t **intra_gather_reqs;
    ompi_request_t **inter_send_reqs;

    int fanout = mca_coll_han_component.han_alltoall_pstages;
    if (!fanout) {
        fanout = 1;
    }
    int subdiv_factor = 1;

    OPAL_OUTPUT_VERBOSE((90, mca_coll_han_component.han_output, "Starting mca_coll_han_alltoall_using_allgather\n"));

    int w_rank = ompi_comm_rank(comm);
    int w_size = ompi_comm_size(comm);


    /* information about sub-communicators */
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);
    int up_size = ompi_comm_size(up_comm);

    while (low_size % subdiv_factor != 0) {
        subdiv_factor--;
        assert(subdiv_factor > 0);
    }
    int up_rank = w_rank / low_size;
    assert( w_rank % low_size == low_rank );

    rc = ompi_datatype_get_extent( sdtype, &lb, &sextent);
    rc = ompi_datatype_get_extent( rdtype, &lb, &rextent);
    opal_datatype_type_size( &sdtype->super, &sndsize);

    MPI_Aint gather_bytes = w_size * low_size * rextent * rcount;
    MPI_Aint send_bytes_per_fan = low_size * rextent * rcount;

    int nreq_intra_per_round = low_size;
    gather_bounce = malloc(gather_bytes);
    send_bounce = malloc(send_bytes_per_fan * fanout);
    intra_gather_reqs = malloc(sizeof(*intra_gather_reqs)*nreq_intra_per_round*fanout);
    inter_send_reqs = malloc(sizeof(*inter_send_reqs) * fanout);
    inter_recv_reqs = malloc(sizeof(*inter_recv_reqs) * up_size );

    int nrounds = up_size;
    /* pre-post all our receives.  We will be ready to receive all data regardless of fan-out.
       (This is not an in-place algorithm)*/
    for (int jround=0; jround<nrounds; jround++) {
            int up_partner = ring_partner(up_rank, jround, up_size);
            int first_remote_rank = up_partner*low_size;
            /* pre-post the receive for remote.  Receive directly into application buffer */
            char *recv_chunk = ((char*)rbuf) + rextent*rcount*first_remote_rank;
            MCA_PML_CALL(irecv
                        (recv_chunk, rcount*low_size, rdtype, first_remote_rank+low_rank,
                        MCA_COLL_BASE_TAG_ALLTOALL,
                        comm, &inter_recv_reqs[jround]));
    }

    rc = low_comm->c_coll->coll_allgather(sbuf, scount*w_size, sdtype, gather_bounce, scount*w_size, sdtype, low_comm, low_comm->c_coll->coll_allgather_module);

    if (rc != 0) {
        OPAL_OUTPUT_VERBOSE((40, mca_coll_han_component.han_output,
        "Allgather failed with %d\n",rc));
        goto cleanup;
    }

    for (int jloop=0; jloop<nrounds+fanout; jloop++) {

        /* complete previous inter-node send */
        if (jloop >= fanout) {
            /* we cannot fill for our next send until the previous send using this buffer is completed. */
            int jfan_slot = jloop % fanout;
            ompi_request_wait(&inter_send_reqs[jfan_slot], MPI_STATUS_IGNORE);
        }

        /* issue intra-node gather */
        if (jloop < nrounds) {
            int jround = jloop;
            int family_partner = ring_partner(up_rank, jround, up_size);
            int first_remote_rank = family_partner*low_size;
            assert(family_partner >= 0);
            assert(family_partner < up_size && family_partner < up_size*subdiv_factor);
            assert(first_remote_rank <= w_size );
            int jfan_slot = jround % fanout;

            /* copy memory for jround */
            for (int jother=0; jother<low_size; jother++) {
                /* post recvs */
                char *copy_to = send_bounce + send_bytes_per_fan*jfan_slot + jother*scount*sextent;
                char *copy_from = gather_bounce + jother*w_size*sextent*scount + sextent*scount*(first_remote_rank + low_rank);
                memcpy( copy_to, copy_from, scount*sextent);
            }

            /* send the data we gathered to our remote partner */
            MCA_PML_CALL(isend
                    (&send_bounce[send_bytes_per_fan*jfan_slot],
                    rcount*low_size, rdtype, first_remote_rank+low_rank,
                    MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
                    comm, &inter_send_reqs[jfan_slot]));
        }
    }
    /* wait for all irecv to complete */
    ompi_request_wait_all(up_size, inter_recv_reqs, MPI_STATUS_IGNORE);

cleanup:
    free(send_bounce);
    free(gather_bounce);
    free(intra_gather_reqs);
    free(inter_send_reqs);
    free(inter_recv_reqs);

    return rc;

}


int mca_coll_han_alltoall_using_smsc(
        const void *sbuf, int scount,
        struct ompi_datatype_t *sdtype,
        void* rbuf, int rcount,
        struct ompi_datatype_t *rdtype,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{

    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;

    OPAL_OUTPUT_VERBOSE((90, mca_coll_han_component.han_output,
                            "Entering mca_coll_han_alltoall_using_smsc\n"));

    if (!mca_smsc || !mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)) {
        /* Hopefully all hosts take this path together :-\ */
        opal_output_verbose(1, mca_coll_han_component.han_output, "in mca_coll_han_alltoall_using_smsc, "
            "but MCA_SMSC_FEATURE_CAN_MAP not available.  Disqualifying this alg!\n");
        HAN_LOAD_FALLBACK_COLLECTIVE(han_module, comm, alltoall);
        return han_module->previous_alltoall(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                             comm, han_module->previous_alltoall_module);
    }

    /* Create the subcommunicators */
    if( OMPI_SUCCESS != mca_coll_han_comm_create_new(comm, han_module) ) {
        opal_output_verbose(1, mca_coll_han_component.han_output,
                             "han cannot handle alltoall with this communicator. Fall back on another component\n");
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(han_module, comm);
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
        HAN_LOAD_FALLBACK_COLLECTIVE(han_module, comm, alltoall);
        return han_module->previous_alltoall(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                             comm, han_module->previous_alltoall_module);
    }

    int rc;
    size_t sndsize;
    MPI_Aint sextent, rextent, lb;
    char *send_bounce;
    const bool USE_MAP=true;

    ompi_communicator_t *low_comm = han_module->sub_comm[INTRA_NODE];
    ompi_communicator_t *up_comm = han_module->sub_comm[INTER_NODE];
    ompi_request_t **inter_recv_reqs;
    ompi_request_t **inter_send_reqs;

    int fanout = mca_coll_han_component.han_alltoall_pstages;
    if (!fanout) {
        fanout = 1;
    }
    int subdiv_factor = 1;

    rc = ompi_datatype_get_extent( sdtype, &lb, &sextent);
    rc = ompi_datatype_get_extent( rdtype, &lb, &rextent);
    opal_datatype_type_size( &sdtype->super, &sndsize);

    int w_rank = ompi_comm_rank(comm);
    int w_size = ompi_comm_size(comm);

    /* information about sub-communicators */
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);
    int up_size = ompi_comm_size(up_comm);

    mca_smsc_endpoint_t **local_smsc_eps;
    local_smsc_eps = malloc( sizeof(*local_smsc_eps) * low_size);
    for (int jlow=0; jlow<low_size; jlow++) {
        if (jlow == low_rank) {continue;}
        struct ompi_proc_t* ompi_proc = ompi_comm_peer_lookup(low_comm, jlow);
        local_smsc_eps[jlow] = mca_coll_han_get_smsc_endpoint(ompi_proc);
    }

    while (low_size % subdiv_factor != 0) {
        subdiv_factor--;
        assert(subdiv_factor > 0);
    }
    int up_rank = w_rank / low_size;
    assert( w_rank % low_size == low_rank );


    MPI_Aint send_bytes_per_fan = low_size * rextent * rcount;

    if (fanout==1) {
        send_bounce = (char*)rbuf + up_rank*low_size*rextent*rcount;
    } else {
        send_bounce = malloc(send_bytes_per_fan * fanout);
    }
    inter_send_reqs = malloc(sizeof(*inter_send_reqs) * fanout);
    inter_recv_reqs = malloc(sizeof(*inter_recv_reqs) * up_size );

    int bytes_to_gather;
    int reg_data_size = 0;
    bytes_to_gather = sizeof(MPI_Aint);
    void *local_reg;
    if (!USE_MAP) {
        if ( mca_smsc_base_has_feature(MCA_SMSC_FEATURE_REQUIRE_REGISTRATION)) {
             bytes_to_gather += mca_smsc->registration_data_size;
             reg_data_size = mca_smsc->registration_data_size;
             local_reg = mca_smsc->register_region((void*)sbuf, sextent*w_size );
        }
    }

    char *gather_buf_out = malloc( low_size * bytes_to_gather );
    char *gather_buf_in  = malloc( bytes_to_gather);

    /* fill gather_buf_in.  Both and address and optionally a registration. */
    *((int**)gather_buf_in) = (int*)sbuf;
    if (reg_data_size) {
        memcpy(gather_buf_in + sizeof(MPI_Aint), local_reg, reg_data_size);
    }

    const char **remote_send_bufs = calloc(low_size, sizeof(*remote_send_bufs));
    const char **local_send_bufs = malloc(low_size * sizeof(*local_send_bufs));
    void **remote_send_reg = malloc(low_size * sizeof(*remote_send_reg));
    void **sbuf_map_ctx = malloc(low_size * sizeof(&sbuf_map_ctx));
    // int64_t sbuf_ptr = (long int) sbuf;

    rc = low_comm->c_coll->coll_allgather(gather_buf_in, bytes_to_gather, MPI_BYTE,
                gather_buf_out, bytes_to_gather, MPI_BYTE, low_comm,
                low_comm->c_coll->coll_allgather_module);

    for (int jlow=0; jlow<low_size; jlow++) {
        remote_send_bufs[jlow] = *(const char**)(gather_buf_out + jlow * bytes_to_gather);
        if (reg_data_size) {
            remote_send_reg[jlow]  = (void*) (gather_buf_out +
                jlow * bytes_to_gather + sizeof(MPI_Aint));
        } else {
            remote_send_reg[jlow] = NULL;
        }
    }

    if (rc != 0) {
        OPAL_OUTPUT_VERBOSE((40, mca_coll_han_component.han_output,
        "Allgather failed with %d\n",rc));
        goto cleanup;
    }

    for (int jother=0; jother<low_size; jother++) {
        if (jother == low_rank) {
            sbuf_map_ctx[low_rank] = NULL;
            local_send_bufs[low_rank] = sbuf;
            continue;
        }
        if (USE_MAP) {
            sbuf_map_ctx[jother] = mca_smsc->map_peer_region(
                local_smsc_eps[jother],
                MCA_RCACHE_FLAGS_PERSIST,
                (void*)remote_send_bufs[jother],
                sextent*w_size*scount,
                (void**)&local_send_bufs[jother] );
        } else {
            local_send_bufs[jother] = remote_send_bufs[jother];
        }
    }


    /* pre-post all our receives.  We will be ready to receive all data regardless of fan-out.
       (This is not an in-place algorithm)*/
    int inter_recv_count = 0;
    int nrounds = up_size;
    for (int jround=0; jround<nrounds; jround++) {
            int up_partner = ring_partner(up_rank, jround, up_size);
            if (fanout == 1 && up_partner == up_rank ) {
                continue;
            }
            int first_remote_rank = up_partner*low_size;
            /* pre-post the receive for remote.  Receive directly into application buffer */
            char *recv_chunk = ((char*)rbuf) + rextent*rcount*first_remote_rank;

            MCA_PML_CALL(irecv
                        (recv_chunk, rcount*low_size, rdtype, first_remote_rank+low_rank,
                        MCA_COLL_BASE_TAG_ALLTOALL,
                        comm, &inter_recv_reqs[inter_recv_count++]));
    }


    for (int jloop=0; jloop<nrounds+fanout; jloop++) {
        int family_partner = ring_partner(up_rank, jloop, up_size);

        /* complete previous inter-node send */
        if (fanout != 1 && jloop >= fanout) {
            /* we cannot fill for our next send until the previous send using this buffer is completed. */
            int jfan_slot = jloop % fanout;
            ompi_request_wait(&inter_send_reqs[jfan_slot], MPI_STATUS_IGNORE);
        }

        if (fanout == 1 && family_partner == up_rank) {
            /* we are using this space as send_bounce, so do the memcopies later */
            continue;
        }

        /* issue intra-node gather */
        if (jloop < nrounds) {
            int jround = jloop;
            int first_remote_rank = family_partner*low_size;
            assert(family_partner >= 0);
            assert(family_partner < up_size && family_partner < up_size*subdiv_factor);
            assert(first_remote_rank <= w_size );
            int jfan_slot = jround % fanout;

            /* copy memory for jround */
            for (int jother=0; jother<low_size; jother++) {
                /* post recvs */
                char *copy_to = send_bounce + send_bytes_per_fan*jfan_slot + jother*scount*sextent;
                const char *copy_from = ((char*)local_send_bufs[jother]) + sextent*scount*(first_remote_rank + low_rank);
                if (USE_MAP) memcpy( copy_to, copy_from, scount*sextent);
                if (!USE_MAP) mca_smsc->copy_from( local_smsc_eps[jother], copy_to, (void *)copy_from, scount*sextent, remote_send_reg[jother]);
            }

            if (fanout == 1) {
                /* for fanout==1, don't bother with isend */
                MCA_PML_CALL(send
                        (send_bounce,
                        rcount*low_size, rdtype, first_remote_rank+low_rank,
                        MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
                        comm) );
            } else {
                /* send the data we gathered to our remote partner */
                MCA_PML_CALL(isend
                        (&send_bounce[send_bytes_per_fan*jfan_slot],
                        rcount*low_size, rdtype, first_remote_rank+low_rank,
                        MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
                        comm, &inter_send_reqs[jfan_slot]));
            }
        }
    }

    if (fanout == 1) {
        /* copy memory for self  */
        for (int jother=0; jother<low_size; jother++) {
            int w_other = up_rank*low_size+jother;
            char *copy_to = (char*)rbuf + (w_other)*rextent*rcount;
            const char *copy_from = ((char*)local_send_bufs[jother]) + sextent*scount*(up_rank*low_size + low_rank);
            if (USE_MAP) memcpy( copy_to, copy_from, scount*sextent);
            if (!USE_MAP) mca_smsc->copy_from( local_smsc_eps[jother], copy_to, (void *)copy_from, scount*sextent, remote_send_reg[jother]);
        }
        /* We need a barrier here to prevent another (local) rank from returning
           the send buf back to the application, and having it overwritten
           before this rank gets to read from it. */
        low_comm->c_coll->coll_barrier(low_comm, low_comm->c_coll->coll_barrier_module);
    }

    /* wait for all irecv to complete */
    ompi_request_wait_all(inter_recv_count, inter_recv_reqs, MPI_STATUS_IGNORE);

cleanup:
    for (int jother=0; jother<low_size; jother++) {
        if (jother != low_rank ) {
            if (USE_MAP) {
                mca_smsc->unmap_peer_region(sbuf_map_ctx[jother]);
            }
        }
    }
    if (fanout != 1) free(send_bounce);
    free(inter_send_reqs);
    free(inter_recv_reqs);
    free(sbuf_map_ctx);
    free(local_send_bufs);
    free(remote_send_bufs);
    free(local_smsc_eps);

    OPAL_OUTPUT_VERBOSE((40, mca_coll_han_component.han_output,
                "Alltoall Complete with %d\n",rc));
    return rc;

}