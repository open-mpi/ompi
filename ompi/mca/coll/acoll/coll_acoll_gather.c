/* -*- Mode: C; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024 Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/pml/pml.h"
#include "opal/util/bit_ops.h"
#include "coll_acoll.h"
#include "coll_acoll_utils.h"

/*
 * mca_coll_acoll_gather_intra
 *
 * Function:    Gather operation using subgroup based algorithm
 * Accepts:     Same arguments as MPI_Gather()
 * Returns:     MPI_SUCCESS or error code
 *
 * Description: Gather is performed across and within subgroups.
 *              Subgroups can be 1 or more based on size and count.
 *
 * Limitations: Current implementation is optimal only for map-by core.
 *
 * Memory:      The base rank of each subgroup may create temporary buffer.
 *
 */
int mca_coll_acoll_gather_intra(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                                void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype, int root,
                                struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    int i, err, rank, size;
    char *wkg = NULL, *workbuf = NULL;
    MPI_Status status;
    MPI_Aint sextent, sgap = 0, ssize;
    MPI_Aint rextent = 0;
    size_t total_recv = 0;
    int sg_cnt, node_cnt;
    int cur_sg, root_sg;
    int cur_node, root_node;
    int is_base, is_local_root;
    int startr, endr, inc;
    int startn, endn;
    int num_nodes;
    mca_coll_acoll_module_t *acoll_module = (mca_coll_acoll_module_t *) module;
    coll_acoll_reserve_mem_t *reserve_mem_gather = &(acoll_module->reserve_mem_s);

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    sg_cnt = acoll_module->sg_cnt;
    node_cnt = acoll_module->node_cnt;
    num_nodes = (size + node_cnt - 1) / node_cnt;
    /* For small messages for nodes 8 and above, fall back to normal */
    if (num_nodes >= 8 && (rcount < 262144)) {
        node_cnt = size;
        sg_cnt = size;
        num_nodes = 1;
    }

    /* Setup root for receive */
    if (rank == root) {
        ompi_datatype_type_extent(rdtype, &rextent);
        /* Just use the recv buffer */
        wkg = (char *) rbuf;
        if (sbuf != MPI_IN_PLACE) {
            MPI_Aint root_ofst = rextent * (ptrdiff_t) (rcount * root);
            err = ompi_datatype_sndrcv((void *) sbuf, scount, sdtype, wkg + (ptrdiff_t) root_ofst,
                                       rcount, rdtype);
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
        total_recv = rcount;
    }

    /* Setup base ranks of non-root subgroups for receive */
    cur_sg = rank / sg_cnt;
    root_sg = root / sg_cnt;
    is_base = (rank % sg_cnt == 0) && (cur_sg != root_sg);
    startr = (rank / sg_cnt) * sg_cnt;
    cur_node = rank / node_cnt;
    root_node = root / node_cnt;
    is_local_root = (rank % node_cnt == 0) && (cur_node != root_node);
    startn = (rank / node_cnt) * node_cnt;

    if (is_base) {
        size_t buf_size = is_local_root ? (size_t) scount * node_cnt : (size_t) scount * sg_cnt;
        ompi_datatype_type_extent(sdtype, &sextent);
        ssize = opal_datatype_span(&sdtype->super, buf_size, &sgap);
        if (cur_sg != root_sg) {
            char *tmprecv = NULL;
            workbuf = (char *) coll_acoll_buf_alloc(reserve_mem_gather, ssize + sgap);
            if (NULL == workbuf) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            wkg = workbuf - sgap;
            tmprecv = wkg + sextent * (ptrdiff_t) (rcount * (rank - startr));
            /* local copy to workbuf */
            err = ompi_datatype_sndrcv((void *) sbuf, scount, sdtype, tmprecv, scount, sdtype);
            if (MPI_SUCCESS != err) {

                return err;
            }
        }
        rdtype = sdtype;
        rcount = scount;
        rextent = sextent;
        total_recv = rcount;
    } else if (rank != root) {
        wkg = (char *) sbuf;
        total_recv = scount;
    }

    /* All base ranks receive from other ranks in their respective subgroup */
    endr = startr + sg_cnt;
    if (endr > size) {
        endr = size;
    }
    inc = (rank == root) ? ((root != 0) ? 0 : 1) : 1;
    if (is_base || (rank == root)) {
        for (i = startr + inc; i < endr; i++) {
            char *tmprecv = NULL;
            if (i == root) {
                continue;
            }
            if (rank == root) {
                tmprecv = wkg + rextent * (ptrdiff_t) (rcount * i);
            } else {
                tmprecv = wkg + rextent * (ptrdiff_t) (rcount * (i - startr));
            }
            err = MCA_PML_CALL(
                recv(tmprecv, rcount, rdtype, i, MCA_COLL_BASE_TAG_GATHER, comm, &status));
            total_recv += rcount;
        }
    } else {
        int peer = (cur_sg == root_sg) ? root : startr;
        err = MCA_PML_CALL(send(sbuf, scount, sdtype, peer, MCA_COLL_BASE_TAG_GATHER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        return err;
    }

    /* All base ranks send to local root */
    endn = startn + node_cnt;
    if (endn > size) {
        endn = size;
    }
    if (sg_cnt < size) {
        int local_root = (root_node == cur_node) ? root : startn;
        for (i = startn; i < endn; i += sg_cnt) {
            int i_sg = i / sg_cnt;
            if ((rank != local_root) && (rank == i) && is_base) {
                err = MCA_PML_CALL(send(workbuf - sgap, total_recv, sdtype, local_root,
                                        MCA_COLL_BASE_TAG_GATHER, MCA_PML_BASE_SEND_STANDARD,
                                        comm));
            }
            if ((rank == local_root) && (rank != i) && (i_sg != root_sg)) {
                size_t recv_amt = (i + sg_cnt > size) ? rcount * (size - i) : rcount * sg_cnt;
                MPI_Aint rcv_ofst = rextent * (ptrdiff_t) (rcount * (i - startn));

                err = MCA_PML_CALL(recv(wkg + (ptrdiff_t) rcv_ofst, recv_amt, rdtype, i,
                                        MCA_COLL_BASE_TAG_GATHER, comm, &status));
                total_recv += recv_amt;
            }
            if (MPI_SUCCESS != err) {
                if (NULL != workbuf) {
                    coll_acoll_buf_free(reserve_mem_gather, workbuf);
                }
                return err;
            }
        }
    }

    /* All local roots ranks send to root */
    if (node_cnt < size && num_nodes > 1) {
        for (i = 0; i < size; i += node_cnt) {
            int i_node = i / node_cnt;
            if ((rank != root) && (rank == i) && is_base) {
                err = MCA_PML_CALL(send(workbuf - sgap, total_recv, sdtype, root,
                                        MCA_COLL_BASE_TAG_GATHER, MCA_PML_BASE_SEND_STANDARD,
                                        comm));
            }
            if ((rank == root) && (rank != i) && (i_node != root_node)) {
                size_t recv_amt = (i + node_cnt > size) ? rcount * (size - i) : rcount * node_cnt;
                MPI_Aint rcv_ofst = rextent * (ptrdiff_t) (rcount * i);

                err = MCA_PML_CALL(recv((char *) rbuf + (ptrdiff_t) rcv_ofst, recv_amt, rdtype, i,
                                        MCA_COLL_BASE_TAG_GATHER, comm, &status));
                total_recv += recv_amt;
            }
            if (MPI_SUCCESS != err) {
                if (NULL != workbuf) {
                    coll_acoll_buf_free(reserve_mem_gather, workbuf);
                }
                return err;
            }
        }
    }

    if (NULL != workbuf) {
        coll_acoll_buf_free(reserve_mem_gather, workbuf);
    }

    /* All done */
    return MPI_SUCCESS;
}
