/*
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_spacc.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "opal/util/bit_ops.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_util.h"
#include "ompi/mca/pml/pml.h"

/*
 * mca_coll_spacc_alltoallv_intra_block
 *
 * Function:  Alltoallv using pairwise exchange algorithm
 * Accepts:   Same arguments as MPI_Alltoallv
 * Parameters: mca_coll_spacc_block_size
 * Returns:   MPI_SUCCESS or error code
 *
 * This algorithm posts all irecvs and isends in comm_size / block_size steps
 * and takes into account zero values in scounts[] and rcounts[].
 *
 * Limitations:
 *   for MPI_IN_PLACE we switch to the linear algorithm
 *   intra-communicators only
 *
 * Memory requirements (per process):
 *   2 * block_size * sizeof(request)
 */
int mca_coll_spacc_alltoallv_intra_block(
    const void *sbuf, const int *scounts, const int *sdisps,
    struct ompi_datatype_t *sdtype, void *rbuf, const int *rcounts,
    const int *rdisps, struct ompi_datatype_t *rdtype,
    struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    int i, j, nops, comm_size, rank, remote, err, nreqs;
    int block_size = mca_coll_spacc_alltoallv_block_size;
    char *psend, *precv;
    ptrdiff_t sdtype_ext, rdtype_ext;
    ompi_request_t **reqs;

    if (MPI_IN_PLACE == sbuf) {
        return mca_coll_base_alltoallv_intra_basic_inplace(rbuf, rcounts, rdisps,
                                                           rdtype, comm, module);
    }

    comm_size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    opal_output_verbose(30, mca_coll_spacc_stream,
                        "coll:spacc:alltoallv: rank %d/%d", rank, comm_size);

    ompi_datatype_type_extent(sdtype, &sdtype_ext);
    ompi_datatype_type_extent(rdtype, &rdtype_ext);

    if (0 != scounts[rank]) {
        psend = (char *)sbuf + (ptrdiff_t)sdisps[rank] * sdtype_ext;
        precv = (char *)rbuf + (ptrdiff_t)rdisps[rank] * rdtype_ext;
        err = ompi_datatype_sndrcv(psend, scounts[rank], sdtype,
                                   precv, rcounts[rank], rdtype);
        if (MPI_SUCCESS != err)
            return err;
    }

    if (1 == comm_size)
        return MPI_SUCCESS;

    if (block_size <= 0 || block_size > comm_size) {
        opal_output_verbose(30, mca_coll_spacc_stream,
                            "coll:spacc:alltoallv: invalid block_size %d, replace to %d",
                            block_size, comm_size);
        block_size = comm_size;
    }
    reqs = malloc(sizeof(*reqs) * 2 * block_size);
    if (NULL == reqs) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto err_hndl;
    }

    for (i = 0; i < comm_size; i += block_size) {
        nreqs = 0;
        nops = comm_size - i < block_size ? comm_size - i : block_size;
        /* Post nops irecvs and isends */
        for (j = 0; j < nops; j++) {
            remote = (rank + i + j) % comm_size;
            if (rcounts[remote] <= 0 || rank == remote)
                continue;
            precv = ((char *)rbuf) + (ptrdiff_t)rdisps[remote] * rdtype_ext;
            err = MCA_PML_CALL(irecv_init(precv, rcounts[remote], rdtype,
                                          remote, MCA_COLL_BASE_TAG_ALLTOALLV,
                                          comm, &reqs[nreqs]));
            if (MPI_SUCCESS != err) { goto err_hndl; }
            nreqs++;
        }
        for (j = 0; j < nops; j++) {
            remote = (rank - i - j + comm_size) % comm_size;
            if (scounts[remote] <= 0 || rank == remote)
                continue;
            psend = ((char *)sbuf) + (ptrdiff_t)sdisps[remote] * sdtype_ext;
            err = MCA_PML_CALL(isend_init(psend, scounts[remote], sdtype,
                                      remote, MCA_COLL_BASE_TAG_ALLTOALLV,
                                      MCA_PML_BASE_SEND_STANDARD, comm,
                                      &reqs[nreqs]));
            if (MPI_SUCCESS != err) { goto err_hndl; }
            nreqs++;
        }
        MCA_PML_CALL(start(nreqs, reqs));
        err = ompi_request_wait_all(nreqs, reqs, MPI_STATUSES_IGNORE);
    }

 err_hndl:
    free(reqs);
    return err;
}
