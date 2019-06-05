/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  Copyright (c) 2017      IBM Corporation.  All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "scoll_mpi.h"
#include "scoll_mpi_dtypes.h"

#define INCOMPATIBLE_SHMEM_OMPI_COLL_APIS 1

int mca_scoll_mpi_barrier(struct oshmem_group_t *group, long *pSync, int alg)
{
    mca_scoll_mpi_module_t *mpi_module;
    int rc;
    MPI_COLL_VERBOSE(20,"RUNNING MPI BARRIER");
    mpi_module = (mca_scoll_mpi_module_t *) group->g_scoll.scoll_barrier_module;

    rc = mpi_module->comm->c_coll->coll_barrier(mpi_module->comm, mpi_module->comm->c_coll->coll_barrier_module);
    if (OMPI_SUCCESS != rc){
        MPI_COLL_VERBOSE(20,"RUNNING FALLBACK BARRIER");
        PREVIOUS_SCOLL_FN(mpi_module, barrier, group,
                pSync,
                SCOLL_DEFAULT_ALG);
    }
    return rc;
}

int mca_scoll_mpi_broadcast(struct oshmem_group_t *group,
                            int PE_root,
                            void *target,
                            const void *source,
                            size_t nlong,
                            long *pSync,
                            bool nlong_type,
                            int alg)
{
    mca_scoll_mpi_module_t *mpi_module;
    ompi_datatype_t* dtype;
    int rc;
    void* buf;
    int root;
    MPI_COLL_VERBOSE(20,"RUNNING MPI BCAST");
    mpi_module = (mca_scoll_mpi_module_t *) group->g_scoll.scoll_broadcast_module;
    if (group->my_pe == PE_root) {
        buf = (void *) source;
    } else {
        buf = target;
    }
    dtype = &ompi_mpi_char.dt;
    root = oshmem_proc_group_find_id(group, PE_root);
    /* Open SHMEM specification has the following constrains (page 85):
     * "If using C/C++, nelems must be of type integer. If you are using Fortran, it must be a
     *  default integer value". And also fortran signature says "INTEGER".
     *  Since ompi coll components doesn't support size_t at the moment,
     *  and considering this contradiction, we cast size_t to int here
     *  in case if the value is less than INT_MAX and fallback to previous module otherwise. */
    if (OPAL_UNLIKELY(!nlong_type || (INT_MAX < nlong))) {
#ifdef INCOMPATIBLE_SHMEM_OMPI_COLL_APIS
        MPI_COLL_VERBOSE(20,"RUNNING FALLBACK BCAST");
        PREVIOUS_SCOLL_FN(mpi_module, broadcast, group,
                PE_root,
                target,
                source,
                nlong,
                pSync,
                nlong_type,
                SCOLL_DEFAULT_ALG);
        return rc;
#else
        MPI_COLL_ERROR(20, "variable broadcast length, or exceeds INT_MAX: %zu", nlong);
        return OSHMEM_ERR_NOT_SUPPORTED;
#endif
    }

    /* Do nothing on zero-length request */
    if (OPAL_UNLIKELY(!nlong)) {
        return OSHMEM_SUCCESS;
    }

    rc = mpi_module->comm->c_coll->coll_bcast(buf, nlong, dtype, root, mpi_module->comm, mpi_module->comm->c_coll->coll_bcast_module);
    if (OMPI_SUCCESS != rc){
        MPI_COLL_VERBOSE(20,"RUNNING FALLBACK BCAST");
        PREVIOUS_SCOLL_FN(mpi_module, broadcast, group,
                PE_root,
                target,
                source,
                nlong,
                pSync,
                nlong_type,
                SCOLL_DEFAULT_ALG);
    }
    return rc;
}

int mca_scoll_mpi_collect(struct oshmem_group_t *group,
                          void *target,
                          const void *source,
                          size_t nlong,
                          long *pSync,
                          bool nlong_type,
                          int alg)
{
    ompi_datatype_t* stype = &ompi_mpi_char.dt;
    ompi_datatype_t* rtype = &ompi_mpi_char.dt;
    mca_scoll_mpi_module_t *mpi_module;
    int rc;
    int len;
    int i;
    void *sbuf, *rbuf;
    int *disps, *recvcounts;
    MPI_COLL_VERBOSE(20,"RUNNING MPI ALLGATHER");
    mpi_module = (mca_scoll_mpi_module_t *) group->g_scoll.scoll_collect_module;

    if (nlong_type == true) {
        /* Do nothing on zero-length request */
        if (OPAL_UNLIKELY(!nlong)) {
            return OSHMEM_SUCCESS;
        }

        sbuf = (void *) source;
        rbuf = target;
        /* Open SHMEM specification has the following constrains (page 85):
         * "If using C/C++, nelems must be of type integer. If you are using Fortran, it must be a
         *  default integer value". And also fortran signature says "INTEGER".
         *  Since ompi coll components doesn't support size_t at the moment,
         *  and considering this contradiction, we cast size_t to int here
         *  in case if the value is less than INT_MAX and fallback to previous module otherwise. */
#ifdef INCOMPATIBLE_SHMEM_OMPI_COLL_APIS
        if (INT_MAX < nlong) {
            MPI_COLL_VERBOSE(20,"RUNNING FALLBACK COLLECT");
            PREVIOUS_SCOLL_FN(mpi_module, collect, group,
                    target,
                    source,
                    nlong,
                    pSync,
                    nlong_type,
                    SCOLL_DEFAULT_ALG);
            return rc;
        }
        rc = mpi_module->comm->c_coll->coll_allgather(sbuf, (int)nlong, stype, rbuf, (int)nlong, rtype, mpi_module->comm, mpi_module->comm->c_coll->coll_allgather_module);
#else
        rc = mpi_module->comm->c_coll->coll_allgather(sbuf, nlong, stype, rbuf, nlong, rtype, mpi_module->comm, mpi_module->comm->c_coll->coll_allgather_module);
#endif
        if (OMPI_SUCCESS != rc){
            MPI_COLL_VERBOSE(20,"RUNNING FALLBACK FCOLLECT");
            PREVIOUS_SCOLL_FN(mpi_module, collect, group,
                    target,
                    source,
                    nlong,
                    pSync,
                    nlong_type,
                    SCOLL_DEFAULT_ALG);
        }
    } else {
        if (INT_MAX < nlong) {
            MPI_COLL_VERBOSE(20,"RUNNING FALLBACK COLLECT");
            PREVIOUS_SCOLL_FN(mpi_module, collect, group,
                              target,
                              source,
                              nlong,
                              pSync,
                              nlong_type,
                              SCOLL_DEFAULT_ALG);
            return rc;
        }

        len   = nlong;
        disps = malloc(group->proc_count * sizeof(*disps));
        if (disps == NULL) {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
            goto complete;
        }

        recvcounts = malloc(group->proc_count * sizeof(*recvcounts));
        if (recvcounts == NULL) {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
            goto failed_mem;
        }

        rc = mpi_module->comm->c_coll->coll_allgather(&len, sizeof(len), stype, recvcounts,
                                                      sizeof(len), rtype, mpi_module->comm,
                                                      mpi_module->comm->c_coll->coll_allgather_module);
        if (rc != OSHMEM_SUCCESS) {
            goto failed_allgather;
        }

        disps[0] = 0;
        for (i = 1; i < group->proc_count; i++) {
            disps[i] = disps[i - 1] + recvcounts[i - 1];
        }

        rc = mpi_module->comm->c_coll->coll_allgatherv(source, nlong, stype, target, recvcounts,
                                                       disps, rtype, mpi_module->comm,
                                                       mpi_module->comm->c_coll->coll_allgatherv_module);
failed_allgather:
        free(recvcounts);
failed_mem:
        free(disps);
    }
complete:
    return rc;
}


int mca_scoll_mpi_reduce(struct oshmem_group_t *group,
        struct oshmem_op_t *op,
        void *target,
        const void *source,
        size_t nlong,
        long *pSync,
        void *pWrk,
        int alg)
{
    mca_scoll_mpi_module_t *mpi_module;
    struct ompi_datatype_t* dtype;
    struct ompi_op_t *h_op;
    int rc;
    size_t count;
    MPI_COLL_VERBOSE(20,"RUNNING MPI REDUCE");
    void *sbuf, *rbuf;
    mpi_module = (mca_scoll_mpi_module_t *) group->g_scoll.scoll_reduce_module;
    sbuf = (void *) source;
    rbuf = target;
    dtype = shmem_dtype_to_ompi_dtype(op);
    h_op = shmem_op_to_ompi_op(op->op);
    count = nlong/op->dt_size;

    /* Do nothing on zero-length request */
    if (OPAL_UNLIKELY(!nlong)) {
        return OSHMEM_SUCCESS;
    }

    /* Open SHMEM specification has the following constrains (page 85):
     * "If using C/C++, nelems must be of type integer. If you are using Fortran, it must be a
     *  default integer value". And also fortran signature says "INTEGER".
     *  Since ompi coll components doesn't support size_t at the moment,
     *  and considering this contradiction, we cast size_t to int here
     *  in case if the value is less than INT_MAX and fallback to previous module otherwise. */
#ifdef INCOMPATIBLE_SHMEM_OMPI_COLL_APIS
    if (INT_MAX < count) {
        MPI_COLL_VERBOSE(20,"RUNNING FALLBACK REDUCE");
        PREVIOUS_SCOLL_FN(mpi_module, reduce, group,
                op,
                target,
                source,
                nlong,
                pSync,
                pWrk,
                SCOLL_DEFAULT_ALG);
        return rc;
    }
    rc = mpi_module->comm->c_coll->coll_allreduce(sbuf, rbuf, (int)count, dtype, h_op, mpi_module->comm, mpi_module->comm->c_coll->coll_allreduce_module);
#else
    rc = mpi_module->comm->c_coll->coll_allreduce(sbuf, rbuf, count, dtype, h_op, mpi_module->comm, mpi_module->comm->c_coll->coll_allreduce_module);
#endif
    if (OMPI_SUCCESS != rc){
        MPI_COLL_VERBOSE(20,"RUNNING FALLBACK REDUCE");
        PREVIOUS_SCOLL_FN(mpi_module, reduce, group,
                op,
                target,
                source,
                nlong,
                pSync,
                pWrk,
                SCOLL_DEFAULT_ALG);
    }
    return rc;
}
