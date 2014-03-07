/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
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
    
    rc = mpi_module->comm->c_coll.coll_barrier(mpi_module->comm, mpi_module->comm->c_coll.coll_barrier_module);
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
#ifdef INCOMPATIBLE_SHMEM_OMPI_COLL_APIS
    if (INT_MAX < nlong) {
        MPI_COLL_VERBOSE(20,"RUNNING FALLBACK BCAST");
        PREVIOUS_SCOLL_FN(mpi_module, broadcast, group,
                PE_root,
                target,
                source,
                nlong,
                pSync,
                SCOLL_DEFAULT_ALG);
        return rc;
    } 
    rc = mpi_module->comm->c_coll.coll_bcast(buf, (int)nlong, dtype, root, mpi_module->comm, mpi_module->comm->c_coll.coll_bcast_module);
#else
    rc = mpi_module->comm->c_coll.coll_bcast(buf, nlong, dtype, root, mpi_module->comm, mpi_module->comm->c_coll.coll_bcast_module);
#endif
    if (OMPI_SUCCESS != rc){
        MPI_COLL_VERBOSE(20,"RUNNING FALLBACK BCAST");
        PREVIOUS_SCOLL_FN(mpi_module, broadcast, group,
                PE_root,
                target,
                source,
                nlong,
                pSync,
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
    mca_scoll_mpi_module_t *mpi_module;
    ompi_datatype_t* stype;
    ompi_datatype_t* rtype;
    int rc;
    void *sbuf, *rbuf;
    MPI_COLL_VERBOSE(20,"RUNNING MPI ALLGATHER");
    mpi_module = (mca_scoll_mpi_module_t *) group->g_scoll.scoll_collect_module;
    if (nlong_type == true) {
        sbuf = (void *) source;
        rbuf = target;
        stype =  &ompi_mpi_char.dt;
        rtype =  &ompi_mpi_char.dt;
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
        rc = mpi_module->comm->c_coll.coll_allgather(sbuf, (int)nlong, stype, rbuf, (int)nlong, rtype, mpi_module->comm, mpi_module->comm->c_coll.coll_allgather_module);
#else
        rc = mpi_module->comm->c_coll.coll_allgather(sbuf, nlong, stype, rbuf, nlong, rtype, mpi_module->comm, mpi_module->comm->c_coll.coll_allgather_module);
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
        MPI_COLL_VERBOSE(20,"RUNNING FALLBACK COLLECT");
        PREVIOUS_SCOLL_FN(mpi_module, collect, group,
            target, 
            source, 
            nlong, 
            pSync, 
            nlong_type, 
            SCOLL_DEFAULT_ALG);
    }
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
    rc = mpi_module->comm->c_coll.coll_allreduce(sbuf, rbuf, (int)count, dtype, h_op, mpi_module->comm, mpi_module->comm->c_coll.coll_allreduce_module);
#else
    rc = mpi_module->comm->c_coll.coll_allreduce(sbuf, rbuf, count, dtype, h_op, mpi_module->comm, mpi_module->comm->c_coll.coll_allreduce_module);
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
