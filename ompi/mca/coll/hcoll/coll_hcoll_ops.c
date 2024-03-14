/**
  Copyright (c) 2011      Mellanox Technologies. All rights reserved.
  Copyright (c) 2015      Research Organization for Information Science
                          and Technology (RIST). All rights reserved.
  Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "coll_hcoll.h"
#include "hcoll/api/hcoll_constants.h"
#include "coll_hcoll_dtypes.h"
#include "hcoll/api/hcoll_dte.h"
int mca_coll_hcoll_barrier(struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module){
    int rc;
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    HCOL_VERBOSE(20,"RUNNING HCOL BARRIER");

    if (OPAL_UNLIKELY(ompi_mpi_state >= OMPI_MPI_STATE_FINALIZE_STARTED)) {
        HCOL_VERBOSE(5, "In finalize, reverting to previous barrier");
        goto orig_barrier;
    }
    rc = hcoll_collectives.coll_barrier(hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK BARRIER");
        rc = hcoll_module->previous_barrier(comm,hcoll_module->previous_barrier_module);
    }
    return rc;
orig_barrier:
    return hcoll_module->previous_barrier(comm,hcoll_module->previous_barrier_module);
}

int mca_coll_hcoll_bcast(void *buff, int count,
                        struct ompi_datatype_t *datatype, int root,
                        struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module)
{
    dte_data_representation_t dtype;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL BCAST");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    dtype = ompi_dtype_2_hcoll_dtype(datatype, TRY_FIND_DERIVED);

    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(dtype))) {
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: %s; calling fallback bcast;",datatype->super.name);
        rc = hcoll_module->previous_bcast(buff,count,datatype,root,
                                         comm,hcoll_module->previous_bcast_module);
        return rc;
    }
    rc = hcoll_collectives.coll_bcast(buff,count,dtype,root,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK BCAST");
        rc = hcoll_module->previous_bcast(buff,count,datatype,root,
                                         comm,hcoll_module->previous_bcast_module);
    }
    return rc;
}

int mca_coll_hcoll_allgather(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL ALLGATHER");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    stype = ompi_dtype_2_hcoll_dtype(sdtype, TRY_FIND_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, TRY_FIND_DERIVED);
    if (sbuf == MPI_IN_PLACE) {
        stype = rtype;
    }
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback allgather;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_allgather(sbuf,scount,sdtype,
                                             rbuf,rcount,rdtype,
                                             comm,
                                             hcoll_module->previous_allgather_module);
        return rc;
    }
    rc = hcoll_collectives.coll_allgather((void *)sbuf,scount,stype,rbuf,rcount,rtype,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK ALLGATHER");
        rc = hcoll_module->previous_allgather(sbuf,scount,sdtype,
                                             rbuf,rcount,rdtype,
                                             comm,
                                             hcoll_module->previous_allgather_module);
    }
    return rc;
}

int mca_coll_hcoll_allgatherv(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, const int *rcount,
                            const int *displs,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL ALLGATHERV");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    stype = ompi_dtype_2_hcoll_dtype(sdtype, NO_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback allgatherv;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_allgatherv(sbuf,scount,sdtype,
                                             rbuf,rcount,
                                             displs, 
                                             rdtype,
                                             comm,
                                             hcoll_module->previous_allgatherv_module);
        return rc;
    }
    rc = hcoll_collectives.coll_allgatherv((void *)sbuf,scount,stype,rbuf,(int*)rcount,
                                           (int*)displs,rtype,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK ALLGATHERV");
        rc = hcoll_module->previous_allgatherv(sbuf,scount,sdtype,
                                             rbuf,rcount,
                                             displs, 
                                             rdtype,
                                             comm,
                                             hcoll_module->previous_allgatherv_module);
    }
    return rc;
}

int mca_coll_hcoll_gather(const void *sbuf, int scount,
                          struct ompi_datatype_t *sdtype,
                          void *rbuf, int rcount,
                          struct ompi_datatype_t *rdtype,
                          int root,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module)
{
    mca_coll_hcoll_module_t  *hcoll_module = (mca_coll_hcoll_module_t*)module;
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;

    HCOL_VERBOSE(20,"RUNNING HCOL GATHER");

    if (root != comm->c_my_rank) {
        rdtype = sdtype;
    }

    stype = ompi_dtype_2_hcoll_dtype(sdtype, NO_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback gather;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_gather(sbuf,scount,sdtype,
                                           rbuf,rcount,rdtype,root,
                                           comm,
                                           hcoll_module->previous_allgather_module);
        return rc;
    }
    rc = hcoll_collectives.coll_gather((void *)sbuf,scount,stype,rbuf,rcount,rtype,root,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK GATHER");
        rc = hcoll_module->previous_gather((void *)sbuf,scount,sdtype,
                                              rbuf,rcount,rdtype,root,
                                              comm,
                                              hcoll_module->previous_allgather_module);
    }
    return rc;

}

int mca_coll_hcoll_allreduce(const void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t Dtype;
    hcoll_dte_op_t *Op;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL ALLREDUCE");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    Dtype = ompi_dtype_2_hcoll_dtype(dtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(Dtype))){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: dtype = %s; calling fallback allreduce;",
                     dtype->super.name);
        rc = hcoll_module->previous_allreduce(sbuf,rbuf,
                                             count,dtype,op,
                                             comm, hcoll_module->previous_allreduce_module);
        return rc;
    }

    Op = ompi_op_2_hcolrte_op(op);
    if (OPAL_UNLIKELY(HCOL_DTE_OP_NULL == Op->id)){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"ompi_op_t is not supported: op = %s; calling fallback allreduce;",
                     op->o_name);
        rc = hcoll_module->previous_allreduce(sbuf,rbuf,
                                             count,dtype,op,
                                             comm, hcoll_module->previous_allreduce_module);
        return rc;
    }

    rc = hcoll_collectives.coll_allreduce((void *)sbuf,rbuf,count,Dtype,Op,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK ALLREDUCE");
        rc = hcoll_module->previous_allreduce(sbuf,rbuf,
                                             count,dtype,op,
                                             comm, hcoll_module->previous_allreduce_module);
    }
    return rc;
}

int mca_coll_hcoll_reduce(const void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            int root,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t Dtype;
    hcoll_dte_op_t *Op;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL REDUCE");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    Dtype = ompi_dtype_2_hcoll_dtype(dtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(Dtype))){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: dtype = %s; calling fallback reduce;",
                     dtype->super.name);
        rc = hcoll_module->previous_reduce(sbuf,rbuf,
                                             count,dtype,op,
                                             root,
                                             comm, hcoll_module->previous_reduce_module);
        return rc;
    }

    Op = ompi_op_2_hcolrte_op(op);
    if (OPAL_UNLIKELY(HCOL_DTE_OP_NULL == Op->id)){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"ompi_op_t is not supported: op = %s; calling fallback reduce;",
                     op->o_name);
        rc = hcoll_module->previous_reduce(sbuf,rbuf,
                                             count,dtype,op,
                                             root,
                                             comm, hcoll_module->previous_reduce_module);
        return rc;
    }

    rc = hcoll_collectives.coll_reduce((void *)sbuf,rbuf,count,Dtype,Op,root,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK REDUCE");
        rc = hcoll_module->previous_reduce(sbuf,rbuf,
                                             count,dtype,op,
                                             root,
                                             comm, hcoll_module->previous_reduce_module);
    }
    return rc;
}

int mca_coll_hcoll_alltoall(const void *sbuf, int scount,
                           struct ompi_datatype_t *sdtype,
                           void* rbuf, int rcount,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL ALLTOALL");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    stype = ompi_dtype_2_hcoll_dtype(sdtype, NO_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback alltoall;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_alltoall(sbuf,scount,sdtype,
                                            rbuf,rcount,rdtype,
                                            comm,
                                            hcoll_module->previous_alltoall_module);
        return rc;
    }
    rc = hcoll_collectives.coll_alltoall((void *)sbuf,scount,stype,rbuf,rcount,rtype,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK ALLTOALL");
        rc = hcoll_module->previous_alltoall(sbuf,scount,sdtype,
                                            rbuf,rcount,rdtype,
                                            comm,
                                            hcoll_module->previous_alltoall_module);
    }
    return rc;
}

int mca_coll_hcoll_alltoallv(const void *sbuf, const int *scounts, const int *sdisps,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, const int *rcounts, const int *rdisps,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL ALLTOALLV");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    stype = ompi_dtype_2_hcoll_dtype(sdtype, NO_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback alltoallv;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_alltoallv(sbuf, scounts, sdisps, sdtype,
                                            rbuf, rcounts, rdisps, rdtype,
                                            comm, hcoll_module->previous_alltoallv_module);
        return rc;
    }
    rc = hcoll_collectives.coll_alltoallv((void *)sbuf, (int *)scounts, (int *)sdisps, stype,
                                            rbuf, (int *)rcounts, (int *)rdisps, rtype,
                                                hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK ALLTOALLV");
        rc = hcoll_module->previous_alltoallv(sbuf, scounts, sdisps, sdtype,
                                            rbuf, rcounts, rdisps, rdtype,
                                            comm, hcoll_module->previous_alltoallv_module);
    }
    return rc;
}

int mca_coll_hcoll_gatherv(const void* sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void* rbuf, const int *rcounts, const int *displs,
                            struct ompi_datatype_t *rdtype,
                            int root,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    mca_coll_hcoll_module_t  *hcoll_module = (mca_coll_hcoll_module_t*)module;
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL GATHERV");

    if (root != comm->c_my_rank) {
        rdtype = sdtype;
    }

    stype = ompi_dtype_2_hcoll_dtype(sdtype, NO_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, NO_DERIVED);

    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback gatherv;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_gatherv(sbuf,scount,sdtype,
                                           rbuf, rcounts, displs, rdtype,root,
                                           comm, hcoll_module->previous_gatherv_module);
        return rc;
    }
    rc = hcoll_collectives.coll_gatherv((void *)sbuf, scount, stype, rbuf,
                                        (int *)rcounts, (int *)displs, rtype,
                                        root, hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK GATHERV");
        rc = hcoll_module->previous_gatherv(sbuf,scount,sdtype,
                                           rbuf, rcounts, displs, rdtype,root,
                                           comm, hcoll_module->previous_igatherv_module);
    }
    return rc;

}

int mca_coll_hcoll_scatterv(const void* sbuf, const int *scounts, const int *displs,
                            struct ompi_datatype_t *sdtype,
                            void* rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            int root,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    mca_coll_hcoll_module_t  *hcoll_module = (mca_coll_hcoll_module_t*)module;
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;

    HCOL_VERBOSE(20,"RUNNING HCOL SCATTERV");

    if (root != comm->c_my_rank) {
        sdtype = rdtype;
    }

    stype = ompi_dtype_2_hcoll_dtype(sdtype, NO_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, NO_DERIVED);

    if (rbuf == MPI_IN_PLACE) {
        assert(root == comm->c_my_rank);
        rtype = stype;
    }
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback scatterv;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_scatterv(sbuf, scounts, displs, sdtype,
                                           rbuf, rcount, rdtype, root,
                                           comm, hcoll_module->previous_scatterv_module);
        return rc;
    }
    rc = hcoll_collectives.coll_scatterv((void *)sbuf, (int *)scounts, (int *)displs, stype, rbuf, rcount, rtype, root, hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK SCATTERV");
        rc = hcoll_module->previous_scatterv(sbuf, scounts, displs, sdtype,
                                           rbuf, rcount, rdtype, root,
                                           comm, hcoll_module->previous_scatterv_module);
    }
    return rc;
}

int mca_coll_hcoll_ibarrier(struct ompi_communicator_t *comm,
                            ompi_request_t ** request,
                            mca_coll_base_module_t *module)
{
    int rc;
    void** rt_handle;
    HCOL_VERBOSE(20,"RUNNING HCOL NON-BLOCKING BARRIER");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    rt_handle = (void**) request;
    rc = hcoll_collectives.coll_ibarrier(hcoll_module->hcoll_context, rt_handle);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK NON-BLOCKING BARRIER");
        rc = hcoll_module->previous_ibarrier(comm, request, hcoll_module->previous_ibarrier_module);
    }
    return rc;
}

int mca_coll_hcoll_ibcast(void *buff, int count,
                        struct ompi_datatype_t *datatype, int root,
                        struct ompi_communicator_t *comm,
                        ompi_request_t ** request,
                        mca_coll_base_module_t *module)
{
    dte_data_representation_t dtype;
    int rc;
    void** rt_handle;
    HCOL_VERBOSE(20,"RUNNING HCOL NON-BLOCKING BCAST");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    rt_handle = (void**) request;
    dtype = ompi_dtype_2_hcoll_dtype(datatype, TRY_FIND_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(dtype))){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: %s; calling fallback non-blocking bcast;",datatype->super.name);
        rc = hcoll_module->previous_ibcast(buff,count,datatype,root,
                                         comm, request, hcoll_module->previous_ibcast_module);
        return rc;
    }
    rc = hcoll_collectives.coll_ibcast(buff, count, dtype, root, rt_handle, hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK NON-BLOCKING BCAST");
        rc = hcoll_module->previous_ibcast(buff,count,datatype,root,
                                         comm, request, hcoll_module->previous_ibcast_module);
    }
    return rc;
}

int mca_coll_hcoll_iallgather(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t ** request,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    void** rt_handle;
    HCOL_VERBOSE(20,"RUNNING HCOL NON-BLOCKING ALLGATHER");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    rt_handle = (void**) request;
    stype = ompi_dtype_2_hcoll_dtype(sdtype, TRY_FIND_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, TRY_FIND_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback non-blocking allgather;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_iallgather(sbuf,scount,sdtype,
                                             rbuf,rcount,rdtype,
                                             comm,
                                             request,
                                             hcoll_module->previous_iallgather_module);
        return rc;
    }
    rc = hcoll_collectives.coll_iallgather((void *)sbuf, scount, stype, rbuf, rcount, rtype, hcoll_module->hcoll_context, rt_handle);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK NON-BLOCKING ALLGATHER");
        rc = hcoll_module->previous_iallgather(sbuf,scount,sdtype,
                                             rbuf,rcount,rdtype,
                                             comm,
                                             request,
                                             hcoll_module->previous_iallgather_module);
    }
    return rc;
}
#if HCOLL_API >= HCOLL_VERSION(3,5)
int mca_coll_hcoll_iallgatherv(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, const int *rcount,
                            const int *displs,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t ** request,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL NON-BLOCKING ALLGATHERV");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    stype = ompi_dtype_2_hcoll_dtype(sdtype, NO_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, NO_DERIVED);
    void **rt_handle = (void **) request;
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback non-blocking allgatherv;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_iallgatherv(sbuf,scount,sdtype,
                                             rbuf,rcount,
                                             displs, 
                                             rdtype,
                                             comm,
                                             request,
                                             hcoll_module->previous_iallgatherv_module);
        return rc;
    }
    rc = hcoll_collectives.coll_iallgatherv((void *)sbuf,scount,stype,rbuf,(int*)rcount,(int*)displs,rtype,
            hcoll_module->hcoll_context, rt_handle);
    if (HCOLL_SUCCESS != rc){
       HCOL_VERBOSE(20,"RUNNING FALLBACK NON-BLOCKING ALLGATHER");
        rc = hcoll_module->previous_iallgatherv(sbuf,scount,sdtype,
                                             rbuf,rcount,
                                             displs, 
                                             rdtype,
                                             comm,
                                             request,
                                             hcoll_module->previous_iallgatherv_module);
    }
    return rc;
}
#endif
int mca_coll_hcoll_iallreduce(const void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            struct ompi_communicator_t *comm,
                            ompi_request_t ** request,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t Dtype;
    hcoll_dte_op_t *Op;
    int rc;
    void** rt_handle;
    HCOL_VERBOSE(20,"RUNNING HCOL NON-BLOCKING ALLREDUCE");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    rt_handle = (void**) request;
    Dtype = ompi_dtype_2_hcoll_dtype(dtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(Dtype))){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: dtype = %s; calling fallback non-blocking allreduce;",
                     dtype->super.name);
        rc = hcoll_module->previous_iallreduce(sbuf,rbuf,
                                             count,dtype,op,
                                             comm, request, hcoll_module->previous_iallreduce_module);
        return rc;
    }

    Op = ompi_op_2_hcolrte_op(op);
    if (OPAL_UNLIKELY(HCOL_DTE_OP_NULL == Op->id)){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"ompi_op_t is not supported: op = %s; calling fallback non-blocking allreduce;",
                     op->o_name);
        rc = hcoll_module->previous_iallreduce(sbuf,rbuf,
                                             count,dtype,op,
                                             comm, request, hcoll_module->previous_iallreduce_module);
        return rc;
    }

    rc = hcoll_collectives.coll_iallreduce((void *)sbuf, rbuf, count, Dtype, Op, hcoll_module->hcoll_context, rt_handle);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK NON-BLOCKING ALLREDUCE");
        rc = hcoll_module->previous_iallreduce(sbuf,rbuf,
                                             count,dtype,op,
                                             comm, request, hcoll_module->previous_iallreduce_module);
    }
    return rc;
}
#if HCOLL_API >= HCOLL_VERSION(3,5)
int mca_coll_hcoll_ireduce(const void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            int root,
                            struct ompi_communicator_t *comm,
                            ompi_request_t ** request,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t Dtype;
    hcoll_dte_op_t *Op;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL NON-BLOCKING REDUCE");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    Dtype = ompi_dtype_2_hcoll_dtype(dtype, NO_DERIVED);
    void **rt_handle = (void**) request;
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(Dtype))){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: dtype = %s; calling fallback non-blocking reduce;",
                     dtype->super.name);
        rc = hcoll_module->previous_ireduce(sbuf,rbuf,count,dtype,op,
                                             root,
                                             comm, request, 
                                             hcoll_module->previous_ireduce_module);
        return rc;
    }

    Op = ompi_op_2_hcolrte_op(op);
    if (OPAL_UNLIKELY(HCOL_DTE_OP_NULL == Op->id)){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"ompi_op_t is not supported: op = %s; calling fallback non-blocking reduce;",
                     op->o_name);
        rc = hcoll_module->previous_ireduce(sbuf,rbuf,
                                             count,dtype,op,
                                             root,
                                             comm, request,
                                             hcoll_module->previous_ireduce_module);
        return rc;
    }

    rc = hcoll_collectives.coll_ireduce((void *)sbuf,rbuf,count,Dtype,Op,root,hcoll_module->hcoll_context,rt_handle);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK NON-BLOCKING REDUCE");
        rc = hcoll_module->previous_ireduce(sbuf,rbuf,
                                             count,dtype,op,
                                             root,
                                             comm, 
                                             request,
                                             hcoll_module->previous_ireduce_module);
    }
    return rc;
}
#endif
int mca_coll_hcoll_igatherv(const void* sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void* rbuf, const int *rcounts, const int *displs,
                            struct ompi_datatype_t *rdtype,
                            int root,
                            struct ompi_communicator_t *comm,
                            ompi_request_t ** request,
                            mca_coll_base_module_t *module)
{
    mca_coll_hcoll_module_t  *hcoll_module = (mca_coll_hcoll_module_t*)module;
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    void** rt_handle;

    HCOL_VERBOSE(20,"RUNNING HCOL IGATHERV");

    rt_handle = (void**) request;

    if (root != comm->c_my_rank) {
        rdtype = sdtype;
    }

    stype = ompi_dtype_2_hcoll_dtype(sdtype, NO_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback igatherv;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_igatherv(sbuf,scount,sdtype,
                                           rbuf, rcounts, displs, rdtype,root,
                                           comm, request,
                                           hcoll_module->previous_igatherv_module);
        return rc;
    }
    rc = hcoll_collectives.coll_igatherv((void *)sbuf, scount, stype, rbuf, (int *)rcounts, (int *)displs, rtype, root, hcoll_module->hcoll_context, rt_handle);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK IGATHERV");
        rc = hcoll_module->previous_igatherv(sbuf,scount,sdtype,
                                           rbuf, rcounts, displs, rdtype,root,
                                           comm, request,
                                           hcoll_module->previous_igatherv_module);
    }
    return rc;

}


#if HCOLL_API >= HCOLL_VERSION(3,7)
int mca_coll_hcoll_ialltoallv(const void *sbuf, const int *scounts, const int *sdisps,
                              struct ompi_datatype_t *sdtype,
                              void *rbuf, const int *rcounts, const int *rdisps,
                              struct ompi_datatype_t *rdtype,
                              struct ompi_communicator_t *comm,
                              ompi_request_t ** request,
                              mca_coll_base_module_t *module)
{
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL IALLTOALLV");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    stype = ompi_dtype_2_hcoll_dtype(sdtype, NO_DERIVED);
    rtype = ompi_dtype_2_hcoll_dtype(rdtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype))) {
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: sdtype = %s, rdtype = %s; calling fallback ialltoallv;",
                     sdtype->super.name,
                     rdtype->super.name);
        rc = hcoll_module->previous_ialltoallv(sbuf, scounts, sdisps, sdtype,
                                               rbuf, rcounts, rdisps, rdtype,
                                               comm, request, hcoll_module->previous_alltoallv_module);
        return rc;
    }
    rc = hcoll_collectives.coll_ialltoallv((void *)sbuf, (int *)scounts, (int *)sdisps, stype,
                                           rbuf, (int *)rcounts, (int *)rdisps, rtype,
                                           hcoll_module->hcoll_context, (void**)request);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK IALLTOALLV");
        rc = hcoll_module->previous_ialltoallv(sbuf, scounts, sdisps, sdtype,
                                               rbuf, rcounts, rdisps, rdtype,
                                               comm, request, hcoll_module->previous_alltoallv_module);
    }
    return rc;
}
#endif

#if HCOLL_API > HCOLL_VERSION(4,5)
int mca_coll_hcoll_reduce_scatter_block(const void *sbuf, void *rbuf, int rcount,
                                        struct ompi_datatype_t *dtype,
                                        struct ompi_op_t *op,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module) {
    dte_data_representation_t Dtype;
    hcoll_dte_op_t *Op;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL REDUCE SCATTER BLOCK");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    Dtype = ompi_dtype_2_hcoll_dtype(dtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(Dtype))){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: dtype = %s; calling fallback allreduce;",
                     dtype->super.name);
        goto fallback;
    }

    Op = ompi_op_2_hcolrte_op(op);
    if (OPAL_UNLIKELY(HCOL_DTE_OP_NULL == Op->id)){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"ompi_op_t is not supported: op = %s; calling fallback allreduce;",
                     op->o_name);
        goto fallback;
    }

    rc = hcoll_collectives.coll_reduce_scatter_block((void *)sbuf,rbuf,rcount,Dtype,Op,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
    fallback:        
        HCOL_VERBOSE(20,"RUNNING FALLBACK ALLREDUCE");
        rc = hcoll_module->previous_reduce_scatter_block(sbuf,rbuf,
                                             rcount,dtype,op,
                                             comm, hcoll_module->previous_allreduce_module);
    }
    return rc;
}

int mca_coll_hcoll_reduce_scatter(const void *sbuf, void *rbuf, const int* rcounts,
                                  struct ompi_datatype_t *dtype,
                                  struct ompi_op_t *op,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module) {
    dte_data_representation_t Dtype;
    hcoll_dte_op_t *Op;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL REDUCE SCATTER");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    Dtype = ompi_dtype_2_hcoll_dtype(dtype, NO_DERIVED);
    if (OPAL_UNLIKELY(HCOL_DTE_IS_ZERO(Dtype))){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"Ompi_datatype is not supported: dtype = %s; calling fallback allreduce;",
                     dtype->super.name);
        goto fallback;
    }

    Op = ompi_op_2_hcolrte_op(op);
    if (OPAL_UNLIKELY(HCOL_DTE_OP_NULL == Op->id)){
        /*If we are here then datatype is not simple predefined datatype */
        /*In future we need to add more complex mapping to the dte_data_representation_t */
        /* Now use fallback */
        HCOL_VERBOSE(20,"ompi_op_t is not supported: op = %s; calling fallback allreduce;",
                     op->o_name);
        goto fallback;
    }

    rc = hcoll_collectives.coll_reduce_scatter((void*)sbuf, rbuf, (int*)rcounts,
                                               Dtype, Op, hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
    fallback:        
        HCOL_VERBOSE(20,"RUNNING FALLBACK ALLREDUCE");
        rc = hcoll_module->previous_reduce_scatter(sbuf,rbuf,
                                             rcounts,dtype,op,
                                             comm, hcoll_module->previous_allreduce_module);
    }
    return rc;
}
#endif
