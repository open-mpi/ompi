/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "coll_hcoll.h"
#include "hcoll_constants.h"
#include "coll_hcoll_dtypes.h"
#include "hcoll_dte.h"
int mca_coll_hcoll_barrier(struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module){
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL BARRIER");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    rc = hcoll_collectives.coll_barrier(hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK BARRIER");
        rc = hcoll_module->previous_barrier(comm,hcoll_module->previous_barrier_module);
    }
    return rc;
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
    dtype = ompi_dtype_2_dte_dtype(datatype);
    if (OPAL_UNLIKELY((HCOL_DTE_IS_ZERO(dtype) || HCOL_DTE_IS_COMPLEX(dtype)))
                        && mca_coll_hcoll_component.hcoll_datatype_fallback){
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

int mca_coll_hcoll_allgather(void *sbuf, int scount,
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
    stype = ompi_dtype_2_dte_dtype(sdtype);
    rtype = ompi_dtype_2_dte_dtype(rdtype);
    if (OPAL_UNLIKELY((HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype)
                        || HCOL_DTE_IS_COMPLEX(stype) || HCOL_DTE_IS_COMPLEX(rtype)))
                        && mca_coll_hcoll_component.hcoll_datatype_fallback){
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
    rc = hcoll_collectives.coll_allgather(sbuf,scount,stype,rbuf,rcount,rtype,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK ALLGATHER");
        rc = hcoll_module->previous_allgather(sbuf,scount,sdtype,
                                             rbuf,rcount,rdtype,
                                             comm,
                                             hcoll_module->previous_allgather_module);
    }
    return rc;
}

int mca_coll_hcoll_gather(void *sbuf, int scount,
                          struct ompi_datatype_t *sdtype,
                          void *rbuf, int rcount,
                          struct ompi_datatype_t *rdtype,
                          int root,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module){
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL GATHER");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    stype = ompi_dtype_2_dte_dtype(sdtype);
    rtype = ompi_dtype_2_dte_dtype(rdtype);
    if (OPAL_UNLIKELY((HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype)
                        || HCOL_DTE_IS_COMPLEX(stype) || HCOL_DTE_IS_COMPLEX(rtype)))
                        && mca_coll_hcoll_component.hcoll_datatype_fallback){
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
    rc = hcoll_collectives.coll_gather(sbuf,scount,stype,rbuf,rcount,rtype,root,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK GATHER");
        rc = hcoll_module->previous_gather(sbuf,scount,sdtype,
                                              rbuf,rcount,rdtype,root,
                                              comm,
                                              hcoll_module->previous_allgather_module);
    }
    return rc;

}

int mca_coll_hcoll_allreduce(void *sbuf, void *rbuf, int count,
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
    Dtype = ompi_dtype_2_dte_dtype(dtype);
    if (OPAL_UNLIKELY((HCOL_DTE_IS_ZERO(Dtype) || HCOL_DTE_IS_COMPLEX(Dtype)))
                        && mca_coll_hcoll_component.hcoll_datatype_fallback){
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

    rc = hcoll_collectives.coll_allreduce(sbuf,rbuf,count,Dtype,Op,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK ALLREDUCE");
        rc = hcoll_module->previous_allreduce(sbuf,rbuf,
                                             count,dtype,op,
                                             comm, hcoll_module->previous_allreduce_module);
    }
    return rc;
}

int mca_coll_hcoll_alltoall(void *sbuf, int scount,
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
    stype = ompi_dtype_2_dte_dtype(sdtype);
    rtype = ompi_dtype_2_dte_dtype(rdtype);
    if (OPAL_UNLIKELY((HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype)
                        || HCOL_DTE_IS_COMPLEX(stype) || HCOL_DTE_IS_COMPLEX(rtype)))
                        && mca_coll_hcoll_component.hcoll_datatype_fallback){
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
    rc = hcoll_collectives.coll_alltoall(sbuf,scount,stype,rbuf,rcount,rtype,hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK ALLTOALL");
        rc = hcoll_module->previous_alltoall(sbuf,scount,sdtype,
                                            rbuf,rcount,rdtype,
                                            comm,
                                            hcoll_module->previous_alltoall_module);
    }
    return rc;
}

int mca_coll_hcoll_gatherv(void* sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void* rbuf, int *rcounts, int *displs,
                            struct ompi_datatype_t *rdtype,
                            int root,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    HCOL_VERBOSE(20,"RUNNING HCOL GATHERV");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    stype = ompi_dtype_2_dte_dtype(sdtype);
    rtype = ompi_dtype_2_dte_dtype(rdtype);
    if (OPAL_UNLIKELY((HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype)
                        || HCOL_DTE_IS_COMPLEX(stype) || HCOL_DTE_IS_COMPLEX(rtype)))
                        && mca_coll_hcoll_component.hcoll_datatype_fallback){
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
    rc = hcoll_collectives.coll_gatherv(sbuf,scount,stype,rbuf,rcounts,displs, rtype, root, hcoll_module->hcoll_context);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK GATHERV");
        rc = hcoll_module->previous_gatherv(sbuf,scount,sdtype,
                                           rbuf, rcounts, displs, rdtype,root,
                                           comm, hcoll_module->previous_igatherv_module);
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
    dtype = ompi_dtype_2_dte_dtype(datatype);
    if (OPAL_UNLIKELY((HCOL_DTE_IS_ZERO(dtype) || HCOL_DTE_IS_COMPLEX(dtype)))
                        && mca_coll_hcoll_component.hcoll_datatype_fallback){
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

int mca_coll_hcoll_iallgather(void *sbuf, int scount,
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
    stype = ompi_dtype_2_dte_dtype(sdtype);
    rtype = ompi_dtype_2_dte_dtype(rdtype);
    if (OPAL_UNLIKELY((HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype)
                        || HCOL_DTE_IS_COMPLEX(stype) || HCOL_DTE_IS_COMPLEX(rtype)))
                        && mca_coll_hcoll_component.hcoll_datatype_fallback){
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
    rc = hcoll_collectives.coll_iallgather(sbuf, scount, stype, rbuf, rcount, rtype, hcoll_module->hcoll_context, rt_handle);
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

int mca_coll_hcoll_iallreduce(void *sbuf, void *rbuf, int count,
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
    Dtype = ompi_dtype_2_dte_dtype(dtype);
    if (OPAL_UNLIKELY((HCOL_DTE_IS_ZERO(Dtype) || HCOL_DTE_IS_COMPLEX(Dtype)))
                        && mca_coll_hcoll_component.hcoll_datatype_fallback){
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

    rc = hcoll_collectives.coll_iallreduce(sbuf, rbuf, count, Dtype, Op, hcoll_module->hcoll_context, rt_handle);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK NON-BLOCKING ALLREDUCE");
        rc = hcoll_module->previous_iallreduce(sbuf,rbuf,
                                             count,dtype,op,
                                             comm, request, hcoll_module->previous_iallreduce_module);
    }
    return rc;
}

int mca_coll_hcoll_igatherv(void* sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void* rbuf, int *rcounts, int *displs,
                            struct ompi_datatype_t *rdtype,
                            int root,
                            struct ompi_communicator_t *comm,
                            ompi_request_t ** request,
                            mca_coll_base_module_t *module)
{
    dte_data_representation_t stype;
    dte_data_representation_t rtype;
    int rc;
    void** rt_handle;
    HCOL_VERBOSE(20,"RUNNING HCOL IGATHERV");
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*)module;
    rt_handle = (void**) request;
    stype = ompi_dtype_2_dte_dtype(sdtype);
    rtype = ompi_dtype_2_dte_dtype(rdtype);
    if (OPAL_UNLIKELY((HCOL_DTE_IS_ZERO(stype) || HCOL_DTE_IS_ZERO(rtype)
                        || HCOL_DTE_IS_COMPLEX(stype) || HCOL_DTE_IS_COMPLEX(rtype)))
                        && mca_coll_hcoll_component.hcoll_datatype_fallback){
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
    rc = hcoll_collectives.coll_igatherv(sbuf,scount,stype,rbuf,rcounts,displs, rtype, root, hcoll_module->hcoll_context, rt_handle);
    if (HCOLL_SUCCESS != rc){
        HCOL_VERBOSE(20,"RUNNING FALLBACK IGATHERV");
        rc = hcoll_module->previous_igatherv(sbuf,scount,sdtype,
                                           rbuf, rcounts, displs, rdtype,root,
                                           comm, request,
                                           hcoll_module->previous_igatherv_module);
    }
    return rc;

}

