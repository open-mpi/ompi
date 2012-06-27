#include "ompi_config.h"

#include "coll_libnbc.h"
#include "ompi/constants.h"

int
ompi_coll_libnbc_ialltoallw(void *sbuf, int *scounts, int *sdisps, struct ompi_datatype_t **sdtypes, 
                            void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t **rdtypes, 
                            struct ompi_communicator_t *comm, ompi_request_t **request, 
                            struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_iexscan(void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, 
                         struct ompi_op_t *op, struct ompi_communicator_t *comm, ompi_request_t **request,
                         struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_ireduce_scatter_block(void *sbuf, void *rbuf, int rcount, struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op, struct ompi_communicator_t *comm, 
                                       ompi_request_t **request, struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_iallgather_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, 
                                  MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                  struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_iallgatherv_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *displs, 
                                   MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                   struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_iallreduce_inter(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op, 
                                  struct ompi_communicator_t *comm, ompi_request_t ** request,
                                  struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_ialltoall_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, 
                                 MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_ialltoallv_inter(void* sendbuf, int *sendcounts, int *sdispls,
                                  MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *rdispls,
                                  MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                  struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_ialltoallw_inter(void *sbuf, int *scounts, int *sdisps, struct ompi_datatype_t **sdtypes, 
                                  void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t **rdtypes, 
                                  struct ompi_communicator_t *comm, ompi_request_t **request, 
                                  struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_ibarrier_inter(struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_igather_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, 
                               MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_igatherv_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, 
                                void* recvbuf, int *recvcounts, int *displs, MPI_Datatype recvtype, 
                                int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_ireduce_inter(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, 
                               MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_ireduce_scatter_inter(void* sendbuf, void* recvbuf, int *recvcounts, MPI_Datatype datatype, 
                                     MPI_Op op, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                       struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_ireduce_scatter_block_inter(void *sbuf, void *rbuf, int rcount, struct ompi_datatype_t *dtype,
                                             struct ompi_op_t *op, struct ompi_communicator_t *comm, 
                                             ompi_request_t **request, struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int ompi_coll_libnbc_iscatter_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, 
                                    void* recvbuf, int recvcount, MPI_Datatype recvtype, int root, 
                                    struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_coll_libnbc_iscatterv_inter(void* sendbuf, int *sendcounts, int *displs, MPI_Datatype sendtype, 
                                 void* recvbuf, int recvcount, MPI_Datatype recvtype, int root, 
                                 struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_0_0_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
