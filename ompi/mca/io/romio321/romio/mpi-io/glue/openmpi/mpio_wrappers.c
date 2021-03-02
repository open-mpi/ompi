#include "mpi-io/mpioimpl.h"

#include "mpi-io/glue/openmpi/mpio_wrappers.h"

int MPIW_Allreduce(const void *sendbuf, void *recvbuf, int count,
                   MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                   void *recvbuf, int recvcount, MPI_Datatype recvtype,
                   MPI_Comm comm)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Allgather(sendbuf, sendcount, sendtype,
                         recvbuf, recvcount, recvtype,
                         comm);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype,
                  MPI_Comm comm)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Alltoall(sendbuf, sendcount, sendtype,
                        recvbuf, recvcount, recvtype,
                        comm);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Alltoallw(const void *sendbuf, const int sendcounts[], const int sdispls[], const MPI_Datatype sendtypes[],
                   void *recvbuf, const int recvcounts[], const int rdispls[], const MPI_Datatype recvtypes[],
                   MPI_Comm comm)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes,
                        recvbuf, recvcounts, rdispls, recvtypes,
                        comm);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Barrier(MPI_Comm comm)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Barrier(comm);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Bcast(void *buffer, int count, MPI_Datatype datatype,
               int root, MPI_Comm comm)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Bcast(buffer, count, datatype, root, comm);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                void *recvbuf, int recvcount, MPI_Datatype recvtype,
                int root, MPI_Comm comm)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Gather(sendbuf, sendcount, sendtype,
                      recvbuf, recvcount, recvtype,
                      root, comm);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, const int recvcounts[], const int displs[],
                 MPI_Datatype recvtype, int root, MPI_Comm comm)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Gatherv(sendbuf, sendcount, sendtype,
                       recvbuf, recvcounts, displs, recvtype,
                      root, comm);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Recv(void *buf, int count, MPI_Datatype datatype, int source,
              int tag, MPI_Comm comm, MPI_Status *status)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Recv(buf, count, datatype, source, tag, comm, status);
    ROMIO_THREAD_CS_ENTER();
    return res;
}


int MPIW_Send(const void *buf, int count, MPI_Datatype datatype, int dest,
              int tag, MPI_Comm comm)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Send(buf, count, datatype, dest, tag, comm);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Waitall(int count, MPI_Request array_of_requests[],
                 MPI_Status array_of_statuses[])
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Waitall(count, array_of_requests, array_of_statuses);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Waitany(int count, MPI_Request array_of_requests[],
                 int *index, MPI_Status *status)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Waitany(count, array_of_requests, index, status);
    ROMIO_THREAD_CS_ENTER();
    return res;
}

int MPIW_Wait(MPI_Request *request, MPI_Status *status)
{
    int res;
    ROMIO_THREAD_CS_EXIT();
    res = PMPI_Wait(request, status);
    ROMIO_THREAD_CS_ENTER();
    return res;
}
