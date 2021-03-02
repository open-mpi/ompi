#include <mpi.h>

int MPIW_Allreduce(const void *sendbuf, void *recvbuf, int count,
                   MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);

int MPIW_Allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                   void *recvbuf, int recvcount, MPI_Datatype recvtype,
                   MPI_Comm comm);

int MPIW_Alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                  void *recvbuf, int recvcount, MPI_Datatype recvtype,
                   MPI_Comm comm);

int MPIW_Alltoallw(const void *sendbuf, const int sendcounts[], const int sdispls[], const MPI_Datatype sendtypes[],
                   void *recvbuf, const int recvcounts[], const int rdispls[], const MPI_Datatype recvtypes[],
                   MPI_Comm comm);

int MPIW_Barrier(MPI_Comm comm);

int MPIW_Bcast(void *buffer, int count, MPI_Datatype datatype,
               int root, MPI_Comm comm);

int MPIW_Gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                void *recvbuf, int recvcount, MPI_Datatype recvtype,
                int root, MPI_Comm comm);

int MPIW_Gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 void *recvbuf, const int recvcounts[], const int displs[],
                 MPI_Datatype recvtype, int root, MPI_Comm comm);

int MPIW_Recv(void *buf, int count, MPI_Datatype datatype, int source,
              int tag, MPI_Comm comm, MPI_Status *status);

int MPIW_Send(const void *buf, int count, MPI_Datatype datatype, int dest,
              int tag, MPI_Comm comm);

int MPIW_Waitall(int count, MPI_Request array_of_requests[],
                 MPI_Status array_of_statuses[]);

int MPIW_Waitany(int count, MPI_Request array_of_requests[],
                 int *index, MPI_Status *status);

int MPIW_Wait(MPI_Request *request, MPI_Status *status);
