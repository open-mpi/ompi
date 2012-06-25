/*
 * Copyright (c) 2006 The Trustees of Indiana University and Indiana
 *                    University Research and Technology
 *                    Corporation.  All rights reserved.
 * Copyright (c) 2006 The Technical University of Chemnitz. All 
 *                    rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#ifndef __NBC_H__
#define __NBC_H__

#ifdef __cplusplus
extern "C" {
#endif


/*******************************************************
 ****** external NBC functions are defined here *******
 *******************************************************/

/* external function prototypes */
int NBC_Ibarrier(MPI_Comm comm, NBC_Handle* handle);
int NBC_Ibcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm, NBC_Handle* handle);
int NBC_Ibcast_inter(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm, NBC_Handle* handle);
int NBC_Ialltoallv(void* sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm, NBC_Handle* handle);
int NBC_Igather(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, NBC_Handle* handle);
int NBC_Igatherv(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *displs, MPI_Datatype recvtype, int root, MPI_Comm comm, NBC_Handle* handle);
int NBC_Iscatter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, NBC_Handle* handle);
int NBC_Iscatterv(void* sendbuf, int *sendcounts, int *displs, MPI_Datatype sendtype, void* recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, NBC_Handle* handle);
int NBC_Iallgather(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm, NBC_Handle *handle);
int NBC_Iallgatherv(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *displs, MPI_Datatype recvtype, MPI_Comm comm, NBC_Handle *handle);
int NBC_Ialltoall(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm, NBC_Handle *handle);
int NBC_Ireduce(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm, NBC_Handle* handle);
int NBC_Iallreduce(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, NBC_Handle* handle);
int NBC_Iscan(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, NBC_Handle* handle);
int NBC_Ireduce_scatter(void* sendbuf, void* recvbuf, int *recvcounts, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, NBC_Handle* handle);
int NBC_Icart_shift_xchg(void *sbuf, int scount, MPI_Datatype stype, void *rbuf, int rcount, MPI_Datatype rtype, int direction, int disp, MPI_Comm comm, NBC_Handle* handle);
int NBC_Ineighbor_xchg(void *sbuf, int scount, MPI_Datatype stype, void *rbuf, int rcount, MPI_Datatype rtype, MPI_Comm comm, NBC_Handle* handle);
int NBC_Ineighbor_xchgv(void *sbuf, int *scounts, int *sdispls, MPI_Datatype stype, void *rbuf, int *rcounts, int *rdispls, MPI_Datatype rtype, MPI_Comm comm, NBC_Handle* handle);

int NBC_Comm_neighbors(MPI_Comm comm, int maxindegree, int sources[], int sourceweights[], int maxoutdegree, int destinations[], int destweights[]);
int NBC_Comm_neighbors_count(MPI_Comm comm, int *indegree, int *outdegree, int *weighted);
int NBC_Wait(NBC_Handle *handle);
int NBC_Test(NBC_Handle *handle, int *flag, MPI_Status *status);

/* TODO: some hacks */
int NBC_Operation(void *buf3, void *buf1, void *buf2, MPI_Op op, MPI_Datatype type, int count);

void NBC_Reset_times(void);
void NBC_Print_times(double div);


#ifdef __cplusplus
}
#endif

#endif
