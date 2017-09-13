/*
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

OMPI_DECLSPEC  int OMPI_Isplit_recv(void *buf, int count, MPI_Datatype type, int source,
                                    int tag, MPI_Comm comm, MPI_Request *request);
OMPI_DECLSPEC  int OMPI_Split_send(const void *buf, int count, MPI_Datatype datatype, int dest,
                                  int tag, MPI_Comm comm);
OMPI_DECLSPEC  int OMPI_Split_recv(void *buf, int count, MPI_Datatype datatype, int dest,
                                    int tag, MPI_Comm comm, MPI_Status *status);
OMPI_DECLSPEC  int OMPI_Isplit_send(const void *buf, int count, MPI_Datatype datatype, int dest,
                                    int tag, MPI_Comm comm, MPI_Request *req);

OMPI_DECLSPEC  int POMPI_Isplit_recv(void *buf, int count, MPI_Datatype type, int source,
                                     int tag, MPI_Comm comm, MPI_Request *request);
OMPI_DECLSPEC  int POMPI_Isplit_send(const void *buf, int count, MPI_Datatype datatype, int dest,
                                     int tag, MPI_Comm comm, MPI_Request *req);
OMPI_DECLSPEC  int POMPI_Split_recv(void *buf, int count, MPI_Datatype datatype, int dest,
                                    int tag, MPI_Comm comm, MPI_Status *status);
OMPI_DECLSPEC  int POMPI_Split_send(const void *buf, int count, MPI_Datatype datatype, int dest,
                                   int tag, MPI_Comm comm);
