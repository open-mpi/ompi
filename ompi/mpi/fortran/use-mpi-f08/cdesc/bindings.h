/*
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_CDESC_BINDINGS_H
#define OMPI_CDESC_BINDINGS_H

#include "cdesc.h"

#include "ompi_config.h"

#include "mpi.h"

void ompi_bsend_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
                      MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm,
                      MPI_Fint *ierr);

void ompi_bsend_init_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
                           MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm,
                           MPI_Fint *request, MPI_Fint *ierr);

void ompi_ibsend_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
		       MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm,
		       MPI_Fint *request, MPI_Fint *ierr);

void ompi_imrecv_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
                       MPI_Fint *message, MPI_Fint *request, MPI_Fint *ierr);

void ompi_irecv_cdesc(CFI_cdesc_t *x, MPI_Fint *count, MPI_Fint *datatype,
		      MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm,
		      MPI_Fint *request, MPI_Fint *ierr);

void ompi_irsend_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
                       MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm,
                       MPI_Fint *request, MPI_Fint *ierr);

void ompi_isend_cdesc(CFI_cdesc_t *x, MPI_Fint *count, MPI_Fint *datatype,
                      MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm,
                      MPI_Fint *request, MPI_Fint *ierr);

void ompi_issend_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
                       MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm,
                       MPI_Fint *request, MPI_Fint *ierr);

void ompi_mrecv_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
                      MPI_Fint *message, MPI_Fint *status, MPI_Fint *ierr);

void ompi_recv_cdesc(CFI_cdesc_t *x, MPI_Fint *count, MPI_Fint *datatype,
                     MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm,
                     MPI_Fint *status, MPI_Fint *ierr);

void ompi_recv_init_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
		          MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm,
		          MPI_Fint *request, MPI_Fint *ierr);

void ompi_rsend_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
		      MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm,
                      MPI_Fint *ierr);

void ompi_rsend_init_cdesc(CFI_cdesc_t* x, MPI_Fint *count,
		           MPI_Fint *datatype, MPI_Fint *dest,
		           MPI_Fint *tag, MPI_Fint *comm,
		           MPI_Fint *request, MPI_Fint *ierr);

void ompi_send_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
                     MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr);

void ompi_send_init_cdesc(CFI_cdesc_t *x, MPI_Fint *count, MPI_Fint *datatype,
		          MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm,
		          MPI_Fint *request, MPI_Fint *ierr);

void ompi_sendrecv_cdesc(CFI_cdesc_t* x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
		         MPI_Fint *dest, MPI_Fint *sendtag, CFI_cdesc_t* x2,
		         MPI_Fint *recvcount, MPI_Fint *recvtype,
		         MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm,
		         MPI_Fint *status, MPI_Fint *ierr);

void ompi_sendrecv_replace_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
			         MPI_Fint *dest, MPI_Fint *sendtag,
			         MPI_Fint *source, MPI_Fint *recvtag,
			         MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr);

void ompi_ssend_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
		      MPI_Fint *dest, MPI_Fint *tag,
		      MPI_Fint *comm, MPI_Fint *ierr);

void ompi_ssend_init_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
		           MPI_Fint *dest, MPI_Fint *tag,
		           MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr);

void ompi_pack_cdesc(CFI_cdesc_t* x, MPI_Fint *incount, MPI_Fint *datatype,
		     char *outbuf, MPI_Fint *outsize, MPI_Fint *position,
		     MPI_Fint *comm, MPI_Fint *ierr);

void ompi_pack_external_cdesc(char *datarep, CFI_cdesc_t* x, MPI_Fint *incount,
			      MPI_Fint *datatype, char *outbuf,
			      MPI_Aint *outsize, MPI_Aint *position,
			      MPI_Fint *ierr, int datarep_len);

void ompi_unpack_cdesc(char *inbuf, MPI_Fint *insize, MPI_Fint *position,
		       CFI_cdesc_t* x, MPI_Fint *outcount, MPI_Fint *datatype,
		       MPI_Fint *comm, MPI_Fint *ierr);

void ompi_unpack_external_cdesc(char *datarep, char *inbuf, MPI_Aint *insize,
			        MPI_Aint *position, CFI_cdesc_t* x,
			        MPI_Fint *outcount, MPI_Fint *datatype,
			        MPI_Fint *ierr, int datarep_len);

void ompi_bcast_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
		      MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr);

void ompi_gather_cdesc(CFI_cdesc_t* x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
		       CFI_cdesc_t* x2, MPI_Fint *recvcount, MPI_Fint *recvtype,
		       MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr);

void ompi_gatherv_cdesc(CFI_cdesc_t* x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
		        char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs,
		        MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm,
		        MPI_Fint *ierr);

void ompi_allgather_cdesc(CFI_cdesc_t* x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
		          CFI_cdesc_t* x2, MPI_Fint *recvcount, MPI_Fint *recvtype,
		          MPI_Fint *comm, MPI_Fint *ierr);

void ompi_allgatherv_cdesc(CFI_cdesc_t* x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
		           char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs,
		           MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr);

void ompi_scatter_cdesc(char *sendbuf, MPI_Fint *sendcount,
		        MPI_Fint *sendtype, CFI_cdesc_t *x,
		        MPI_Fint *recvcount, MPI_Fint *recvtype,
		        MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr);

void ompi_scatterv_cdesc(char *sendbuf, MPI_Fint *sendcounts,
		         MPI_Fint *displs, MPI_Fint *sendtype,
		         CFI_cdesc_t *x, MPI_Fint *recvcount,
		         MPI_Fint *recvtype, MPI_Fint *root,
		         MPI_Fint *comm, MPI_Fint *ierr);

void ompi_alltoall_cdesc(CFI_cdesc_t* x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
		         CFI_cdesc_t* x2, MPI_Fint *recvcount, MPI_Fint *recvtype,
		         MPI_Fint *comm, MPI_Fint *ierr);

#endif /* OMPI_CDESC_BINDINGS_H */
