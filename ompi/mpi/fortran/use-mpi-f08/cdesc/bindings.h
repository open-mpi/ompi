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

#endif /* OMPI_CDESC_BINDINGS_H */
