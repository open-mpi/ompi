/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "datatype/datatype.h"
#include "datatype/convertor.h"
#include "request/request.h"
#include "mca/pml/pml.h"


/*
 *	ompi_dtsndrcv
 *
 *	Function:	- copy MPI message from buffer into another
 *			- send/recv done if cannot optimize
 *	Accepts:	- send buffer
 *			- send count
 *			- send datatype
 *			- receive buffer
 *			- receive count
 *			- receive datatype
 *			- tag
 *			- communicator
 *	Returns:	- MPI_SUCCESS or error code
 */
int32_t ompi_ddt_sndrcv( void *sbuf, int32_t scount, const ompi_datatype_t* sdtype,
                         void *rbuf, int32_t rcount, const ompi_datatype_t* rdtype)
{
   int err;
   ompi_convertor_t *send_convertor, *recv_convertor;
   struct iovec iov;
   int length, completed;
   uint32_t iov_count;
   size_t max_data;
   int32_t freeAfter;

   /* First check if we really have something to do */
   if (0 == rcount) {
       if (0 == scount) {
           return MPI_SUCCESS;
       } else {
           return MPI_ERR_TRUNCATE;
       }
   }
   
   /* If same datatypes used, just copy. */
   if (sdtype == rdtype) {
      max_data = ( scount < rcount ? scount : rcount );
      ompi_ddt_copy_content_same_ddt(rdtype, max_data, (char*)rbuf, (char*)sbuf);
      return ((scount > rcount) ? MPI_ERR_TRUNCATE : MPI_SUCCESS);
   }

   /* If receive packed. */
   if (rdtype == MPI_PACKED) {
      send_convertor = OBJ_NEW(ompi_convertor_t);
      ompi_convertor_prepare_for_send( send_convertor, sdtype, scount, sbuf );
      ompi_convertor_personalize( send_convertor, 0, 0, NULL );

      iov_count = 1;
      iov.iov_len = rcount;
      iov.iov_base = rbuf;
      max_data = ( iov.iov_len > (scount * sdtype->size) ? (scount * sdtype->size) : iov.iov_len );

      err = ompi_convertor_pack( send_convertor, &iov, &iov_count, &max_data, &freeAfter );
      OBJ_RELEASE( send_convertor );
      return ((max_data < (uint32_t)rcount) ? MPI_ERR_TRUNCATE : MPI_SUCCESS);
   }

   /* If send packed. */
   if (sdtype == MPI_PACKED) {
      recv_convertor = OBJ_NEW(ompi_convertor_t);
      ompi_convertor_prepare( recv_convertor, rdtype, rcount, rbuf );
      ompi_convertor_prepare_for_recv( recv_convertor, 0, 0, NULL );

      iov_count = 1;
      iov.iov_len = scount;
      iov.iov_base = sbuf;
      max_data = ( iov.iov_len < (rcount * rdtype->size) ? iov.iov_len : (rcount * rdtype->size) );

      err = ompi_convertor_unpack( recv_convertor, &iov, &iov_count, &max_data, &freeAfter );
      if( scount > (int32_t)(rcount * rdtype->size) )
         err = MPI_ERR_TRUNCATE;
      OBJ_RELEASE( recv_convertor );
      return err;
   }

   iov.iov_len = length = 64 * 1024;
   iov.iov_base = (void*)malloc( length * sizeof(char) );

   send_convertor = OBJ_NEW(ompi_convertor_t);
   recv_convertor = OBJ_NEW(ompi_convertor_t);
   ompi_convertor_prepare( send_convertor, sdtype, scount, sbuf );
   ompi_convertor_prepare( recv_convertor, rdtype, rcount, rbuf );
   ompi_convertor_prepare_for_send( send_convertor, 0, 0, NULL );
   ompi_convertor_prepare_for_recv( recv_convertor, 0, 0, NULL );

   completed = 0;
   while( !completed ) {
      iov.iov_len = length;
      iov_count = 1;
      max_data = length;
      completed |= ompi_convertor_pack( send_convertor, &iov, &iov_count, &max_data, &freeAfter );
      completed |= ompi_convertor_unpack( recv_convertor, &iov, &iov_count, &max_data, &freeAfter );
   }
   free( iov.iov_base );
   OBJ_RELEASE( send_convertor );
   OBJ_RELEASE( recv_convertor );

   return ( (scount * sdtype->size) <= (rcount * rdtype->size) ? MPI_SUCCESS : MPI_ERR_TRUNCATE );
}

