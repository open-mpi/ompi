/*
 * Temporary file to test MPI-3 interfaces with descriptors
 */

#include "ompi_config.h"
#include "ISO_Fortran_binding.h"
#include <stdio.h>

#define DEBUG_PRINT 0

void * ompi_f08_addr(void * buf)
{
#if DEBUG_PRINT
   printf("ompi_f08_addr = %p val=%d\n", buf, ((int*)buf)[0]);
#endif
   return buf;
}

void * ompi_f08_print_addr(void * buf)
{
   printf("    ompi_f08_addr = %p val=%d\n", buf, ((int*)buf)[0]);
   return buf;
}

size_t ompi_f08_addr_diff(void * buf1, void * buf2)
{
   size_t diff = (char*) buf2 - (char*) buf1;
#if DEBUG_PRINT
   printf("ompi_f08_addr_diff buf1 = %p val=%d\n", buf1, ((int*)buf1)[0]);
   printf("ompi_f08_addr_diff buf2 = %p val=%d\n", buf2, ((int*)buf2)[0]);
   printf("ompi_f08_addr_diff diff = %ld\n", diff);
#endif
   return diff;
}

/*
 * Returns true if the array described by desc is contiguous
 */
int isContiguous(CFI_cdesc_t * desc)
{
   int r;
   size_t sm = desc->elem_len;

   for (r = 0; r < desc->rank; r++) {
      if (sm == desc->dim[r].sm) {
         sm *= desc->dim[r].extent;
      } else {
         return 0;
      }
   }

   return 1;
}

/*
 * Returns the number of elements in the array described by desc.
 * The array may be non-contiguous.
 */
size_t numElements(CFI_cdesc_t * desc)
{
   int r;
   size_t num = 1;
   
   /* TODO - can have 0 size arrays? */

   for (r = 0; r < desc->rank; r++) {
      num *= desc->dim[r].extent;
   }
   return num;
}

/*
 * General routine to copy the elements from the array described by desc
 * to cont_buf.  The array itself may be non-contiguous.  For an array
 * of specific rank and type there exists more efficient methods to
 * copy the buffer.  Returns number of bytes copied.
 */
void * copyToContiguous(CFI_cdesc_t * desc, void * cont_buf, size_t offset, int rank)
{
   size_t b, e, num_copied;
   char * next_out;
   
   char * in  = (char *) desc->base_addr + offset;
   char * out = (char *) cont_buf;

   if (rank == 0) {
      /* copy scalar element */
      for (b = 0; b < desc->elem_len; b++) {
         *out++ = *in++;
      }
      cont_buf = out;
   }
   else {
      rank -= 1;
      for (e = 0; e < desc->dim[rank].extent; e++) {
         /* recur on subarrays of lesser rank */
         cont_buf = copyToContiguous(desc, cont_buf, offset, rank);
         offset += desc->dim[rank].sm;
      }
   }

   return cont_buf;
}

/*
 * General routine to copy the elements to the array described by desc
 * from cont_buf.  The array itself may be non-contiguous.  For an array
 * of specific rank and type there exists more efficient methods to
 * copy the buffer.  Returns number of bytes copied.
 */
void * copyFromContiguous(CFI_cdesc_t * desc, void * cont_buf, size_t offset, int rank)
{
   size_t b, e, num_copied;
   char * next_out;
   
   char * out = (char *) desc->base_addr + offset;
   char * in  = (char *) cont_buf;

   if (rank == 0) {
      /* copy scalar element */
      for (b = 0; b < desc->elem_len; b++) {
         *out++ = *in++;
      }
      cont_buf = in;
   }
   else {
      rank -= 1;
      for (e = 0; e < desc->dim[rank].extent; e++) {
         /* recur on subarrays of lesser rank */
         cont_buf = copyFromContiguous(desc, cont_buf, offset, rank);
         offset += desc->dim[rank].sm;
      }
   }

   return cont_buf;
}

/* From ../mpif-h/send_f.c
 */
void ompi_recv_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, 
                 MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, 
                 MPI_Fint *status, MPI_Fint *ierr);

void ompi_recv_f08_desc_f(CFI_cdesc_t *desc, MPI_Fint *count, MPI_Fint *datatype, 
                          MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, 
                          MPI_Fint *status, MPI_Fint *ierr)
{
   size_t num_bytes = 0;

   if (isContiguous(desc)) {
      //printf("ompi_recv_f08_desc_f: buf is contiguous\n");
      ompi_recv_f(desc->base_addr, count, datatype, source, tag, comm, status, ierr);
   } else {
      size_t cont_size = desc->elem_len * numElements(desc);
      void * cont_buf = malloc(cont_size);
      //assert(cont_buf);

      //printf("ompi_recv_f08_desc_f: buf not contiguous, # elements==%ld, receiving %ld bytes\n", numElements(desc), cont_size);
      ompi_recv_f(cont_buf, count, datatype, source, tag, comm, status, ierr);

      num_bytes = (char*) copyFromContiguous(desc, cont_buf, 0, desc->rank) - (char*) cont_buf;
      //printf("ompi_recv_f08_desc_f: received %d bytes\n", num_bytes);

      free(cont_buf);
   }
}

/* From ../mpif-h/send_f.c
 */
void ompi_send_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, 
                 MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr);

void ompi_send_f08_desc_f(CFI_cdesc_t *desc, MPI_Fint *count, MPI_Fint *datatype, 
                          MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr)
{
   size_t num_bytes = 0;

   if (isContiguous(desc)) {
      //printf("ompi_send_f08_desc_f: buf is contiguous\n");
      ompi_send_f(desc->base_addr, count, datatype, dest, tag, comm, ierr);
   } else {
      size_t cont_size = desc->elem_len * numElements(desc);
      void * cont_buf = malloc(cont_size);
      //assert(cont_buf);

      num_bytes = (char*) copyToContiguous(desc, cont_buf, 0, desc->rank) - (char*) cont_buf;

      //printf("ompi_send_f08_desc_f: buf not contiguous, # elements==%ld, sending %ld bytes\n", numElements(desc), num_bytes);
      ompi_send_f(cont_buf, count, datatype, dest, tag, comm, ierr);

      free(cont_buf);
   }

}
