/*
 * $HEADER$
 */

#ifndef OMPI_UIO_H
#define OMPI_UIO_H

#define RETRIES 2 /* ft-mpi defines it this way */
#include "ompi_declspec.h"

/* define the iovec structure */
struct iovec{
      void * iov_base;
      size_t iov_len;
};


#if defined(c_plusplus) || defined (__cplusplus)
extern "C" {
#endif
/*
 * writev:
   writev  writes  data  to  file  descriptor  fd,  and  from  the buffers
   described by iov. The number of buffers is specified by  cnt.  The
   buffers  are  used  in  the  order specified.  Operates just like write
   except that data is taken from iov instead of a contiguous buffer.
 */
OMPI_DECLSPEC int writev (int fd, struct iovec *iov, int cnt);

/* 
   readv  reads  data  from file descriptor fd, and puts the result in the
   buffers described by iov. The number  of  buffers  is  specified  by
   cnt.  The  buffers  are filled in the order specified.  Operates just
   like read except that data is put in iov  instead  of  a  contiguous
   buffer.
 */
OMPI_DECLSPEC int readv (int fd, struct iovec *iov, int cnt);

/* static inlined helper functions to push the write through. 
   This was almost completely lifted from ft-mpi code. please
   check Harness/hcore/share/snipe_lite.c for more details. 
   The only difference being that harness code was implemented 
   for blocking operations only */

OMPI_DECLSPEC int writeconn (int s,char * data,int len);

OMPI_DECLSPEC int readconn (int s,char * data,int len);

   
#if defined(c_plusplus) || defined (__cplusplus)
}
#endif

#endif /* OMPI_UIO_H */
