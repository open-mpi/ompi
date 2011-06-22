/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_UNIFY_MPI_H_
#define _VT_UNIFY_MPI_H_

#include "vt_defs.h" /* to get VT_MPI_INT */

/* use MPI's profile interface for the library version of the unifier */
#ifdef VT_LIB
#  define CALL_MPI( call ) P##call
#else /* VT_LIB */
#  define CALL_MPI( call ) call
#endif /* VT_LIB */

#ifndef VT_UNIFY_MPI_WRAP

#  include "mpi.h"

#else /* VT_UNIFY_MPI_WRAP */

#  ifdef __cplusplus
#     define EXTERN extern "C"
#  else
#     define EXTERN extern
#  endif

#  define VTUnify_MPI_COMM_WORLD         0

#  define VTUnify_MPI_CHAR               0
#  define VTUnify_MPI_INT                1
#  define VTUnify_MPI_LONG_LONG_INT      2
#  define VTUnify_MPI_DOUBLE             3
#  define VTUnify_MPI_UNSIGNED           4
#  define VTUnify_MPI_UNSIGNED_SHORT     5
#  define VTUnify_MPI_PACKED             6

#  define VTUnify_MPI_MIN                0
#  define VTUnify_MPI_MAX                1
#  define VTUnify_MPI_SUM                2

#  define VTUnify_MPI_ANY_SOURCE         -1
#  define VTUnify_MPI_BOTTOM             ((void*)0)
#  define VTUnify_MPI_STATUS_IGNORE      ((VTUnify_MPI_Status*)0)

   typedef struct {
      VT_MPI_INT MPI_SOURCE;
      VT_MPI_INT MPI_TAG;
   } VTUnify_MPI_Status;

   typedef long long VTUnify_MPI_Aint;
   typedef int       VTUnify_MPI_Comm;
   typedef int       VTUnify_MPI_Datatype;
   typedef int       VTUnify_MPI_Op;
   typedef int       VTUnify_MPI_Request;

   EXTERN VT_MPI_INT VTUnify_MPI_Abort( VTUnify_MPI_Comm ucomm,
                        VT_MPI_INT errorcode );

   EXTERN VT_MPI_INT VTUnify_MPI_Address( void * location,
                        VTUnify_MPI_Aint * address );

   EXTERN VT_MPI_INT VTUnify_MPI_Allgather( void * sendbuf,
                        VT_MPI_INT sendcount, VTUnify_MPI_Datatype usendtype,
                        void * recvbuf, VT_MPI_INT recvcount,
                        VTUnify_MPI_Datatype urecvtype,
                        VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Allgatherv( void * sendbuf,
                        VT_MPI_INT sendcount, VTUnify_MPI_Datatype usendtype,
                        void * recvbuf, VT_MPI_INT * recvcount,
                        VT_MPI_INT * displs, VTUnify_MPI_Datatype urecvtype,
                        VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Allreduce( void * sendbuf, void * recvbuf,
                        int count, VTUnify_MPI_Datatype utype, VTUnify_MPI_Op uop,
                        VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Alltoall( void * sendbuf, VT_MPI_INT sendcount,
                        VTUnify_MPI_Datatype usendtype, void * recvbuf,
                        VT_MPI_INT recvcount, VTUnify_MPI_Datatype urecvtype,
                        VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Alltoallv( void * sendbuf,
                        VT_MPI_INT * sendcounts, VT_MPI_INT * sdispls,
                        VTUnify_MPI_Datatype usendtype, void * recvbuf,
                        VT_MPI_INT * recvcounts, VT_MPI_INT * rdispls,
                        VTUnify_MPI_Datatype urecvtype,
                        VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Barrier( VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Bcast( void * buffer, VT_MPI_INT count,
                        VTUnify_MPI_Datatype utype, VT_MPI_INT root,
                        VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Comm_rank( VTUnify_MPI_Comm ucomm,
                        VT_MPI_INT * rank );

   EXTERN VT_MPI_INT VTUnify_MPI_Comm_size( VTUnify_MPI_Comm ucomm,
                        VT_MPI_INT * size );

   EXTERN VT_MPI_INT VTUnify_MPI_Gather( void * sendbuf, VT_MPI_INT sendcount,
                        VTUnify_MPI_Datatype usendtype, void * recvbuf,
                        VT_MPI_INT recvcount, VTUnify_MPI_Datatype urecvtype,
                        VT_MPI_INT root, VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Gatherv( void * sendbuf, VT_MPI_INT sendcount,
                        VTUnify_MPI_Datatype usendtype, void * recvbuf,
                        VT_MPI_INT * recvcount, VT_MPI_INT * displs,
                        VTUnify_MPI_Datatype urecvtype, VT_MPI_INT root,
                        VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Get_count( VTUnify_MPI_Status * ustatus,
                        VTUnify_MPI_Datatype utype, VT_MPI_INT * count );

   EXTERN VT_MPI_INT VTUnify_MPI_Finalize( void );

   EXTERN VT_MPI_INT VTUnify_MPI_Init( VT_MPI_INT * argc, char *** argv );

   EXTERN VT_MPI_INT VTUnify_MPI_Isend( void * buf, VT_MPI_INT count,
                        VTUnify_MPI_Datatype utype, VT_MPI_INT dest,
                        VT_MPI_INT tag, VTUnify_MPI_Comm ucomm,
                        VTUnify_MPI_Request * urequest );

   EXTERN VT_MPI_INT VTUnify_MPI_Pack( void * inbuf, VT_MPI_INT incount,
                        VTUnify_MPI_Datatype utype, void * outbuf,
                        VT_MPI_INT outsize, VT_MPI_INT * position,
                        VTUnify_MPI_Comm ucomm);

   EXTERN VT_MPI_INT VTUnify_MPI_Pack_size( VT_MPI_INT incount,
                        VTUnify_MPI_Datatype utype, VTUnify_MPI_Comm ucomm,
                        VT_MPI_INT * size );

   EXTERN VT_MPI_INT VTUnify_MPI_Probe( VT_MPI_INT source, VT_MPI_INT tag,
                        VTUnify_MPI_Comm ucomm, VTUnify_MPI_Status * ustatus );

   EXTERN VT_MPI_INT VTUnify_MPI_Recv( void * buf, VT_MPI_INT count,
                        VTUnify_MPI_Datatype utype, VT_MPI_INT source,
                        VT_MPI_INT tag, VTUnify_MPI_Comm ucomm,
                        VTUnify_MPI_Status * ustatus );

   EXTERN VT_MPI_INT VTUnify_MPI_Send( void * buf, VT_MPI_INT count,
                        VTUnify_MPI_Datatype utype, VT_MPI_INT dest,
                        VT_MPI_INT tag, VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Test( VTUnify_MPI_Request * urequest,
                        VT_MPI_INT * flag, VTUnify_MPI_Status * ustatus );

   EXTERN VT_MPI_INT VTUnify_MPI_Type_commit( VTUnify_MPI_Datatype * utype );

   EXTERN VT_MPI_INT VTUnify_MPI_Type_free( VTUnify_MPI_Datatype * utype );

   EXTERN VT_MPI_INT VTUnify_MPI_Type_struct( VT_MPI_INT count,
                        VT_MPI_INT * array_of_blocklengths,
                        VTUnify_MPI_Aint * array_of_udisplacements,
                        VTUnify_MPI_Datatype * array_of_utypes,
                        VTUnify_MPI_Datatype * newutype );

   EXTERN VT_MPI_INT VTUnify_MPI_Unpack( void * inbuf, VT_MPI_INT insize,
                        VT_MPI_INT * position, void * outbuf, VT_MPI_INT outcount,
                        VTUnify_MPI_Datatype utype, VTUnify_MPI_Comm ucomm );

   EXTERN VT_MPI_INT VTUnify_MPI_Wait( VTUnify_MPI_Request * urequest,
                        VTUnify_MPI_Status * ustatus );

#  ifndef VT_UNIFY_MPI_WRAP_NODEF
#     undef  CALL_MPI
#     define CALL_MPI( call )       VTUnify_##call

#     define MPI_COMM_WORLD         VTUnify_MPI_COMM_WORLD

#     define MPI_CHAR               VTUnify_MPI_CHAR
#     define MPI_INT                VTUnify_MPI_INT
#     define MPI_LONG_LONG_INT      VTUnify_MPI_LONG_LONG_INT
#     define MPI_DOUBLE             VTUnify_MPI_DOUBLE
#     define MPI_UNSIGNED           VTUnify_MPI_UNSIGNED
#     define MPI_UNSIGNED_SHORT     VTUnify_MPI_UNSIGNED_SHORT
#     define MPI_PACKED             VTUnify_MPI_PACKED

#     define MPI_MIN                VTUnify_MPI_MIN
#     define MPI_MAX                VTUnify_MPI_MAX
#     define MPI_SUM                VTUnify_MPI_SUM

#     define MPI_ANY_SOURCE         VTUnify_MPI_ANY_SOURCE
#     define MPI_BOTTOM             VTUnify_MPI_BOTTOM
#     define MPI_STATUS_IGNORE      VTUnify_MPI_STATUS_IGNORE

#     define MPI_Aint               VTUnify_MPI_Aint
#     define MPI_Comm               VTUnify_MPI_Comm
#     define MPI_Datatype           VTUnify_MPI_Datatype

#     define MPI_Op                 VTUnify_MPI_Op
#     define MPI_Request            VTUnify_MPI_Request
#     define MPI_Status             VTUnify_MPI_Status
#  endif /* VT_UNIFY_MPI_WRAP_NODEF */

#endif /* VT_UNIFY_MPI_WRAP */

#endif /* _VT_UNIFY_MPI_H_ */
