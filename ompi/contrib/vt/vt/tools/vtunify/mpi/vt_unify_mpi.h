/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_UNIFY_MPI_H_
#define _VT_UNIFY_MPI_H_

#ifdef __cplusplus
#  define EXTERN extern "C" 
#else
#  define EXTERN extern 
#endif

#include "vt_inttypes.h"

#include <stdlib.h>

#define VTUnify_MPI_COMM_WORLD         0

#define VTUnify_MPI_CHAR               0
#define VTUnify_MPI_INT                1
#define VTUnify_MPI_LONG_LONG_INT      2
#define VTUnify_MPI_DOUBLE             3
#define VTUnify_MPI_UNSIGNED           4
#define VTUnify_MPI_UNSIGNED_SHORT     5
#define VTUnify_MPI_UNSIGNED_LONG_LONG 6
#define VTUnify_MPI_PACKED             7

#define VTUnify_MPI_BOTTOM             ((void*)0)
#define VTUnify_MPI_STATUS_IGNORE      ((VTUnify_MPI_Status*)0)

typedef struct {
   VT_MPI_INT source;
   VT_MPI_INT tag;
} VTUnify_MPI_Status;

typedef long long VTUnify_MPI_Aint;
typedef int       VTUnify_MPI_Comm;
typedef int       VTUnify_MPI_Datatype;

EXTERN VT_MPI_INT VTUnify_MPI_Abort( VTUnify_MPI_Comm ucomm,
                                     VT_MPI_INT errorcode );

EXTERN VT_MPI_INT VTUnify_MPI_Address( void * location,
                                       VTUnify_MPI_Aint * address );

EXTERN VT_MPI_INT VTUnify_MPI_Barrier( VTUnify_MPI_Comm ucomm );

EXTERN VT_MPI_INT VTUnify_MPI_Bcast( void * buffer, VT_MPI_INT count,
                                     VTUnify_MPI_Datatype utype,
                                     VT_MPI_INT root, VTUnify_MPI_Comm ucomm );

EXTERN VT_MPI_INT VTUnify_MPI_Comm_rank( VTUnify_MPI_Comm ucomm,
                                         VT_MPI_INT * rank );

EXTERN VT_MPI_INT VTUnify_MPI_Comm_size( VTUnify_MPI_Comm ucomm,
                                         VT_MPI_INT * size );

EXTERN VT_MPI_INT VTUnify_MPI_Finalize( void );

EXTERN VT_MPI_INT VTUnify_MPI_Init( VT_MPI_INT * argc, char *** argv );

EXTERN VT_MPI_INT VTUnify_MPI_Pack( void * inbuf, VT_MPI_INT incount,
                                    VTUnify_MPI_Datatype utype, void * outbuf,
                                    VT_MPI_INT outsize, VT_MPI_INT * position,
                                    VTUnify_MPI_Comm ucomm);

EXTERN VT_MPI_INT VTUnify_MPI_Pack_size( VT_MPI_INT incount,
                                         VTUnify_MPI_Datatype utype,
                                         VTUnify_MPI_Comm ucomm,
                                         VT_MPI_INT * size );

EXTERN VT_MPI_INT VTUnify_MPI_Recv( void * buf, VT_MPI_INT count,
                                    VTUnify_MPI_Datatype utype,
                                    VT_MPI_INT source, VT_MPI_INT tag,
                                    VTUnify_MPI_Comm ucomm,
                                    VTUnify_MPI_Status * ustatus );

EXTERN VT_MPI_INT VTUnify_MPI_Send( void * buf, VT_MPI_INT count,
                                    VTUnify_MPI_Datatype utype,
                                    VT_MPI_INT dest, VT_MPI_INT tag,
                                    VTUnify_MPI_Comm ucomm );

EXTERN VT_MPI_INT VTUnify_MPI_Type_commit( VTUnify_MPI_Datatype * utype );

EXTERN VT_MPI_INT VTUnify_MPI_Type_free( VTUnify_MPI_Datatype * utype );

EXTERN VT_MPI_INT VTUnify_MPI_Type_struct( VT_MPI_INT count,
                                           VT_MPI_INT * array_of_blocklengths,
                                           VTUnify_MPI_Aint * array_of_udisplacements,
                                           VTUnify_MPI_Datatype * array_of_utypes,
                                           VTUnify_MPI_Datatype * newutype );

EXTERN VT_MPI_INT VTUnify_MPI_Unpack( void * inbuf, VT_MPI_INT insize,
                                      VT_MPI_INT * position, void * outbuf,
                                      VT_MPI_INT outcount,
                                      VTUnify_MPI_Datatype utype,
                                      VTUnify_MPI_Comm ucomm );

#endif /* _VT_UNIFY_MPI_H_ */
