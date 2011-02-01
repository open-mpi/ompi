/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef ROMIO_DATALOOP_H
#define ROMIO_DATALOOP_H

#include <assert.h>
#include <stdlib.h>

/* romioconf.h must be included *before* mpi.h to avoid some redeclarations */
#ifdef HAVE_MPITYPEDEFS_H
#include "mpitypedefs.h"
#endif
#ifdef HAVE_MPICHCONF_H
#include "mpichconf.h"
#endif
#include "romioconf.h"

#include <mpi.h>

/* Note: this is where you define the prefix that will be prepended on
 * all externally visible generic dataloop and segment functions.
 */
#define PREPEND_PREFIX(fn) MPIO_ ## fn

struct MPIO_iovec {
    MPI_Offset base;
    MPI_Offset len;
};

/* These following dataloop-specific types will be used throughout the DLOOP
 * instance:
 */
#define DLOOP_Offset     MPI_Offset
#define DLOOP_Count      MPI_Offset
#define DLOOP_Handle     MPI_Datatype
#define DLOOP_Type       MPI_Datatype
#define DLOOP_Buffer     void *
#define DLOOP_VECTOR     struct MPIO_iovec
#define DLOOP_VECTOR_LEN len
#define DLOOP_VECTOR_BUF base

/* The following accessor functions must also be defined:
 *
 * DLOOP_Handle_extent()
 * DLOOP_Handle_size()
 * DLOOP_Handle_loopptr()
 * DLOOP_Handle_loopdepth()
 * DLOOP_Handle_hasloop()
 *
 */

/* USE THE NOTATION THAT BILL USED IN MPIIMPL.H AND MAKE THESE MACROS */

/* NOTE: put get size into mpiimpl.h; the others go here until such time
 * as we see that we need them elsewhere.
 */
#define DLOOP_Handle_get_loopdepth_macro(handle_,depth_,flag_) \
    MPIO_Datatype_get_loopdepth(handle_,&(depth_),flag_)

#define DLOOP_Handle_get_loopsize_macro(handle_,size_,flag_) \
    MPIO_Datatype_get_loopsize(handle_,&(size_),flag_)

#define DLOOP_Handle_set_loopptr_macro(handle_,lptr_,flag_) \
    MPIO_Datatype_set_loopptr(handle_,lptr_,flag_)

#define DLOOP_Handle_set_loopdepth_macro(handle_,depth_,flag_) \
    MPIO_Datatype_set_loopdepth(handle_,depth_,flag_)

#define DLOOP_Handle_set_loopsize_macro(handle_,size_,flag_) \
    MPIO_Datatype_set_loopsize(handle_,size_,flag_)

#define DLOOP_Handle_get_loopptr_macro(handle_,lptr_,flag_) \
    MPIO_Datatype_get_loopptr(handle_,&(lptr_),flag_)

#define DLOOP_Handle_get_size_macro(handle_,size_) \
    MPIO_Datatype_get_size(handle_,&(size_))

#define DLOOP_Handle_get_basic_type_macro(handle_,eltype_) \
    MPIO_Datatype_get_el_type(handle_, &(eltype_), 0)

#define DLOOP_Handle_get_extent_macro(handle_,extent_) \
    MPIO_Datatype_get_extent(handle_,&(extent_))

#define DLOOP_Handle_hasloop_macro(handle_)	\
    (MPIO_Datatype_is_nontrivial(handle_))

/* allocate and free functions must also be defined. */
#define DLOOP_Malloc malloc
#define DLOOP_Free   free

/* debugging output function */
#define DLOOP_dbg_printf printf

/* assert function */
#define DLOOP_Assert assert

/* contents access functions -- use MPICH2 versions for now. */
#define MPIO_Type_access_contents MPID_Type_access_contents
#define MPIO_Type_release_contents MPID_Type_release_contents

/* Include dataloop_parts.h at the end to get the rest of the prototypes
 * and defines, in terms of the prefixes and types above.
 */
#include "./dataloop_parts.h"
#include "./dataloop_create.h"

/* accessor functions */
void MPIO_Datatype_init_dataloop(MPI_Datatype type);
void MPIO_Datatype_get_size(MPI_Datatype type, MPI_Offset *size_p);
void MPIO_Datatype_get_extent(MPI_Datatype type, MPI_Offset *extent_p);
void MPIO_Datatype_get_block_info(MPI_Datatype type, MPI_Offset *true_lb,
				  MPI_Offset *count, int *n_contig);
int MPIO_Datatype_is_nontrivial(MPI_Datatype type);
void MPIO_Datatype_get_el_type(MPI_Datatype type, MPI_Datatype *eltype_p,
			       int flag);

/* accessor functions used only by dataloop code proper */
void MPIO_Datatype_get_loopptr(MPI_Datatype type, MPIO_Dataloop **ptr_p,
			       int flag);
void MPIO_Datatype_get_loopsize(MPI_Datatype type, int *size_p, int flag);
void MPIO_Datatype_get_loopdepth(MPI_Datatype type, int *depth_p, int flag);
void MPIO_Datatype_set_loopptr(MPI_Datatype type, MPIO_Dataloop *ptr, int flag);
void MPIO_Datatype_set_loopsize(MPI_Datatype type, int size, int flag);
void MPIO_Datatype_set_loopdepth(MPI_Datatype type, int depth, int flag);

/* accessor functions from elsewhere */
void MPIO_Type_access_contents(MPI_Datatype type,
			       int **ints_p,
			       MPI_Aint **aints_p,
			       MPI_Datatype **types_p);
void MPIO_Type_release_contents(MPI_Datatype type,
				int **ints_p,
				MPI_Aint **aints_p,
				MPI_Datatype **types_p);

/* These values are defined by DLOOP code.
 *
 * Note: DLOOP_DATALOOP_ALL_BYTES not currently used in MPICH2.
 */
#define MPID_DATALOOP_HETEROGENEOUS DLOOP_DATALOOP_HETEROGENEOUS
#define MPID_DATALOOP_HOMOGENEOUS   DLOOP_DATALOOP_HOMOGENEOUS

#endif
