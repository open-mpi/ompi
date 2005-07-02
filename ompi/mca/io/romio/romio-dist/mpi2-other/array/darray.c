/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: darray.c,v 1.8 2002/10/24 17:01:27 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_Type_create_darray = PMPI_Type_create_darray
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_Type_create_darray MPI_Type_create_darray
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_Type_create_darray as PMPI_Type_create_darray
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#undef MPIO_BUILD_PROFILING
#endif

void MPIOI_Type_block(int *array_of_gsizes, int dim, int ndims, int nprocs,
		      int rank, int darg, int order, MPI_Aint orig_extent,
		      MPI_Datatype type_old, MPI_Datatype *type_new,
		      MPI_Aint *st_offset);
void MPIOI_Type_cyclic(int *array_of_gsizes, int dim, int ndims, int nprocs,
		      int rank, int darg, int order, MPI_Aint orig_extent,
		      MPI_Datatype type_old, MPI_Datatype *type_new,
		      MPI_Aint *st_offset);


/*@
MPI_Type_create_darray - Creates a datatype corresponding to a distributed, multidimensional array

Input Parameters:
. size - size of process group (positive integer)
. rank - rank in process group (nonnegative integer)
. ndims - number of array dimensions as well as process grid dimensions (positive integer)
. array_of_gsizes - number of elements of type oldtype in each dimension of global array (array of positive integers)
. array_of_distribs - distribution of array in each dimension (array of state)
. array_of_dargs - distribution argument in each dimension (array of positive integers)
. array_of_psizes - size of process grid in each dimension (array of positive integers)
. order - array storage order flag (state)
. oldtype - old datatype (handle)

Output Parameters:
. newtype - new datatype (handle)

.N fortran
@*/
int MPI_Type_create_darray(int size, int rank, int ndims, 
     	                   int *array_of_gsizes, int *array_of_distribs, 
                           int *array_of_dargs, int *array_of_psizes, 
                           int order, MPI_Datatype oldtype, 
                           MPI_Datatype *newtype) 
{
    MPI_Datatype type_old, type_new, types[3];
    int procs, tmp_rank, i, tmp_size, blklens[3], *coords;
    MPI_Aint *st_offsets, orig_extent, disps[3], size_with_aint;
    MPI_Offset size_with_offset;

    if (size <= 0) {
	FPRINTF(stderr, "MPI_Type_create_darray: Invalid size argument\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    if (rank < 0) {
	FPRINTF(stderr, "MPI_Type_create_darray: Invalid rank argument\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    if (ndims <= 0) {
	FPRINTF(stderr, "MPI_Type_create_darray: Invalid ndims argument\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    if (array_of_gsizes <= (int *) 0) {
	FPRINTF(stderr, "MPI_Type_create_darray: array_of_gsizes is an invalid address\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    if (array_of_distribs <= (int *) 0) {
	FPRINTF(stderr, "MPI_Type_create_darray: array_of_distribs is an invalid address\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    if (array_of_dargs <= (int *) 0) {
	FPRINTF(stderr, "MPI_Type_create_darray: array_of_dargs is an invalid address\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    if (array_of_psizes <= (int *) 0) {
	FPRINTF(stderr, "MPI_Type_create_darray: array_of_psizes is an invalid address\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    for (i=0; i<ndims; i++) {
	if (array_of_gsizes[i] <= 0) {
	    FPRINTF(stderr, "MPI_Type_create_darray: Invalid value in array_of_gsizes\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	/* array_of_distribs checked below */

	if ((array_of_dargs[i] != MPI_DISTRIBUTE_DFLT_DARG) && 
	                 (array_of_dargs[i] <= 0)) {
	    FPRINTF(stderr, "MPI_Type_create_darray: Invalid value in array_of_dargs\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	if (array_of_psizes[i] <= 0) {
	    FPRINTF(stderr, "MPI_Type_create_darray: Invalid value in array_of_psizes\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
    }

    /* order argument checked below */

    if (oldtype == MPI_DATATYPE_NULL) {
	FPRINTF(stderr, "MPI_Type_create_darray: oldtype is an invalid datatype\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Type_extent(oldtype, &orig_extent);

/* check if MPI_Aint is large enough for size of global array. 
   if not, complain. */

    size_with_aint = orig_extent;
    for (i=0; i<ndims; i++) size_with_aint *= array_of_gsizes[i];
    size_with_offset = orig_extent;
    for (i=0; i<ndims; i++) size_with_offset *= array_of_gsizes[i];
    if (size_with_aint != size_with_offset) {
	FPRINTF(stderr, "MPI_Type_create_darray: Can't use an array of this size unless the MPI implementation defines a 64-bit MPI_Aint\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

/* calculate position in Cartesian grid as MPI would (row-major
   ordering) */
    coords = (int *) ADIOI_Malloc(ndims*sizeof(int));
    procs = size;
    tmp_rank = rank;
    for (i=0; i<ndims; i++) {
	procs = procs/array_of_psizes[i];
	coords[i] = tmp_rank/procs;
	tmp_rank = tmp_rank % procs;
    }

    st_offsets = (MPI_Aint *) ADIOI_Malloc(ndims*sizeof(MPI_Aint));
    type_old = oldtype;

    if (order == MPI_ORDER_FORTRAN) {
      /* dimension 0 changes fastest */
	for (i=0; i<ndims; i++) {
	    switch(array_of_distribs[i]) {
	    case MPI_DISTRIBUTE_BLOCK:
		MPIOI_Type_block(array_of_gsizes, i, ndims, array_of_psizes[i],
			 coords[i], array_of_dargs[i], order, orig_extent, 
			      type_old, &type_new, st_offsets+i); 
		break;
	    case MPI_DISTRIBUTE_CYCLIC:
		MPIOI_Type_cyclic(array_of_gsizes, i, ndims, 
		   array_of_psizes[i], coords[i], array_of_dargs[i], order,
                        orig_extent, type_old, &type_new, st_offsets+i);
		break;
	    case MPI_DISTRIBUTE_NONE:
		if (array_of_psizes[i] != 1) {
		    FPRINTF(stderr, "MPI_Type_create_darray: For MPI_DISTRIBUTE_NONE, the number of processes in that dimension of the grid must be 1\n");
		    MPI_Abort(MPI_COMM_WORLD, 1);
		}
		/* treat it as a block distribution on 1 process */
		MPIOI_Type_block(array_of_gsizes, i, ndims, 1, 0, 
		      MPI_DISTRIBUTE_DFLT_DARG, order, orig_extent, 
                           type_old, &type_new, st_offsets+i); 
		break;
	    default:
		FPRINTF(stderr, "MPI_Type_create_darray: Invalid value in array_of_distribs\n");
		MPI_Abort(MPI_COMM_WORLD, 1);
	    }
	    if (i) MPI_Type_free(&type_old);
	    type_old = type_new;
	}

	/* add displacement and UB */
	disps[1] = st_offsets[0];
	tmp_size = 1;
	for (i=1; i<ndims; i++) {
	    tmp_size *= array_of_gsizes[i-1];
	    disps[1] += tmp_size*st_offsets[i];
	}
        /* rest done below for both Fortran and C order */
    }

    else if (order == MPI_ORDER_C) {
        /* dimension ndims-1 changes fastest */
	for (i=ndims-1; i>=0; i--) {
	    switch(array_of_distribs[i]) {
	    case MPI_DISTRIBUTE_BLOCK:
		MPIOI_Type_block(array_of_gsizes, i, ndims, array_of_psizes[i],
		      coords[i], array_of_dargs[i], order, orig_extent, 
                           type_old, &type_new, st_offsets+i); 
		break;
	    case MPI_DISTRIBUTE_CYCLIC:
		MPIOI_Type_cyclic(array_of_gsizes, i, ndims, 
		      array_of_psizes[i], coords[i], array_of_dargs[i], order, 
			  orig_extent, type_old, &type_new, st_offsets+i);
		break;
	    case MPI_DISTRIBUTE_NONE:
		if (array_of_psizes[i] != 1) {
		    FPRINTF(stderr, "MPI_Type_create_darray: For MPI_DISTRIBUTE_NONE, the number of processes in that dimension of the grid must be 1\n");
		    MPI_Abort(MPI_COMM_WORLD, 1);
		}
		/* treat it as a block distribution on 1 process */
		MPIOI_Type_block(array_of_gsizes, i, ndims, array_of_psizes[i],
		      coords[i], MPI_DISTRIBUTE_DFLT_DARG, order, orig_extent, 
                           type_old, &type_new, st_offsets+i); 
		break;
	    default:
		FPRINTF(stderr, "MPI_Type_create_darray: Invalid value in array_of_distribs\n");
		MPI_Abort(MPI_COMM_WORLD, 1);
	    }
	    if (i != ndims-1) MPI_Type_free(&type_old);
	    type_old = type_new;
	}

	/* add displacement and UB */
	disps[1] = st_offsets[ndims-1];
	tmp_size = 1;
	for (i=ndims-2; i>=0; i--) {
	    tmp_size *= array_of_gsizes[i+1];
	    disps[1] += tmp_size*st_offsets[i];
	}
    }
    else {
        FPRINTF(stderr, "MPI_Type_create_darray: Invalid order argument\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    disps[1] *= orig_extent;

    disps[2] = orig_extent;
    for (i=0; i<ndims; i++) disps[2] *= array_of_gsizes[i];
	
    disps[0] = 0;
    blklens[0] = blklens[1] = blklens[2] = 1;
    types[0] = MPI_LB;
    types[1] = type_new;
    types[2] = MPI_UB;
    
    MPI_Type_struct(3, blklens, disps, types, newtype);

    MPI_Type_free(&type_new);
    ADIOI_Free(st_offsets);
    ADIOI_Free(coords);
    return MPI_SUCCESS;
}


#ifndef MPIO_BUILD_PROFILING
void MPIOI_Type_block(int *array_of_gsizes, int dim, int ndims, int nprocs,
		      int rank, int darg, int order, MPI_Aint orig_extent,
		      MPI_Datatype type_old, MPI_Datatype *type_new,
		      MPI_Aint *st_offset) 
{
/* nprocs = no. of processes in dimension dim of grid
   rank = coordinate of this process in dimension dim */

    int blksize, global_size, mysize, i, j;
    MPI_Aint stride;
    
    global_size = array_of_gsizes[dim];

    if (darg == MPI_DISTRIBUTE_DFLT_DARG)
	blksize = (global_size + nprocs - 1)/nprocs;
    else {
	blksize = darg;
	if (blksize <= 0) {
	   FPRINTF(stderr, "MPI_Type_create_darray: m <= 0 is not valid for a block(m) distribution\n");
	   MPI_Abort(MPI_COMM_WORLD, 1);
	}
	if (blksize * nprocs < global_size) {
	    FPRINTF(stderr, "MPI_Type_create_darray: m * nprocs < array_size is not valid for a block(m) distribution\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
    }

    j = global_size - blksize*rank;
    mysize = ADIOI_MIN(blksize, j);
    if (mysize < 0) mysize = 0;

    stride = orig_extent;
    if (order == MPI_ORDER_FORTRAN) {
	if (dim == 0) 
	    MPI_Type_contiguous(mysize, type_old, type_new);
	else {
	    for (i=0; i<dim; i++) stride *= array_of_gsizes[i];
	    MPI_Type_hvector(mysize, 1, stride, type_old, type_new);
	}
    }
    else {
	if (dim == ndims-1) 
	    MPI_Type_contiguous(mysize, type_old, type_new);
	else {
	    for (i=ndims-1; i>dim; i--) stride *= array_of_gsizes[i];
	    MPI_Type_hvector(mysize, 1, stride, type_old, type_new);
	}

    }

    *st_offset = blksize * rank;
     /* in terms of no. of elements of type oldtype in this dimension */
    if (mysize == 0) *st_offset = 0;
}


void MPIOI_Type_cyclic(int *array_of_gsizes, int dim, int ndims, int nprocs,
		      int rank, int darg, int order, MPI_Aint orig_extent,
		      MPI_Datatype type_old, MPI_Datatype *type_new,
		      MPI_Aint *st_offset) 
{
/* nprocs = no. of processes in dimension dim of grid
   rank = coordinate of this process in dimension dim */

    int blksize, i, blklens[2], st_index, end_index, local_size, rem, count;
    MPI_Aint stride, disps[2];
    MPI_Datatype type_tmp, types[2];

    if (darg == MPI_DISTRIBUTE_DFLT_DARG) blksize = 1;
    else blksize = darg;

    if (blksize <= 0) {
	FPRINTF(stderr, "MPI_Type_create_darray: m <= 0 is not valid for a cyclic(m) distribution\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    
    st_index = rank*blksize;
    end_index = array_of_gsizes[dim] - 1;

    if (end_index < st_index) local_size = 0;
    else {
	local_size = ((end_index - st_index + 1)/(nprocs*blksize))*blksize;
	rem = (end_index - st_index + 1) % (nprocs*blksize);
	local_size += ADIOI_MIN(rem, blksize);
    }

    count = local_size/blksize;
    rem = local_size % blksize;
    
    stride = nprocs*blksize*orig_extent;
    if (order == MPI_ORDER_FORTRAN)
	for (i=0; i<dim; i++) stride *= array_of_gsizes[i];
    else for (i=ndims-1; i>dim; i--) stride *= array_of_gsizes[i];

    MPI_Type_hvector(count, blksize, stride, type_old, type_new);

    if (rem) {
	/* if the last block is of size less than blksize, include
	   it separately using MPI_Type_struct */

	types[0] = *type_new;
	types[1] = type_old;
	disps[0] = 0;
	disps[1] = count*stride;
	blklens[0] = 1;
	blklens[1] = rem;

	MPI_Type_struct(2, blklens, disps, types, &type_tmp);

	MPI_Type_free(type_new);
	*type_new = type_tmp;
    }

    /* need to set the UB for block-cyclic to work */
    types[0] = *type_new;
    types[1] = MPI_UB;
    disps[0] = 0;
    disps[1] = orig_extent;
    if (order == MPI_ORDER_FORTRAN)
	for (i=0; i<=dim; i++) disps[1] *= array_of_gsizes[i];
    else for (i=ndims-1; i>=dim; i--) disps[1] *= array_of_gsizes[i];
    blklens[0] = blklens[1] = 1;
    MPI_Type_struct(2, blklens, disps, types, &type_tmp);
    MPI_Type_free(type_new);
    *type_new = type_tmp;

    *st_offset = rank * blksize; 
     /* in terms of no. of elements of type oldtype in this dimension */
    if (local_size == 0) *st_offset = 0;
}
#endif
