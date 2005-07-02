/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: subarray.c,v 1.8 2002/10/24 17:01:27 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_Type_create_subarray = PMPI_Type_create_subarray
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_Type_create_subarray MPI_Type_create_subarray
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_Type_create_subarray as PMPI_Type_create_subarray
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
MPI_Type_create_subarray - Creates a datatype describing a subarray of a multidimensional array

Input Parameters:
. ndims - number of array dimensions (positive integer)
. array_of_sizes - number of elements of type oldtype in each dimension of the full array (array of positive integers)
. array_of_subsizes - number of elements of type oldtype in each dimension of the subarray (array of positive integers)
. array_of_starts - starting coordinates of the subarray in each dimension (array of nonnegative integers)
. order - array storage order flag (state)
. oldtype - old datatype (handle)

Output Parameters:
. newtype - new datatype (handle)

.N fortran
@*/
int MPI_Type_create_subarray(int ndims, int *array_of_sizes, 
                             int *array_of_subsizes, int *array_of_starts,
                             int order, MPI_Datatype oldtype, 
                             MPI_Datatype *newtype)
{
    MPI_Aint extent, disps[3], size, size_with_aint;
    int i, blklens[3];
    MPI_Datatype tmp1, tmp2, types[3];
    MPI_Offset size_with_offset;

    if (ndims <= 0) {
	FPRINTF(stderr, "MPI_Type_create_subarray: Invalid ndims argument\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    if (array_of_sizes <= (int *) 0) {
	FPRINTF(stderr, "MPI_Type_create_subarray: array_of_sizes is an invalid address\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    if (array_of_subsizes <= (int *) 0) {
	FPRINTF(stderr, "MPI_Type_create_subarray: array_of_subsizes is an invalid address\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    if (array_of_starts <= (int *) 0) {
	FPRINTF(stderr, "MPI_Type_create_subarray: array_of_starts is an invalid address\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    for (i=0; i<ndims; i++) {
        if (array_of_sizes[i] <= 0) {
            FPRINTF(stderr, "MPI_Type_create_subarray: Invalid value in array_of_sizes\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        if (array_of_subsizes[i] <= 0) {
            FPRINTF(stderr, "MPI_Type_create_subarray: Invalid value in array_of_subsizes\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        if (array_of_starts[i] < 0) {
            FPRINTF(stderr, "MPI_Type_create_subarray: Invalid value in array_of_starts\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        if (array_of_subsizes[i] > array_of_sizes[i]) {
            FPRINTF(stderr, "MPI_Type_create_subarray: Error! array_of_subsizes[%d] > array_of_sizes[%d]\n", i, i);
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        if (array_of_starts[i] > (array_of_sizes[i] - array_of_subsizes[i])) {
            FPRINTF(stderr, "MPI_Type_create_subarray: Error! array_of_starts[%d] > (array_of_sizes[%d] - array_of_subsizes[%d])\n", i, i, i);
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }

    /* order argument checked below */

    if (oldtype == MPI_DATATYPE_NULL) {
        FPRINTF(stderr, "MPI_Type_create_subarray: oldtype is an invalid datatype\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Type_extent(oldtype, &extent);

/* check if MPI_Aint is large enough for size of global array. 
   if not, complain. */

    size_with_aint = extent;
    for (i=0; i<ndims; i++) size_with_aint *= array_of_sizes[i];
    size_with_offset = extent;
    for (i=0; i<ndims; i++) size_with_offset *= array_of_sizes[i];
    if (size_with_aint != size_with_offset) {
	FPRINTF(stderr, "MPI_Type_create_subarray: Can't use an array of this size unless the MPI implementation defines a 64-bit MPI_Aint\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (order == MPI_ORDER_FORTRAN) {
      /* dimension 0 changes fastest */
	if (ndims == 1)
	    MPI_Type_contiguous(array_of_subsizes[0], oldtype, &tmp1);
	else {
	    MPI_Type_vector(array_of_subsizes[1], array_of_subsizes[0],
			    array_of_sizes[0], oldtype, &tmp1);
	    
	    size = array_of_sizes[0]*extent;
	    for (i=2; i<ndims; i++) {
		size *= array_of_sizes[i-1];
		MPI_Type_hvector(array_of_subsizes[i], 1, size, tmp1, &tmp2);
		MPI_Type_free(&tmp1);
		tmp1 = tmp2;
	    }
	}
	
	/* add displacement and UB */
	
	disps[1] = array_of_starts[0];
	size = 1;
	for (i=1; i<ndims; i++) {
	    size *= array_of_sizes[i-1];
	    disps[1] += size*array_of_starts[i];
	}  
        /* rest done below for both Fortran and C order */
    }

    else if (order == MPI_ORDER_C) {
	/* dimension ndims-1 changes fastest */
	if (ndims == 1)
	    MPI_Type_contiguous(array_of_subsizes[0], oldtype, &tmp1);
	else {
	    MPI_Type_vector(array_of_subsizes[ndims-2],
			    array_of_subsizes[ndims-1],
			    array_of_sizes[ndims-1], oldtype, &tmp1);
	    
	    size = array_of_sizes[ndims-1]*extent;
	    for (i=ndims-3; i>=0; i--) {
		size *= array_of_sizes[i+1];
		MPI_Type_hvector(array_of_subsizes[i], 1, size, tmp1, &tmp2);
		MPI_Type_free(&tmp1);
		tmp1 = tmp2;
	    }
	}
	
	/* add displacement and UB */
	
	disps[1] = array_of_starts[ndims-1];
	size = 1;
	for (i=ndims-2; i>=0; i--) {
	    size *= array_of_sizes[i+1];
	    disps[1] += size*array_of_starts[i];
	}
    }
    else {
	FPRINTF(stderr, "MPI_Type_create_subarray: Invalid order argument\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    
    disps[1] *= extent;
    
    disps[2] = extent;
    for (i=0; i<ndims; i++) disps[2] *= array_of_sizes[i];
    
    disps[0] = 0;
    blklens[0] = blklens[1] = blklens[2] = 1;
    types[0] = MPI_LB;
    types[1] = tmp1;
    types[2] = MPI_UB;
    
    MPI_Type_struct(3, blklens, disps, types, newtype);

    MPI_Type_free(&tmp1);

    return MPI_SUCCESS;
}
