/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *   Copyright (C) 2014 UChicgo/Argonne, LLC.
 *   See COPYRIGHT notice in top-level directory.
 */

#include <adio.h>
#include <limits.h>


/* utility function for creating large contiguous types: algorithim from BigMPI
 * https://github.com/jeffhammond/BigMPI */

static int type_create_contiguous_x(MPI_Count count,
	MPI_Datatype oldtype, MPI_Datatype *newtype)
{
    /* to make 'count' fit MPI-3 type processing routines (which take integer
     * counts), we construct a type consisting of N INT_MAX chunks followed by
     * a remainder.  e.g for a count of 4000000000 bytes you would end up with
     * one 2147483647-byte chunk followed immediately by a 1852516353-byte
     * chunk */
    MPI_Datatype chunks, remainder;
    MPI_Aint lb, extent, disps[2];
    int blocklens[2];
    MPI_Datatype types[2];

    MPI_Count c = count/INT_MAX;
    MPI_Count r = count%INT_MAX;

    MPI_Type_vector(c, INT_MAX, INT_MAX, oldtype, &chunks);
    MPI_Type_contiguous(r, oldtype, &remainder);

    MPI_Type_get_extent(oldtype, &lb, &extent);

    blocklens[0] = 1;      blocklens[1] = 1;
    disps[0]     = 0;      disps[1]     = c*extent*INT_MAX;
    types[0]     = chunks; types[1]     = remainder;

    MPI_Type_create_struct(2, blocklens, disps, types, newtype);

    MPI_Type_free(&chunks);
    MPI_Type_free(&remainder);

    return MPI_SUCCESS;
}
/* like MPI_Type_create_hindexed, except array_of_lengths can be a larger datatype.
 *
 * Hindexed provides 'count' pairs of (displacement, length), but what if
 * length is longer than an integer?  We will create 'count' types, using
 * contig if length is small enough, or something more complex if not */

int ADIOI_Type_create_hindexed_x(int count,
		const MPI_Count array_of_blocklengths[],
		const MPI_Aint array_of_displacements[],
		MPI_Datatype oldtype,
		MPI_Datatype *newtype)
{
    int i, ret;
    MPI_Datatype *types;
    int *blocklens;

    types = ADIOI_Malloc(count*sizeof(MPI_Datatype));
    blocklens = ADIOI_Malloc(count*sizeof(int));

    for(i=0; i<count; i++) {
	blocklens[i] = 1;
	type_create_contiguous_x(array_of_blocklengths[i], oldtype,  &(types[i]));
    }

    ret = MPI_Type_create_struct(count, blocklens, array_of_displacements,
	    types, newtype);
    for (i=0; i< count; i++)
	MPI_Type_free(&(types[i]));
    ADIOI_Free(types);
    ADIOI_Free(blocklens);

    return ret;
}

/*
 * vim: ts=8 sts=4 sw=4 noexpandtab
 */
