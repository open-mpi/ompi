/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *   Copyright (C) 2008 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "../adio/include/adio.h"
#include "../adio/include/adio_extern.h"
#include "mpi.h"

int main (int argc, char **argv)
{
    int i;
    ADIO_File fd;
    ADIO_Offset min_st_offset, max_end_offset;
    int rank;
    int nprocs_for_coll;
    int lb;
    int size, extent;

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (MPI_COMM_WORLD, &rank);

    if (argc != 4) {
	if (!rank)
	    printf ("Usage: file_realms_test <number of aggregators> <lower bound> <upper bound>\n"
		    "    simulates file_realm calculation\n");
	MPI_Finalize();
	return 1;
    }

    nprocs_for_coll = atoi (argv[1]);

    min_st_offset = atoi (argv[2]);
    max_end_offset = atoi (argv[3]);

    if (max_end_offset < min_st_offset){
	if (!rank)
	    printf ("end offset %lld is less then start offset %lld\n",
		    max_end_offset, min_st_offset);
	MPI_Finalize();
	return 1;
    }

    printf ("min_st_offset = %lld\nmax_end_offset = %lld\n",
	    min_st_offset, max_end_offset);

    fd = (ADIO_File) ADIOI_Malloc (sizeof (struct ADIOI_FileD));
    fd->hints = (ADIOI_Hints *)
	ADIOI_Malloc (sizeof(struct ADIOI_Hints_struct));
    fd->hints->cb_nodes = nprocs_for_coll;
    ADIOI_Calc_file_realms (fd, min_st_offset, max_end_offset);

    for (i=0; i < nprocs_for_coll; i++) {
	printf ("file_realm_st_offs[%d] = %lld\n", i, fd->file_realm_st_offs[i]);
    }
    for (i=0; i < nprocs_for_coll; i++) {
	MPI_Type_size (fd->file_realm_types[i], &size);
	printf ("file_realm [%d] size = %d\n", i, size);
    }
    for (i=0; i < nprocs_for_coll; i++) {
	MPI_Type_get_extent (fd->file_realm_types[i], &lb, &extent);
	printf ("file_realm [%d] extent = %d\n", i, extent);
    }

    for (i=0; i < nprocs_for_coll; i++)
	MPI_Type_free (&fd->file_realm_types[i]);
    ADIOI_Free (fd->file_realm_st_offs);
    ADIOI_Free (fd->file_realm_types);
    ADIOI_Free (fd->hints);
    ADIOI_Free (fd);

    MPI_Finalize();

    return 0;
}
