/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_testfs_delete.c,v 1.2 2002/10/24 17:01:03 gropp Exp $    
 *
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_testfs.h"
#include "adioi.h"

void ADIOI_TESTFS_Delete(char *filename, int *error_code)
{
    int myrank, nprocs;

    *error_code = MPI_SUCCESS;

    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
    FPRINTF(stdout, "[%d/%d] ADIOI_TESTFS_Delete called on %s\n", 
	    myrank, nprocs, filename);
}
