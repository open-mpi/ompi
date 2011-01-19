/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef DATALOOP_CREATE_H
#define DATALOOP_CREATE_H

/* Dataloop construction functions */
void PREPEND_PREFIX(Dataloop_create)(MPI_Datatype type,
				     DLOOP_Dataloop **dlp_p,
				     int *dlsz_p,
				     int *dldepth_p,
				     int flag);
int PREPEND_PREFIX(Dataloop_create_contiguous)(int count,
					       MPI_Datatype oldtype,
					       DLOOP_Dataloop **dlp_p,
					       int *dlsz_p,
					       int *dldepth_p,
					       int flag);
int PREPEND_PREFIX(Dataloop_create_vector)(int count,
					   int blocklength,
					   MPI_Aint stride,
					   int strideinbytes,
					   MPI_Datatype oldtype,
					   DLOOP_Dataloop **dlp_p,
					   int *dlsz_p,
					   int *dldepth_p,
					   int flag);
int PREPEND_PREFIX(Dataloop_create_blockindexed)(int count,
						 int blklen,
						 void *disp_array,
						 int dispinbytes,
						 MPI_Datatype oldtype,
						 DLOOP_Dataloop **dlp_p,
						 int *dlsz_p,
						 int *dldepth_p,
						 int flag);
int PREPEND_PREFIX(Dataloop_create_indexed)(int count,
					    int *blocklength_array,
					    void *displacement_array,
					    int dispinbytes,
					    MPI_Datatype oldtype,
					    DLOOP_Dataloop **dlp_p,
					    int *dlsz_p,
					    int *dldepth_p,
					    int flag);
int PREPEND_PREFIX(Dataloop_create_struct)(int count,
					   int *blklen_array,
					   MPI_Aint *disp_array,
					   MPI_Datatype *oldtype_array,
					   DLOOP_Dataloop **dlp_p,
					   int *dlsz_p,
					   int *dldepth_p,
					   int flag);
int PREPEND_PREFIX(Dataloop_create_pairtype)(MPI_Datatype type,
					     DLOOP_Dataloop **dlp_p,
					     int *dlsz_p,
					     int *dldepth_p,
					     int flag);

/* Helper functions for dataloop construction */
int PREPEND_PREFIX(Type_convert_subarray)(int ndims,
					  int *array_of_sizes, 
					  int *array_of_subsizes,
					  int *array_of_starts,
					  int order,
					  MPI_Datatype oldtype, 
					  MPI_Datatype *newtype);
int PREPEND_PREFIX(Type_convert_darray)(int size,
					int rank,
					int ndims, 
					int *array_of_gsizes,
					int *array_of_distribs, 
					int *array_of_dargs,
					int *array_of_psizes, 
					int order,
					MPI_Datatype oldtype, 
					MPI_Datatype *newtype);

#endif
