/*
 * $HEADER$
 */

#ifndef OMPI_OP_PREDEFINED_H
#define OMPI_OP_PREDEFINED_H

#include "op/op.h"

/**
 * Handler function for MPI_MAX
 */
void ompi_mpi_op_max_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_MIN
 */
void ompi_mpi_op_min_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_SUM
 */
void ompi_mpi_op_sum_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_PROD
 */
void ompi_mpi_op_prod_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_LAND
 */
void ompi_mpi_op_land_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_BAND
 */
void ompi_mpi_op_band_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_LOR
 */
void ompi_mpi_op_lor_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_BOR
 */
void ompi_mpi_op_bor_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_LXOR
 */
void ompi_mpi_op_lxor_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_BXOR
 */
void ompi_mpi_op_bxor_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_MAXLOC
 */
void ompi_mpi_op_maxloc_func(void *in, void *out, int *count, 
                            MPI_Datatype *type);

/**
 * Handler function for MPI_MINLOC
 */
void ompi_mpi_op_minloc_func(void *in, void *out, int *count, 
                            MPI_Datatype *type);

/**
 * Handler function for MPI_REPLACE
 */
void ompi_mpi_op_replace_func(void *in, void *out, int *count, 
                             MPI_Datatype *type);


#endif /* OMPI_OP_PREDEFINED_H */
