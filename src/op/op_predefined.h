/*
 * $HEADER$
 */

#ifndef LAM_OP_PREDEFINED_H
#define LAM_OP_PREDEFINED_H

#include "op/op.h"

/**
 * Handler function for MPI_MAX
 */
void lam_mpi_op_max_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_MIN
 */
void lam_mpi_op_min_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_SUM
 */
void lam_mpi_op_sum_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_PROD
 */
void lam_mpi_op_prod_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_LAND
 */
void lam_mpi_op_land_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_BAND
 */
void lam_mpi_op_band_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_LOR
 */
void lam_mpi_op_lor_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_BOR
 */
void lam_mpi_op_bor_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_LXOR
 */
void lam_mpi_op_lxor_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_BXOR
 */
void lam_mpi_op_bxor_func(void *in, void *out, int *count, MPI_Datatype *type);

/**
 * Handler function for MPI_MAXLOC
 */
void lam_mpi_op_maxloc_func(void *in, void *out, int *count, 
                            MPI_Datatype *type);

/**
 * Handler function for MPI_MINLOC
 */
void lam_mpi_op_minloc_func(void *in, void *out, int *count, 
                            MPI_Datatype *type);

/**
 * Handler function for MPI_REPLACE
 */
void lam_mpi_op_replace_func(void *in, void *out, int *count, 
                             MPI_Datatype *type);


#endif /* LAM_OP_PREDEFINED_H */
