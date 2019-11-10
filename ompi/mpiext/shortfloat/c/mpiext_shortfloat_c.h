/*
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

OMPI_DECLSPEC extern struct ompi_predefined_datatype_t ompi_mpi_short_float;
OMPI_DECLSPEC extern struct ompi_predefined_datatype_t ompi_mpi_c_short_float_complex;
OMPI_DECLSPEC extern struct ompi_predefined_datatype_t ompi_mpi_cxx_sfltcplex;

#define MPIX_SHORT_FLOAT             OMPI_PREDEFINED_GLOBAL(MPI_Datatype, ompi_mpi_short_float)
#define MPIX_C_SHORT_FLOAT_COMPLEX   OMPI_PREDEFINED_GLOBAL(MPI_Datatype, ompi_mpi_c_short_float_complex)
#define MPIX_CXX_SHORT_FLOAT_COMPLEX OMPI_PREDEFINED_GLOBAL(MPI_Datatype, ompi_mpi_cxx_sfltcplex)

#if 0
#define MPIX_C_FLOAT16               OMPI_PREDEFINED_GLOBAL(MPI_Datatype, ompi_mpi_short_float)
#endif
