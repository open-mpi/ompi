   /*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include "oshmem/shmem/fortran/bindings.h"
#include "oshmem/include/shmem.h"
#include "oshmem/shmem/shmem_api_logger.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/mca/spml/spml.h"
#include "ompi/datatype/ompi_datatype.h"
#include "stdio.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"

SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_GETMEM_NBI, shmem_getmem_nbi)

SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_CHARACTER_GET_NBI, shmem_character_get_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_COMPLEX_GET_NBI, shmem_complex_get_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_DOUBLE_GET_NBI, shmem_double_get_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_INTEGER_GET_NBI, shmem_integer_get_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_LOGICAL_GET_NBI, shmem_logical_get_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_REAL_GET_NBI, shmem_real_get_nbi)

SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_GET4_NBI, shmem_get4_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_GET8_NBI, shmem_get8_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_GET32_NBI, shmem_get32_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_GET64_NBI, shmem_get64_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_GET128_NBI, shmem_get128_nbi)

#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_GETMEM_NBI,
        shmem_getmem_nbi_,
        shmem_getmem_nbi__,
        shmem_getmem_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_getmem_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len),
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_CHARACTER_GET_NBI,
        shmem_character_get_nbi_,
        shmem_character_get_nbi__,
        shmem_character_get_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe))

void shmem_character_get_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    size_t character_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_character.dt, &character_type_size);

    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * character_type_size,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_COMPLEX_GET_NBI,
        shmem_complex_get_nbi_,
        shmem_complex_get_nbi__,
        shmem_complex_get_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_complex_get_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    size_t complex_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_cplex.dt, &complex_type_size);

    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * complex_type_size,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_DOUBLE_GET_NBI,
        shmem_double_get_nbi_,
        shmem_double_get_nbi__,
        shmem_double_get_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_double_get_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    size_t double_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_dblprec.dt, &double_type_size);

    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * double_type_size,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_INTEGER_GET_NBI,
        shmem_integer_get_nbi_,
        shmem_integer_get_nbi__,
        shmem_integer_get_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_integer_get_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    size_t integer_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_integer.dt, &integer_type_size);

    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * integer_type_size,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_LOGICAL_GET_NBI,
        shmem_logical_get_nbi_,
        shmem_logical_get_nbi__,
        shmem_logical_get_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_logical_get_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    size_t logical_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_logical.dt, &logical_type_size);

    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * logical_type_size,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_REAL_GET_NBI,
        shmem_real_get_nbi_,
        shmem_real_get_nbi__,
        shmem_real_get_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_real_get_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    size_t real_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_real.dt, &real_type_size);

    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * real_type_size,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}


SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_GET4_NBI,
        shmem_get4_nbi_,
        shmem_get4_nbi__,
        shmem_get4_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_get4_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * 4,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_GET8_NBI,
        shmem_get8_nbi_,
        shmem_get8_nbi__,
        shmem_get8_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_get8_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * 8,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_GET32_NBI,
        shmem_get32_nbi_,
        shmem_get32_nbi__,
        shmem_get32_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_get32_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * 4,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_GET64_NBI,
        shmem_get64_nbi_,
        shmem_get64_nbi__,
        shmem_get64_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_get64_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * 8,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_GET128_NBI,
        shmem_get128_nbi_,
        shmem_get128_nbi__,
        shmem_get128_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe),
        (target,source,len,pe) )

void shmem_get128_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    MCA_SPML_CALL(get_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*len) * 16,
        FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*pe), NULL));
}
