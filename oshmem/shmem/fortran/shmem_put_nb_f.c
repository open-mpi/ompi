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

SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_PUTMEM_NBI, shmem_putmem_nbi)

SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_CHARACTER_PUT_NBI, shmem_character_put_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_COMPLEX_PUT_NBI, shmem_complex_put_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_DOUBLE_PUT_NBI, shmem_double_put_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_INTEGER_PUT_NBI, shmem_integer_put_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_LOGICAL_PUT_NBI, shmem_logical_put_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_REAL_PUT_NBI, shmem_real_put_nbi)

SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_PUT4_NBI, shmem_put4_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_PUT8_NBI, shmem_put8_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_PUT32_NBI, shmem_put32_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_PUT64_NBI, shmem_put64_nbi)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_PUT128_NBI, shmem_put128_nbi)

#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_PUTMEM_NBI,
        shmem_putmem_nbi_,
        shmem_putmem_nbi__,
        shmem_putmem_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_putmem_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length),
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_CHARACTER_PUT_NBI,
        shmem_character_put_nbi_,
        shmem_character_put_nbi__,
        shmem_character_put_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_character_put_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    size_t character_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_character.dt, &character_type_size);

    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * character_type_size,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_COMPLEX_PUT_NBI,
        shmem_complex_put_nbi_,
        shmem_complex_put_nbi__,
        shmem_complex_put_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_complex_put_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    size_t complex_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_cplex.dt, &complex_type_size);

    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * complex_type_size,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_DOUBLE_PUT_NBI,
        shmem_double_put_nbi_,
        shmem_double_put_nbi__,
        shmem_double_put_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_double_put_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    size_t double_precision_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_dblprec.dt, &double_precision_type_size);

    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * double_precision_type_size,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_INTEGER_PUT_NBI,
        shmem_integer_put_nbi_,
        shmem_integer_put_nbi__,
        shmem_integer_put_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_integer_put_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    size_t integer_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_integer.dt, &integer_type_size);

    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * integer_type_size,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_LOGICAL_PUT_NBI,
        shmem_logical_put_nbi_,
        shmem_logical_put_nbi__,
        shmem_logical_put_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_logical_put_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    size_t logical_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_logical.dt, &logical_type_size);

    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * logical_type_size,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_REAL_PUT_NBI,
        shmem_real_put_nbi_,
        shmem_real_put_nbi__,
        shmem_real_put_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_real_put_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    size_t real_type_size = 0;
    ompi_datatype_type_size(&ompi_mpi_real.dt, &real_type_size);

    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * real_type_size,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_PUT4_NBI,
        shmem_put4_nbi_,
        shmem_put4_nbi__,
        shmem_put4_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_put4_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * 4,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_PUT8_NBI,
        shmem_put8_nbi_,
        shmem_put8_nbi__,
        shmem_put8_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_put8_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * 8,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_PUT32_NBI,
        shmem_put32_nbi_,
        shmem_put32_nbi__,
        shmem_put32_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_put32_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * 4,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_PUT64_NBI,
        shmem_put64_nbi_,
        shmem_put64_nbi__,
        shmem_put64_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_put64_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * 8,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_PUT128_NBI,
        shmem_put128_nbi_,
        shmem_put128_nbi__,
        shmem_put128_nbi_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe),
        (target,source,length,pe) )

void shmem_put128_nbi_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe)
{
    MCA_SPML_CALL(put_nb(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        OMPI_FINT_2_INT(*length) * 16,
        FPTR_2_VOID_PTR(source),
        OMPI_FINT_2_INT(*pe), NULL));
}
