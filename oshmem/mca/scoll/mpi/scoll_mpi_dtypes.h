#ifndef SCOLL_MPI_DTYPES_H
#define SCOLL_MPI_DTYPES_H

#include "oshmem/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"

static struct ompi_datatype_t* shmem_dtype_to_ompi_dtype(oshmem_op_t *op)
{
    int dtype = op->dt;
    int dtsize = op->dt_size * 8;
    switch (dtype) {
    case OSHMEM_OP_TYPE_FLOAT:
        return &ompi_mpi_float.dt;
    case OSHMEM_OP_TYPE_DOUBLE:
        return &ompi_mpi_double.dt;
    case OSHMEM_OP_TYPE_LDOUBLE:
        return &ompi_mpi_long_double.dt;
    case OSHMEM_OP_TYPE_FCOMPLEX:
        return &ompi_mpi_c_float_complex.dt;
    case OSHMEM_OP_TYPE_DCOMPLEX:
        return &ompi_mpi_c_double_complex.dt;
    default:
        switch (dtsize) {
            case 64:
                return &ompi_mpi_int64_t.dt;
            case 32:
                return &ompi_mpi_int32_t.dt;
            case 16:
                return &ompi_mpi_int16_t.dt;
            case 8:
                return &ompi_mpi_int8_t.dt;
            default:
                return &ompi_mpi_datatype_null.dt;
        }
    }
}

static struct ompi_op_t* shmem_op_to_ompi_op(int op)
{
    switch (op) {
    case OSHMEM_OP_AND:
        return &ompi_mpi_op_band;
    case OSHMEM_OP_OR:
        return &ompi_mpi_op_bor;
    case OSHMEM_OP_XOR:
        return &ompi_mpi_op_bxor;
    case OSHMEM_OP_MAX:
        return &ompi_mpi_op_max;
    case OSHMEM_OP_MIN:
        return &ompi_mpi_op_min;
    case OSHMEM_OP_SUM:
        return &ompi_mpi_op_sum;
    case OSHMEM_OP_PROD:
        return &ompi_mpi_op_prod;
    default:
        return &ompi_mpi_op_null;
    }
}

#endif /* SCOLL_MPI_DTYPES_H */
