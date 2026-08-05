/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * The datatype builders and their by-hand pack/unpack baselines below provide a
 * single definition of each construction that any number of tests can share.
 * They use only the public MPI API; the DDTBench_* constructors and baselines
 * are imported and adapted from DDTBench 1.2.1.
 */

#ifdef HAVE_CONFIG_H
#    include "ompi_config.h"
#endif

#include "datatype_corpus.h"

#include "mpi.h"

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Dumping a committed datatype is a consumer reporting concern that reaches into
 * Open MPI internals, so within this public-API-only corpus MPI_DDT_DUMP is a
 * no-op.  A consumer that wants to dump does so from its own driver.
 */
#define MPI_DDT_DUMP(ddt)                                                                  \
    do {                                                                                   \
        (void) (ddt);                                                                      \
    } while (0)

/*
 * The layout mirrored by pack_byhand_create_merged_contig_with_gaps(): an
 * MPI_DOUBLE, an MPI_LONG and an MPI_CHAR laid out exactly like this C struct.
 * Deriving the MPI displacements and the extent from the struct (rather than
 * hard-coding {0, 8, 16} and sizeof(long) == 8) keeps the datatype and its
 * by-hand baseline in sync on ABIs where sizeof(long) != 8 (ILP32, LLP64, ...).
 */
struct merged_contig_with_gaps {
    double d;
    long l;
    char c;
};

static MPI_Datatype create_merged_contig_with_gaps(int count) /* count of the basic datatype */
{
    struct merged_contig_with_gaps data[1];
    int array_of_blocklengths[] = {1, 1, 1};
    MPI_Aint array_of_displacements[3];
    MPI_Datatype array_of_types[] = {MPI_DOUBLE, MPI_LONG, MPI_CHAR};
    MPI_Datatype type, temp;
    MPI_Aint base;

    MPI_Get_address(&data[0], &base);
    MPI_Get_address(&data[0].d, &array_of_displacements[0]);
    MPI_Get_address(&data[0].l, &array_of_displacements[1]);
    MPI_Get_address(&data[0].c, &array_of_displacements[2]);
    array_of_displacements[0] -= base;
    array_of_displacements[1] -= base;
    array_of_displacements[2] -= base;

    MPI_Type_create_struct(3, array_of_blocklengths, array_of_displacements, array_of_types,
                           &temp);
    /* Force the extent to the C struct size so the by-hand baseline can stride
     * by sizeof(struct merged_contig_with_gaps) on every ABI. */
    MPI_Type_create_resized(temp, 0, sizeof(data[0]), &type);
    MPI_Type_free(&temp);
    if (1 < count) {
        temp = type;
        MPI_Type_contiguous(count, temp, &type);
        MPI_Type_free(&temp);
    }
    MPI_Type_commit(&type);
    MPI_DDT_DUMP(type);
    return type;
}

/* Create a non-contiguous resized datatype */
struct structure {
    double not_transferred;
    double transferred_1;
    long   transferred_2;
};

static MPI_Datatype create_struct_constant_gap_resized_ddt(
    int number,      /* IGNORED: number of repetitions */
    int contig_size, /* IGNORED: number of elements in a contiguous chunk */
    int gap_size)    /* IGNORED: number of elements in a gap */
{
    struct structure data[1];
    MPI_Datatype struct_type, temp_type;
    MPI_Datatype types[2] = {MPI_DOUBLE, MPI_LONG};
    int blocklens[2] = {1, 1};
    MPI_Aint disps[3];

    MPI_Get_address(&data[0].transferred_1, &disps[0]);
    MPI_Get_address(&data[0].transferred_2, &disps[1]);
    MPI_Get_address(&data[0], &disps[2]);
    disps[1] -= disps[2]; /* 16 */
    disps[0] -= disps[2]; /*  8 */

    MPI_Type_create_struct(2, blocklens, disps, types, &temp_type);
    MPI_Type_create_resized(temp_type, 0, sizeof(data[0]), &struct_type);
    MPI_Type_commit(&struct_type);
    MPI_Type_free(&temp_type);
    MPI_DDT_DUMP(struct_type);

    return struct_type;
}

/* Create a datatype similar to the one use by HPL */
static MPI_Datatype
create_indexed_constant_gap_ddt(int number,      /* number of repetitions */
                                int contig_size, /* number of elements in a contiguous chunk */
                                int gap_size)    /* number of elements in a gap */
{
    MPI_Datatype dt, *types;
    int i, *bLength;
    MPI_Aint *displ;

    types = (MPI_Datatype *) malloc(sizeof(MPI_Datatype) * number);
    bLength = (int *) malloc(sizeof(int) * number);
    displ = (MPI_Aint *) malloc(sizeof(MPI_Aint) * number);

    types[0] = MPI_DOUBLE;
    bLength[0] = contig_size;
    displ[0] = 0;
    for (i = 1; i < number; i++) {
        types[i] = MPI_DOUBLE;
        bLength[i] = contig_size;
        displ[i] = displ[i - 1] + sizeof(double) * (contig_size + gap_size);
    }
    MPI_Type_create_struct(number, bLength, displ, types, &dt);
    MPI_DDT_DUMP(dt);
    free(types);
    free(bLength);
    free(displ);
    MPI_Type_commit(&dt);
    return dt;
}

/*
 * Build the same typemap as create_indexed_constant_gap_ddt(), but make each
 * contiguous payload a struct-derived subtype before placing the payloads at
 * constant displacements. This avoids relying on the plain homogeneous
 * block/displacement constructor shape when checking for same-size strided
 * fusion opportunities.
 */
static MPI_Datatype
create_struct_constant_gap_ddt(int number,      /* number of repetitions */
                               int contig_size, /* number of elements in a contiguous chunk */
                               int gap_size)    /* number of elements in a gap */
{
    MPI_Datatype payload_type, dt, *types;
    int i, *blocklengths;
    MPI_Aint *displacements;
    int payload_blocklength = contig_size;
    MPI_Aint payload_displacement = 0;
    MPI_Datatype payload_datatype = MPI_DOUBLE;

    types = (MPI_Datatype *) malloc(sizeof(MPI_Datatype) * number);
    blocklengths = (int *) malloc(sizeof(int) * number);
    displacements = (MPI_Aint *) malloc(sizeof(MPI_Aint) * number);

    MPI_Type_create_struct(1, &payload_blocklength, &payload_displacement, &payload_datatype,
                           &payload_type);

    for (i = 0; i < number; i++) {
        types[i] = payload_type;
        blocklengths[i] = 1;
        displacements[i] = (MPI_Aint) i * sizeof(double) * (contig_size + gap_size);
    }

    MPI_Type_create_struct(number, blocklengths, displacements, types, &dt);
    MPI_DDT_DUMP(dt);
    MPI_Type_free(&payload_type);
    free(types);
    free(blocklengths);
    free(displacements);
    MPI_Type_commit(&dt);
    return dt;
}

static MPI_Datatype create_optimized_indexed_constant_gap_ddt(
    int number,      /* number of repetitions */
    int contig_size, /* number of elements in a contiguous chunk */
    int gap_size)    /* number of elements in a gap */
{
    MPI_Datatype dt;

    MPI_Type_vector(number, contig_size, (contig_size + gap_size), MPI_DOUBLE, &dt);
    MPI_Type_commit(&dt);
    MPI_DDT_DUMP(dt);
    return dt;
}

typedef struct {
    int i[2];
    float f;
} internal_struct;
typedef struct {
    int v1;
    int gap1;
    internal_struct is[3];
} ddt_gap;

static MPI_Datatype create_indexed_gap_ddt(void)
{
    ddt_gap dt[2];
    MPI_Datatype dt1, dt2, dt3;
    int bLength[2] = {2, 1};
    MPI_Datatype types[2] = {MPI_INT, MPI_FLOAT};
    MPI_Aint displ[2];

    MPI_Get_address(&(dt[0].is[0].i[0]), &(displ[0]));
    MPI_Get_address(&(dt[0].is[0].f), &(displ[1]));
    displ[1] -= displ[0];
    displ[0] -= displ[0];
    MPI_Type_create_struct(2, bLength, displ, types, &dt1);
    /*MPI_DDT_DUMP( dt1 );*/
    MPI_Type_contiguous(3, dt1, &dt2);
    /*MPI_DDT_DUMP( dt2 );*/
    bLength[0] = 1;
    bLength[1] = 1;
    MPI_Get_address(&(dt[0].v1), &(displ[0]));
    MPI_Get_address(&(dt[0].is[0]), &(displ[1]));
    displ[1] -= displ[0];
    displ[0] -= displ[0];
    types[0] = MPI_INT;
    types[1] = dt2;
    MPI_Type_create_struct(2, bLength, displ, types, &dt3);
    /*MPI_DDT_DUMP( dt3 );*/
    MPI_Type_free(&dt1);
    MPI_Type_free(&dt2);
    MPI_Type_contiguous(10, dt3, &dt1);
    MPI_DDT_DUMP(dt1);
    MPI_Type_free(&dt3);
    MPI_Type_commit(&dt1);
    return dt1;
}

/*
 * Hand-built form equivalent to create_indexed_gap_ddt(). This is the target
 * shape for the optimizer: a 4-byte prologue, 9 repeated 40-byte payloads with
 * extent 44, and a final 36-byte epilogue.
 */
static MPI_Datatype create_indexed_gap_optimized_ddt(void)
{
    MPI_Datatype dt1, dt2, dt3, dt_struct;
    int bLength[3];
    MPI_Datatype types[3];
    MPI_Aint displ[3];

    MPI_Type_contiguous(10, MPI_FLOAT, &dt1);
    MPI_Type_create_resized(dt1, 0, 44, &dt2);

    bLength[0] = 1;
    bLength[1] = 9;
    bLength[2] = 9;

    types[0] = MPI_FLOAT;
    types[1] = dt2;
    types[2] = MPI_FLOAT;

    displ[0] = 0;
    displ[1] = 8;
    displ[2] = 44 * 9 + 8;

    MPI_Type_create_struct(3, bLength, displ, types, &dt_struct);

    /* The inner resized (extent-44) payload plants an upper-bound marker; per the MPI
     * bound-marker model that marker propagates into the enclosing struct and clips its
     * extent to the end of the last payload (404) rather than the trailing float
     * epilogue (440).  Resize the aggregate to the intended 10 * 44 = 440 so this
     * hand-built shape is a faithful optimizer-target equivalent of
     * create_indexed_gap_ddt() (extent 440) and strides identically for count > 1. */
    MPI_Type_create_resized(dt_struct, 0, 44 * 10, &dt3);

    MPI_Type_free(&dt1);
    MPI_Type_free(&dt2);
    MPI_Type_free(&dt_struct);
    MPI_DDT_DUMP(dt3);
    MPI_Type_commit(&dt3);
    return dt3;
}

enum {
    DDTBENCH_INDEX_STEP = 37,

    DDTBENCH_FFT_DIM = 256,
    DDTBENCH_FFT_PROCS = 2,

    DDTBENCH_MILC_DIM2 = 8,
    DDTBENCH_MILC_DIM3 = 8,
    DDTBENCH_MILC_DIM4 = 8,
    DDTBENCH_MILC_DIM5 = 8,

    DDTBENCH_NAS_LU_DIM2 = 12,
    DDTBENCH_NAS_LU_DIM3 = 12,
    DDTBENCH_NAS_LU_X_DIM2 = 1020,
    DDTBENCH_NAS_LU_CELL_DOUBLES = 5,

    DDTBENCH_NAS_MG_DIM1 = 34,
    DDTBENCH_NAS_MG_DIM2 = 18,
    DDTBENCH_NAS_MG_DIM3 = 18,

    DDTBENCH_LAMMPS_FULL_DIM = 3534,
    DDTBENCH_LAMMPS_FULL_ICOUNT = 3062,
    DDTBENCH_LAMMPS_ATOMIC_DIM = 4084,
    DDTBENCH_LAMMPS_ATOMIC_ICOUNT = 243,

    DDTBENCH_SPECFEM3D_OC_DIM = 3697,
    DDTBENCH_SPECFEM3D_OC_ICOUNT = 493,
    DDTBENCH_SPECFEM3D_CM_DIM_CM = 39929,
    DDTBENCH_SPECFEM3D_CM_DIM_IC = 1225,
    DDTBENCH_SPECFEM3D_CM_ICOUNT_CM = 1957,
    DDTBENCH_SPECFEM3D_CM_ICOUNT_IC = 245,
    DDTBENCH_SPECFEM3D_MT_DIM1 = 3,
    DDTBENCH_SPECFEM3D_MT_DIM2 = 2,
    DDTBENCH_SPECFEM3D_MT_DIM3 = 6200,

    DDTBENCH_WRF_NUMBER_2D = 4,
    DDTBENCH_WRF_NUMBER_3D = 3,
    DDTBENCH_WRF_NUMBER_4D = 2,
    DDTBENCH_WRF_DIM1 = 19,
    DDTBENCH_WRF_DIM2 = 65,
    DDTBENCH_WRF_DIM3 = 24,
    DDTBENCH_WRF_LIMIT_4D = 2,
    DDTBENCH_WRF_IS = 9,
    DDTBENCH_WRF_KS = 0,
    DDTBENCH_WRF_JS = 4,
    DDTBENCH_WRF_PARAM_FIRST_SCALAR = 1,
    DDTBENCH_WRF_SUB_DIM1 = 3,
    DDTBENCH_WRF_SUB_DIM2 = 65,
    DDTBENCH_WRF_SUB_DIM3 = 16
};

/*
 * The ddtbench_* datatype constructors and by-hand baselines are imported and
 * adapted from DDTBench 1.2.1.
 */

static inline size_t ddtbench_lammps_full_ax_offset(void)
{
    return 0;
}

static inline size_t ddtbench_lammps_full_atag_offset(void)
{
    return 3 * DDTBENCH_LAMMPS_FULL_DIM * sizeof(double);
}

static inline size_t ddtbench_lammps_full_atype_offset(void)
{
    return ddtbench_lammps_full_atag_offset() + DDTBENCH_LAMMPS_FULL_DIM * sizeof(double);
}

static inline size_t ddtbench_lammps_full_amask_offset(void)
{
    return ddtbench_lammps_full_atype_offset() + DDTBENCH_LAMMPS_FULL_DIM * sizeof(double);
}

static inline size_t ddtbench_lammps_full_aq_offset(void)
{
    return ddtbench_lammps_full_amask_offset() + DDTBENCH_LAMMPS_FULL_DIM * sizeof(double);
}

static inline size_t ddtbench_lammps_full_amolecule_offset(void)
{
    return ddtbench_lammps_full_aq_offset() + DDTBENCH_LAMMPS_FULL_DIM * sizeof(double);
}

static inline size_t ddtbench_lammps_full_extent(void)
{
    return ddtbench_lammps_full_amolecule_offset()
           + DDTBENCH_LAMMPS_FULL_DIM * sizeof(double);
}

static inline size_t ddtbench_lammps_atomic_ax_offset(void)
{
    return 0;
}

static inline size_t ddtbench_lammps_atomic_atag_offset(void)
{
    return 3 * DDTBENCH_LAMMPS_ATOMIC_DIM * sizeof(double);
}

static inline size_t ddtbench_lammps_atomic_atype_offset(void)
{
    return ddtbench_lammps_atomic_atag_offset()
           + DDTBENCH_LAMMPS_ATOMIC_DIM * sizeof(double);
}

static inline size_t ddtbench_lammps_atomic_amask_offset(void)
{
    return ddtbench_lammps_atomic_atype_offset()
           + DDTBENCH_LAMMPS_ATOMIC_DIM * sizeof(double);
}

static inline size_t ddtbench_lammps_atomic_extent(void)
{
    return ddtbench_lammps_atomic_amask_offset()
           + DDTBENCH_LAMMPS_ATOMIC_DIM * sizeof(double);
}

static inline size_t ddtbench_specfem3d_cm_array_ic_offset(void)
{
    return 3 * DDTBENCH_SPECFEM3D_CM_DIM_CM * sizeof(float);
}

static inline size_t ddtbench_specfem3d_cm_extent(void)
{
    return ddtbench_specfem3d_cm_array_ic_offset()
           + 3 * DDTBENCH_SPECFEM3D_CM_DIM_IC * sizeof(float);
}

static inline size_t ddtbench_wrf_2d_array_floats(void)
{
    return DDTBENCH_WRF_DIM1 * DDTBENCH_WRF_DIM3;
}

static inline size_t ddtbench_wrf_3d_array_floats(void)
{
    return DDTBENCH_WRF_DIM1 * DDTBENCH_WRF_DIM2 * DDTBENCH_WRF_DIM3;
}

static inline size_t ddtbench_wrf_4d_array_floats(void)
{
    return DDTBENCH_WRF_DIM1 * DDTBENCH_WRF_DIM2 * DDTBENCH_WRF_DIM3
           * DDTBENCH_WRF_LIMIT_4D;
}

static inline size_t ddtbench_wrf_3d_arrays_offset(void)
{
    return DDTBENCH_WRF_NUMBER_2D * ddtbench_wrf_2d_array_floats() * sizeof(float);
}

static inline size_t ddtbench_wrf_4d_arrays_offset(void)
{
    return ddtbench_wrf_3d_arrays_offset()
           + DDTBENCH_WRF_NUMBER_3D * ddtbench_wrf_3d_array_floats() * sizeof(float);
}

static inline size_t ddtbench_wrf_extent(void)
{
    return ddtbench_wrf_4d_arrays_offset()
           + DDTBENCH_WRF_NUMBER_4D * ddtbench_wrf_4d_array_floats() * sizeof(float);
}

static inline int ddtbench_idx2d(int x, int y, int dim1)
{
    return x + y * dim1;
}

static inline int ddtbench_idx3d(int x, int y, int z, int dim1, int dim2)
{
    return x + dim1 * (y + dim2 * z);
}

static inline int ddtbench_idx4d(int x, int y, int z, int t, int dim1, int dim2, int dim3)
{
    return x + dim1 * (y + dim2 * (z + dim3 * t));
}

static inline int ddtbench_index(int index, int dim)
{
    return (index * DDTBENCH_INDEX_STEP) % dim;
}

static int *ddtbench_alloc_int_array(int count)
{
    int *array = (int *) malloc((size_t) count * sizeof(*array));

    if (NULL == array) {
        fprintf(stderr, "Unable to allocate DDTBench displacement array\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_NO_MEM);
    }
    return array;
}

static void ddtbench_fill_displacements(int *displacements, int count, int dim, int scale)
{
    for (int i = 0; i < count; ++i) {
        displacements[i] = scale * ddtbench_index(i, dim);
    }
}

static MPI_Datatype ddtbench_resize_and_commit(MPI_Datatype raw_type, MPI_Aint extent)
{
    MPI_Datatype type;

    MPI_Type_create_resized(raw_type, 0, extent, &type);
    MPI_Type_commit(&type);
    MPI_Type_free(&raw_type);
    return type;
}

static MPI_Datatype create_ddtbench_fft2d_scatter_ddt(void)
{
    const int columns = DDTBENCH_FFT_DIM / DDTBENCH_FFT_PROCS;
    MPI_Aint extent = 2 * (MPI_Aint) sizeof(double);
    MPI_Datatype complex_type, vector_type, resized_type, scatter_type;

    MPI_Type_contiguous(2, MPI_DOUBLE, &complex_type);
    MPI_Type_vector(columns, 1, DDTBENCH_FFT_DIM, complex_type, &vector_type);
    MPI_Type_create_resized(vector_type, 0, extent, &resized_type);
    MPI_Type_contiguous(columns, resized_type, &scatter_type);
    MPI_Type_commit(&scatter_type);

    MPI_Type_free(&complex_type);
    MPI_Type_free(&vector_type);
    MPI_Type_free(&resized_type);
    return scatter_type;
}

static MPI_Datatype create_ddtbench_fft2d_gather_ddt(void)
{
    const int columns = DDTBENCH_FFT_DIM / DDTBENCH_FFT_PROCS;
    MPI_Aint extent = columns * 2 * (MPI_Aint) sizeof(double);
    MPI_Datatype complex_type, vector_type, gather_type;

    MPI_Type_contiguous(2, MPI_DOUBLE, &complex_type);
    MPI_Type_vector(columns, columns, DDTBENCH_FFT_DIM, complex_type, &vector_type);
    MPI_Type_create_resized(vector_type, 0, extent, &gather_type);
    MPI_Type_commit(&gather_type);

    MPI_Type_free(&complex_type);
    MPI_Type_free(&vector_type);
    return gather_type;
}

static MPI_Datatype create_ddtbench_milc_su3_zdown_ddt(void)
{
    MPI_Datatype su3_vector_type, temp_type, su3_zdown_type;
    MPI_Aint stride;
    int su3_vector_size;

    MPI_Type_contiguous(6, MPI_FLOAT, &su3_vector_type);
    MPI_Type_vector(DDTBENCH_MILC_DIM5, DDTBENCH_MILC_DIM2 * DDTBENCH_MILC_DIM3 / 2,
                    DDTBENCH_MILC_DIM2 * DDTBENCH_MILC_DIM3 * DDTBENCH_MILC_DIM4 / 2,
                    su3_vector_type, &temp_type);
    MPI_Type_size(su3_vector_type, &su3_vector_size);
    stride = (MPI_Aint) su3_vector_size * DDTBENCH_MILC_DIM2 * DDTBENCH_MILC_DIM3
             * DDTBENCH_MILC_DIM4 * DDTBENCH_MILC_DIM5 / 2;
    MPI_Type_create_hvector(2, 1, stride, temp_type, &su3_zdown_type);
    MPI_Type_commit(&su3_zdown_type);

    MPI_Type_free(&su3_vector_type);
    MPI_Type_free(&temp_type);
    return su3_zdown_type;
}

static MPI_Datatype create_ddtbench_nas_lu_y_ddt(void)
{
    MPI_Datatype temp_type, y_type;

    MPI_Type_contiguous(DDTBENCH_NAS_LU_CELL_DOUBLES, MPI_DOUBLE, &temp_type);
    MPI_Type_vector(DDTBENCH_NAS_LU_DIM3, 1, DDTBENCH_NAS_LU_DIM2 + 2, temp_type,
                    &y_type);
    MPI_Type_commit(&y_type);
    MPI_Type_free(&temp_type);
    return y_type;
}

static MPI_Datatype create_ddtbench_nas_lu_x_ddt(void)
{
    MPI_Datatype x_type;

    MPI_Type_contiguous(DDTBENCH_NAS_LU_CELL_DOUBLES * DDTBENCH_NAS_LU_X_DIM2,
                        MPI_DOUBLE, &x_type);
    MPI_Type_commit(&x_type);
    return x_type;
}

static MPI_Datatype create_ddtbench_nas_mg_x_ddt(void)
{
    MPI_Datatype temp_type, face_x_type;
    MPI_Aint stride = DDTBENCH_NAS_MG_DIM1 * DDTBENCH_NAS_MG_DIM2
                      * (MPI_Aint) sizeof(double);

    MPI_Type_vector(DDTBENCH_NAS_MG_DIM2 - 2, 1, DDTBENCH_NAS_MG_DIM1, MPI_DOUBLE,
                    &temp_type);
    MPI_Type_create_hvector(DDTBENCH_NAS_MG_DIM3 - 2, 1, stride, temp_type,
                            &face_x_type);
    MPI_Type_commit(&face_x_type);
    MPI_Type_free(&temp_type);
    return face_x_type;
}

static MPI_Datatype create_ddtbench_nas_mg_y_ddt(void)
{
    MPI_Datatype face_y_type;

    MPI_Type_vector(DDTBENCH_NAS_MG_DIM3 - 2, DDTBENCH_NAS_MG_DIM1 - 2,
                    DDTBENCH_NAS_MG_DIM1 * DDTBENCH_NAS_MG_DIM2, MPI_DOUBLE,
                    &face_y_type);
    MPI_Type_commit(&face_y_type);
    return face_y_type;
}

static MPI_Datatype create_ddtbench_nas_mg_z_ddt(void)
{
    MPI_Datatype face_z_type;

    MPI_Type_vector(DDTBENCH_NAS_MG_DIM2 - 2, DDTBENCH_NAS_MG_DIM1 - 2,
                    DDTBENCH_NAS_MG_DIM1, MPI_DOUBLE, &face_z_type);
    MPI_Type_commit(&face_z_type);
    return face_z_type;
}

static void create_ddtbench_lammps_full_ddt(MPI_Datatype *send_type, MPI_Datatype *recv_type)
{
    MPI_Datatype indexed1_type, indexed3_type, cont1_type, cont3_type, raw_send_type;
    MPI_Datatype raw_recv_type, types[6];
    MPI_Aint displacements[6];
    int blocklengths[6] = {1, 1, 1, 1, 1, 1};
    int *index1_displacements, *index3_displacements;

    index1_displacements = ddtbench_alloc_int_array(DDTBENCH_LAMMPS_FULL_ICOUNT);
    index3_displacements = ddtbench_alloc_int_array(DDTBENCH_LAMMPS_FULL_ICOUNT);
    ddtbench_fill_displacements(index1_displacements, DDTBENCH_LAMMPS_FULL_ICOUNT,
                                DDTBENCH_LAMMPS_FULL_DIM, 1);
    ddtbench_fill_displacements(index3_displacements, DDTBENCH_LAMMPS_FULL_ICOUNT,
                                DDTBENCH_LAMMPS_FULL_DIM, 3);

    MPI_Type_create_indexed_block(DDTBENCH_LAMMPS_FULL_ICOUNT, 1, index1_displacements,
                                  MPI_DOUBLE, &indexed1_type);
    MPI_Type_create_indexed_block(DDTBENCH_LAMMPS_FULL_ICOUNT, 3, index3_displacements,
                                  MPI_DOUBLE, &indexed3_type);

    types[0] = indexed3_type;
    for (int i = 1; i < 6; ++i) {
        types[i] = indexed1_type;
    }
    displacements[0] = (MPI_Aint) ddtbench_lammps_full_ax_offset();
    displacements[1] = (MPI_Aint) ddtbench_lammps_full_atag_offset();
    displacements[2] = (MPI_Aint) ddtbench_lammps_full_atype_offset();
    displacements[3] = (MPI_Aint) ddtbench_lammps_full_amask_offset();
    displacements[4] = (MPI_Aint) ddtbench_lammps_full_aq_offset();
    displacements[5] = (MPI_Aint) ddtbench_lammps_full_amolecule_offset();
    MPI_Type_create_struct(6, blocklengths, displacements, types, &raw_send_type);
    *send_type = ddtbench_resize_and_commit(raw_send_type,
                                            (MPI_Aint) ddtbench_lammps_full_extent());

    MPI_Type_contiguous(DDTBENCH_LAMMPS_FULL_ICOUNT, MPI_DOUBLE, &cont1_type);
    MPI_Type_contiguous(3 * DDTBENCH_LAMMPS_FULL_ICOUNT, MPI_DOUBLE, &cont3_type);
    types[0] = cont3_type;
    for (int i = 1; i < 6; ++i) {
        types[i] = cont1_type;
    }
    MPI_Type_create_struct(6, blocklengths, displacements, types, &raw_recv_type);
    *recv_type = ddtbench_resize_and_commit(raw_recv_type,
                                            (MPI_Aint) ddtbench_lammps_full_extent());

    MPI_Type_free(&indexed1_type);
    MPI_Type_free(&indexed3_type);
    MPI_Type_free(&cont1_type);
    MPI_Type_free(&cont3_type);
    free(index1_displacements);
    free(index3_displacements);
}

static void create_ddtbench_lammps_atomic_ddt(MPI_Datatype *send_type, MPI_Datatype *recv_type)
{
    MPI_Datatype indexed1_type, indexed3_type, cont1_type, cont3_type, raw_send_type;
    MPI_Datatype raw_recv_type, types[4];
    MPI_Aint displacements[4];
    int blocklengths[4] = {1, 1, 1, 1};
    int *index1_displacements, *index3_displacements;

    index1_displacements = ddtbench_alloc_int_array(DDTBENCH_LAMMPS_ATOMIC_ICOUNT);
    index3_displacements = ddtbench_alloc_int_array(DDTBENCH_LAMMPS_ATOMIC_ICOUNT);
    ddtbench_fill_displacements(index1_displacements, DDTBENCH_LAMMPS_ATOMIC_ICOUNT,
                                DDTBENCH_LAMMPS_ATOMIC_DIM, 1);
    ddtbench_fill_displacements(index3_displacements, DDTBENCH_LAMMPS_ATOMIC_ICOUNT,
                                DDTBENCH_LAMMPS_ATOMIC_DIM, 3);

    MPI_Type_create_indexed_block(DDTBENCH_LAMMPS_ATOMIC_ICOUNT, 1, index1_displacements,
                                  MPI_DOUBLE, &indexed1_type);
    MPI_Type_create_indexed_block(DDTBENCH_LAMMPS_ATOMIC_ICOUNT, 3, index3_displacements,
                                  MPI_DOUBLE, &indexed3_type);

    types[0] = indexed3_type;
    for (int i = 1; i < 4; ++i) {
        types[i] = indexed1_type;
    }
    displacements[0] = (MPI_Aint) ddtbench_lammps_atomic_ax_offset();
    displacements[1] = (MPI_Aint) ddtbench_lammps_atomic_atag_offset();
    displacements[2] = (MPI_Aint) ddtbench_lammps_atomic_atype_offset();
    displacements[3] = (MPI_Aint) ddtbench_lammps_atomic_amask_offset();
    MPI_Type_create_struct(4, blocklengths, displacements, types, &raw_send_type);
    *send_type = ddtbench_resize_and_commit(raw_send_type,
                                            (MPI_Aint) ddtbench_lammps_atomic_extent());

    MPI_Type_contiguous(DDTBENCH_LAMMPS_ATOMIC_ICOUNT, MPI_DOUBLE, &cont1_type);
    MPI_Type_contiguous(3 * DDTBENCH_LAMMPS_ATOMIC_ICOUNT, MPI_DOUBLE, &cont3_type);
    types[0] = cont3_type;
    for (int i = 1; i < 4; ++i) {
        types[i] = cont1_type;
    }
    MPI_Type_create_struct(4, blocklengths, displacements, types, &raw_recv_type);
    *recv_type = ddtbench_resize_and_commit(raw_recv_type,
                                            (MPI_Aint) ddtbench_lammps_atomic_extent());

    MPI_Type_free(&indexed1_type);
    MPI_Type_free(&indexed3_type);
    MPI_Type_free(&cont1_type);
    MPI_Type_free(&cont3_type);
    free(index1_displacements);
    free(index3_displacements);
}

static MPI_Datatype create_ddtbench_specfem3d_oc_ddt(void)
{
    MPI_Datatype raw_type;
    int *displacements;

    displacements = ddtbench_alloc_int_array(DDTBENCH_SPECFEM3D_OC_ICOUNT);
    ddtbench_fill_displacements(displacements, DDTBENCH_SPECFEM3D_OC_ICOUNT,
                                DDTBENCH_SPECFEM3D_OC_DIM, 1);
    MPI_Type_create_indexed_block(DDTBENCH_SPECFEM3D_OC_ICOUNT, 1, displacements,
                                  MPI_FLOAT, &raw_type);
    free(displacements);
    return ddtbench_resize_and_commit(raw_type,
                                      DDTBENCH_SPECFEM3D_OC_DIM * (MPI_Aint) sizeof(float));
}

static MPI_Datatype create_ddtbench_specfem3d_cm_ddt(void)
{
    MPI_Datatype temp_types[2], raw_type;
    MPI_Datatype types[2];
    MPI_Aint displacements[2];
    int blocklengths[2] = {1, 1};
    int *cm_displacements, *ic_displacements;

    cm_displacements = ddtbench_alloc_int_array(DDTBENCH_SPECFEM3D_CM_ICOUNT_CM);
    ic_displacements = ddtbench_alloc_int_array(DDTBENCH_SPECFEM3D_CM_ICOUNT_IC);
    ddtbench_fill_displacements(cm_displacements, DDTBENCH_SPECFEM3D_CM_ICOUNT_CM,
                                DDTBENCH_SPECFEM3D_CM_DIM_CM, 3);
    ddtbench_fill_displacements(ic_displacements, DDTBENCH_SPECFEM3D_CM_ICOUNT_IC,
                                DDTBENCH_SPECFEM3D_CM_DIM_IC, 3);

    MPI_Type_create_indexed_block(DDTBENCH_SPECFEM3D_CM_ICOUNT_CM, 3, cm_displacements,
                                  MPI_FLOAT, &temp_types[0]);
    MPI_Type_create_indexed_block(DDTBENCH_SPECFEM3D_CM_ICOUNT_IC, 3, ic_displacements,
                                  MPI_FLOAT, &temp_types[1]);

    types[0] = temp_types[0];
    types[1] = temp_types[1];
    displacements[0] = 0;
    displacements[1] = (MPI_Aint) ddtbench_specfem3d_cm_array_ic_offset();
    MPI_Type_create_struct(2, blocklengths, displacements, types, &raw_type);

    MPI_Type_free(&temp_types[0]);
    MPI_Type_free(&temp_types[1]);
    free(cm_displacements);
    free(ic_displacements);
    return ddtbench_resize_and_commit(raw_type, (MPI_Aint) ddtbench_specfem3d_cm_extent());
}

static void create_ddtbench_specfem3d_mt_ddt(MPI_Datatype *send_type, MPI_Datatype *recv_type)
{
    MPI_Datatype temp_type;

    MPI_Type_contiguous(DDTBENCH_SPECFEM3D_MT_DIM1, MPI_FLOAT, &temp_type);
    MPI_Type_vector(DDTBENCH_SPECFEM3D_MT_DIM3, 1, DDTBENCH_SPECFEM3D_MT_DIM2, temp_type,
                    send_type);
    MPI_Type_commit(send_type);
    MPI_Type_free(&temp_type);

    MPI_Type_contiguous(DDTBENCH_SPECFEM3D_MT_DIM3 * DDTBENCH_SPECFEM3D_MT_DIM1,
                        MPI_FLOAT, recv_type);
    MPI_Type_commit(recv_type);
}

static MPI_Datatype create_ddtbench_wrf_vec_ddt(void)
{
    MPI_Datatype temp_2d_type, temp_type, temp_3d_type, raw_type;
    MPI_Datatype oldtypes[DDTBENCH_WRF_NUMBER_2D + DDTBENCH_WRF_NUMBER_3D
                          + DDTBENCH_WRF_NUMBER_4D];
    MPI_Aint displacements[DDTBENCH_WRF_NUMBER_2D + DDTBENCH_WRF_NUMBER_3D
                           + DDTBENCH_WRF_NUMBER_4D];
    int blocklengths[DDTBENCH_WRF_NUMBER_2D + DDTBENCH_WRF_NUMBER_3D
                     + DDTBENCH_WRF_NUMBER_4D];
    MPI_Aint stride;
    int counter = 0;

    for (int i = 0; i < DDTBENCH_WRF_NUMBER_2D + DDTBENCH_WRF_NUMBER_3D
                            + DDTBENCH_WRF_NUMBER_4D;
         ++i) {
        blocklengths[i] = 1;
    }

    MPI_Type_vector(DDTBENCH_WRF_SUB_DIM3, DDTBENCH_WRF_SUB_DIM1, DDTBENCH_WRF_DIM1,
                    MPI_FLOAT, &temp_2d_type);
    for (int i = 0; i < DDTBENCH_WRF_NUMBER_2D; ++i) {
        const MPI_Aint array_offset = (MPI_Aint) (i * ddtbench_wrf_2d_array_floats()
                                                  * sizeof(float));

        displacements[counter] = array_offset
                                 + (MPI_Aint) ddtbench_idx2d(DDTBENCH_WRF_IS, DDTBENCH_WRF_JS,
                                                             DDTBENCH_WRF_DIM1)
                                       * (MPI_Aint) sizeof(float);
        oldtypes[counter++] = temp_2d_type;
    }

    MPI_Type_vector(DDTBENCH_WRF_SUB_DIM2, DDTBENCH_WRF_SUB_DIM1, DDTBENCH_WRF_DIM1,
                    MPI_FLOAT, &temp_type);
    stride = DDTBENCH_WRF_DIM1 * DDTBENCH_WRF_DIM2 * (MPI_Aint) sizeof(float);
    MPI_Type_create_hvector(DDTBENCH_WRF_SUB_DIM3, 1, stride, temp_type, &temp_3d_type);
    MPI_Type_free(&temp_type);
    for (int i = 0; i < DDTBENCH_WRF_NUMBER_3D; ++i) {
        const MPI_Aint array_offset = (MPI_Aint) (ddtbench_wrf_3d_arrays_offset()
                                                  + i * ddtbench_wrf_3d_array_floats()
                                                        * sizeof(float));

        displacements[counter] = array_offset
                                 + (MPI_Aint) ddtbench_idx3d(DDTBENCH_WRF_IS, DDTBENCH_WRF_KS,
                                                             DDTBENCH_WRF_JS, DDTBENCH_WRF_DIM1,
                                                             DDTBENCH_WRF_DIM2)
                                       * (MPI_Aint) sizeof(float);
        oldtypes[counter++] = temp_3d_type;
    }

    stride *= DDTBENCH_WRF_DIM3;
    for (int i = 0; i < DDTBENCH_WRF_NUMBER_4D; ++i) {
        MPI_Datatype temp_4d_type;
        const MPI_Aint array_offset = (MPI_Aint) (ddtbench_wrf_4d_arrays_offset()
                                                  + i * ddtbench_wrf_4d_array_floats()
                                                        * sizeof(float));

        MPI_Type_create_hvector(DDTBENCH_WRF_LIMIT_4D - DDTBENCH_WRF_PARAM_FIRST_SCALAR, 1,
                                stride, temp_3d_type, &temp_4d_type);
        displacements[counter] = array_offset
                                 + (MPI_Aint) ddtbench_idx4d(DDTBENCH_WRF_IS, DDTBENCH_WRF_KS,
                                                             DDTBENCH_WRF_JS,
                                                             DDTBENCH_WRF_PARAM_FIRST_SCALAR,
                                                             DDTBENCH_WRF_DIM1, DDTBENCH_WRF_DIM2,
                                                             DDTBENCH_WRF_DIM3)
                                       * (MPI_Aint) sizeof(float);
        oldtypes[counter++] = temp_4d_type;
    }

    MPI_Type_create_struct(counter, blocklengths, displacements, oldtypes, &raw_type);

    for (int i = DDTBENCH_WRF_NUMBER_2D + DDTBENCH_WRF_NUMBER_3D; i < counter; ++i) {
        MPI_Type_free(&oldtypes[i]);
    }
    MPI_Type_free(&temp_2d_type);
    MPI_Type_free(&temp_3d_type);
    return ddtbench_resize_and_commit(raw_type, (MPI_Aint) ddtbench_wrf_extent());
}

static MPI_Datatype create_ddtbench_wrf_subarray_ddt(void)
{
    MPI_Datatype temp_2d_type, temp_3d_type, temp_4d_type, raw_type;
    MPI_Datatype oldtypes[DDTBENCH_WRF_NUMBER_2D + DDTBENCH_WRF_NUMBER_3D
                          + DDTBENCH_WRF_NUMBER_4D];
    MPI_Aint displacements[DDTBENCH_WRF_NUMBER_2D + DDTBENCH_WRF_NUMBER_3D
                           + DDTBENCH_WRF_NUMBER_4D];
    int blocklengths[DDTBENCH_WRF_NUMBER_2D + DDTBENCH_WRF_NUMBER_3D
                     + DDTBENCH_WRF_NUMBER_4D];
    int arraysize[4], subarraysize[4], subarraystart[4];
    int counter = 0;

    for (int i = 0; i < DDTBENCH_WRF_NUMBER_2D + DDTBENCH_WRF_NUMBER_3D
                            + DDTBENCH_WRF_NUMBER_4D;
         ++i) {
        blocklengths[i] = 1;
    }

    arraysize[2] = DDTBENCH_WRF_DIM3;
    arraysize[3] = DDTBENCH_WRF_DIM1;
    subarraysize[2] = DDTBENCH_WRF_SUB_DIM3;
    subarraysize[3] = DDTBENCH_WRF_SUB_DIM1;
    subarraystart[2] = DDTBENCH_WRF_JS;
    subarraystart[3] = DDTBENCH_WRF_IS;
    MPI_Type_create_subarray(2, &arraysize[2], &subarraysize[2], &subarraystart[2],
                             MPI_ORDER_C, MPI_FLOAT, &temp_2d_type);
    for (int i = 0; i < DDTBENCH_WRF_NUMBER_2D; ++i) {
        const MPI_Aint array_offset = (MPI_Aint) (i * ddtbench_wrf_2d_array_floats()
                                                  * sizeof(float));

        displacements[counter] = array_offset;
        oldtypes[counter++] = temp_2d_type;
    }

    arraysize[1] = DDTBENCH_WRF_DIM3;
    arraysize[2] = DDTBENCH_WRF_DIM2;
    arraysize[3] = DDTBENCH_WRF_DIM1;
    subarraysize[1] = DDTBENCH_WRF_SUB_DIM3;
    subarraysize[2] = DDTBENCH_WRF_SUB_DIM2;
    subarraysize[3] = DDTBENCH_WRF_SUB_DIM1;
    subarraystart[1] = DDTBENCH_WRF_JS;
    subarraystart[2] = DDTBENCH_WRF_KS;
    subarraystart[3] = DDTBENCH_WRF_IS;
    MPI_Type_create_subarray(3, &arraysize[1], &subarraysize[1], &subarraystart[1],
                             MPI_ORDER_C, MPI_FLOAT, &temp_3d_type);
    for (int i = 0; i < DDTBENCH_WRF_NUMBER_3D; ++i) {
        const MPI_Aint array_offset = (MPI_Aint) (ddtbench_wrf_3d_arrays_offset()
                                                  + i * ddtbench_wrf_3d_array_floats()
                                                        * sizeof(float));

        displacements[counter] = array_offset;
        oldtypes[counter++] = temp_3d_type;
    }

    arraysize[0] = DDTBENCH_WRF_LIMIT_4D;
    arraysize[1] = DDTBENCH_WRF_DIM3;
    arraysize[2] = DDTBENCH_WRF_DIM2;
    arraysize[3] = DDTBENCH_WRF_DIM1;
    subarraysize[0] = DDTBENCH_WRF_LIMIT_4D - DDTBENCH_WRF_PARAM_FIRST_SCALAR;
    subarraysize[1] = DDTBENCH_WRF_SUB_DIM3;
    subarraysize[2] = DDTBENCH_WRF_SUB_DIM2;
    subarraysize[3] = DDTBENCH_WRF_SUB_DIM1;
    subarraystart[0] = DDTBENCH_WRF_PARAM_FIRST_SCALAR;
    subarraystart[1] = DDTBENCH_WRF_JS;
    subarraystart[2] = DDTBENCH_WRF_KS;
    subarraystart[3] = DDTBENCH_WRF_IS;
    MPI_Type_create_subarray(4, arraysize, subarraysize, subarraystart, MPI_ORDER_C,
                             MPI_FLOAT, &temp_4d_type);
    for (int i = 0; i < DDTBENCH_WRF_NUMBER_4D; ++i) {
        const MPI_Aint array_offset = (MPI_Aint) (ddtbench_wrf_4d_arrays_offset()
                                                  + i * ddtbench_wrf_4d_array_floats()
                                                        * sizeof(float));

        displacements[counter] = array_offset;
        oldtypes[counter++] = temp_4d_type;
    }

    MPI_Type_create_struct(counter, blocklengths, displacements, oldtypes, &raw_type);
    MPI_Type_free(&temp_2d_type);
    MPI_Type_free(&temp_3d_type);
    MPI_Type_free(&temp_4d_type);
    return ddtbench_resize_and_commit(raw_type, (MPI_Aint) ddtbench_wrf_extent());
}

/*
 * A single-level float-complex hvector whose block-to-block stride is a multiple of the 4-byte
 * complex alignment but NOT of the 8-byte element size (a small trailing gap of GAP_BYTES that is
 * not a whole element).  This is the only datatype in the suite that drives the predefined
 * pack/unpack movers onto their byte-exact "unaligned" advance path
 * (elem->extent % sizeof(element) != 0); every other shape here advances in whole elements.
 * BLOCKLEN stays <= 8 so the copy goes through the unrolled predefined element mover rather than
 * the medium/large vectorized loops.
 */
#define COMPLEX_HVECTOR_BLOCKS    2048
#define COMPLEX_HVECTOR_BLOCKLEN  3
#define COMPLEX_HVECTOR_GAP_BYTES 4

static MPI_Datatype create_complex_hvector_ddt(void)
{
    MPI_Datatype dt;
    MPI_Aint lb, elem_extent, stride;

    MPI_Type_get_extent(MPI_C_FLOAT_COMPLEX, &lb, &elem_extent);
    stride = (MPI_Aint) COMPLEX_HVECTOR_BLOCKLEN * elem_extent + COMPLEX_HVECTOR_GAP_BYTES;
    MPI_Type_create_hvector(COMPLEX_HVECTOR_BLOCKS, COMPLEX_HVECTOR_BLOCKLEN, stride,
                            MPI_C_FLOAT_COMPLEX, &dt);
    MPI_Type_commit(&dt);
    return dt;
}

static void pack_byhand_complex_hvector_ddt(void *dst, const void *src, int count)
{
    const size_t elem_size = sizeof(float _Complex);
    const size_t block_bytes = (size_t) COMPLEX_HVECTOR_BLOCKLEN * elem_size;
    const size_t stride = block_bytes + COMPLEX_HVECTOR_GAP_BYTES;
    const size_t instance_extent = (size_t) (COMPLEX_HVECTOR_BLOCKS - 1) * stride + block_bytes;
    char *d = (char *) dst;
    const char *s = (const char *) src;

    for (int c = 0; c < count; ++c) {
        const char *row = s + (size_t) c * instance_extent;
        for (int b = 0; b < COMPLEX_HVECTOR_BLOCKS; ++b) {
            memcpy(d, row + (size_t) b * stride, block_bytes);
            d += block_bytes;
        }
    }
}

static void unpack_byhand_complex_hvector_ddt(void *dst, const void *src, int count)
{
    const size_t elem_size = sizeof(float _Complex);
    const size_t block_bytes = (size_t) COMPLEX_HVECTOR_BLOCKLEN * elem_size;
    const size_t stride = block_bytes + COMPLEX_HVECTOR_GAP_BYTES;
    const size_t instance_extent = (size_t) (COMPLEX_HVECTOR_BLOCKS - 1) * stride + block_bytes;
    char *d = (char *) dst;
    const char *s = (const char *) src;

    for (int c = 0; c < count; ++c) {
        char *row = d + (size_t) c * instance_extent;
        for (int b = 0; b < COMPLEX_HVECTOR_BLOCKS; ++b) {
            memcpy(row + (size_t) b * stride, s, block_bytes);
            s += block_bytes;
        }
    }
}

static inline void byhand_pack_region(char **packed, const char *base, size_t displacement,
                                      size_t length)
{
    memcpy(*packed, base + displacement, length);
    *packed += length;
}

static inline void byhand_unpack_region(char *base, size_t displacement, const char **packed,
                                        size_t length)
{
    memcpy(base + displacement, *packed, length);
    *packed += length;
}

static void pack_byhand_contiguous_datatype(void *dst, const void *src, int count)
{
    memcpy(dst, src, (size_t) count * sizeof(int));
}

static void unpack_byhand_contiguous_datatype(void *dst, const void *src, int count)
{
    memcpy(dst, src, (size_t) count * sizeof(int));
}

static void pack_byhand_create_merged_contig_with_gaps(void *dst, const void *src, int count)
{
    const size_t extent = sizeof(struct merged_contig_with_gaps);
    const size_t payload_length = offsetof(struct merged_contig_with_gaps, c) + sizeof(char);
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int i = 0; i < count; i++) {
        byhand_pack_region(&packed, input + (size_t) i * extent, 0, payload_length);
    }
}

static void unpack_byhand_create_merged_contig_with_gaps(void *dst, const void *src, int count)
{
    const size_t extent = sizeof(struct merged_contig_with_gaps);
    const size_t payload_length = offsetof(struct merged_contig_with_gaps, c) + sizeof(char);
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int i = 0; i < count; i++) {
        byhand_unpack_region(output + (size_t) i * extent, 0, &packed, payload_length);
    }
}

static void pack_byhand_create_struct_constant_gap_resized_ddt(void *dst, const void *src,
                                                              int count)
{
    const size_t extent = sizeof(struct structure);
    const size_t payload_offset = offsetof(struct structure, transferred_1);
    const size_t payload_length = sizeof(double) + sizeof(long);
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int i = 0; i < count; i++) {
        byhand_pack_region(&packed, input + (size_t) i * extent, payload_offset, payload_length);
    }
}

static void unpack_byhand_create_struct_constant_gap_resized_ddt(void *dst, const void *src,
                                                                int count)
{
    const size_t extent = sizeof(struct structure);
    const size_t payload_offset = offsetof(struct structure, transferred_1);
    const size_t payload_length = sizeof(double) + sizeof(long);
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int i = 0; i < count; i++) {
        byhand_unpack_region(output + (size_t) i * extent, payload_offset, &packed,
                             payload_length);
    }
}

static void pack_byhand_constant_gap_layout(void *dst, const void *src, int count)
{
    const int block_count = 80;
    const size_t block_length = 100 * sizeof(double);
    const size_t block_stride = 101 * sizeof(double);
    const size_t extent = (size_t) (block_count - 1) * block_stride + block_length;
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int i = 0; i < count; i++) {
        const char *base = input + (size_t) i * extent;
        for (int block = 0; block < block_count; block++) {
            byhand_pack_region(&packed, base, (size_t) block * block_stride, block_length);
        }
    }
}

static void unpack_byhand_constant_gap_layout(void *dst, const void *src, int count)
{
    const int block_count = 80;
    const size_t block_length = 100 * sizeof(double);
    const size_t block_stride = 101 * sizeof(double);
    const size_t extent = (size_t) (block_count - 1) * block_stride + block_length;
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int i = 0; i < count; i++) {
        char *base = output + (size_t) i * extent;
        for (int block = 0; block < block_count; block++) {
            byhand_unpack_region(base, (size_t) block * block_stride, &packed, block_length);
        }
    }
}

static void pack_byhand_create_indexed_constant_gap_ddt(void *dst, const void *src, int count)
{
    pack_byhand_constant_gap_layout(dst, src, count);
}

static void unpack_byhand_create_indexed_constant_gap_ddt(void *dst, const void *src, int count)
{
    unpack_byhand_constant_gap_layout(dst, src, count);
}

static void pack_byhand_create_struct_constant_gap_ddt(void *dst, const void *src, int count)
{
    pack_byhand_constant_gap_layout(dst, src, count);
}

static void unpack_byhand_create_struct_constant_gap_ddt(void *dst, const void *src, int count)
{
    unpack_byhand_constant_gap_layout(dst, src, count);
}

static void pack_byhand_create_optimized_indexed_constant_gap_ddt(void *dst, const void *src,
                                                                 int count)
{
    pack_byhand_constant_gap_layout(dst, src, count);
}

static void unpack_byhand_create_optimized_indexed_constant_gap_ddt(void *dst, const void *src,
                                                                   int count)
{
    unpack_byhand_constant_gap_layout(dst, src, count);
}

static void pack_byhand_indexed_gap_layout(void *dst, const void *src, int count)
{
    const int record_count = 10;
    const size_t record_extent = sizeof(ddt_gap);
    const size_t payload_offset = offsetof(ddt_gap, is);
    const size_t payload_length = 3 * sizeof(internal_struct);
    const size_t lead_length = sizeof(int);
    const size_t extent = (size_t) record_count * record_extent;
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int i = 0; i < count; i++) {
        const char *base = input + (size_t) i * extent;

        byhand_pack_region(&packed, base, 0, lead_length);
        for (int record = 0; record < record_count - 1; record++) {
            byhand_pack_region(&packed, base, (size_t) record * record_extent + payload_offset,
                               payload_length + lead_length);
        }
        byhand_pack_region(&packed, base,
                           (size_t) (record_count - 1) * record_extent + payload_offset,
                           payload_length);
    }
}

static void unpack_byhand_indexed_gap_layout(void *dst, const void *src, int count)
{
    const int record_count = 10;
    const size_t record_extent = sizeof(ddt_gap);
    const size_t payload_offset = offsetof(ddt_gap, is);
    const size_t payload_length = 3 * sizeof(internal_struct);
    const size_t lead_length = sizeof(int);
    const size_t extent = (size_t) record_count * record_extent;
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int i = 0; i < count; i++) {
        char *base = output + (size_t) i * extent;

        byhand_unpack_region(base, 0, &packed, lead_length);
        for (int record = 0; record < record_count - 1; record++) {
            byhand_unpack_region(base, (size_t) record * record_extent + payload_offset, &packed,
                                 payload_length + lead_length);
        }
        byhand_unpack_region(base, (size_t) (record_count - 1) * record_extent + payload_offset,
                             &packed, payload_length);
    }
}

static void pack_byhand_create_indexed_gap_ddt(void *dst, const void *src, int count)
{
    pack_byhand_indexed_gap_layout(dst, src, count);
}

static void unpack_byhand_create_indexed_gap_ddt(void *dst, const void *src, int count)
{
    unpack_byhand_indexed_gap_layout(dst, src, count);
}

static void pack_byhand_create_indexed_gap_optimized_ddt(void *dst, const void *src, int count)
{
    pack_byhand_indexed_gap_layout(dst, src, count);
}

static void unpack_byhand_create_indexed_gap_optimized_ddt(void *dst, const void *src, int count)
{
    unpack_byhand_indexed_gap_layout(dst, src, count);
}

static void pack_byhand_ddtbench_fft2d_scatter_ddt(void *dst, const void *src, int count)
{
    const int columns = DDTBENCH_FFT_DIM / DDTBENCH_FFT_PROCS;
    const size_t complex_bytes = 2 * sizeof(double);
    const size_t extent = (size_t) columns * complex_bytes;
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int column = 0; column < columns; ++column) {
            for (int row = 0; row < columns; ++row) {
                byhand_pack_region(&packed, base,
                                   ((size_t) column + (size_t) row * DDTBENCH_FFT_DIM)
                                       * complex_bytes,
                                   complex_bytes);
            }
        }
    }
}

static void unpack_byhand_ddtbench_fft2d_scatter_ddt(void *dst, const void *src, int count)
{
    const int columns = DDTBENCH_FFT_DIM / DDTBENCH_FFT_PROCS;
    const size_t complex_bytes = 2 * sizeof(double);
    const size_t extent = (size_t) columns * complex_bytes;
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int column = 0; column < columns; ++column) {
            for (int row = 0; row < columns; ++row) {
                byhand_unpack_region(base,
                                     ((size_t) column + (size_t) row * DDTBENCH_FFT_DIM)
                                         * complex_bytes,
                                     &packed, complex_bytes);
            }
        }
    }
}

static void pack_byhand_ddtbench_fft2d_gather_ddt(void *dst, const void *src, int count)
{
    const int columns = DDTBENCH_FFT_DIM / DDTBENCH_FFT_PROCS;
    const size_t complex_bytes = 2 * sizeof(double);
    const size_t row_bytes = (size_t) columns * complex_bytes;
    const size_t extent = row_bytes;
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int row = 0; row < columns; ++row) {
            byhand_pack_region(&packed, base, (size_t) row * DDTBENCH_FFT_DIM * complex_bytes,
                               row_bytes);
        }
    }
}

static void unpack_byhand_ddtbench_fft2d_gather_ddt(void *dst, const void *src, int count)
{
    const int columns = DDTBENCH_FFT_DIM / DDTBENCH_FFT_PROCS;
    const size_t complex_bytes = 2 * sizeof(double);
    const size_t row_bytes = (size_t) columns * complex_bytes;
    const size_t extent = row_bytes;
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int row = 0; row < columns; ++row) {
            byhand_unpack_region(base, (size_t) row * DDTBENCH_FFT_DIM * complex_bytes, &packed,
                                 row_bytes);
        }
    }
}

static void pack_byhand_ddtbench_milc_su3_zdown_ddt(void *dst, const void *src, int count)
{
    const size_t su3_bytes = 6 * sizeof(float);
    const size_t block_bytes = (DDTBENCH_MILC_DIM2 * DDTBENCH_MILC_DIM3 / 2) * su3_bytes;
    const size_t temp_stride = (DDTBENCH_MILC_DIM2 * DDTBENCH_MILC_DIM3
                                * DDTBENCH_MILC_DIM4 / 2)
                               * su3_bytes;
    const size_t hvector_stride = DDTBENCH_MILC_DIM2 * DDTBENCH_MILC_DIM3
                                  * DDTBENCH_MILC_DIM4 * DDTBENCH_MILC_DIM5 / 2
                                  * su3_bytes;
    const size_t temp_extent = (DDTBENCH_MILC_DIM5 - 1) * temp_stride + block_bytes;
    const size_t extent = hvector_stride + temp_extent;
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int z = 0; z < 2; ++z) {
            for (int t = 0; t < DDTBENCH_MILC_DIM5; ++t) {
                byhand_pack_region(&packed, base,
                                   (size_t) z * hvector_stride + (size_t) t * temp_stride,
                                   block_bytes);
            }
        }
    }
}

static void unpack_byhand_ddtbench_milc_su3_zdown_ddt(void *dst, const void *src, int count)
{
    const size_t su3_bytes = 6 * sizeof(float);
    const size_t block_bytes = (DDTBENCH_MILC_DIM2 * DDTBENCH_MILC_DIM3 / 2) * su3_bytes;
    const size_t temp_stride = (DDTBENCH_MILC_DIM2 * DDTBENCH_MILC_DIM3
                                * DDTBENCH_MILC_DIM4 / 2)
                               * su3_bytes;
    const size_t hvector_stride = DDTBENCH_MILC_DIM2 * DDTBENCH_MILC_DIM3
                                  * DDTBENCH_MILC_DIM4 * DDTBENCH_MILC_DIM5 / 2
                                  * su3_bytes;
    const size_t temp_extent = (DDTBENCH_MILC_DIM5 - 1) * temp_stride + block_bytes;
    const size_t extent = hvector_stride + temp_extent;
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int z = 0; z < 2; ++z) {
            for (int t = 0; t < DDTBENCH_MILC_DIM5; ++t) {
                byhand_unpack_region(base,
                                     (size_t) z * hvector_stride + (size_t) t * temp_stride,
                                     &packed, block_bytes);
            }
        }
    }
}

static void pack_byhand_ddtbench_nas_lu_y_ddt(void *dst, const void *src, int count)
{
    const size_t block_bytes = DDTBENCH_NAS_LU_CELL_DOUBLES * sizeof(double);
    const size_t stride = (DDTBENCH_NAS_LU_DIM2 + 2) * block_bytes;
    const size_t extent = (DDTBENCH_NAS_LU_DIM3 - 1) * stride + block_bytes;
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int row = 0; row < DDTBENCH_NAS_LU_DIM3; ++row) {
            byhand_pack_region(&packed, base, (size_t) row * stride, block_bytes);
        }
    }
}

static void unpack_byhand_ddtbench_nas_lu_y_ddt(void *dst, const void *src, int count)
{
    const size_t block_bytes = DDTBENCH_NAS_LU_CELL_DOUBLES * sizeof(double);
    const size_t stride = (DDTBENCH_NAS_LU_DIM2 + 2) * block_bytes;
    const size_t extent = (DDTBENCH_NAS_LU_DIM3 - 1) * stride + block_bytes;
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int row = 0; row < DDTBENCH_NAS_LU_DIM3; ++row) {
            byhand_unpack_region(base, (size_t) row * stride, &packed, block_bytes);
        }
    }
}

static void pack_byhand_ddtbench_nas_lu_x_ddt(void *dst, const void *src, int count)
{
    const size_t extent = DDTBENCH_NAS_LU_CELL_DOUBLES * DDTBENCH_NAS_LU_X_DIM2
                          * sizeof(double);

    memcpy(dst, src, (size_t) count * extent);
}

static void unpack_byhand_ddtbench_nas_lu_x_ddt(void *dst, const void *src, int count)
{
    const size_t extent = DDTBENCH_NAS_LU_CELL_DOUBLES * DDTBENCH_NAS_LU_X_DIM2
                          * sizeof(double);

    memcpy(dst, src, (size_t) count * extent);
}

static void pack_byhand_ddtbench_nas_mg_x_ddt(void *dst, const void *src, int count)
{
    const size_t z_stride = DDTBENCH_NAS_MG_DIM1 * DDTBENCH_NAS_MG_DIM2 * sizeof(double);
    const size_t y_stride = DDTBENCH_NAS_MG_DIM1 * sizeof(double);
    const size_t extent = (DDTBENCH_NAS_MG_DIM3 - 3) * z_stride
                          + (DDTBENCH_NAS_MG_DIM2 - 3) * y_stride + sizeof(double);
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int z = 0; z < DDTBENCH_NAS_MG_DIM3 - 2; ++z) {
            for (int y = 0; y < DDTBENCH_NAS_MG_DIM2 - 2; ++y) {
                byhand_pack_region(&packed, base, (size_t) z * z_stride + (size_t) y * y_stride,
                                   sizeof(double));
            }
        }
    }
}

static void unpack_byhand_ddtbench_nas_mg_x_ddt(void *dst, const void *src, int count)
{
    const size_t z_stride = DDTBENCH_NAS_MG_DIM1 * DDTBENCH_NAS_MG_DIM2 * sizeof(double);
    const size_t y_stride = DDTBENCH_NAS_MG_DIM1 * sizeof(double);
    const size_t extent = (DDTBENCH_NAS_MG_DIM3 - 3) * z_stride
                          + (DDTBENCH_NAS_MG_DIM2 - 3) * y_stride + sizeof(double);
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int z = 0; z < DDTBENCH_NAS_MG_DIM3 - 2; ++z) {
            for (int y = 0; y < DDTBENCH_NAS_MG_DIM2 - 2; ++y) {
                byhand_unpack_region(base, (size_t) z * z_stride + (size_t) y * y_stride,
                                     &packed, sizeof(double));
            }
        }
    }
}

static void pack_byhand_ddtbench_nas_mg_y_ddt(void *dst, const void *src, int count)
{
    const size_t row_bytes = (DDTBENCH_NAS_MG_DIM1 - 2) * sizeof(double);
    const size_t z_stride = DDTBENCH_NAS_MG_DIM1 * DDTBENCH_NAS_MG_DIM2 * sizeof(double);
    const size_t extent = (DDTBENCH_NAS_MG_DIM3 - 3) * z_stride + row_bytes;
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int z = 0; z < DDTBENCH_NAS_MG_DIM3 - 2; ++z) {
            byhand_pack_region(&packed, base, (size_t) z * z_stride, row_bytes);
        }
    }
}

static void unpack_byhand_ddtbench_nas_mg_y_ddt(void *dst, const void *src, int count)
{
    const size_t row_bytes = (DDTBENCH_NAS_MG_DIM1 - 2) * sizeof(double);
    const size_t z_stride = DDTBENCH_NAS_MG_DIM1 * DDTBENCH_NAS_MG_DIM2 * sizeof(double);
    const size_t extent = (DDTBENCH_NAS_MG_DIM3 - 3) * z_stride + row_bytes;
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int z = 0; z < DDTBENCH_NAS_MG_DIM3 - 2; ++z) {
            byhand_unpack_region(base, (size_t) z * z_stride, &packed, row_bytes);
        }
    }
}

static void pack_byhand_ddtbench_nas_mg_z_ddt(void *dst, const void *src, int count)
{
    const size_t row_bytes = (DDTBENCH_NAS_MG_DIM1 - 2) * sizeof(double);
    const size_t y_stride = DDTBENCH_NAS_MG_DIM1 * sizeof(double);
    const size_t extent = (DDTBENCH_NAS_MG_DIM2 - 3) * y_stride + row_bytes;
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int y = 0; y < DDTBENCH_NAS_MG_DIM2 - 2; ++y) {
            byhand_pack_region(&packed, base, (size_t) y * y_stride, row_bytes);
        }
    }
}

static void unpack_byhand_ddtbench_nas_mg_z_ddt(void *dst, const void *src, int count)
{
    const size_t row_bytes = (DDTBENCH_NAS_MG_DIM1 - 2) * sizeof(double);
    const size_t y_stride = DDTBENCH_NAS_MG_DIM1 * sizeof(double);
    const size_t extent = (DDTBENCH_NAS_MG_DIM2 - 3) * y_stride + row_bytes;
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int y = 0; y < DDTBENCH_NAS_MG_DIM2 - 2; ++y) {
            byhand_unpack_region(base, (size_t) y * y_stride, &packed, row_bytes);
        }
    }
}

static void pack_byhand_ddtbench_lammps_full_ddt(void *dst, const void *src, int count)
{
    const size_t extent = ddtbench_lammps_full_extent();
    const size_t scalar_offsets[] = {ddtbench_lammps_full_atag_offset(),
                                     ddtbench_lammps_full_atype_offset(),
                                     ddtbench_lammps_full_amask_offset(),
                                     ddtbench_lammps_full_aq_offset(),
                                     ddtbench_lammps_full_amolecule_offset()};
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int i = 0; i < DDTBENCH_LAMMPS_FULL_ICOUNT; ++i) {
            byhand_pack_region(&packed, base,
                               ddtbench_lammps_full_ax_offset()
                                   + 3 * (size_t) ddtbench_index(i, DDTBENCH_LAMMPS_FULL_DIM)
                                         * sizeof(double),
                               3 * sizeof(double));
        }
        for (int field = 0; field < 5; ++field) {
            for (int i = 0; i < DDTBENCH_LAMMPS_FULL_ICOUNT; ++i) {
                byhand_pack_region(&packed, base,
                                   scalar_offsets[field]
                                       + (size_t) ddtbench_index(i, DDTBENCH_LAMMPS_FULL_DIM)
                                             * sizeof(double),
                                   sizeof(double));
            }
        }
    }
}

static void unpack_byhand_ddtbench_lammps_full_ddt(void *dst, const void *src, int count)
{
    const size_t extent = ddtbench_lammps_full_extent();
    const size_t scalar_offsets[] = {ddtbench_lammps_full_atag_offset(),
                                     ddtbench_lammps_full_atype_offset(),
                                     ddtbench_lammps_full_amask_offset(),
                                     ddtbench_lammps_full_aq_offset(),
                                     ddtbench_lammps_full_amolecule_offset()};
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int i = 0; i < DDTBENCH_LAMMPS_FULL_ICOUNT; ++i) {
            byhand_unpack_region(base,
                                 ddtbench_lammps_full_ax_offset()
                                     + 3 * (size_t) i * sizeof(double),
                                 &packed, 3 * sizeof(double));
        }
        for (int field = 0; field < 5; ++field) {
            for (int i = 0; i < DDTBENCH_LAMMPS_FULL_ICOUNT; ++i) {
                byhand_unpack_region(base, scalar_offsets[field] + (size_t) i * sizeof(double),
                                     &packed, sizeof(double));
            }
        }
    }
}

static void pack_byhand_ddtbench_lammps_atomic_ddt(void *dst, const void *src, int count)
{
    const size_t extent = ddtbench_lammps_atomic_extent();
    const size_t scalar_offsets[] = {ddtbench_lammps_atomic_atag_offset(),
                                     ddtbench_lammps_atomic_atype_offset(),
                                     ddtbench_lammps_atomic_amask_offset()};
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int i = 0; i < DDTBENCH_LAMMPS_ATOMIC_ICOUNT; ++i) {
            byhand_pack_region(&packed, base,
                               ddtbench_lammps_atomic_ax_offset()
                                   + 3 * (size_t) ddtbench_index(i, DDTBENCH_LAMMPS_ATOMIC_DIM)
                                         * sizeof(double),
                               3 * sizeof(double));
        }
        for (int field = 0; field < 3; ++field) {
            for (int i = 0; i < DDTBENCH_LAMMPS_ATOMIC_ICOUNT; ++i) {
                byhand_pack_region(&packed, base,
                                   scalar_offsets[field]
                                       + (size_t) ddtbench_index(i, DDTBENCH_LAMMPS_ATOMIC_DIM)
                                             * sizeof(double),
                                   sizeof(double));
            }
        }
    }
}

static void unpack_byhand_ddtbench_lammps_atomic_ddt(void *dst, const void *src, int count)
{
    const size_t extent = ddtbench_lammps_atomic_extent();
    const size_t scalar_offsets[] = {ddtbench_lammps_atomic_atag_offset(),
                                     ddtbench_lammps_atomic_atype_offset(),
                                     ddtbench_lammps_atomic_amask_offset()};
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int i = 0; i < DDTBENCH_LAMMPS_ATOMIC_ICOUNT; ++i) {
            byhand_unpack_region(base,
                                 ddtbench_lammps_atomic_ax_offset()
                                     + 3 * (size_t) i * sizeof(double),
                                 &packed, 3 * sizeof(double));
        }
        for (int field = 0; field < 3; ++field) {
            for (int i = 0; i < DDTBENCH_LAMMPS_ATOMIC_ICOUNT; ++i) {
                byhand_unpack_region(base, scalar_offsets[field] + (size_t) i * sizeof(double),
                                     &packed, sizeof(double));
            }
        }
    }
}

static void pack_byhand_ddtbench_specfem3d_oc_ddt(void *dst, const void *src, int count)
{
    const size_t extent = DDTBENCH_SPECFEM3D_OC_DIM * sizeof(float);
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int i = 0; i < DDTBENCH_SPECFEM3D_OC_ICOUNT; ++i) {
            byhand_pack_region(&packed, base,
                               (size_t) ddtbench_index(i, DDTBENCH_SPECFEM3D_OC_DIM)
                                   * sizeof(float),
                               sizeof(float));
        }
    }
}

static void unpack_byhand_ddtbench_specfem3d_oc_ddt(void *dst, const void *src, int count)
{
    const size_t extent = DDTBENCH_SPECFEM3D_OC_DIM * sizeof(float);
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int i = 0; i < DDTBENCH_SPECFEM3D_OC_ICOUNT; ++i) {
            byhand_unpack_region(base,
                                 (size_t) ddtbench_index(i, DDTBENCH_SPECFEM3D_OC_DIM)
                                     * sizeof(float),
                                 &packed, sizeof(float));
        }
    }
}

static void pack_byhand_ddtbench_specfem3d_cm_ddt(void *dst, const void *src, int count)
{
    const size_t extent = ddtbench_specfem3d_cm_extent();
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int i = 0; i < DDTBENCH_SPECFEM3D_CM_ICOUNT_CM; ++i) {
            byhand_pack_region(&packed, base,
                               3 * (size_t) ddtbench_index(i, DDTBENCH_SPECFEM3D_CM_DIM_CM)
                                   * sizeof(float),
                               3 * sizeof(float));
        }
        for (int i = 0; i < DDTBENCH_SPECFEM3D_CM_ICOUNT_IC; ++i) {
            byhand_pack_region(&packed, base,
                               ddtbench_specfem3d_cm_array_ic_offset()
                                   + 3
                                         * (size_t) ddtbench_index(
                                               i, DDTBENCH_SPECFEM3D_CM_DIM_IC)
                                         * sizeof(float),
                               3 * sizeof(float));
        }
    }
}

static void unpack_byhand_ddtbench_specfem3d_cm_ddt(void *dst, const void *src, int count)
{
    const size_t extent = ddtbench_specfem3d_cm_extent();
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        for (int i = 0; i < DDTBENCH_SPECFEM3D_CM_ICOUNT_CM; ++i) {
            byhand_unpack_region(base,
                                 3
                                     * (size_t) ddtbench_index(
                                           i, DDTBENCH_SPECFEM3D_CM_DIM_CM)
                                     * sizeof(float),
                                 &packed, 3 * sizeof(float));
        }
        for (int i = 0; i < DDTBENCH_SPECFEM3D_CM_ICOUNT_IC; ++i) {
            byhand_unpack_region(base,
                                 ddtbench_specfem3d_cm_array_ic_offset()
                                     + 3
                                           * (size_t) ddtbench_index(
                                                 i, DDTBENCH_SPECFEM3D_CM_DIM_IC)
                                           * sizeof(float),
                                 &packed, 3 * sizeof(float));
        }
    }
}

static void pack_byhand_ddtbench_specfem3d_mt_ddt(void *dst, const void *src, int count)
{
    const size_t block_bytes = DDTBENCH_SPECFEM3D_MT_DIM1 * sizeof(float);
    const size_t stride = DDTBENCH_SPECFEM3D_MT_DIM2 * block_bytes;
    const size_t extent = (DDTBENCH_SPECFEM3D_MT_DIM3 - 1) * stride + block_bytes;
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * extent;

        for (int i = 0; i < DDTBENCH_SPECFEM3D_MT_DIM3; ++i) {
            byhand_pack_region(&packed, base, (size_t) i * stride, block_bytes);
        }
    }
}

static void unpack_byhand_ddtbench_specfem3d_mt_ddt(void *dst, const void *src, int count)
{
    const size_t extent = DDTBENCH_SPECFEM3D_MT_DIM3 * DDTBENCH_SPECFEM3D_MT_DIM1
                          * sizeof(float);
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * extent;

        byhand_unpack_region(base, 0, &packed, extent);
    }
}

static void pack_byhand_ddtbench_wrf_layout(void *dst, const void *src, int count)
{
    const size_t row_bytes = DDTBENCH_WRF_SUB_DIM1 * sizeof(float);
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        const char *base = input + (size_t) datatype * ddtbench_wrf_extent();

        for (int array = 0; array < DDTBENCH_WRF_NUMBER_2D; ++array) {
            const size_t array_offset = array * ddtbench_wrf_2d_array_floats() * sizeof(float);

            for (int z = 0; z < DDTBENCH_WRF_SUB_DIM3; ++z) {
                byhand_pack_region(&packed, base,
                                   array_offset
                                       + (size_t) ddtbench_idx2d(DDTBENCH_WRF_IS,
                                                                 DDTBENCH_WRF_JS + z,
                                                                 DDTBENCH_WRF_DIM1)
                                             * sizeof(float),
                                   row_bytes);
            }
        }

        for (int array = 0; array < DDTBENCH_WRF_NUMBER_3D; ++array) {
            const size_t array_offset = ddtbench_wrf_3d_arrays_offset()
                                        + array * ddtbench_wrf_3d_array_floats()
                                              * sizeof(float);

            for (int z = 0; z < DDTBENCH_WRF_SUB_DIM3; ++z) {
                for (int y = 0; y < DDTBENCH_WRF_SUB_DIM2; ++y) {
                    byhand_pack_region(
                        &packed, base,
                        array_offset
                            + (size_t) ddtbench_idx3d(DDTBENCH_WRF_IS, DDTBENCH_WRF_KS + y,
                                                       DDTBENCH_WRF_JS + z,
                                                       DDTBENCH_WRF_DIM1, DDTBENCH_WRF_DIM2)
                                  * sizeof(float),
                        row_bytes);
                }
            }
        }

        for (int array = 0; array < DDTBENCH_WRF_NUMBER_4D; ++array) {
            const size_t array_offset = ddtbench_wrf_4d_arrays_offset()
                                        + array * ddtbench_wrf_4d_array_floats()
                                              * sizeof(float);

            for (int t = DDTBENCH_WRF_PARAM_FIRST_SCALAR; t < DDTBENCH_WRF_LIMIT_4D; ++t) {
                for (int z = 0; z < DDTBENCH_WRF_SUB_DIM3; ++z) {
                    for (int y = 0; y < DDTBENCH_WRF_SUB_DIM2; ++y) {
                        byhand_pack_region(
                            &packed, base,
                            array_offset
                                + (size_t) ddtbench_idx4d(
                                      DDTBENCH_WRF_IS, DDTBENCH_WRF_KS + y,
                                      DDTBENCH_WRF_JS + z, t, DDTBENCH_WRF_DIM1,
                                      DDTBENCH_WRF_DIM2, DDTBENCH_WRF_DIM3)
                                      * sizeof(float),
                            row_bytes);
                    }
                }
            }
        }
    }
}

static void unpack_byhand_ddtbench_wrf_layout(void *dst, const void *src, int count)
{
    const size_t row_bytes = DDTBENCH_WRF_SUB_DIM1 * sizeof(float);
    const char *packed = (const char *) src;
    char *output = (char *) dst;

    for (int datatype = 0; datatype < count; ++datatype) {
        char *base = output + (size_t) datatype * ddtbench_wrf_extent();

        for (int array = 0; array < DDTBENCH_WRF_NUMBER_2D; ++array) {
            const size_t array_offset = array * ddtbench_wrf_2d_array_floats() * sizeof(float);

            for (int z = 0; z < DDTBENCH_WRF_SUB_DIM3; ++z) {
                byhand_unpack_region(base,
                                     array_offset
                                         + (size_t) ddtbench_idx2d(DDTBENCH_WRF_IS,
                                                                   DDTBENCH_WRF_JS + z,
                                                                   DDTBENCH_WRF_DIM1)
                                               * sizeof(float),
                                     &packed, row_bytes);
            }
        }

        for (int array = 0; array < DDTBENCH_WRF_NUMBER_3D; ++array) {
            const size_t array_offset = ddtbench_wrf_3d_arrays_offset()
                                        + array * ddtbench_wrf_3d_array_floats()
                                              * sizeof(float);

            for (int z = 0; z < DDTBENCH_WRF_SUB_DIM3; ++z) {
                for (int y = 0; y < DDTBENCH_WRF_SUB_DIM2; ++y) {
                    byhand_unpack_region(
                        base,
                        array_offset
                            + (size_t) ddtbench_idx3d(DDTBENCH_WRF_IS, DDTBENCH_WRF_KS + y,
                                                       DDTBENCH_WRF_JS + z,
                                                       DDTBENCH_WRF_DIM1, DDTBENCH_WRF_DIM2)
                                  * sizeof(float),
                        &packed, row_bytes);
                }
            }
        }

        for (int array = 0; array < DDTBENCH_WRF_NUMBER_4D; ++array) {
            const size_t array_offset = ddtbench_wrf_4d_arrays_offset()
                                        + array * ddtbench_wrf_4d_array_floats()
                                              * sizeof(float);

            for (int t = DDTBENCH_WRF_PARAM_FIRST_SCALAR; t < DDTBENCH_WRF_LIMIT_4D; ++t) {
                for (int z = 0; z < DDTBENCH_WRF_SUB_DIM3; ++z) {
                    for (int y = 0; y < DDTBENCH_WRF_SUB_DIM2; ++y) {
                        byhand_unpack_region(
                            base,
                            array_offset
                                + (size_t) ddtbench_idx4d(
                                      DDTBENCH_WRF_IS, DDTBENCH_WRF_KS + y,
                                      DDTBENCH_WRF_JS + z, t, DDTBENCH_WRF_DIM1,
                                      DDTBENCH_WRF_DIM2, DDTBENCH_WRF_DIM3)
                                      * sizeof(float),
                            &packed, row_bytes);
                    }
                }
            }
        }
    }
}

static void pack_byhand_ddtbench_wrf_vec_ddt(void *dst, const void *src, int count)
{
    pack_byhand_ddtbench_wrf_layout(dst, src, count);
}

static void unpack_byhand_ddtbench_wrf_vec_ddt(void *dst, const void *src, int count)
{
    unpack_byhand_ddtbench_wrf_layout(dst, src, count);
}

static void pack_byhand_ddtbench_wrf_subarray_ddt(void *dst, const void *src, int count)
{
    pack_byhand_ddtbench_wrf_layout(dst, src, count);
}

static void unpack_byhand_ddtbench_wrf_subarray_ddt(void *dst, const void *src, int count)
{
    unpack_byhand_ddtbench_wrf_layout(dst, src, count);
}

/* ==== END builders and by-hand baselines ==== */

/*
 * Adversarial datatypes added for the optimizer equivalence check.  These are
 * deliberately degenerate shapes -- a single-iteration loop with a trailing gap,
 * a zero-extent (self-overlapping) type, a negative-extent (backward-striding)
 * type, and a mixed same-size type run that invites the optimizer to promote /
 * merge elements -- that stress corners the DDTBench-derived shapes do not.
 *
 * Their by-hand baselines follow the same convention the harness uses for every
 * datatype: @src / @dst point at the instance-zero base (buffer + true_lb), and
 * instance i lives at that base + i * extent, so a negative extent strides
 * backward from the base.
 */
#define ADV_SINGLE_ITER_BLOCK 3                       /* ints in the single block */
#define ADV_SINGLE_ITER_EXTENT (6 * sizeof(int))      /* block + trailing gap */

static MPI_Datatype create_adv_single_iter_gap(void)
{
    MPI_Datatype block, type;

    MPI_Type_contiguous(ADV_SINGLE_ITER_BLOCK, MPI_INT, &block);
    MPI_Type_create_resized(block, 0, (MPI_Aint) ADV_SINGLE_ITER_EXTENT, &type);
    MPI_Type_free(&block);
    MPI_Type_commit(&type);
    return type;
}

static void pack_byhand_adv_single_iter_gap(void *dst, const void *src, int count)
{
    const char *in = (const char *) src;
    char *packed = (char *) dst;
    const size_t payload = ADV_SINGLE_ITER_BLOCK * sizeof(int);

    for (int i = 0; i < count; ++i) {
        memcpy(packed, in + (size_t) i * ADV_SINGLE_ITER_EXTENT, payload);
        packed += payload;
    }
}

static void unpack_byhand_adv_single_iter_gap(void *dst, const void *src, int count)
{
    const char *packed = (const char *) src;
    char *out = (char *) dst;
    const size_t payload = ADV_SINGLE_ITER_BLOCK * sizeof(int);

    for (int i = 0; i < count; ++i) {
        memcpy(out + (size_t) i * ADV_SINGLE_ITER_EXTENT, packed, payload);
        packed += payload;
    }
}

#define ADV_OVERLAP_BLOCK 2                       /* ints per instance */

static MPI_Datatype create_adv_zero_extent_overlap(void)
{
    MPI_Datatype block, type;

    MPI_Type_contiguous(ADV_OVERLAP_BLOCK, MPI_INT, &block);
    MPI_Type_create_resized(block, 0, 0, &type); /* extent 0: every instance aliases the first */
    MPI_Type_free(&block);
    MPI_Type_commit(&type);
    return type;
}

static void pack_byhand_adv_zero_extent_overlap(void *dst, const void *src, int count)
{
    const char *in = (const char *) src;
    char *packed = (char *) dst;
    const size_t payload = ADV_OVERLAP_BLOCK * sizeof(int);

    for (int i = 0; i < count; ++i) {
        memcpy(packed, in, payload); /* extent 0: always instance zero */
        packed += payload;
    }
}

static void unpack_byhand_adv_zero_extent_overlap(void *dst, const void *src, int count)
{
    const char *packed = (const char *) src;
    char *out = (char *) dst;
    const size_t payload = ADV_OVERLAP_BLOCK * sizeof(int);

    /* Every instance writes the same region; the last one wins, matching the engine. */
    for (int i = 0; i < count; ++i) {
        memcpy(out, packed, payload);
        packed += payload;
    }
}

#define ADV_NEG_BLOCK 2                                  /* ints per instance */
#define ADV_NEG_EXTENT (-(ptrdiff_t) (ADV_NEG_BLOCK * sizeof(int)))

static MPI_Datatype create_adv_neg_extent(void)
{
    MPI_Datatype block, type;

    MPI_Type_contiguous(ADV_NEG_BLOCK, MPI_INT, &block);
    MPI_Type_create_resized(block, 0, (MPI_Aint) ADV_NEG_EXTENT, &type);
    MPI_Type_free(&block);
    MPI_Type_commit(&type);
    return type;
}

static void pack_byhand_adv_neg_extent(void *dst, const void *src, int count)
{
    const char *in = (const char *) src;
    char *packed = (char *) dst;
    const size_t payload = ADV_NEG_BLOCK * sizeof(int);

    for (int i = 0; i < count; ++i) {
        memcpy(packed, in + (ptrdiff_t) i * ADV_NEG_EXTENT, payload);
        packed += payload;
    }
}

static void unpack_byhand_adv_neg_extent(void *dst, const void *src, int count)
{
    const char *packed = (const char *) src;
    char *out = (char *) dst;
    const size_t payload = ADV_NEG_BLOCK * sizeof(int);

    for (int i = 0; i < count; ++i) {
        memcpy(out + (ptrdiff_t) i * ADV_NEG_EXTENT, packed, payload);
        packed += payload;
    }
}

#define ADV_MIXED_REPEAT 4 /* struct{int,float} instances made contiguous */

static MPI_Datatype create_adv_mixed_promote(void)
{
    MPI_Datatype types[2] = {MPI_INT, MPI_FLOAT};
    int blocklens[2] = {1, 1};
    MPI_Aint disps[2] = {0, (MPI_Aint) sizeof(int)};
    MPI_Datatype pair, resized, type;

    MPI_Type_create_struct(2, blocklens, disps, types, &pair);
    MPI_Type_create_resized(pair, 0, (MPI_Aint) (2 * sizeof(int)), &resized);
    MPI_Type_contiguous(ADV_MIXED_REPEAT, resized, &type);
    MPI_Type_free(&pair);
    MPI_Type_free(&resized);
    MPI_Type_commit(&type);
    return type;
}

static void pack_byhand_adv_mixed_promote(void *dst, const void *src, int count)
{
    /* int and float are laid out contiguously with no gaps, so an instance is
     * one contiguous ADV_MIXED_REPEAT * 8-byte run. */
    memcpy(dst, src, (size_t) count * ADV_MIXED_REPEAT * 2 * sizeof(int));
}

static void unpack_byhand_adv_mixed_promote(void *dst, const void *src, int count)
{
    memcpy(dst, src, (size_t) count * ADV_MIXED_REPEAT * 2 * sizeof(int));
}

/*
 * Thunks that bake in the fixed construction parameters, so every row in the
 * registry is a uniform zero-argument builder.
 */
static MPI_Datatype build_merged_contig_with_gaps(void)
{
    return create_merged_contig_with_gaps(1);
}

static MPI_Datatype build_indexed_constant_gap(void)
{
    return create_indexed_constant_gap_ddt(80, 100, 1);
}

static MPI_Datatype build_optimized_indexed_constant_gap(void)
{
    return create_optimized_indexed_constant_gap_ddt(80, 100, 1);
}

static MPI_Datatype build_struct_constant_gap(void)
{
    return create_struct_constant_gap_ddt(80, 100, 1);
}

static MPI_Datatype build_struct_constant_gap_resized(void)
{
    return create_struct_constant_gap_resized_ddt(0, 0, 0);
}

/*
 * "contig" historically exercises a run of plain MPI_INT elements, i.e. a
 * predefined handle used as-is.  Returning it through the ordinary builder means
 * the registry needs no separate predefined slot; datatype_corpus_finalize()
 * recognizes predefined handles and leaves them alone.
 */
static MPI_Datatype build_contig(void)
{
    return MPI_INT;
}

/*
 * The registry.  A row builds its datatype through exactly one of:
 *   build       -- a single handle used for both send and recv.  It may be a
 *                  freshly committed derived type or a predefined handle; either
 *                  way finalize only frees it if it is derived.
 *   build_pair  -- a distinct send/recv pair (both freed if derived).
 */
typedef MPI_Datatype (*corpus_build_fn)(void);
typedef void (*corpus_build_pair_fn)(MPI_Datatype *send_type, MPI_Datatype *recv_type);

typedef struct {
    const char          *name;
    corpus_build_fn      build;
    corpus_build_pair_fn build_pair;
    datatype_byhand_fn_t pack_byhand;
    datatype_byhand_fn_t unpack_byhand;
    unsigned int         traits;
} corpus_desc_t;

static const corpus_desc_t corpus_desc[] = {
    {"contig", build_contig, NULL,
     pack_byhand_contiguous_datatype, unpack_byhand_contiguous_datatype,
     DT_TRAIT_CONTIGUOUS},
    {"indexed_gap", create_indexed_gap_ddt, NULL,
     pack_byhand_create_indexed_gap_ddt, unpack_byhand_create_indexed_gap_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_NESTED_LOOP | DT_TRAIT_MIXED_TYPES},
    {"optimized_indexed_gap", create_indexed_gap_optimized_ddt, NULL,
     pack_byhand_create_indexed_gap_optimized_ddt, unpack_byhand_create_indexed_gap_optimized_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_RESIZED},
    {"constant_gap", build_indexed_constant_gap, NULL,
     pack_byhand_create_indexed_constant_gap_ddt, unpack_byhand_create_indexed_constant_gap_ddt,
     DT_TRAIT_HAS_GAPS},
    {"optimized_constant_gap", build_optimized_indexed_constant_gap, NULL,
     pack_byhand_create_optimized_indexed_constant_gap_ddt,
     unpack_byhand_create_optimized_indexed_constant_gap_ddt,
     DT_TRAIT_HAS_GAPS},
    {"struct_constant_gap", build_struct_constant_gap, NULL,
     pack_byhand_create_struct_constant_gap_ddt, unpack_byhand_create_struct_constant_gap_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_NESTED_LOOP},
    {"struct_constant_gap_resized", build_struct_constant_gap_resized, NULL,
     pack_byhand_create_struct_constant_gap_resized_ddt,
     unpack_byhand_create_struct_constant_gap_resized_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_RESIZED | DT_TRAIT_MIXED_TYPES},
    {"struct_merged_with_gap_resized", build_merged_contig_with_gaps, NULL,
     pack_byhand_create_merged_contig_with_gaps, unpack_byhand_create_merged_contig_with_gaps,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_RESIZED | DT_TRAIT_MIXED_TYPES},
    {"ddtbench_fft2d_scatter", create_ddtbench_fft2d_scatter_ddt, NULL,
     pack_byhand_ddtbench_fft2d_scatter_ddt, unpack_byhand_ddtbench_fft2d_scatter_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_RESIZED},
    {"ddtbench_fft2d_gather", create_ddtbench_fft2d_gather_ddt, NULL,
     pack_byhand_ddtbench_fft2d_gather_ddt, unpack_byhand_ddtbench_fft2d_gather_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_RESIZED},
    {"ddtbench_milc_su3_zdown", create_ddtbench_milc_su3_zdown_ddt, NULL,
     pack_byhand_ddtbench_milc_su3_zdown_ddt, unpack_byhand_ddtbench_milc_su3_zdown_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_NESTED_LOOP},
    {"ddtbench_nas_lu_y", create_ddtbench_nas_lu_y_ddt, NULL,
     pack_byhand_ddtbench_nas_lu_y_ddt, unpack_byhand_ddtbench_nas_lu_y_ddt,
     DT_TRAIT_HAS_GAPS},
    {"ddtbench_nas_lu_x", create_ddtbench_nas_lu_x_ddt, NULL,
     pack_byhand_ddtbench_nas_lu_x_ddt, unpack_byhand_ddtbench_nas_lu_x_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_NESTED_LOOP},
    {"ddtbench_nas_mg_x", create_ddtbench_nas_mg_x_ddt, NULL,
     pack_byhand_ddtbench_nas_mg_x_ddt, unpack_byhand_ddtbench_nas_mg_x_ddt,
     DT_TRAIT_HAS_GAPS},
    {"ddtbench_nas_mg_y", create_ddtbench_nas_mg_y_ddt, NULL,
     pack_byhand_ddtbench_nas_mg_y_ddt, unpack_byhand_ddtbench_nas_mg_y_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_NESTED_LOOP},
    {"ddtbench_nas_mg_z", create_ddtbench_nas_mg_z_ddt, NULL,
     pack_byhand_ddtbench_nas_mg_z_ddt, unpack_byhand_ddtbench_nas_mg_z_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_NESTED_LOOP},
    {"ddtbench_lammps_full", NULL, create_ddtbench_lammps_full_ddt,
     pack_byhand_ddtbench_lammps_full_ddt, unpack_byhand_ddtbench_lammps_full_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_PAIR},
    {"ddtbench_lammps_atomic", NULL, create_ddtbench_lammps_atomic_ddt,
     pack_byhand_ddtbench_lammps_atomic_ddt, unpack_byhand_ddtbench_lammps_atomic_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_PAIR},
    {"ddtbench_specfem3d_oc", create_ddtbench_specfem3d_oc_ddt, NULL,
     pack_byhand_ddtbench_specfem3d_oc_ddt, unpack_byhand_ddtbench_specfem3d_oc_ddt,
     DT_TRAIT_HAS_GAPS},
    {"ddtbench_specfem3d_cm", create_ddtbench_specfem3d_cm_ddt, NULL,
     pack_byhand_ddtbench_specfem3d_cm_ddt, unpack_byhand_ddtbench_specfem3d_cm_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_RESIZED},
    {"ddtbench_specfem3d_mt", NULL, create_ddtbench_specfem3d_mt_ddt,
     pack_byhand_ddtbench_specfem3d_mt_ddt, unpack_byhand_ddtbench_specfem3d_mt_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_PAIR},
    {"ddtbench_wrf_vec", create_ddtbench_wrf_vec_ddt, NULL,
     pack_byhand_ddtbench_wrf_vec_ddt, unpack_byhand_ddtbench_wrf_vec_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_NESTED_LOOP},
    {"ddtbench_wrf_subarray", create_ddtbench_wrf_subarray_ddt, NULL,
     pack_byhand_ddtbench_wrf_subarray_ddt, unpack_byhand_ddtbench_wrf_subarray_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_NESTED_LOOP},
    {"complex_hvector", create_complex_hvector_ddt, NULL,
     pack_byhand_complex_hvector_ddt, unpack_byhand_complex_hvector_ddt,
     DT_TRAIT_HAS_GAPS | DT_TRAIT_NESTED_LOOP},
    /*
     * Adversarial shapes carrying no by-hand baseline of the benchmark family;
     * they exist to stress the optimizer/positioning edges (single iteration,
     * zero/negative extent, overlap, mixed-type promotion).  A consumer that
     * only wants the classic benchmark set can filter them out by trait.
     */
    {"adv_single_iter_gap", create_adv_single_iter_gap, NULL,
     pack_byhand_adv_single_iter_gap, unpack_byhand_adv_single_iter_gap,
     DT_TRAIT_SINGLE_ITER | DT_TRAIT_HAS_GAPS | DT_TRAIT_RESIZED},
    {"adv_zero_extent_overlap", create_adv_zero_extent_overlap, NULL,
     pack_byhand_adv_zero_extent_overlap, unpack_byhand_adv_zero_extent_overlap,
     DT_TRAIT_ZERO_EXTENT | DT_TRAIT_OVERLAP | DT_TRAIT_RESIZED},
    {"adv_neg_extent", create_adv_neg_extent, NULL,
     pack_byhand_adv_neg_extent, unpack_byhand_adv_neg_extent,
     DT_TRAIT_NEG_EXTENT | DT_TRAIT_OVERLAP | DT_TRAIT_RESIZED},
    {"adv_mixed_promote", create_adv_mixed_promote, NULL,
     pack_byhand_adv_mixed_promote, unpack_byhand_adv_mixed_promote,
     DT_TRAIT_MIXED_TYPES | DT_TRAIT_NESTED_LOOP},
};

datatype_corpus_t *datatype_corpus_init(void)
{
    const size_t n = sizeof(corpus_desc) / sizeof(corpus_desc[0]);
    datatype_corpus_t *corpus = (datatype_corpus_t *) calloc(1, sizeof(*corpus));
    datatype_corpus_entry_t *entries
        = (datatype_corpus_entry_t *) calloc(n, sizeof(*entries));

    if ((NULL == corpus) || (NULL == entries)) {
        fprintf(stderr, "datatype_corpus_init: out of memory\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_NO_MEM);
    }

    for (size_t i = 0; i < n; ++i) {
        const corpus_desc_t *desc = &corpus_desc[i];
        datatype_corpus_entry_t *entry = &entries[i];

        entry->name = desc->name;
        entry->pack_byhand = desc->pack_byhand;
        entry->unpack_byhand = desc->unpack_byhand;
        entry->traits = desc->traits;

        if (NULL != desc->build_pair) {
            desc->build_pair(&entry->send_type, &entry->recv_type);
        } else {
            entry->send_type = desc->build();
            entry->recv_type = entry->send_type; /* one handle used both ways */
        }
    }

    corpus->entries = entries;
    corpus->count = n;
    return corpus;
}

/*
 * A predefined datatype must never be freed; a derived one must.  MPI_Type_free
 * is only legal on derived types, and MPI_Type_get_envelope reports predefined
 * handles with the MPI_COMBINER_NAMED combiner, so we let the handle itself tell
 * us whether finalize owns it -- no separate ownership bookkeeping needed.
 */
static int corpus_type_is_derived(MPI_Datatype type)
{
    int num_integers, num_addresses, num_datatypes, combiner;

    MPI_Type_get_envelope(type, &num_integers, &num_addresses, &num_datatypes, &combiner);
    return MPI_COMBINER_NAMED != combiner;
}

void datatype_corpus_finalize(datatype_corpus_t *corpus)
{
    if (NULL == corpus) {
        return;
    }
    for (size_t i = 0; i < corpus->count; ++i) {
        MPI_Datatype send_type = corpus->entries[i].send_type;
        MPI_Datatype recv_type = corpus->entries[i].recv_type;

        if (corpus_type_is_derived(send_type)) {
            MPI_Type_free(&corpus->entries[i].send_type);
        }
        /* For a single-handle entry send_type == recv_type: free it only once. */
        if ((recv_type != send_type) && corpus_type_is_derived(recv_type)) {
            MPI_Type_free(&corpus->entries[i].recv_type);
        }
    }
    free(corpus->entries);
    free(corpus);
}
