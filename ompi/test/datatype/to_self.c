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

#ifdef HAVE_CONFIG_H
#    include "ompi_config.h"
#endif

#include "mpi.h"

/*
 * Keep this benchmark usable as a plain MPI application.  The in-tree Open MPI
 * build includes ompi_config.h above, which leaves OMPI_BUILDING set to 1; an
 * installed Open MPI mpi.h defines OMPI_MAJOR_VERSION too, but keeps
 * OMPI_BUILDING at 0 for applications and must use only the public MPI API.
 */
#if defined(OMPI_MAJOR_VERSION) && defined(OMPI_BUILDING) && OMPI_BUILDING
#    include "ompi/datatype/ompi_datatype.h"
#    define TO_SELF_HAVE_OMPI_INTERNALS 1
#else
#    define TO_SELF_HAVE_OMPI_INTERNALS 0
#endif

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if TO_SELF_HAVE_OMPI_INTERNALS
/* Keep datatype dumps opt-in; the normal benchmark output should stay compact. */
static int dump_datatypes = 0;

/* Dump committed datatype internals when --dump is selected. */
#define MPI_DDT_DUMP(ddt)                                                                  \
    do {                                                                                   \
        if (dump_datatypes) {                                                              \
            ompi_datatype_dump((ddt));                                                     \
        }                                                                                  \
    } while (0)
#else
/* Standalone MPI builds cannot inspect the implementation-defined datatype handle. */
#define MPI_DDT_DUMP(ddt)                                                                  \
    do {                                                                                   \
        (void) (ddt);                                                                      \
    } while (0)
#endif

static MPI_Datatype create_merged_contig_with_gaps(int count) /* count of the basic datatype */
{
    int array_of_blocklengths[] = {1, 1, 1};
    MPI_Aint array_of_displacements[] = {0, 8, 16};
    MPI_Datatype array_of_types[] = {MPI_DOUBLE, MPI_LONG, MPI_CHAR};
    MPI_Datatype type;

    MPI_Type_create_struct(3, array_of_blocklengths, array_of_displacements, array_of_types, &type);
    if (1 < count) {
        MPI_Datatype temp = type;
        MPI_Type_contiguous(count, temp, &type);
    }
    MPI_Type_commit(&type);
    MPI_DDT_DUMP(type);
    return type;
}

/* Create a non-contiguous resized datatype */
struct structure {
    double not_transfered;
    double transfered_1;
    long   transfered_2;
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

    MPI_Get_address(&data[0].transfered_1, &disps[0]);
    MPI_Get_address(&data[0].transfered_2, &disps[1]);
    MPI_Get_address(&data[0], &disps[2]);
    disps[1] -= disps[2]; /*  8 */
    disps[0] -= disps[2]; /* 16 */

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
    MPI_Datatype dt1, dt2, dt3;
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

    MPI_Type_create_struct(3, bLength, displ, types, &dt3);

    MPI_Type_free(&dt1);
    MPI_Type_free(&dt2);
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
 * adapted from DDTBench 1.2.1. Keep the benchmark headings tagged with this
 * source version so the generated performance data is unambiguous.
 */
#define DDTBENCH_IMPORTED_SOURCE "DDTBench 1.2.1"
#define PRINT_DDTBENCH_TEST(name) printf("\n%s %s\n\n", DDTBENCH_IMPORTED_SOURCE, (name))

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
        displacements[counter] = (MPI_Aint) ((i * ddtbench_wrf_2d_array_floats()
                                              + ddtbench_idx2d(DDTBENCH_WRF_IS,
                                                               DDTBENCH_WRF_JS,
                                                               DDTBENCH_WRF_DIM1))
                                             * sizeof(float));
        oldtypes[counter++] = temp_2d_type;
    }

    MPI_Type_vector(DDTBENCH_WRF_SUB_DIM2, DDTBENCH_WRF_SUB_DIM1, DDTBENCH_WRF_DIM1,
                    MPI_FLOAT, &temp_type);
    stride = DDTBENCH_WRF_DIM1 * DDTBENCH_WRF_DIM2 * (MPI_Aint) sizeof(float);
    MPI_Type_create_hvector(DDTBENCH_WRF_SUB_DIM3, 1, stride, temp_type, &temp_3d_type);
    MPI_Type_free(&temp_type);
    for (int i = 0; i < DDTBENCH_WRF_NUMBER_3D; ++i) {
        displacements[counter] = (MPI_Aint) ddtbench_wrf_3d_arrays_offset()
                                 + (MPI_Aint) ((i * ddtbench_wrf_3d_array_floats()
                                                + ddtbench_idx3d(DDTBENCH_WRF_IS,
                                                                 DDTBENCH_WRF_KS,
                                                                 DDTBENCH_WRF_JS,
                                                                 DDTBENCH_WRF_DIM1,
                                                                 DDTBENCH_WRF_DIM2))
                                               * sizeof(float));
        oldtypes[counter++] = temp_3d_type;
    }

    stride *= DDTBENCH_WRF_DIM3;
    for (int i = 0; i < DDTBENCH_WRF_NUMBER_4D; ++i) {
        MPI_Datatype temp_4d_type;

        MPI_Type_create_hvector(DDTBENCH_WRF_LIMIT_4D - DDTBENCH_WRF_PARAM_FIRST_SCALAR, 1,
                                stride, temp_3d_type, &temp_4d_type);
        displacements[counter] = (MPI_Aint) ddtbench_wrf_4d_arrays_offset()
                                 + (MPI_Aint) ((i * ddtbench_wrf_4d_array_floats()
                                                + ddtbench_idx4d(
                                                      DDTBENCH_WRF_IS, DDTBENCH_WRF_KS,
                                                      DDTBENCH_WRF_JS,
                                                      DDTBENCH_WRF_PARAM_FIRST_SCALAR,
                                                      DDTBENCH_WRF_DIM1, DDTBENCH_WRF_DIM2,
                                                      DDTBENCH_WRF_DIM3))
                                               * sizeof(float));
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
        displacements[counter] = (MPI_Aint) (i * ddtbench_wrf_2d_array_floats()
                                             * sizeof(float));
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
        displacements[counter] = (MPI_Aint) ddtbench_wrf_3d_arrays_offset()
                                 + (MPI_Aint) (i * ddtbench_wrf_3d_array_floats()
                                               * sizeof(float));
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
        displacements[counter] = (MPI_Aint) ddtbench_wrf_4d_arrays_offset()
                                 + (MPI_Aint) (i * ddtbench_wrf_4d_array_floats()
                                               * sizeof(float));
        oldtypes[counter++] = temp_4d_type;
    }

    MPI_Type_create_struct(counter, blocklengths, displacements, oldtypes, &raw_type);
    MPI_Type_free(&temp_2d_type);
    MPI_Type_free(&temp_3d_type);
    MPI_Type_free(&temp_4d_type);
    return ddtbench_resize_and_commit(raw_type, (MPI_Aint) ddtbench_wrf_extent());
}

typedef void (*byhand_copy_fn_t)(void *dst, const void *src, int count);

typedef struct {
    double d;
    long l;
    char c;
} byhand_merged_contig_with_gaps_layout_t;

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
    const size_t extent = sizeof(byhand_merged_contig_with_gaps_layout_t);
    const size_t payload_length = offsetof(byhand_merged_contig_with_gaps_layout_t, c)
                                  + sizeof(char);
    const char *input = (const char *) src;
    char *packed = (char *) dst;

    for (int i = 0; i < count; i++) {
        byhand_pack_region(&packed, input + (size_t) i * extent, 0, payload_length);
    }
}

static void unpack_byhand_create_merged_contig_with_gaps(void *dst, const void *src, int count)
{
    const size_t extent = sizeof(byhand_merged_contig_with_gaps_layout_t);
    const size_t payload_length = offsetof(byhand_merged_contig_with_gaps_layout_t, c)
                                  + sizeof(char);
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
    const size_t payload_offset = offsetof(struct structure, transfered_1);
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
    const size_t payload_offset = offsetof(struct structure, transfered_1);
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
            const size_t array_offset = array * ddtbench_wrf_2d_array_floats();

            for (int z = 0; z < DDTBENCH_WRF_SUB_DIM3; ++z) {
                byhand_pack_region(&packed, base,
                                   (array_offset
                                    + ddtbench_idx2d(DDTBENCH_WRF_IS,
                                                     DDTBENCH_WRF_JS + z,
                                                     DDTBENCH_WRF_DIM1))
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
            const size_t array_offset = array * ddtbench_wrf_2d_array_floats();

            for (int z = 0; z < DDTBENCH_WRF_SUB_DIM3; ++z) {
                byhand_unpack_region(base,
                                     (array_offset
                                      + ddtbench_idx2d(DDTBENCH_WRF_IS,
                                                       DDTBENCH_WRF_JS + z,
                                                       DDTBENCH_WRF_DIM1))
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

/********************************************************************
 *******************************************************************/

typedef uint64_t test_mask_t;

#define DO_CONTIG                         UINT64_C(0x0000000000000001)
#define DO_CONSTANT_GAP                   UINT64_C(0x0000000000000002)
#define DO_INDEXED_GAP                    UINT64_C(0x0000000000000004)
#define DO_OPTIMIZED_INDEXED_GAP          UINT64_C(0x0000000000000008)
#define DO_STRUCT_CONSTANT_GAP_RESIZED    UINT64_C(0x0000000000000010)
#define DO_STRUCT_MERGED_WITH_GAP_RESIZED UINT64_C(0x0000000000000020)
#define DO_OPTIMIZED_CONSTANT_GAP         UINT64_C(0x0000000000000040)
#define DO_STRUCT_CONSTANT_GAP            UINT64_C(0x0000000000000800)
#define DO_DDTBENCH_FFT2D_SCATTER         UINT64_C(0x0000000000001000)
#define DO_DDTBENCH_FFT2D_GATHER          UINT64_C(0x0000000000002000)
#define DO_DDTBENCH_MILC_SU3_ZDOWN        UINT64_C(0x0000000000004000)
#define DO_DDTBENCH_NAS_LU_Y              UINT64_C(0x0000000000008000)
#define DO_DDTBENCH_NAS_LU_X              UINT64_C(0x0000000000010000)
#define DO_DDTBENCH_NAS_MG_X              UINT64_C(0x0000000000020000)
#define DO_DDTBENCH_NAS_MG_Y              UINT64_C(0x0000000000040000)
#define DO_DDTBENCH_NAS_MG_Z              UINT64_C(0x0000000000080000)
#define DO_DDTBENCH_LAMMPS_FULL           UINT64_C(0x0000000000100000)
#define DO_DDTBENCH_LAMMPS_ATOMIC         UINT64_C(0x0000000000200000)
#define DO_DDTBENCH_SPECFEM3D_OC          UINT64_C(0x0000000000400000)
#define DO_DDTBENCH_SPECFEM3D_CM          UINT64_C(0x0000000000800000)
#define DO_DDTBENCH_SPECFEM3D_MT          UINT64_C(0x0000000001000000)
#define DO_DDTBENCH_WRF_VEC               UINT64_C(0x0000000002000000)
#define DO_DDTBENCH_WRF_SUBARRAY          UINT64_C(0x0000000004000000)
#define DO_PACK                                     UINT64_C(0x0000000400000000)
#define DO_UNPACK                                   UINT64_C(0x0000000800000000)
#define DO_ISEND_RECV                               UINT64_C(0x0000001000000000)
#define DO_ISEND_IRECV                              UINT64_C(0x0000002000000000)
#define DO_IRECV_SEND                               UINT64_C(0x0000004000000000)
#define DO_IRECV_ISEND                              UINT64_C(0x0000008000000000)
#define DO_PACK_BYHAND                              UINT64_C(0x0000010000000000)
#define DO_UNPACK_BYHAND                            UINT64_C(0x0000020000000000)

/* Default test sets used when --data or --check is omitted. */
#define DO_DATATYPE_TESTS                                                                    \
    (DO_CONTIG | DO_CONSTANT_GAP | DO_INDEXED_GAP | DO_OPTIMIZED_INDEXED_GAP                 \
     | DO_STRUCT_CONSTANT_GAP_RESIZED | DO_STRUCT_MERGED_WITH_GAP_RESIZED                    \
     | DO_OPTIMIZED_CONSTANT_GAP | DO_STRUCT_CONSTANT_GAP                                    \
     | DO_DDTBENCH_FFT2D_SCATTER | DO_DDTBENCH_FFT2D_GATHER                                 \
     | DO_DDTBENCH_MILC_SU3_ZDOWN | DO_DDTBENCH_NAS_LU_Y | DO_DDTBENCH_NAS_LU_X              \
     | DO_DDTBENCH_NAS_MG_X | DO_DDTBENCH_NAS_MG_Y | DO_DDTBENCH_NAS_MG_Z                    \
     | DO_DDTBENCH_LAMMPS_FULL | DO_DDTBENCH_LAMMPS_ATOMIC                                  \
     | DO_DDTBENCH_SPECFEM3D_OC | DO_DDTBENCH_SPECFEM3D_CM                                  \
     | DO_DDTBENCH_SPECFEM3D_MT | DO_DDTBENCH_WRF_VEC | DO_DDTBENCH_WRF_SUBARRAY)
#define DO_OPERATION_TESTS                                                                 \
    (DO_PACK | DO_UNPACK | DO_PACK_BYHAND | DO_UNPACK_BYHAND | DO_ISEND_RECV               \
     | DO_ISEND_IRECV | DO_IRECV_SEND | DO_IRECV_ISEND)

#define MIN_GOOD_TIMERS 5
#define MIN_LENGTH 1024
#define MAX_LENGTH (1024 * 1024)
#define MAX_TIMER_SIZE_POINTS 32
static int cycles = 100;
static int trials = 20;
static int warmups = 2;
static int min_work_bytes = 0;
static int raw_timers = 0;

typedef struct {
    const char *operation;
    double seconds;
    int length;
    int trial;
    int retained;
} raw_timer_record_t;

static raw_timer_record_t *raw_timer_records = NULL;
static size_t raw_timer_count = 0;
static size_t raw_timer_capacity = 0;

typedef struct {
    const char *name;
    test_mask_t flag;
} check_option_t;

/* --data selects datatype constructors. The help text exposes these indexes. */
static const check_option_t data_options[] = {
    {"contig", DO_CONTIG},
    {"constant_gap", DO_CONSTANT_GAP},
    {"optimized_constant_gap", DO_OPTIMIZED_CONSTANT_GAP},
    {"indexed_gap", DO_INDEXED_GAP},
    {"optimized_indexed_gap", DO_OPTIMIZED_INDEXED_GAP},
    {"struct_constant_gap_resized", DO_STRUCT_CONSTANT_GAP_RESIZED},
    {"struct_merged_with_gap_resized", DO_STRUCT_MERGED_WITH_GAP_RESIZED},
    {"struct_constant_gap", DO_STRUCT_CONSTANT_GAP},
    {"ddtbench_fft2d_scatter", DO_DDTBENCH_FFT2D_SCATTER},
    {"ddtbench_fft2d_gather", DO_DDTBENCH_FFT2D_GATHER},
    {"ddtbench_milc_su3_zdown", DO_DDTBENCH_MILC_SU3_ZDOWN},
    {"ddtbench_nas_lu_y", DO_DDTBENCH_NAS_LU_Y},
    {"ddtbench_nas_lu_x", DO_DDTBENCH_NAS_LU_X},
    {"ddtbench_nas_mg_x", DO_DDTBENCH_NAS_MG_X},
    {"ddtbench_nas_mg_y", DO_DDTBENCH_NAS_MG_Y},
    {"ddtbench_nas_mg_z", DO_DDTBENCH_NAS_MG_Z},
    {"ddtbench_lammps_full", DO_DDTBENCH_LAMMPS_FULL},
    {"ddtbench_lammps_atomic", DO_DDTBENCH_LAMMPS_ATOMIC},
    {"ddtbench_specfem3d_oc", DO_DDTBENCH_SPECFEM3D_OC},
    {"ddtbench_specfem3d_cm", DO_DDTBENCH_SPECFEM3D_CM},
    {"ddtbench_specfem3d_mt", DO_DDTBENCH_SPECFEM3D_MT},
    {"ddtbench_wrf_vec", DO_DDTBENCH_WRF_VEC},
    {"ddtbench_wrf_subarray", DO_DDTBENCH_WRF_SUBARRAY},
};

/* --check selects the operation family to run against the selected datatypes. */
static const check_option_t check_options[] = {
    {"pack", DO_PACK},
    {"unpack", DO_UNPACK},
    {"pack_byhand", DO_PACK_BYHAND},
    {"unpack_byhand", DO_UNPACK_BYHAND},
    {"isend_recv", DO_ISEND_RECV},
    {"isend_irecv", DO_ISEND_IRECV},
    {"irecv_send", DO_IRECV_SEND},
    {"irecv_isend", DO_IRECV_ISEND},
};

/* Print a comma-delimited option namespace, optionally with numeric indexes. */
static void print_options(FILE *stream, const char *option_name, const check_option_t *options,
                          size_t option_count, int show_indexes)
{
    fprintf(stream, "Valid %s entries: all", option_name);
    for (size_t i = 0; i < option_count; ++i) {
        if (show_indexes) {
            fprintf(stream, ", %zu=%s", i, options[i].name);
        } else {
            fprintf(stream, ", %s", options[i].name);
        }
    }
    fprintf(stream, "\n");
}

/* Print the command-line syntax and the valid entries for both namespaces. */
static void print_usage(FILE *stream, const char *program_name)
{
    fprintf(stream, "Usage: %s [--check=list] [--data=list]", program_name);
#if TO_SELF_HAVE_OMPI_INTERNALS
    fprintf(stream, " [--dump]");
#endif
    fprintf(stream,
            " [--cycles=N] [--trials=N] [--warmups=N] [--min-work-bytes=N] [--raw-timers]\n");
    fprintf(stream,
            "Defaults: --check=all --data=all --cycles=%d --trials=%d --warmups=%d "
            "--min-work-bytes=%d\n",
            cycles, trials, warmups, min_work_bytes);
#if TO_SELF_HAVE_OMPI_INTERNALS
    fprintf(stream, "--dump prints committed datatype internals\n");
#endif
    fprintf(stream, "--raw-timers prints every trial after timing and whether the summary retained it\n");
    print_options(stream, "--check", check_options, sizeof(check_options) / sizeof(check_options[0]),
                  0);
    print_options(stream, "--data", data_options, sizeof(data_options) / sizeof(data_options[0]),
                  1);
}

/* Accept whitespace around comma-delimited --check and --data entries. */
static char *trim_whitespace(char *string)
{
    char *end;

    while (isspace((unsigned char) *string)) {
        ++string;
    }

    if ('\0' == *string) {
        return string;
    }

    end = string + strlen(string) - 1;
    while ((end > string) && isspace((unsigned char) *end)) {
        *end = '\0';
        --end;
    }

    return string;
}

/* Match options case-insensitively and accept '-' as an alias for '_'. */
static void normalize_check_name(char *name)
{
    for (char *p = name; '\0' != *p; ++p) {
        if ('-' == *p) {
            *p = '_';
        } else {
            *p = (char) tolower((unsigned char) *p);
        }
    }
}

/* Parse bounded integer controls such as --cycles, --trials, and --warmups. */
static int parse_integer_option(const char *option_name, const char *option_value, int min_value,
                                int *value)
{
    char *end;
    long parsed_value;

    if ((NULL == option_value) || ('\0' == option_value[0])) {
        fprintf(stderr, "%s requires an integer value\n", option_name);
        return -1;
    }

    errno = 0;
    parsed_value = strtol(option_value, &end, 10);
    if ((0 != errno) || ('\0' != *end) || (parsed_value < min_value)
        || (parsed_value > INT_MAX)) {
        fprintf(stderr, "%s must be an integer >= %d\n", option_name, min_value);
        return -1;
    }

    *value = (int) parsed_value;
    return 0;
}

/* Resolve one list token to a bit flag, with numeric indexes for --data only. */
static int option_name_to_flag(const char *name, const check_option_t *options, size_t option_count,
                               test_mask_t all_flags, int allow_indexes, test_mask_t *flag)
{
    if (0 == strncmp(name, "do_", 3)) {
        name += 3;
    }

    if (0 == strcmp(name, "all")) {
        *flag = all_flags;
        return 0;
    }

    if (allow_indexes) {
        char *end;
        long index;

        errno = 0;
        index = strtol(name, &end, 10);
        if ((0 == errno) && ('\0' == *end) && (0 <= index) && ((size_t) index < option_count)) {
            *flag = options[(size_t) index].flag;
            return 0;
        }
    }

    for (size_t i = 0; i < option_count; ++i) {
        if (0 == strcmp(name, options[i].name)) {
            *flag = options[i].flag;
            return 0;
        }
    }

    return -1;
}

/* Parse a comma-delimited option list into the corresponding DO_* bit mask. */
static int parse_option_list(const char *option_name, const char *option_list,
                             const check_option_t *options, size_t option_count,
                             test_mask_t all_flags, int allow_indexes,
                             test_mask_t *selected_tests)
{
    char *copy, *token;
    size_t length;
    test_mask_t parsed_tests = 0;

    if ((NULL == option_list) || ('\0' == option_list[0])) {
        fprintf(stderr, "%s requires a comma-delimited list of tests\n", option_name);
        print_options(stderr, option_name, options, option_count, allow_indexes);
        return -1;
    }

    length = strlen(option_list);
    copy = (char *) malloc(length + 1);
    if (NULL == copy) {
        fprintf(stderr, "Unable to allocate memory while parsing %s\n", option_name);
        return -1;
    }
    memcpy(copy, option_list, length + 1);

    token = strtok(copy, ",");
    while (NULL != token) {
        test_mask_t flag;
        char *name = trim_whitespace(token);

        normalize_check_name(name);
        if ((0 != option_name_to_flag(name, options, option_count, all_flags, allow_indexes, &flag))
            || ('\0' == name[0])) {
            fprintf(stderr, "Unknown %s test: %s\n", option_name, name);
            print_options(stderr, option_name, options, option_count, allow_indexes);
            free(copy);
            return -1;
        }

        parsed_tests |= flag;
        token = strtok(NULL, ",");
    }

    free(copy);

    if (0 == parsed_tests) {
        fprintf(stderr, "%s did not select any tests\n", option_name);
        print_options(stderr, option_name, options, option_count, allow_indexes);
        return -1;
    }

    *selected_tests = parsed_tests;
    return 0;
}

/*
 * Split the benchmark selection into two orthogonal namespaces:
 * --data controls the datatype constructors and --check controls the actions.
 * Both default to all when omitted.
 */
static int parse_args(int argc, char *argv[], test_mask_t *run_tests)
{
    test_mask_t check_tests = DO_OPERATION_TESTS;
    test_mask_t data_tests = DO_DATATYPE_TESTS;

    for (int i = 1; i < argc; ++i) {
        if ((0 == strcmp(argv[i], "--help")) || (0 == strcmp(argv[i], "-h"))) {
            print_usage(stdout, argv[0]);
            return 1;
        } else if (0 == strcmp(argv[i], "--dump")) {
#if TO_SELF_HAVE_OMPI_INTERNALS
            dump_datatypes = 1;
#else
            fprintf(stderr, "--dump requires Open MPI datatype internals\n");
            return -1;
#endif
        } else if (0 == strncmp(argv[i], "--check=", strlen("--check="))) {
            if (0 != parse_option_list("--check", argv[i] + strlen("--check="), check_options,
                                       sizeof(check_options) / sizeof(check_options[0]),
                                       DO_OPERATION_TESTS, 0, &check_tests)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--check")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--check requires a comma-delimited list of tests\n");
                print_options(stderr, "--check", check_options,
                              sizeof(check_options) / sizeof(check_options[0]), 0);
                return -1;
            }
            ++i;
            if (0 != parse_option_list("--check", argv[i], check_options,
                                       sizeof(check_options) / sizeof(check_options[0]),
                                       DO_OPERATION_TESTS, 0, &check_tests)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--data=", strlen("--data="))) {
            if (0 != parse_option_list("--data", argv[i] + strlen("--data="), data_options,
                                       sizeof(data_options) / sizeof(data_options[0]),
                                       DO_DATATYPE_TESTS, 1, &data_tests)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--data")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--data requires a comma-delimited list of tests\n");
                print_options(stderr, "--data", data_options,
                              sizeof(data_options) / sizeof(data_options[0]), 1);
                return -1;
            }
            ++i;
            if (0 != parse_option_list("--data", argv[i], data_options,
                                       sizeof(data_options) / sizeof(data_options[0]),
                                       DO_DATATYPE_TESTS, 1, &data_tests)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--cycles=", strlen("--cycles="))) {
            if (0 != parse_integer_option("--cycles", argv[i] + strlen("--cycles="), 1, &cycles)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--cycles")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--cycles requires an integer value\n");
                return -1;
            }
            ++i;
            if (0 != parse_integer_option("--cycles", argv[i], 1, &cycles)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--trials=", strlen("--trials="))) {
            if (0 != parse_integer_option("--trials", argv[i] + strlen("--trials="), 2, &trials)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--trials")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--trials requires an integer value\n");
                return -1;
            }
            ++i;
            if (0 != parse_integer_option("--trials", argv[i], 2, &trials)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--warmups=", strlen("--warmups="))) {
            if (0 != parse_integer_option("--warmups", argv[i] + strlen("--warmups="), 0,
                                          &warmups)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--warmups")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--warmups requires an integer value\n");
                return -1;
            }
            ++i;
            if (0 != parse_integer_option("--warmups", argv[i], 0, &warmups)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--min-work-bytes=", strlen("--min-work-bytes="))) {
            if (0 != parse_integer_option("--min-work-bytes",
                                          argv[i] + strlen("--min-work-bytes="), 0,
                                          &min_work_bytes)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--min-work-bytes")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--min-work-bytes requires an integer value\n");
                return -1;
            }
            ++i;
            if (0 != parse_integer_option("--min-work-bytes", argv[i], 0, &min_work_bytes)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--raw-timers")) {
            raw_timers = 1;
        } else {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            print_usage(stderr, argv[0]);
            return -1;
        }
    }

    *run_tests = data_tests | check_tests;
    return 0;
}

/* Preallocate raw timer storage so instrumentation performs no I/O or allocation between sizes. */
static int prepare_raw_timer_records(test_mask_t run_tests)
{
    size_t datatype_count = 0, operation_count = 0, result_count;

    if (!raw_timers) {
        return 0;
    }
    for (size_t i = 0; i < sizeof(data_options) / sizeof(data_options[0]); ++i) {
        datatype_count += !!(run_tests & data_options[i].flag);
    }
    for (size_t i = 0; i < sizeof(check_options) / sizeof(check_options[0]); ++i) {
        operation_count += !!(run_tests & check_options[i].flag);
    }

    result_count = datatype_count * operation_count * MAX_TIMER_SIZE_POINTS;
    if ((0 == result_count) || ((size_t) trials > SIZE_MAX / result_count)
        || ((size_t) trials * result_count > SIZE_MAX / sizeof(*raw_timer_records))) {
        return -1;
    }
    raw_timer_capacity = (size_t) trials * result_count;
    raw_timer_records = (raw_timer_record_t *) malloc(raw_timer_capacity * sizeof(*raw_timer_records));
    return (NULL == raw_timer_records) ? -1 : 0;
}

/* Emit buffered trials only after every selected benchmark has completed. */
static void print_raw_timer_records(void)
{
    for (size_t i = 0; i < raw_timer_count; ++i) {
        const raw_timer_record_t *record = &raw_timer_records[i];

        printf("# raw-timer\t%s\t%d\t%d\t%.17g\t%d\n", record->operation, record->length,
               record->trial, record->seconds, record->retained);
    }
}

/*
 * Sort timing samples, remove Tukey-IQR outliers, and report bandwidth with
 * min/max/stddev over the retained samples.
 */
static void print_result(const char *operation, int length, int num_trials, const double *timers)
{
    double bandwidth, clock_prec, temp, q1, q3, iqr, lower_bound, upper_bound;
    double min_time, max_time, average, std_dev = 0.0;
    double ordered[num_trials];
    int order[num_trials], retained[num_trials];
    int t, pos, quartile_start, quartile_end, good_start = -1, good_end = -1;
    int good_count = 0, filtered_count = 0, fallback = 0;

    for (t = 0; t < num_trials; t++) {
        int trial = t;

        temp = timers[t];
        pos = t;
        while ((pos > 0) && (ordered[pos - 1] > temp)) {
            ordered[pos] = ordered[pos - 1];
            order[pos] = order[pos - 1];
            --pos;
        }
        ordered[pos] = temp;
        order[pos] = trial;
    }

    /*
     * Use Tukey's IQR fence to triage timing outliers before computing
     * summary statistics. If too few samples remain, fall back to the
     * previous middle-half method and ask for more trials.
     */
    q1 = ordered[num_trials / 4];
    q3 = ordered[(3 * num_trials) / 4];
    iqr = q3 - q1;
    lower_bound = q1 - 1.5 * iqr;
    upper_bound = q3 + 1.5 * iqr;

    for (t = 0; t < num_trials; t++) {
        if ((ordered[t] >= lower_bound) && (ordered[t] <= upper_bound)) {
            if (-1 == good_start) {
                good_start = t;
            }
            good_end = t + 1;
            ++filtered_count;
        }
    }

    if (filtered_count < MIN_GOOD_TIMERS) {
        fallback = 1;
        quartile_start = num_trials - (3 * num_trials) / 4;
        quartile_end = num_trials - (1 * num_trials) / 4;
        good_start = quartile_start;
        good_end = quartile_end;
    }

    good_count = good_end - good_start;
    memset(retained, 0, sizeof(retained));
    for (t = good_start; t < good_end; ++t) {
        retained[order[t]] = 1;
    }

    /* Preserve raw trials without adding I/O between this result and the next measured size. */
    if (raw_timers) {
        for (t = 0; t < num_trials; ++t) {
            raw_timer_record_t *record;

            if (raw_timer_count == raw_timer_capacity) {
                fprintf(stderr, "Raw timer buffer capacity exceeded\n");
                abort();
            }
            record = &raw_timer_records[raw_timer_count++];
            record->operation = operation;
            record->seconds = timers[t];
            record->length = length;
            record->trial = t;
            record->retained = retained[t];
        }
    }

    clock_prec = MPI_Wtick();
    min_time = ordered[good_start];
    max_time = ordered[good_end - 1];
    average = 0.0;
    for (t = good_start; t < good_end; t++) {
        average += ordered[t];
    }
    average /= good_count;
    for (t = good_start; t < good_end; t++) {
        std_dev += (ordered[t] - average) * (ordered[t] - average);
    }
    std_dev = sqrt(std_dev / good_count);

    if (fallback) {
        printf("# Not enough good timing data after outlier filtering (%d/%d retained); rerun "
               "with a larger --trials value\n",
               filtered_count, num_trials);
    }

    bandwidth = (length * clock_prec) / (1024.0 * 1024.0) / (average * clock_prec);
    printf("%8d\t%15g\t%10.3f MB/s [min %10g max %10g std %2.2f%%]\n", length, average, bandwidth,
           min_time, max_time, (100.0 * std_dev) / average);
}

static int pack(int num_cycles, MPI_Datatype sdt, int scount, void *sbuf, void *packed_buf)
{
    int position, myself, c, t, outsize;
    double timers[trials];

    MPI_Type_size(sdt, &outsize);
    outsize *= scount;

    MPI_Comm_rank(MPI_COMM_WORLD, &myself);

    for (t = 0; t < warmups; t++) {
        for (c = 0; c < num_cycles; c++) {
            position = 0;
            MPI_Pack(sbuf, scount, sdt, packed_buf, outsize, &position, MPI_COMM_WORLD);
        }
    }

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            position = 0;
            MPI_Pack(sbuf, scount, sdt, packed_buf, outsize, &position, MPI_COMM_WORLD);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("pack", outsize, trials, timers);
    return 0;
}

static int unpack(int num_cycles, void *packed_buf, MPI_Datatype rdt, int rcount, void *rbuf)
{
    int position, myself, c, t, insize;
    double timers[trials];

    MPI_Type_size(rdt, &insize);
    insize *= rcount;

    MPI_Comm_rank(MPI_COMM_WORLD, &myself);

    for (t = 0; t < warmups; t++) {
        for (c = 0; c < num_cycles; c++) {
            position = 0;
            MPI_Unpack(packed_buf, insize, &position, rbuf, rcount, rdt, MPI_COMM_WORLD);
        }
    }

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            position = 0;
            MPI_Unpack(packed_buf, insize, &position, rbuf, rcount, rdt, MPI_COMM_WORLD);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("unpack", insize, trials, timers);
    return 0;
}

static int run_pack_byhand(int num_cycles, byhand_copy_fn_t pack_byhand_fn, MPI_Datatype sdt,
                           int scount, void *sbuf, void *packed_buf)
{
    int c, t, outsize;
    double timers[trials];

    if (NULL == pack_byhand_fn) {
        return 0;
    }

    MPI_Type_size(sdt, &outsize);
    outsize *= scount;

    for (t = 0; t < warmups; t++) {
        for (c = 0; c < num_cycles; c++) {
            pack_byhand_fn(packed_buf, sbuf, scount);
        }
    }

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            pack_byhand_fn(packed_buf, sbuf, scount);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("pack_byhand", outsize, trials, timers);
    return 0;
}

static int run_unpack_byhand(int num_cycles, byhand_copy_fn_t unpack_byhand_fn, MPI_Datatype rdt,
                             int rcount, void *packed_buf, void *rbuf)
{
    int c, t, insize;
    double timers[trials];

    if (NULL == unpack_byhand_fn) {
        return 0;
    }

    MPI_Type_size(rdt, &insize);
    insize *= rcount;

    for (t = 0; t < warmups; t++) {
        for (c = 0; c < num_cycles; c++) {
            unpack_byhand_fn(rbuf, packed_buf, rcount);
        }
    }

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            unpack_byhand_fn(rbuf, packed_buf, rcount);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("unpack_byhand", insize, trials, timers);
    return 0;
}

static int isend_recv(int num_cycles, MPI_Datatype sdt, int scount, void *sbuf, MPI_Datatype rdt,
                      int rcount, void *rbuf)
{
    int myself, tag = 0, c, t, slength, rlength;
    MPI_Status status;
    MPI_Request req;
    double timers[trials];

    MPI_Type_size(sdt, &slength);
    slength *= scount;
    MPI_Type_size(rdt, &rlength);
    rlength *= rcount;

    MPI_Comm_rank(MPI_COMM_WORLD, &myself);

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            MPI_Isend(sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD, &req);
            MPI_Recv(rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &status);
            MPI_Wait(&req, &status);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("isend_recv", rlength, trials, timers);
    return 0;
}

static int irecv_send(int num_cycles, MPI_Datatype sdt, int scount, void *sbuf, MPI_Datatype rdt,
                      int rcount, void *rbuf)
{
    int myself, tag = 0, c, t, slength, rlength;
    MPI_Request req;
    MPI_Status status;
    double timers[trials];

    MPI_Type_size(sdt, &slength);
    slength *= scount;
    MPI_Type_size(rdt, &rlength);
    rlength *= rcount;

    MPI_Comm_rank(MPI_COMM_WORLD, &myself);

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            MPI_Irecv(rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &req);
            MPI_Send(sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD);
            MPI_Wait(&req, &status);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("irecv_send", rlength, trials, timers);
    return 0;
}

static int isend_irecv_wait(int num_cycles, MPI_Datatype sdt, int scount, void *sbuf, MPI_Datatype rdt,
                            int rcount, void *rbuf)
{
    int myself, tag = 0, c, t, slength, rlength;
    MPI_Request requests[2];
    MPI_Status statuses[2];
    double timers[trials];

    MPI_Type_size(sdt, &slength);
    slength *= scount;
    MPI_Type_size(rdt, &rlength);
    rlength *= rcount;

    MPI_Comm_rank(MPI_COMM_WORLD, &myself);

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            MPI_Isend(sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD, &requests[0]);
            MPI_Irecv(rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &requests[1]);
            MPI_Waitall(2, requests, statuses);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("isend_irecv", rlength, trials, timers);
    return 0;
}

static int irecv_isend_wait(int num_cycles, MPI_Datatype sdt, int scount, void *sbuf, MPI_Datatype rdt,
                            int rcount, void *rbuf)
{
    int myself, tag = 0, c, t, slength, rlength;
    MPI_Request requests[2];
    MPI_Status statuses[2];
    double timers[trials];

    MPI_Type_size(sdt, &slength);
    slength *= scount;
    MPI_Type_size(rdt, &rlength);
    rlength *= rcount;

    MPI_Comm_rank(MPI_COMM_WORLD, &myself);

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            MPI_Irecv(rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &requests[0]);
            MPI_Isend(sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD, &requests[1]);
            MPI_Waitall(2, requests, statuses);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("irecv_isend", rlength, trials, timers);
    return 0;
}

static size_t datatype_count_span(MPI_Datatype datatype, int count)
{
    MPI_Aint lb, extent, true_lb, true_extent, upper_bound;

    MPI_Type_get_extent(datatype, &lb, &extent);
    MPI_Type_get_true_extent(datatype, &true_lb, &true_extent);
    if (0 > true_lb) {
        fprintf(stderr, "Negative true lower bounds are not supported by this tester\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_INTERN);
    }

    upper_bound = true_lb + true_extent;
    if (1 < count) {
        upper_bound += (MPI_Aint) (count - 1) * extent;
    }
    return (size_t) upper_bound;
}

/* Raise the cycle count for small messages without increasing large-message benchmark work. */
static int cycles_for_packed_size(size_t packed_size)
{
    size_t required_cycles;

    if ((0 == min_work_bytes) || (0 == packed_size)) {
        return cycles;
    }
    required_cycles = 1 + ((size_t) min_work_bytes - 1) / packed_size;
    if (required_cycles > INT_MAX) {
        return INT_MAX;
    }
    return ((size_t) cycles < required_cycles) ? (int) required_cycles : cycles;
}

static int do_test_for_ddt(test_mask_t doop, MPI_Datatype sddt, MPI_Datatype rddt, int length,
                           byhand_copy_fn_t pack_byhand_fn,
                           byhand_copy_fn_t unpack_byhand_fn)
{
    size_t sbuf_length, rbuf_length, packed_length, source_span, recv_span;
    char *sbuf, *rbuf;
    int i, max_count, stype_size, rtype_size;

    MPI_Type_size(sddt, &stype_size);
    MPI_Type_size(rddt, &rtype_size);
    if (stype_size != rtype_size) {
        fprintf(stderr, "Send and receive datatype sizes differ (%d != %d)\n", stype_size,
                rtype_size);
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_INTERN);
    }

    max_count = length / stype_size;
    if (1 > max_count) {
        max_count = 1;
    }

    packed_length = (size_t) stype_size * (size_t) max_count;
    source_span = datatype_count_span(sddt, max_count);
    recv_span = datatype_count_span(rddt, max_count);
    sbuf_length = (source_span > packed_length) ? source_span : packed_length;
    rbuf_length = (recv_span > packed_length) ? recv_span : packed_length;

    sbuf = (char *) malloc(sbuf_length);
    rbuf = (char *) malloc(rbuf_length);
    if ((NULL == sbuf) || (NULL == rbuf)) {
        fprintf(stderr, "Unable to allocate benchmark buffers\n");
        free(sbuf);
        free(rbuf);
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_NO_MEM);
    }
    if (doop & DO_PACK) {
        printf("# Pack (max length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            pack(cycles_for_packed_size((size_t) stype_size * (size_t) i), sddt, i, sbuf, rbuf);
        }
    }

    if ((doop & DO_PACK_BYHAND) && (NULL != pack_byhand_fn)) {
        printf("# Pack by hand (max length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            run_pack_byhand(cycles_for_packed_size((size_t) stype_size * (size_t) i),
                            pack_byhand_fn, sddt, i, sbuf, rbuf);
        }
    }

    if (doop & DO_UNPACK) {
        printf("# Unpack (length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            unpack(cycles_for_packed_size((size_t) rtype_size * (size_t) i), sbuf, rddt, i, rbuf);
        }
    }

    if ((doop & DO_UNPACK_BYHAND) && (NULL != unpack_byhand_fn)) {
        printf("# Unpack by hand (length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            run_unpack_byhand(cycles_for_packed_size((size_t) rtype_size * (size_t) i),
                              unpack_byhand_fn, rddt, i, sbuf, rbuf);
        }
    }

    if (doop & DO_ISEND_RECV) {
        printf("# Isend recv (length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            isend_recv(cycles_for_packed_size((size_t) stype_size * (size_t) i), sddt, i, sbuf,
                       rddt, i, rbuf);
        }
    }

    if (doop & DO_ISEND_IRECV) {
        printf("# Isend Irecv Wait (length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            isend_irecv_wait(cycles_for_packed_size((size_t) stype_size * (size_t) i), sddt, i,
                             sbuf, rddt, i, rbuf);
        }
    }

    if (doop & DO_IRECV_SEND) {
        printf("# Irecv send (length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            irecv_send(cycles_for_packed_size((size_t) stype_size * (size_t) i), sddt, i, sbuf,
                       rddt, i, rbuf);
        }
    }

    if (doop & DO_IRECV_ISEND) {
        printf("# Irecv Isend Wait (length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            irecv_isend_wait(cycles_for_packed_size((size_t) stype_size * (size_t) i), sddt, i,
                             sbuf, rddt, i, rbuf);
        }
    }
    free(sbuf);
    free(rbuf);
    return 0;
}

int main(int argc, char *argv[])
{
    test_mask_t run_tests = DO_DATATYPE_TESTS | DO_OPERATION_TESTS;
    int parse_result, rank, size;
    MPI_Datatype ddt, sddt, rddt;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (0 == rank) {
        parse_result = parse_args(argc, argv, &run_tests);
        if (0 > parse_result) {
            MPI_Abort(MPI_COMM_WORLD, 1);
        } else if (0 < parse_result) {
            MPI_Finalize();
            exit(0);
        }
    }

    if (rank != 0) {
        MPI_Finalize();
        exit(0);
    }

    if (0 != prepare_raw_timer_records(run_tests)) {
        fprintf(stderr, "Unable to allocate raw timer records\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (run_tests & DO_CONTIG) {
        printf("\ncontiguous datatype\n\n");
        do_test_for_ddt(run_tests, MPI_INT, MPI_INT, MAX_LENGTH, pack_byhand_contiguous_datatype,
                        unpack_byhand_contiguous_datatype);
    }

    if (run_tests & DO_INDEXED_GAP) {
        printf("\nindexed gap\n\n");
        ddt = create_indexed_gap_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH, pack_byhand_create_indexed_gap_ddt,
                        unpack_byhand_create_indexed_gap_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_OPTIMIZED_INDEXED_GAP) {
        printf("\noptimized indexed gap\n\n");
        ddt = create_indexed_gap_optimized_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_create_indexed_gap_optimized_ddt,
                        unpack_byhand_create_indexed_gap_optimized_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_CONSTANT_GAP) {
        printf("\nconstant indexed gap\n\n");
        ddt = create_indexed_constant_gap_ddt(80, 100, 1);
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_create_indexed_constant_gap_ddt,
                        unpack_byhand_create_indexed_constant_gap_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_OPTIMIZED_CONSTANT_GAP) {
        printf("\noptimized constant indexed gap\n\n");
        ddt = create_optimized_indexed_constant_gap_ddt(80, 100, 1);
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_create_optimized_indexed_constant_gap_ddt,
                        unpack_byhand_create_optimized_indexed_constant_gap_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_STRUCT_CONSTANT_GAP) {
        printf("\nstruct constant gap\n\n");
        ddt = create_struct_constant_gap_ddt(80, 100, 1);
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_create_struct_constant_gap_ddt,
                        unpack_byhand_create_struct_constant_gap_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_STRUCT_CONSTANT_GAP_RESIZED) {
        printf("\nstruct constant gap resized\n\n");
        ddt = create_struct_constant_gap_resized_ddt(0 /* unused */, 0 /* unused */,
                                                     0 /* unused */);
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_create_struct_constant_gap_resized_ddt,
                        unpack_byhand_create_struct_constant_gap_resized_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_STRUCT_MERGED_WITH_GAP_RESIZED) {
        printf("\nstruct merged with gap resized\n\n");
        ddt = create_merged_contig_with_gaps(1);
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_create_merged_contig_with_gaps,
                        unpack_byhand_create_merged_contig_with_gaps);
        MPI_Type_free(&ddt);
    }


    if (run_tests & DO_DDTBENCH_FFT2D_SCATTER) {
        PRINT_DDTBENCH_TEST("FFT2D scatter");
        ddt = create_ddtbench_fft2d_scatter_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_ddtbench_fft2d_scatter_ddt,
                        unpack_byhand_ddtbench_fft2d_scatter_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_FFT2D_GATHER) {
        PRINT_DDTBENCH_TEST("FFT2D gather");
        ddt = create_ddtbench_fft2d_gather_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_ddtbench_fft2d_gather_ddt,
                        unpack_byhand_ddtbench_fft2d_gather_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_MILC_SU3_ZDOWN) {
        PRINT_DDTBENCH_TEST("MILC su3 zdown");
        ddt = create_ddtbench_milc_su3_zdown_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_ddtbench_milc_su3_zdown_ddt,
                        unpack_byhand_ddtbench_milc_su3_zdown_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_NAS_LU_Y) {
        PRINT_DDTBENCH_TEST("NAS LU y");
        ddt = create_ddtbench_nas_lu_y_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH, pack_byhand_ddtbench_nas_lu_y_ddt,
                        unpack_byhand_ddtbench_nas_lu_y_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_NAS_LU_X) {
        PRINT_DDTBENCH_TEST("NAS LU x");
        ddt = create_ddtbench_nas_lu_x_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH, pack_byhand_ddtbench_nas_lu_x_ddt,
                        unpack_byhand_ddtbench_nas_lu_x_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_NAS_MG_X) {
        PRINT_DDTBENCH_TEST("NAS MG x");
        ddt = create_ddtbench_nas_mg_x_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH, pack_byhand_ddtbench_nas_mg_x_ddt,
                        unpack_byhand_ddtbench_nas_mg_x_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_NAS_MG_Y) {
        PRINT_DDTBENCH_TEST("NAS MG y");
        ddt = create_ddtbench_nas_mg_y_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH, pack_byhand_ddtbench_nas_mg_y_ddt,
                        unpack_byhand_ddtbench_nas_mg_y_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_NAS_MG_Z) {
        PRINT_DDTBENCH_TEST("NAS MG z");
        ddt = create_ddtbench_nas_mg_z_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH, pack_byhand_ddtbench_nas_mg_z_ddt,
                        unpack_byhand_ddtbench_nas_mg_z_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_LAMMPS_FULL) {
        PRINT_DDTBENCH_TEST("LAMMPS full");
        create_ddtbench_lammps_full_ddt(&sddt, &rddt);
        MPI_DDT_DUMP(sddt);
        MPI_DDT_DUMP(rddt);
        do_test_for_ddt(run_tests, sddt, rddt, MAX_LENGTH,
                        pack_byhand_ddtbench_lammps_full_ddt,
                        unpack_byhand_ddtbench_lammps_full_ddt);
        MPI_Type_free(&sddt);
        MPI_Type_free(&rddt);
    }

    if (run_tests & DO_DDTBENCH_LAMMPS_ATOMIC) {
        PRINT_DDTBENCH_TEST("LAMMPS atomic");
        create_ddtbench_lammps_atomic_ddt(&sddt, &rddt);
        MPI_DDT_DUMP(sddt);
        MPI_DDT_DUMP(rddt);
        do_test_for_ddt(run_tests, sddt, rddt, MAX_LENGTH,
                        pack_byhand_ddtbench_lammps_atomic_ddt,
                        unpack_byhand_ddtbench_lammps_atomic_ddt);
        MPI_Type_free(&sddt);
        MPI_Type_free(&rddt);
    }

    if (run_tests & DO_DDTBENCH_SPECFEM3D_OC) {
        PRINT_DDTBENCH_TEST("SPECFEM3D oc");
        ddt = create_ddtbench_specfem3d_oc_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_ddtbench_specfem3d_oc_ddt,
                        unpack_byhand_ddtbench_specfem3d_oc_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_SPECFEM3D_CM) {
        PRINT_DDTBENCH_TEST("SPECFEM3D cm");
        ddt = create_ddtbench_specfem3d_cm_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_ddtbench_specfem3d_cm_ddt,
                        unpack_byhand_ddtbench_specfem3d_cm_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_SPECFEM3D_MT) {
        PRINT_DDTBENCH_TEST("SPECFEM3D mt");
        create_ddtbench_specfem3d_mt_ddt(&sddt, &rddt);
        MPI_DDT_DUMP(sddt);
        MPI_DDT_DUMP(rddt);
        do_test_for_ddt(run_tests, sddt, rddt, MAX_LENGTH,
                        pack_byhand_ddtbench_specfem3d_mt_ddt,
                        unpack_byhand_ddtbench_specfem3d_mt_ddt);
        MPI_Type_free(&sddt);
        MPI_Type_free(&rddt);
    }

    if (run_tests & DO_DDTBENCH_WRF_VEC) {
        PRINT_DDTBENCH_TEST("WRF vec");
        ddt = create_ddtbench_wrf_vec_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH, pack_byhand_ddtbench_wrf_vec_ddt,
                        unpack_byhand_ddtbench_wrf_vec_ddt);
        MPI_Type_free(&ddt);
    }

    if (run_tests & DO_DDTBENCH_WRF_SUBARRAY) {
        PRINT_DDTBENCH_TEST("WRF subarray");
        ddt = create_ddtbench_wrf_subarray_ddt();
        MPI_DDT_DUMP(ddt);
        do_test_for_ddt(run_tests, ddt, ddt, MAX_LENGTH,
                        pack_byhand_ddtbench_wrf_subarray_ddt,
                        unpack_byhand_ddtbench_wrf_subarray_ddt);
        MPI_Type_free(&ddt);
    }

    print_raw_timer_records();
    free(raw_timer_records);
    MPI_Finalize();
    exit(0);
}
