/*
 * Copyright (c) 2013-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2016 Inria.  All rights reserved.
 * Copyright (c) 2013-2015 Bull SAS.  All rights reserved.
 * Copyright (c) 2016      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
pml monitoring PMPI profiler

Designed by:
  George Bosilca <bosilca@icl.utk.edu>
  Emmanuel Jeannot <emmanuel.jeannot@inria.fr>
  Guillaume Papauré <guillaume.papaure@bull.net>
  Clément Foyer <clement.foyer@inria.fr>

Contact the authors for questions.

To be run as:

mpirun -np 4 \
    --mca pml_monitoring_enable 1 \
    -x LD_PRELOAD=ompi_install_dir/lib/ompi_monitoring_prof.so \
    ./my_app

...
...
...

writing 4x4 matrix to monitoring_msg.mat
writing 4x4 matrix to monitoring_size.mat
writing 4x4 matrix to monitoring_avg.mat

*/

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

static MPI_T_pvar_session session;
static int comm_world_size;
static int comm_world_rank;

struct monitoring_result
{
    char * pvar_name;
    int pvar_idx;
    MPI_T_pvar_handle pvar_handle;
    uint64_t * vector;
};
typedef struct monitoring_result monitoring_result;

/* PML Sent */
static monitoring_result pml_counts;
static monitoring_result pml_sizes;
/* OSC Sent */
static monitoring_result osc_scounts;
static monitoring_result osc_ssizes;
/* OSC Recv */
static monitoring_result osc_rcounts;
static monitoring_result osc_rsizes;
/* COLL Sent/Recv */
static monitoring_result coll_counts;
static monitoring_result coll_sizes;

static int  write_mat(char *, uint64_t *, unsigned int);
static void init_monitoring_result(const char *, monitoring_result *);
static void start_monitoring_result(monitoring_result *);
static void stop_monitoring_result(monitoring_result *);
static void get_monitoring_result(monitoring_result *);
static void destroy_monitoring_result(monitoring_result *);

int MPI_Init(int* argc, char*** argv)
{
    int result, MPIT_result;
    int provided;

    result = PMPI_Init(argc, argv);

    PMPI_Comm_size(MPI_COMM_WORLD, &comm_world_size);
    PMPI_Comm_rank(MPI_COMM_WORLD, &comm_world_rank);

    MPIT_result = MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);
    if (MPIT_result != MPI_SUCCESS) {
        fprintf(stderr, "ERROR : failed to intialize MPI_T interface, preventing to get monitoring results: check your OpenMPI installation\n");
        PMPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_session_create(&session);
    if (MPIT_result != MPI_SUCCESS) {
        fprintf(stderr, "ERROR : failed to create MPI_T session, preventing to get monitoring results: check your OpenMPI installation\n");
        PMPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    init_monitoring_result("pml_monitoring_messages_count", &pml_counts);
    init_monitoring_result("pml_monitoring_messages_size", &pml_sizes);
    init_monitoring_result("osc_monitoring_messages_sent_count", &osc_scounts);
    init_monitoring_result("osc_monitoring_messages_sent_size", &osc_ssizes);
    init_monitoring_result("osc_monitoring_messages_recv_count", &osc_rcounts);
    init_monitoring_result("osc_monitoring_messages_recv_size", &osc_rsizes);
    init_monitoring_result("coll_monitoring_messages_count", &coll_counts);
    init_monitoring_result("coll_monitoring_messages_size", &coll_sizes);
    
    start_monitoring_result(&pml_counts);
    start_monitoring_result(&pml_sizes);
    start_monitoring_result(&osc_scounts);
    start_monitoring_result(&osc_ssizes);
    start_monitoring_result(&osc_rcounts);
    start_monitoring_result(&osc_rsizes);
    start_monitoring_result(&coll_counts);
    start_monitoring_result(&coll_sizes);

    return result;
}

int MPI_Finalize(void)
{
    int result, MPIT_result;
    uint64_t * exchange_count_matrix    = NULL;
    uint64_t * exchange_size_matrix     = NULL;
    uint64_t * exchange_scount_matrix   = NULL;
    uint64_t * exchange_ssize_matrix    = NULL;
    uint64_t * exchange_rcount_matrix   = NULL;
    uint64_t * exchange_rsize_matrix    = NULL;
    uint64_t * exchange_ccount_matrix   = NULL;
    uint64_t * exchange_csize_matrix    = NULL;
    uint64_t * exchange_avg_size_matrix = NULL;

    if (0 == comm_world_rank) {
        exchange_count_matrix    = (uint64_t *) calloc(comm_world_size * comm_world_size, sizeof(uint64_t));
        exchange_size_matrix     = (uint64_t *) calloc(comm_world_size * comm_world_size, sizeof(uint64_t));
        exchange_scount_matrix   = (uint64_t *) calloc(comm_world_size * comm_world_size, sizeof(uint64_t));
        exchange_ssize_matrix    = (uint64_t *) calloc(comm_world_size * comm_world_size, sizeof(uint64_t));
        exchange_rcount_matrix   = (uint64_t *) calloc(comm_world_size * comm_world_size, sizeof(uint64_t));
        exchange_rsize_matrix    = (uint64_t *) calloc(comm_world_size * comm_world_size, sizeof(uint64_t));
        exchange_ccount_matrix   = (uint64_t *) calloc(comm_world_size * comm_world_size, sizeof(uint64_t));
        exchange_csize_matrix    = (uint64_t *) calloc(comm_world_size * comm_world_size, sizeof(uint64_t));
        exchange_avg_size_matrix = (uint64_t *) calloc(comm_world_size * comm_world_size, sizeof(uint64_t));
    }

    stop_monitoring_result(&pml_counts);
    stop_monitoring_result(&pml_sizes);
    stop_monitoring_result(&osc_scounts);
    stop_monitoring_result(&osc_ssizes);
    stop_monitoring_result(&osc_rcounts);
    stop_monitoring_result(&osc_rsizes);
    stop_monitoring_result(&coll_counts);
    stop_monitoring_result(&coll_sizes);

    get_monitoring_result(&pml_counts);
    get_monitoring_result(&pml_sizes);
    get_monitoring_result(&osc_scounts);
    get_monitoring_result(&osc_ssizes);
    get_monitoring_result(&osc_rcounts);
    get_monitoring_result(&osc_rsizes);
    get_monitoring_result(&coll_counts);
    get_monitoring_result(&coll_sizes);

    PMPI_Gather(pml_counts.vector,  comm_world_size, MPI_UNSIGNED_LONG, exchange_count_matrix,  comm_world_size, MPI_UNSIGNED_LONG, 0, MPI_COMM_WORLD);
    PMPI_Gather(pml_sizes.vector,   comm_world_size, MPI_UNSIGNED_LONG, exchange_size_matrix,   comm_world_size, MPI_UNSIGNED_LONG, 0, MPI_COMM_WORLD);
    PMPI_Gather(osc_scounts.vector, comm_world_size, MPI_UNSIGNED_LONG, exchange_scount_matrix, comm_world_size, MPI_UNSIGNED_LONG, 0, MPI_COMM_WORLD);
    PMPI_Gather(osc_ssizes.vector,  comm_world_size, MPI_UNSIGNED_LONG, exchange_ssize_matrix,  comm_world_size, MPI_UNSIGNED_LONG, 0, MPI_COMM_WORLD);
    PMPI_Gather(osc_rcounts.vector, comm_world_size, MPI_UNSIGNED_LONG, exchange_rcount_matrix, comm_world_size, MPI_UNSIGNED_LONG, 0, MPI_COMM_WORLD);
    PMPI_Gather(osc_rsizes.vector,  comm_world_size, MPI_UNSIGNED_LONG, exchange_rsize_matrix,  comm_world_size, MPI_UNSIGNED_LONG, 0, MPI_COMM_WORLD);
    PMPI_Gather(coll_counts.vector, comm_world_size, MPI_UNSIGNED_LONG, exchange_ccount_matrix, comm_world_size, MPI_UNSIGNED_LONG, 0, MPI_COMM_WORLD);
    PMPI_Gather(coll_sizes.vector,  comm_world_size, MPI_UNSIGNED_LONG, exchange_csize_matrix,  comm_world_size, MPI_UNSIGNED_LONG, 0, MPI_COMM_WORLD);

    if (0 == comm_world_rank) {
        int i, j;

        //Get the same matrix than profile2mat.pl
        for (i = 0; i < comm_world_size; ++i) {
            for (j = i + 1; j < comm_world_size; ++j) {
                exchange_count_matrix[i * comm_world_size + j] += exchange_rcount_matrix[j * comm_world_size + i] + exchange_scount_matrix[i * comm_world_size + j] + exchange_ccount_matrix[i * comm_world_size + j];
                exchange_count_matrix[j * comm_world_size + i] += exchange_rcount_matrix[i * comm_world_size + j] + exchange_scount_matrix[j * comm_world_size + i] + exchange_ccount_matrix[j * comm_world_size + i];
                exchange_count_matrix[i * comm_world_size + j] = exchange_count_matrix[j * comm_world_size + i] = (exchange_count_matrix[i * comm_world_size + j] + exchange_count_matrix[j * comm_world_size + i]) / 2;
                exchange_size_matrix[i * comm_world_size + j] += exchange_rsize_matrix[j * comm_world_size + i] + exchange_ssize_matrix[i * comm_world_size + j] + exchange_csize_matrix[i * comm_world_size + j];
                exchange_size_matrix[j * comm_world_size + i] += exchange_rsize_matrix[i * comm_world_size + j] + exchange_ssize_matrix[j * comm_world_size + i] + exchange_csize_matrix[j * comm_world_size + i];
                exchange_size_matrix[i * comm_world_size + j] = exchange_size_matrix[j * comm_world_size + i] = (exchange_size_matrix[i * comm_world_size + j] + exchange_size_matrix[j * comm_world_size + i]) / 2;
                if (exchange_count_matrix[i * comm_world_size + j] != 0)
                    exchange_avg_size_matrix[i * comm_world_size + j] = exchange_avg_size_matrix[j * comm_world_size + i] = exchange_size_matrix[i * comm_world_size + j] / exchange_count_matrix[i * comm_world_size + j];
            }
        }

        write_mat("monitoring_msg.mat", exchange_count_matrix, comm_world_size);
        write_mat("monitoring_size.mat", exchange_size_matrix, comm_world_size);
        write_mat("monitoring_avg.mat", exchange_avg_size_matrix, comm_world_size);
    }

    free(exchange_count_matrix);
    free(exchange_size_matrix);
    free(exchange_scount_matrix);
    free(exchange_ssize_matrix);
    free(exchange_rcount_matrix);
    free(exchange_rsize_matrix);
    free(exchange_ccount_matrix);
    free(exchange_csize_matrix);
    free(exchange_avg_size_matrix);
    
    destroy_monitoring_result(&pml_counts);
    destroy_monitoring_result(&pml_sizes);
    destroy_monitoring_result(&osc_scounts);
    destroy_monitoring_result(&osc_ssizes);
    destroy_monitoring_result(&osc_rcounts);
    destroy_monitoring_result(&osc_rsizes);
    destroy_monitoring_result(&coll_counts);
    destroy_monitoring_result(&coll_sizes);

    MPIT_result = MPI_T_pvar_session_free(&session);
    if (MPIT_result != MPI_SUCCESS) {
        fprintf(stderr, "WARNING : failed to free MPI_T session, monitoring results may be impacted : check your OpenMPI installation\n");
    }

    MPIT_result = MPI_T_finalize();
    if (MPIT_result != MPI_SUCCESS) {
        fprintf(stderr, "WARNING : failed to finalize MPI_T interface, monitoring results may be impacted : check your OpenMPI installation\n");
    }

    result = PMPI_Finalize();

    return result;
}

void init_monitoring_result(const char * pvar_name, monitoring_result * res)
{
    int count;
    int MPIT_result;
    MPI_Comm comm_world = MPI_COMM_WORLD;

    res->pvar_name = strdup(pvar_name);

    MPIT_result = MPI_T_pvar_get_index(res->pvar_name, MPI_T_PVAR_CLASS_SIZE, &(res->pvar_idx));
    if (MPIT_result != MPI_SUCCESS) {
        fprintf(stderr, "ERROR : cannot find monitoring MPI_T \"%s\" pvar, check that you have monitoring pml\n", pvar_name);
        PMPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_handle_alloc(session, res->pvar_idx, comm_world, &(res->pvar_handle), &count);
    if (MPIT_result != MPI_SUCCESS) {
        fprintf(stderr, "ERROR : failed to allocate handle on \"%s\" pvar, check that you have monitoring pml\n", pvar_name);
        PMPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    if (count != comm_world_size) {
        fprintf(stderr, "ERROR : COMM_WORLD has %d ranks \"%s\" pvar contains %d values, check that you have monitoring pml\n", comm_world_size, pvar_name, count);
        PMPI_Abort(MPI_COMM_WORLD, count);
    }

    res->vector = (uint64_t *) malloc(comm_world_size * sizeof(uint64_t));
}

void start_monitoring_result(monitoring_result * res)
{
    int MPIT_result;

    MPIT_result = MPI_T_pvar_start(session, res->pvar_handle);
    if (MPIT_result != MPI_SUCCESS) {
        fprintf(stderr, "ERROR : failed to start handle on \"%s\" pvar, check that you have enabled the monitoring pml\n", res->pvar_name);
        PMPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
}

void stop_monitoring_result(monitoring_result * res)
{
    int MPIT_result;

    MPIT_result = MPI_T_pvar_stop(session, res->pvar_handle);
    if (MPIT_result != MPI_SUCCESS) {
        fprintf(stderr, "ERROR : failed to stop handle on \"%s\" pvar, check that you have enabled the monitoring pml\n", res->pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
}

void get_monitoring_result(monitoring_result * res)
{
    int MPIT_result;

    MPIT_result = MPI_T_pvar_read(session, res->pvar_handle, res->vector);
    if (MPIT_result != MPI_SUCCESS) {
        fprintf(stderr, "ERROR : failed to read \"%s\" pvar, check that you have enabled the monitoring pml\n", res->pvar_name);
        PMPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
}

void destroy_monitoring_result(monitoring_result * res)
{
    int MPIT_result;

    MPIT_result = MPI_T_pvar_handle_free(session, &(res->pvar_handle));
    if (MPIT_result != MPI_SUCCESS) {
        printf("ERROR : failed to free handle on \"%s\" pvar, check that you have enabled the monitoring pml\n", res->pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    free(res->pvar_name);
    free(res->vector);
}

int write_mat(char * filename, uint64_t * mat, unsigned int dim)
{
    FILE *matrix_file;
    int i, j;

    matrix_file = fopen(filename, "w");
    if (!matrix_file) {
        fprintf(stderr, "ERROR : failed to open \"%s\" file in write mode, check your permissions\n", filename);
        return -1;
    }

    printf("writing %ux%u matrix to %s\n", dim, dim, filename);

    for (i = 0; i < comm_world_size; ++i) {
        for (j = 0; j < comm_world_size; ++j) {
            fprintf(matrix_file, "%" PRIu64 " ", mat[i * comm_world_size + j]);
        }
        fprintf(matrix_file, "\n");
    }
    fflush(matrix_file);
    fclose(matrix_file);

    return 0;
}

/**
 * MPI binding for fortran
 */

#include "ompi_config.h"
#define OMPI_F77_PROTOTYPES_MPI_H
#include "ompi/mpi/fortran/mpif-h/bindings.h"
#undef OMPI_F77_PROTOTYPES_MPI_H
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"

void monitoring_prof_mpi_init_f2c( MPI_Fint *ierr ) { 
    int c_ierr;
    int argc = 0;
    char ** argv = NULL; 

    c_ierr = MPI_Init(&argc, &argv); 
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr); 
}

void monitoring_prof_mpi_finalize_f2c( MPI_Fint *ierr ) { 
    int c_ierr;

    c_ierr = MPI_Finalize(); 
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr); 
}

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INIT = monitoring_prof_mpi_init_f2c
#pragma weak mpi_init = monitoring_prof_mpi_init_f2c
#pragma weak mpi_init_ = monitoring_prof_mpi_init_f2c
#pragma weak mpi_init__ = monitoring_prof_mpi_init_f2c
#pragma weak MPI_Init_f = monitoring_prof_mpi_init_f2c
#pragma weak MPI_Init_f08 = monitoring_prof_mpi_init_f2c

#pragma weak MPI_FINALIZE = monitoring_prof_mpi_finalize_f2c
#pragma weak mpi_finalize = monitoring_prof_mpi_finalize_f2c
#pragma weak mpi_finalize_ = monitoring_prof_mpi_finalize_f2c
#pragma weak mpi_finalize__ = monitoring_prof_mpi_finalize_f2c
#pragma weak MPI_Finalize_f = monitoring_prof_mpi_finalize_f2c
#pragma weak MPI_Finalize_f08 = monitoring_prof_mpi_finalize_f2c
#else
OMPI_GENERATE_F77_BINDINGS (MPI_INIT,
                           mpi_init,
                           mpi_init_,
                           mpi_init__,
                           monitoring_prof_mpi_init_f2c,
                           (MPI_Fint *ierr),
                           (ierr) )

OMPI_GENERATE_F77_BINDINGS (MPI_FINALIZE,
                           mpi_finalize,
                           mpi_finalize_,
                           mpi_finalize__,
                           monitoring_prof_mpi_finalize_f2c,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif
