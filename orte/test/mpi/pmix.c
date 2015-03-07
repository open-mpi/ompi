/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/*
 * To compile test:
 * mpicc -I$src_dir -I$src_dir/opal/include -I$src_dir/orte/include -I$src_dir/ompi/include -DOMPI_BUILDING=1 pmix.c -o pmix
 * To run test:
 * mpirun -np 2 <any mca params> ./pmix
 * Test should print "Passed" in case of success and print pmix time intervals at process with rank 0.
 * */
#include <stdio.h>
#include <unistd.h>
#include <mpi.h>
#include <sys/time.h>

#include "opal/mca/pmix/pmix.h"
#include "ompi/proc/proc.h"

#define DO_FINALIZE(rc,flag,format,args...) \
    do {                                    \
        if (flag) {                         \
            fprintf(stderr, format, args);  \
        }                                   \
        if (opal_pmix.initialized) {        \
            opal_pmix.finalize();           \
        }                                   \
        MPI_Finalize();                     \
        return rc;                          \
    } while(0);

inline double get_timestamp(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return ((tv.tv_sec) + (tv.tv_usec) * 1.0e-6);
}

int main(int argc, char* argv[])
{
    int rc, my_rank;
    int recv_data;
    size_t i, numprocs;
    ompi_proc_t **procs, *thisproc;
    double t0, t1, t2, t3, t4, t5, t6;
    int *ptr;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    if (NULL != opal_pmix.init) {
        rc = opal_pmix.init();
        if (OPAL_SUCCESS != rc) {
            DO_FINALIZE(rc, 1, "[%d] pmix_init failed.\n", my_rank);
        }
    }
    
    int data = my_rank;
    t0 = get_timestamp();
    OPAL_MODEX_SEND_VALUE(rc, PMIX_SYNC_REQD, PMIX_GLOBAL,
                           "MY_RANK", &data, OPAL_INT);
    t1 = get_timestamp();
    if (OPAL_SUCCESS != rc) {
        DO_FINALIZE(rc, 1, "[%d] OPAL_MODEX_SEND_STRING failed.\n", my_rank);
    }
    t2 = get_timestamp();
    OPAL_FENCE(NULL, 0, NULL, NULL);
    t3 = get_timestamp();
    procs = ompi_proc_world ( &numprocs );
    ptr = &recv_data;
    t4 = get_timestamp();
    for ( i = 0; i < numprocs; i++ ) {
        thisproc = procs[i];
        OPAL_MODEX_RECV_VALUE(rc, "MY_RANK", &thisproc->super, (void**)&ptr, OPAL_INT);
        /* check return status and received data */
        if (OPAL_SUCCESS != rc || i != recv_data) {
            rc = OPAL_ERROR;
            DO_FINALIZE(rc, 1, "[%d] OPAL_MODEX_RECV_VALUE failed from rank %d.\n", my_rank, i);
        }
    }
    t5 = get_timestamp();
  
    /* using fence as a barrier */
    opal_pmix.fence(NULL, 0);
    t6 = get_timestamp();

    free(procs);

    fprintf(stderr, "[%d] Test passed.\n", my_rank);
    fprintf(stderr, "[%d] \"MODEX_SEND\" %f\n", my_rank, t1-t0);
    fprintf(stderr, "[%d] \"FENCE\" %f\n", my_rank, t3-t2);
    fprintf(stderr, "[%d] \"MODEX_RECV\" %f\n", my_rank, t5-t4);
    fprintf(stderr, "[%d] \"BARRIER\" %f\n", my_rank, t6-t5);
    fprintf(stderr, "[%d] \"TOTAL\" %f\n", my_rank, t6-t0);

    DO_FINALIZE(0, 0, 0, 0);
}
