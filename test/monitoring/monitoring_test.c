/*
 * Copyright (c) 2013-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Inria.  All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
pml monitoring tester.

Designed by George Bosilca <bosilca@icl.utk.edu> and  Emmanuel Jeannot <emmanuel.jeannot@inria.fr>
Contact the authors for questions.

To be run as:

mpirun -np 4 --mca pml_monitoring_enable 2 ./monitoring_test
pm
Then, the output should be:

flushing to ./prof/phase_1_2.prof
flushing to ./prof/phase_1_0.prof
flushing to ./prof/phase_1_3.prof
flushing to ./prof/phase_2_1.prof
flushing to ./prof/phase_2_3.prof
flushing to ./prof/phase_2_0.prof
flushing to ./prof/phase_2_2.prof
I	0	1	108 bytes	27 msgs sent
E	0	1	1012 bytes	30 msgs sent
E	0	2	23052 bytes	61 msgs sent
I	1	2	104 bytes	26 msgs sent
I	1	3	208 bytes	52 msgs sent
E	1	0	860 bytes	24 msgs sent
E	1	3	2552 bytes	56 msgs sent
I	2	3	104 bytes	26 msgs sent
E	2	0	22804 bytes	49 msgs sent
E	2	3	860 bytes	24 msgs sent
I	3	0	104 bytes	26 msgs sent
I	3	1	204 bytes	51 msgs sent
E	3	1	2304 bytes	44 msgs sent
E	3	2	860 bytes	24 msgs sent

or as

mpirun -np 4 --mca pml_monitoring_enable 1 ./monitoring_test

for an output as:

flushing to ./prof/phase_1_1.prof
flushing to ./prof/phase_1_0.prof
flushing to ./prof/phase_1_2.prof
flushing to ./prof/phase_1_3.prof
flushing to ./prof/phase_2_1.prof
flushing to ./prof/phase_2_3.prof
flushing to ./prof/phase_2_2.prof
flushing to ./prof/phase_2_0.prof
I	0	1	1120 bytes	57 msgs sent
I	0	2	23052 bytes	61 msgs sent
I	1	0	860 bytes	24 msgs sent
I	1	2	104 bytes	26 msgs sent
I	1	3	2760 bytes	108 msgs sent
I	2	0	22804 bytes	49 msgs sent
I	2	3	964 bytes	50 msgs sent
I	3	0	104 bytes	26 msgs sent
I	3	1	2508 bytes	95 msgs sent
I	3	2	860 bytes	24 msgs sent
*/



#include <stdio.h>
#include "mpi.h"

static MPI_T_pvar_handle flush_handle;
static const char flush_pvar_name[] = "pml_monitoring_flush";
static const char flush_cvar_name[] = "pml_monitoring_enable";
static int flush_pvar_idx;

int main(int argc, char* argv[])
{
    int rank, size, n, to, from, tagno, MPIT_result, provided, count;
    MPI_T_pvar_session session;
    MPI_Status status;
    MPI_Comm newcomm;
    MPI_Request request;
    char filename[1024];


    /* first phase : make a token circulated in MPI_COMM_WORLD */
    n = -1;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    to = (rank + 1) % size;
    from = (rank - 1) % size;
    tagno = 201;

    MPIT_result = MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);
    if (MPIT_result != MPI_SUCCESS)
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);

    MPIT_result = MPI_T_pvar_get_index(flush_pvar_name, MPI_T_PVAR_CLASS_GENERIC, &flush_pvar_idx);
    if (MPIT_result != MPI_SUCCESS) {
        printf("cannot find monitoring MPI_T \"%s\" pvar, check that you have monitoring pml\n",
               flush_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_session_create(&session);
    if (MPIT_result != MPI_SUCCESS) {
        printf("cannot create a session for \"%s\" pvar\n", flush_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    /* Allocating a new PVAR in a session will reset the counters */
    MPIT_result = MPI_T_pvar_handle_alloc(session, flush_pvar_idx,
                                          MPI_COMM_WORLD, &flush_handle, &count);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to allocate handle on \"%s\" pvar, check that you have monitoring pml\n",
               flush_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_start(session, flush_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to start handle on \"%s\" pvar, check that you have monitoring pml\n",
               flush_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    
    if (rank == 0) {
        n = 25;
        MPI_Isend(&n,1,MPI_INT,to,tagno,MPI_COMM_WORLD,&request);
    }
    while (1) {
        MPI_Irecv(&n,1,MPI_INT,from,tagno,MPI_COMM_WORLD, &request);
        MPI_Wait(&request,&status);
        if (rank == 0) {n--;tagno++;}
        MPI_Isend(&n,1,MPI_INT,to,tagno,MPI_COMM_WORLD, &request);
        if (rank != 0) {n--;tagno++;}
        if (n<0){
            break;
        }
    }

    /* Build one file per processes
       Every thing that has been monitored by each
       process since the last flush will be output in filename */

    /*
      Requires directory prof to be created.
      Filename format should display the phase number
      and the process rank for ease of parsing with
      aggregate_profile.pl script
    */
    sprintf(filename,"prof/phase_1_%d.prof",rank);
    if( MPI_SUCCESS != MPI_T_pvar_write(session, flush_handle, filename) ) {
        fprintf(stderr, "Process %d cannot save monitoring in %s\n", rank, filename);
    }
    /* Force the writing of the monitoring data */
    MPIT_result = MPI_T_pvar_stop(session, flush_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to stop handle on \"%s\" pvar, check that you have monitoring pml\n",
               flush_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_start(session, flush_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to start handle on \"%s\" pvar, check that you have monitoring pml\n",
               flush_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    /* Don't set a filename. If we stop the session before setting it, then no output ile
     * will be generated.
     */
    if( MPI_SUCCESS != MPI_T_pvar_write(session, flush_handle, NULL) ) {
        fprintf(stderr, "Process %d cannot save monitoring in %s\n", rank, filename);
    }
    
    /*
      Second phase. Work with different communicators.
      even ranls will circulate a token
      while odd ranks wil perform a all_to_all
    */
    MPI_Comm_split(MPI_COMM_WORLD, rank%2, rank, &newcomm);

    /* the filename for flushing monitoring now uses 2 as phase number! */
    sprintf(filename, "prof/phase_2_%d.prof", rank);

    if(rank%2){ /*even ranks (in COMM_WORD) circulate a token*/
        MPI_Comm_rank(newcomm, &rank);
        MPI_Comm_size(newcomm, &size);
        if( size > 1 ) {
            to = (rank + 1) % size;;
            from = (rank - 1) % size ;
            tagno = 201;
            if (rank == 0){
                n = 50;
                MPI_Send(&n, 1, MPI_INT, to, tagno, newcomm);
            }
            while (1){
                MPI_Recv(&n, 1, MPI_INT, from, tagno, newcomm, &status);
                if (rank == 0) {n--; tagno++;}
                MPI_Send(&n, 1, MPI_INT, to, tagno, newcomm);
                if (rank != 0) {n--; tagno++;}
                if (n<0){
                    if( MPI_SUCCESS != MPI_T_pvar_write(session, flush_handle, filename) ) {
                        fprintf(stderr, "Process %d cannot save monitoring in %s\n", rank, filename);
                    }
                    break;
                }
            }
        }
    } else { /*odd ranks (in COMM_WORD) will perform a all_to_all and a barrier*/
        int send_buff[10240];
        int recv_buff[10240];
        MPI_Comm_rank(newcomm, &rank);
        MPI_Comm_size(newcomm, &size);
        MPI_Alltoall(send_buff, 10240/size, MPI_INT, recv_buff, 10240/size, MPI_INT, newcomm);
        MPI_Comm_split(newcomm, rank%2, rank, &newcomm);
        MPI_Barrier(newcomm);
        if( MPI_SUCCESS != MPI_T_pvar_write(session, flush_handle, filename) ) {
            fprintf(stderr, "Process %d cannot save monitoring in %s\n", rank, filename);
        }
    }

    MPIT_result = MPI_T_pvar_stop(session, flush_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to stop handle on \"%s\" pvar, check that you have monitoring pml\n",
               flush_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_handle_free(session, &flush_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to free handle on \"%s\" pvar, check that you have monitoring pml\n",
               flush_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_session_free(&session);
    if (MPIT_result != MPI_SUCCESS) {
        printf("cannot close a session for \"%s\" pvar\n", flush_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    (void)PMPI_T_finalize();

    /* Now, in MPI_Finalize(), the pml_monitoring library outputs, in
       STDERR, the aggregated recorded monitoring of all the phases*/
    MPI_Finalize();
    return 0;
}
