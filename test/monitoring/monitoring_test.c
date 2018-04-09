/*
 * Copyright (c) 2013-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2017 Inria.  All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
pml monitoring tester.

Designed by George Bosilca <bosilca@icl.utk.edu> Emmanuel Jeannot <emmanuel.jeannot@inria.fr> and Clément Foyer <clement.foyer@inria.fr>
Contact the authors for questions.

To options are available for this test, with/without MPI_Tools, and with/without RMA operations. The default mode is without MPI_Tools, and with RMA operations.
To enable the MPI_Tools use, add "--with-mpit" as an application parameter.
To disable the RMA operations testing, add "--without-rma" as an application parameter.

To be run as (without using MPI_Tool):

mpirun -np 4 --mca pml_monitoring_enable 2 --mca pml_monitoring_enable_output 3 --mca pml_monitoring_filename prof/output ./monitoring_test

with the results being, as an example:
output.1.prof
# POINT TO POINT
E       1       2       104 bytes       26 msgs sent    0,0,0,26,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
E       1       3       208 bytes       52 msgs sent    8,0,0,65,1,5,2,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
I       1       0       140 bytes       27 msgs sent
I       1       2       2068 bytes      1 msgs sent
I       1       3       2256 bytes      31 msgs sent
# OSC
S       1       0       0 bytes 1 msgs sent
R       1       0       40960 bytes     1 msgs sent
S       1       2       40960 bytes     1 msgs sent
# COLLECTIVES
C       1       0       140 bytes       27 msgs sent
C       1       2       140 bytes       27 msgs sent
C       1       3       140 bytes       27 msgs sent
D       MPI COMMUNICATOR 4 DUP FROM 0   procs: 0,1,2,3
O2A     1       0 bytes 0 msgs sent
A2O     1       0 bytes 0 msgs sent
A2A     1       276 bytes       15 msgs sent
D       MPI_COMM_WORLD  procs: 0,1,2,3
O2A     1       0 bytes 0 msgs sent
A2O     1       0 bytes 0 msgs sent
A2A     1       96 bytes        9 msgs sent
D       MPI COMMUNICATOR 5 SPLIT_TYPE FROM 4    procs: 0,1,2,3
O2A     1       0 bytes 0 msgs sent
A2O     1       0 bytes 0 msgs sent
A2A     1       48 bytes        3 msgs sent
D       MPI COMMUNICATOR 3 SPLIT FROM 0 procs: 1,3
O2A     1       0 bytes 0 msgs sent
A2O     1       0 bytes 0 msgs sent
A2A     1       0 bytes 0 msgs sent

*/


#include "mpi.h"
#include <stdio.h>
#include <string.h>

static MPI_T_pvar_handle flush_handle;
static const char flush_pvar_name[] = "pml_monitoring_flush";
static const void*nullbuf = NULL;
static int flush_pvar_idx;
static int with_mpit = 0;
static int with_rma  = 1;

int main(int argc, char* argv[])
{
    int rank, size, n, to, from, tagno, MPIT_result, provided, count, world_rank;
    MPI_T_pvar_session session;
    MPI_Comm newcomm;
    char filename[1024];
    
    for ( int arg_it = 1; argc > 1 && arg_it < argc; ++arg_it ) {
        if( 0 == strcmp(argv[arg_it], "--with-mpit") ) {
            with_mpit = 1;
            printf("enable MPIT support\n");
        } else if( 0 == strcmp(argv[arg_it], "--without-rma") ) {
            with_rma = 0;
            printf("disable RMA testing\n");
        }
    }

    /* first phase : make a token circulated in MPI_COMM_WORLD */
    n = -1;
    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    rank = world_rank;
    to = (rank + 1) % size;
    from = (rank - 1) % size;
    tagno = 201;

    if( with_mpit ) {
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
    }

    if (rank == 0) {
        n = 25;
        MPI_Send(&n,1,MPI_INT,to,tagno,MPI_COMM_WORLD);
    }
    while (1) {
        MPI_Recv(&n, 1, MPI_INT, from, tagno, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        if (rank == 0) {n--;tagno++;}
        MPI_Send(&n, 1, MPI_INT, to, tagno, MPI_COMM_WORLD);
        if (rank != 0) {n--;tagno++;}
        if (n<0){
            break;
        }
    }

    if( with_mpit ) {
        /* Build one file per processes
              Every thing that has been monitored by each
              process since the last flush will be output in filename */
        /*
            Requires directory prof to be created.
              Filename format should display the phase number
                and the process rank for ease of parsing with
                  aggregate_profile.pl script
        */
        sprintf(filename, "prof/phase_1");

        if( MPI_SUCCESS != MPI_T_pvar_write(session, flush_handle, filename) ) {
            fprintf(stderr, "Process %d cannot save monitoring in %s.%d.prof\n",
                    world_rank, filename, world_rank);
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
        /* Don't set a filename. If we stop the session before setting it, then no output file
         * will be generated.
         */
        if( MPI_SUCCESS != MPI_T_pvar_write(session, flush_handle, (void*)&nullbuf) ) {
            fprintf(stderr, "Process %d cannot save monitoring in %s\n", world_rank, filename);
        }
    }

    /*
      Second phase. Work with different communicators.
      even ranks will circulate a token
      while odd ranks will perform a all_to_all
    */
    MPI_Comm_split(MPI_COMM_WORLD, rank%2, rank, &newcomm);

    if(rank%2){ /*odd ranks (in COMM_WORD) circulate a token*/
        MPI_Comm_rank(newcomm, &rank);
        MPI_Comm_size(newcomm, &size);
        if( size > 1 ) {
            to = (rank + 1) % size;
            from = (rank - 1) % size;
            tagno = 201;
            if (rank == 0){
                n = 50;
                MPI_Send(&n, 1, MPI_INT, to, tagno, newcomm);
            }
            while (1){
                MPI_Recv(&n, 1, MPI_INT, from, tagno, newcomm, MPI_STATUS_IGNORE);
                if (rank == 0) {n--; tagno++;}
                MPI_Send(&n, 1, MPI_INT, to, tagno, newcomm);
                if (rank != 0) {n--; tagno++;}
                if (n<0){
                    break;
                }
            }
        }
    } else { /*even ranks (in COMM_WORD) will perform a all_to_all and a barrier*/
        int send_buff[10240];
        int recv_buff[10240];
        MPI_Comm newcomm2;
        MPI_Comm_rank(newcomm, &rank);
        MPI_Comm_size(newcomm, &size);
        MPI_Alltoall(send_buff, 10240/size, MPI_INT, recv_buff, 10240/size, MPI_INT, newcomm);
        MPI_Comm_split(newcomm, rank%2, rank, &newcomm2);
        MPI_Barrier(newcomm2);
        MPI_Comm_free(&newcomm2);
    }

    if( with_mpit ) {
        /* Build one file per processes
              Every thing that has been monitored by each
              process since the last flush will be output in filename */
        /*
            Requires directory prof to be created.
              Filename format should display the phase number
                and the process rank for ease of parsing with
                                                    aggregate_profile.pl script
                                                  */
        sprintf(filename, "prof/phase_2");

        if( MPI_SUCCESS != MPI_T_pvar_write(session, flush_handle, filename) ) {
            fprintf(stderr, "Process %d cannot save monitoring in %s.%d.prof\n",
                    world_rank, filename, world_rank);
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
        /* Don't set a filename. If we stop the session before setting it, then no output
         * will be generated.
         */
        if( MPI_SUCCESS != MPI_T_pvar_write(session, flush_handle, (void*)&nullbuf ) ) {
            fprintf(stderr, "Process %d cannot save monitoring in %s\n", world_rank, filename);
        }
    }

    if( with_rma ) {
      MPI_Win win;
      int rs_buff[10240];
      int win_buff[10240];
      MPI_Comm_rank(MPI_COMM_WORLD, &rank);
      MPI_Comm_size(MPI_COMM_WORLD, &size);
      to = (rank + 1) % size;
      from = (rank + size - 1) % size;
      for( int v = 0; v < 10240; ++v )
        rs_buff[v] = win_buff[v] = rank;

      MPI_Win_create(win_buff, 10240 * sizeof(int), sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &win);
      MPI_Win_fence(MPI_MODE_NOPRECEDE, win);
      if( rank%2 ) {
        MPI_Win_fence(MPI_MODE_NOSTORE | MPI_MODE_NOPUT, win);
        MPI_Get(rs_buff, 10240, MPI_INT, from, 0, 10240, MPI_INT, win);
      } else {
        MPI_Put(rs_buff, 10240, MPI_INT, to, 0, 10240, MPI_INT, win);
        MPI_Win_fence(MPI_MODE_NOSTORE | MPI_MODE_NOPUT, win);
      }
      MPI_Win_fence(MPI_MODE_NOSUCCEED, win);

      for( int v = 0; v < 10240; ++v )
        if( rs_buff[v] != win_buff[v] && ((rank%2 && rs_buff[v] != from) || (!(rank%2) && rs_buff[v] != rank)) ) {
	  printf("Error on checking exchanged values: %s_buff[%d] == %d instead of %d\n",
		 rank%2 ? "rs" : "win", v, rs_buff[v], rank%2 ? from : rank);
	  MPI_Abort(MPI_COMM_WORLD, -1);
        }

      MPI_Group world_group, newcomm_group, distant_group;
      MPI_Comm_group(MPI_COMM_WORLD, &world_group);
      MPI_Comm_group(newcomm, &newcomm_group);
      MPI_Group_difference(world_group, newcomm_group, &distant_group);
      if( rank%2 ) {
        MPI_Win_post(distant_group, 0, win);
        MPI_Win_wait(win);
        /* Check received values */
        for( int v = 0; v < 10240; ++v )
	  if( from != win_buff[v] ) {
	    printf("Error on checking exchanged values: win_buff[%d] == %d instead of %d\n",
		   v, win_buff[v], from);
	    MPI_Abort(MPI_COMM_WORLD, -1);
	  }
      } else {
        MPI_Win_start(distant_group, 0, win);
        MPI_Put(rs_buff, 10240, MPI_INT, to, 0, 10240, MPI_INT, win);
        MPI_Win_complete(win);
      }
      MPI_Group_free(&world_group);
      MPI_Group_free(&newcomm_group);
      MPI_Group_free(&distant_group);
      MPI_Barrier(MPI_COMM_WORLD);

      for( int v = 0; v < 10240; ++v ) rs_buff[v] = rank;

      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, to, 0, win);
      MPI_Put(rs_buff, 10240, MPI_INT, to, 0, 10240, MPI_INT, win);
      MPI_Win_unlock(to, win);

      MPI_Barrier(MPI_COMM_WORLD);

      /* Check received values */
      for( int v = 0; v < 10240; ++v )
        if( from != win_buff[v] ) {
	  printf("Error on checking exchanged values: win_buff[%d] == %d instead of %d\n",
		 v, win_buff[v], from);
	  MPI_Abort(MPI_COMM_WORLD, -1);
        }

      MPI_Win_free(&win);
    }

    if( with_mpit ) {
        /* the filename for flushing monitoring now uses 3 as phase number! */
        sprintf(filename, "prof/phase_3");

        if( MPI_SUCCESS != MPI_T_pvar_write(session, flush_handle, filename) ) {
            fprintf(stderr, "Process %d cannot save monitoring in %s.%d.prof\n",
                    world_rank, filename, world_rank);
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

        (void)MPI_T_finalize();
    }

    MPI_Comm_free(&newcomm);
    /* Now, in MPI_Finalize(), the pml_monitoring library outputs, in
       STDERR, the aggregated recorded monitoring of all the phases*/
    MPI_Finalize();
    return 0;
}
