/*
 * Copyright (c) 2013-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2016 Inria.  All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
pml monitoring tester.

Designed by George Bosilca <bosilca@icl.utk.edu>,  Emmanuel Jeannot <emmanuel.jeannot@inria.fr> and
Clement Foyer <clement.foyer@inria.fr>
Contact the authors for questions.

To be run as:

mpirun -np 4 --mca pml_monitoring_enable 2 ./test_pvar_access

Then, the output should be:
Flushing phase 1:
I       0       1       108 bytes       27 msgs sent
I       1       2       104 bytes       26 msgs sent
I       2       3       104 bytes       26 msgs sent
I       3       0       104 bytes       26 msgs sent
Flushing phase 2:
I       0       1       20 bytes        4 msgs sent
I       0       2       20528 bytes     9 msgs sent
I       1       0       20 bytes        4 msgs sent
I       1       2       104 bytes       26 msgs sent
I       1       3       236 bytes       56 msgs sent
I       2       0       20528 bytes     9 msgs sent
I       2       3       112 bytes       27 msgs sent
I       3       1       220 bytes       52 msgs sent
I       3       2       20 bytes        4 msgs sent

*/

#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

static MPI_T_pvar_handle count_handle;
static MPI_T_pvar_handle msize_handle;
static const char count_pvar_name[] = "pml_monitoring_messages_count";
static const char msize_pvar_name[] = "pml_monitoring_messages_size";
static int count_pvar_idx, msize_pvar_idx;
static int world_rank, world_size;

static void print_vars(int rank, int size, size_t* msg_count, size_t*msg_size)
{
    int i;
    for(i = 0; i < size; ++i) {
        if(0 != msg_size[i])
            printf("I\t%d\t%d\t%zu bytes\t%zu msgs sent\n", rank, i, msg_size[i], msg_count[i]);
    }
}

int main(int argc, char* argv[])
{
    int rank, size, n, to, from, tagno, MPIT_result, provided, count;
    MPI_T_pvar_session session;
    MPI_Status status;
    MPI_Comm newcomm;
    MPI_Request request;
    size_t*msg_count_p1, *msg_size_p1;
    size_t*msg_count_p2, *msg_size_p2;

    /* first phase : make a token circulated in MPI_COMM_WORLD */
    n = -1;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    world_size = size;
    world_rank = rank;
    to = (rank + 1) % size;
    from = (rank - 1) % size;
    tagno = 201;

    MPIT_result = MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);
    if (MPIT_result != MPI_SUCCESS)
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    
    /* Retrieve the pvar indices */
    MPIT_result = MPI_T_pvar_get_index(count_pvar_name, MPI_T_PVAR_CLASS_SIZE, &count_pvar_idx);
    if (MPIT_result != MPI_SUCCESS) {
        printf("cannot find monitoring MPI_T \"%s\" pvar, check that you have monitoring pml\n",
               count_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    MPIT_result = MPI_T_pvar_get_index(msize_pvar_name, MPI_T_PVAR_CLASS_SIZE, &msize_pvar_idx);
    if (MPIT_result != MPI_SUCCESS) {
        printf("cannot find monitoring MPI_T \"%s\" pvar, check that you have monitoring pml\n",
               msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    /* Get session for pvar binding */
    MPIT_result = MPI_T_pvar_session_create(&session);
    if (MPIT_result != MPI_SUCCESS) {
        printf("cannot create a session for \"%s\" and \"%s\" pvars\n",
               count_pvar_name, msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    /* Allocating a new PVAR in a session will reset the counters */
    MPIT_result = MPI_T_pvar_handle_alloc(session, count_pvar_idx,
                                          MPI_COMM_WORLD, &count_handle, &count);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to allocate handle on \"%s\" pvar, check that you have monitoring pml\n",
               count_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    MPIT_result = MPI_T_pvar_handle_alloc(session, msize_pvar_idx,
                                          MPI_COMM_WORLD, &msize_handle, &count);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to allocate handle on \"%s\" pvar, check that you have monitoring pml\n",
               msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    /* Allocate arrays to retrieve results */
    msg_count_p1 = calloc(count * 4, sizeof(size_t));
    msg_size_p1 = &msg_count_p1[count];
    msg_count_p2 = &msg_count_p1[2*count];
    msg_size_p2 = &msg_count_p1[3*count];

    /* Start pvar */
    MPIT_result = MPI_T_pvar_start(session, count_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to start handle on \"%s\" pvar, check that you have monitoring pml\n",
               count_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    MPIT_result = MPI_T_pvar_start(session, msize_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to start handle on \"%s\" pvar, check that you have monitoring pml\n",
               msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    
    if (rank == 0) {
        n = 25;
        MPI_Isend(&n,1,MPI_INT,to,tagno,MPI_COMM_WORLD,&request);
    }
    while (1) {
        MPI_Irecv(&n, 1, MPI_INT, from, tagno, MPI_COMM_WORLD, &request);
        MPI_Wait(&request, &status);
        if (rank == 0) {n--;tagno++;}
        MPI_Isend(&n, 1, MPI_INT, to, tagno, MPI_COMM_WORLD, &request);
        if (rank != 0) {n--;tagno++;}
        if (n<0){
            break;
        }
    }

    /* Test stopping variable then get values */
    MPIT_result = MPI_T_pvar_stop(session, count_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to stop handle on \"%s\" pvar, check that you have monitoring pml\n",
               count_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }    
    MPIT_result = MPI_T_pvar_stop(session, msize_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to stop handle on \"%s\" pvar, check that you have monitoring pml\n",
               msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_read(session, count_handle, msg_count_p1);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to fetch handle on \"%s\" pvar, check that you have monitoring pml\n",
               count_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    MPIT_result = MPI_T_pvar_read(session, msize_handle, msg_size_p1);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to fetch handle on \"%s\" pvar, check that you have monitoring pml\n",
               msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    /* Circulate a token to proper display the results */
    if(0 == world_rank) {
        printf("Flushing phase 1:\n");
        print_vars(world_rank, world_size, msg_count_p1, msg_size_p1);
        MPI_Send(NULL, 0, MPI_BYTE, (world_rank + 1) % world_size, 300, MPI_COMM_WORLD);
        MPI_Recv(NULL, 0, MPI_BYTE, (world_rank - 1) % world_size, 300, MPI_COMM_WORLD, &status);
    } else {
        MPI_Recv(NULL, 0, MPI_BYTE, (world_rank - 1) % world_size, 300, MPI_COMM_WORLD, &status);
        print_vars(world_rank, world_size, msg_count_p1, msg_size_p1);
        MPI_Send(NULL, 0, MPI_BYTE, (world_rank + 1) % world_size, 300, MPI_COMM_WORLD);
    }

    /* Add to the phase 1 the display token ring message count */
    MPIT_result = MPI_T_pvar_read(session, count_handle, msg_count_p1);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to fetch handle on \"%s\" pvar, check that you have monitoring pml\n",
               count_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    MPIT_result = MPI_T_pvar_read(session, msize_handle, msg_size_p1);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to fetch handle on \"%s\" pvar, check that you have monitoring pml\n",
               msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    
    /*
      Second phase. Work with different communicators.
      even ranks will circulate a token
      while odd ranks will perform a all_to_all
    */
    MPIT_result = MPI_T_pvar_start(session, count_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to start handle on \"%s\" pvar, check that you have monitoring pml\n",
               count_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    MPIT_result = MPI_T_pvar_start(session, msize_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to start handle on \"%s\" pvar, check that you have monitoring pml\n",
               msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    
    MPI_Comm_split(MPI_COMM_WORLD, rank%2, rank, &newcomm);

    if(rank%2){ /*even ranks (in COMM_WORD) circulate a token*/
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
                MPI_Recv(&n, 1, MPI_INT, from, tagno, newcomm, &status);
                if (rank == 0) {n--; tagno++;}
                MPI_Send(&n, 1, MPI_INT, to, tagno, newcomm);
                if (rank != 0) {n--; tagno++;}
                if (n<0){
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
    }

    MPIT_result = MPI_T_pvar_read(session, count_handle, msg_count_p2);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to fetch handle on \"%s\" pvar, check that you have monitoring pml\n",
               count_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    MPIT_result = MPI_T_pvar_read(session, msize_handle, msg_size_p2);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to fetch handle on \"%s\" pvar, check that you have monitoring pml\n",
               msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    /* Taking only in account the second phase */
    for(int i = 0; i < size; ++i) {
        msg_count_p2[i] -= msg_count_p1[i];
        msg_size_p2[i] -= msg_size_p1[i];
    }

    /* Circulate a token to proper display the results */
    if(0 == world_rank) {
        printf("Flushing phase 2:\n");
        print_vars(world_rank, world_size, msg_count_p2, msg_size_p2);
        MPI_Send(NULL, 0, MPI_BYTE, (world_rank + 1) % world_size, 300, MPI_COMM_WORLD);
        MPI_Recv(NULL, 0, MPI_BYTE, (world_rank - 1) % world_size, 300, MPI_COMM_WORLD, &status);
    } else {
        MPI_Recv(NULL, 0, MPI_BYTE, (world_rank - 1) % world_size, 300, MPI_COMM_WORLD, &status);
        print_vars(world_rank, world_size, msg_count_p2, msg_size_p2);
        MPI_Send(NULL, 0, MPI_BYTE, (world_rank + 1) % world_size, 300, MPI_COMM_WORLD);
    }

    MPIT_result = MPI_T_pvar_handle_free(session, &count_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to free handle on \"%s\" pvar, check that you have monitoring pml\n",
               count_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }
    MPIT_result = MPI_T_pvar_handle_free(session, &msize_handle);
    if (MPIT_result != MPI_SUCCESS) {
        printf("failed to free handle on \"%s\" pvar, check that you have monitoring pml\n",
               msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_session_free(&session);
    if (MPIT_result != MPI_SUCCESS) {
        printf("cannot close a session for \"%s\" and \"%s\" pvars\n",
               count_pvar_name, msize_pvar_name);
        MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    (void)MPI_T_finalize();

    free(msg_count_p1);
    
    MPI_Finalize();
    return EXIT_SUCCESS;
}
