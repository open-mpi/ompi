/*
 * Copyright (c) 2017 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

static MPI_T_pvar_handle count_handle;
static const char count_pvar_name[] = "pml_monitoring_messages_count";
static int count_pvar_idx;

int main(int argc, char**argv)
{
    int rank, size, n, to, from, tagno, MPIT_result, provided, count;
    MPI_T_pvar_session session;
    MPI_Status status;
    MPI_Request request;
    size_t*counts;

    n = -1;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    to = (rank + 1) % size;
    from = (rank + size - 1) % size;
    tagno = 201;

    MPIT_result = MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);
    if (MPIT_result != MPI_SUCCESS)
	MPI_Abort(MPI_COMM_WORLD, MPIT_result);

    MPIT_result = MPI_T_pvar_get_index(count_pvar_name, MPI_T_PVAR_CLASS_SIZE, &count_pvar_idx);
    if (MPIT_result != MPI_SUCCESS) {
	printf("cannot find monitoring MPI_T \"%s\" pvar, check that you have monitoring pml\n",
	       count_pvar_name);
	MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_session_create(&session);
    if (MPIT_result != MPI_SUCCESS) {
	printf("cannot create a session for \"%s\" pvar\n", count_pvar_name);
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

    counts = (size_t*)malloc(count * sizeof(size_t));

    MPIT_result = MPI_T_pvar_start(session, count_handle);
    if (MPIT_result != MPI_SUCCESS) {
	printf("failed to start handle on \"%s\" pvar, check that you have monitoring pml\n",
	       count_pvar_name);
	MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    /* Token Ring communications */
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

    MPIT_result = MPI_T_pvar_read(session, count_handle, counts);
    if (MPIT_result != MPI_SUCCESS) {
	printf("failed to read handle on \"%s\" pvar, check that you have monitoring pml\n",
	       count_pvar_name);
	MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    /*** REDUCE ***/
    MPI_Allreduce(MPI_IN_PLACE, counts, count, MPI_UNSIGNED_LONG, MPI_MAX, MPI_COMM_WORLD);

    if(0 == rank) {
	for(n = 0; n < count; ++n)
	    printf("%zu%s", counts[n], n < count - 1 ? ", " : "\n");
    }

    free(counts);

    MPIT_result = MPI_T_pvar_stop(session, count_handle);
    if (MPIT_result != MPI_SUCCESS) {
	printf("failed to stop handle on \"%s\" pvar, check that you have monitoring pml\n",
	       count_pvar_name);
	MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_handle_free(session, &count_handle);
    if (MPIT_result != MPI_SUCCESS) {
	printf("failed to free handle on \"%s\" pvar, check that you have monitoring pml\n",
	       count_pvar_name);
	MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    MPIT_result = MPI_T_pvar_session_free(&session);
    if (MPIT_result != MPI_SUCCESS) {
	printf("cannot close a session for \"%s\" pvar\n", count_pvar_name);
	MPI_Abort(MPI_COMM_WORLD, MPIT_result);
    }

    (void)MPI_T_finalize();

    MPI_Finalize();
    
    return EXIT_SUCCESS;
}
