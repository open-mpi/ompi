/*
 * Copyright (c) 2018      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *
 * Simple example usage of SPCs through MPI_T.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpi.h"

/* Sends 'num_messages' messages of 'message_size' bytes from rank 0 to rank 1.
 * All messages are send synchronously and with the same tag in MPI_COMM_WORLD.
 */
void message_exchange(int num_messages, int message_size)
{
    int i, rank;
    /* Use calloc to initialize data to 0's */
    char *data = (char*)calloc(message_size, sizeof(char));
    MPI_Status status;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if(rank == 0) {
        for(i = 0; i < num_messages; i++)
            MPI_Send(data, message_size, MPI_BYTE, 1, 123, MPI_COMM_WORLD);
    } else if(rank == 1) {
        for(i = 0; i < num_messages; i++)
            MPI_Recv(data, message_size, MPI_BYTE, 0, 123, MPI_COMM_WORLD, &status);
    }

    free(data);
}

int main(int argc, char **argv)
{
    int num_messages, message_size;

    if(argc < 3) {
        printf("Usage: mpirun -np 2 --mca mpi_spc_attach all --mca mpi_spc_dump_enabled true ./spc_example [num_messages] [message_size]\n");
        return -1;
    } else {
        num_messages = atoi(argv[1]);
        message_size = atoi(argv[2]);
    }

    int i, rank, size, provided, num, name_len, desc_len, verbosity, bind, var_class, readonly, continuous, atomic, count, index;
    MPI_Datatype datatype;
    MPI_T_enum enumtype;
    MPI_Comm comm;
    char name[256], description[256];

    /* Counter names to be read by ranks 0 and 1 */
    char *counter_names[] = {"runtime_spc_OMPI_BYTES_SENT_USER",
                             "runtime_spc_OMPI_BYTES_RECEIVED_USER" };

    MPI_Init(NULL, NULL);
    MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 2) {
        fprintf(stderr, "ERROR: This test should be run with two MPI processes.\n");
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    /* Determine the MPI_T pvar indices for the OMPI_BYTES_SENT/RECIEVED_USER SPCs */
    index = -1;
    MPI_T_pvar_get_num(&num);
    for(i = 0; i < num; i++) {
        name_len = desc_len = 256;
        PMPI_T_pvar_get_info(i, name, &name_len, &verbosity,
                             &var_class, &datatype, &enumtype, description, &desc_len, &bind,
                             &readonly, &continuous, &atomic);
        if(strcmp(name, counter_names[rank]) == 0) {
            index = i;
            printf("[%d] %s -> %s\n", rank, name, description);
        }
    }

    /* Make sure we found the counters */
    if(index == -1) {
        fprintf(stderr, "ERROR: Couldn't find the appropriate SPC counter in the MPI_T pvars.\n");
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    int ret;
    long long value;

    MPI_T_pvar_session session;
    MPI_T_pvar_handle handle;
    /* Create the MPI_T sessions/handles for the counters and start the counters */
    ret = MPI_T_pvar_session_create(&session);
    ret = MPI_T_pvar_handle_alloc(session, index, NULL, &handle, &count);
    ret = MPI_T_pvar_start(session, handle);

    message_exchange(num_messages, message_size);

    ret = MPI_T_pvar_read(session, handle, &value);
    /* Print the counter values in order by rank */
    for(i = 0; i < 2; i++) {
        if(i == rank) {
            printf("[%d] Value Read: %lld\n", rank, value);
            fflush(stdout);
        }
        MPI_Barrier(MPI_COMM_WORLD);
    }
    /* Stop the MPI_T session, free the handle, and then free the session */
    ret = MPI_T_pvar_stop(session, handle);
    ret = MPI_T_pvar_handle_free(session, &handle);
    ret = MPI_T_pvar_session_free(&session);

    MPI_T_finalize();
    MPI_Finalize();

    return 0;
}
