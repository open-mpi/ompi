/*
 * Copyright (c) 2018 The University of Tennessee and The University
 *                    of Tennessee Research Foundation.  All rights
 *                    reserved.
 *
 * Simple example usage of SPCs through MPI_T.
 */

#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 1000000

/* Sends 'num_messages' messages of 'message_size' bytes from rank 0 to rank 1.
 * All messages are sent synchronously and with the same tag in MPI_COMM_WORLD.
 */
static void message_exchange(int num_messages, int message_size)
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
    int i, rank, size, provided, num, name_len, desc_len, verbosity, bind, var_class, readonly, continuous, atomic, count, index, MPI_result;
    MPI_Datatype datatype;
    MPI_T_enum enumtype;
    char name[256], description[256];

    /* Counter names to be read by ranks 0 and 1 */
    char *counter_names[] = { "runtime_spc_OMPI_SPC_BYTES_SENT_USER",
                              "runtime_spc_OMPI_SPC_BYTES_RECEIVED_USER" };

    MPI_Init(NULL, NULL);
    MPI_result = MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);
    if(MPI_result != MPI_SUCCESS) {
        fprintf(stderr, "Failed to initialize MPI_T thread.\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_result);
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 2) {
        fprintf(stderr, "ERROR: This test should be run with two MPI processes.\n");
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    /* Determine the MPI_T pvar indices for the OMPI_BYTES_SENT/RECIEVED_USER SPCs */
    index = -1;
    MPI_result = MPI_T_pvar_get_num(&num);
    if(MPI_result != MPI_SUCCESS) {
        fprintf(stderr, "Failed to get the number of pvars.\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_result);
    }

    for(i = 0; i < num; i++) {
        name_len = desc_len = 256;
        MPI_T_pvar_get_info(i, name, &name_len, &verbosity,
                             &var_class, &datatype, &enumtype, description, &desc_len, &bind,
                             &readonly, &continuous, &atomic);
        if(MPI_result != MPI_SUCCESS || MPI_result == MPI_T_ERR_PVAR_NO_STARTSTOP) {
            fprintf(stderr, "Failed to get pvar info.\n");
            MPI_Abort(MPI_COMM_WORLD, MPI_result);
        }

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

    long long value;

    MPI_T_pvar_session session;
    MPI_T_pvar_handle handle;
    /* Create the MPI_T sessions/handles for the counters and start the counters */
    MPI_result = MPI_T_pvar_session_create(&session);
    if(MPI_result != MPI_SUCCESS) {
        fprintf(stderr, "Failed to create MPI_T pvar session.\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_result);
    }

    MPI_result = MPI_T_pvar_handle_alloc(session, index, NULL, &handle, &count);
    if(MPI_result != MPI_SUCCESS) {
        fprintf(stderr, "Failed to allocate the pvar handle.\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_result);
    }

    MPI_result = MPI_T_pvar_start(session, handle);
    if(MPI_result != MPI_SUCCESS) {
        if(MPI_result != MPI_T_ERR_PVAR_NO_STARTSTOP) {
            fprintf(stderr, "Failed to start the pvar session.\n");
            MPI_Abort(MPI_COMM_WORLD, MPI_result);
        }
    }

    int message_size = 1, expected_bytes = 0;
    while(message_size <= MAX_SIZE) {
        expected_bytes += message_size;
        message_exchange(1, message_size);
        message_size *= 10;
    }

    MPI_result = MPI_T_pvar_read(session, handle, &value);
    if(MPI_result != MPI_SUCCESS) {
        fprintf(stderr, "Failed to read the pvar.\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_result);
    }

    /* Print the counter values in order by rank */
    for(i = 0; i < 2; i++) {
        if(i == rank) {
            printf("[%d] Value Read: %lld\n", rank, value);
            fflush(stdout);
            if(value != expected_bytes){
                fprintf(stderr, "The counter value is inaccurate!  It is '%lld'.  It should be '%d'\n", value, expected_bytes);
                MPI_Abort(MPI_COMM_WORLD, MPI_ERR_OTHER);
            }
        }
        MPI_Barrier(MPI_COMM_WORLD);
    }
    /* Stop the MPI_T session, free the handle, and then free the session */
    MPI_result = MPI_T_pvar_stop(session, handle);
    if(MPI_result != MPI_SUCCESS) {
        if(MPI_result != MPI_T_ERR_PVAR_NO_STARTSTOP) {
            fprintf(stderr, "Failed to stop the pvar session.\n");
            MPI_Abort(MPI_COMM_WORLD, MPI_result);
        }
    }

    MPI_result = MPI_T_pvar_handle_free(session, &handle);
    if(MPI_result != MPI_SUCCESS) {
        fprintf(stderr, "Failed to free the pvar handle.\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_result);
    }

    MPI_result = MPI_T_pvar_session_free(&session);
    if(MPI_result != MPI_SUCCESS) {
        fprintf(stderr, "Failed to free the pvar session.\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_result);
    }

    MPI_result = MPI_T_finalize();
    if(MPI_result != MPI_SUCCESS) {
        fprintf(stderr, "Failed to finalize MPI_T.\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_result);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}
