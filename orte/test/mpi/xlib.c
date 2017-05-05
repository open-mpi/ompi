#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>
#include <pmix.h>

#define SIZE 20
#define POS 10
#define INITIAL_VALUE 10

static pmix_proc_t myproc;

/* this is the event notification function we pass down below
 * when registering for general events - i.e.,, the default
 * handler. We don't technically need to register one, but it
 * is usually good practice to catch any events that occur */
static void notification_fn(size_t evhdlr_registration_id,
                            pmix_status_t status,
                            const pmix_proc_t *source,
                            pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc,
                            void *cbdata)
{
    /* this example doesn't do anything with default events */
    fprintf(stderr, "Default event handler called with status %s\n", PMIx_Error_string(status));

    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
}

/* this is an event notification function that we explicitly request
 * be called when the PMIX_MODEL_DECLARED notification is issued.
 * We could catch it in the general event notification function and test
 * the status to see if the status matched, but it often is simpler
 * to declare a use-specific notification callback point. In this case,
 * we are asking to know whenever a programming model library is
 * instantiated */
static void model_callback(size_t evhdlr_registration_id,
                           pmix_status_t status,
                           const pmix_proc_t *source,
                           pmix_info_t info[], size_t ninfo,
                           pmix_info_t results[], size_t nresults,
                           pmix_event_notification_cbfunc_fn_t cbfunc,
                           void *cbdata)
{
    size_t n;

    fprintf(stderr, "Model event handler called with status %d(%s)\n", status, PMIx_Error_string(status));

    /* check to see what model declared itself */
    for (n=0; n < ninfo; n++) {
        if (PMIX_STRING == info[n].value.type) {
            fprintf(stderr, "\t%s:\t%s\n", info[n].key, info[n].value.data.string);
        }
    }

    /* we must NOT tell the event handler state machine that we
     * are the last step as that will prevent it from notifying
     * anyone else that might be listening for declarations */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

/* event handler registration is done asynchronously because it
 * may involve the PMIx server registering with the host RM for
 * external events. So we provide a callback function that returns
 * the status of the request (success or an error), plus a numerical index
 * to the registered event. The index is used later on to deregister
 * an event handler - if we don't explicitly deregister it, then the
 * PMIx server will do so when it see us exit */
static void model_registration_callback(pmix_status_t status,
                                        size_t evhandler_ref,
                                        void *cbdata)
{
    volatile int *active = (volatile int*)cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                   myproc.nspace, myproc.rank, status, (unsigned long)evhandler_ref);
    }
    *active = status;
}

int main(int argc, char *argv[])
{
    int i, rank, size, next, prev, tag = 201;
    int array_size = SIZE;
    int pos = POS;
    int *send_array;
    int *recv_array;
    pmix_info_t *info;
    size_t ninfo;
    pmix_status_t code = PMIX_MODEL_DECLARED;
    pmix_status_t rc;
    volatile int active;


    if (1 < argc) {
        fprintf(stderr, "Declaring ourselves\n");
        /* declare ourselves as a non-MPI library prior to MPI_Init */
        ninfo = 4;
        PMIX_INFO_CREATE(info, ninfo);
        PMIX_INFO_LOAD(&info[0], PMIX_PROGRAMMING_MODEL, "EXAMPLE", PMIX_STRING);
        PMIX_INFO_LOAD(&info[1], PMIX_MODEL_LIBRARY_NAME, "FOOL", PMIX_STRING);
        PMIX_INFO_LOAD(&info[2], PMIX_MODEL_LIBRARY_VERSION, "1.2.3", PMIX_STRING);
        PMIX_INFO_LOAD(&info[3], PMIX_THREADING_MODEL, "NONE", PMIX_STRING);
        if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, info, ninfo))) {
            fprintf(stderr, "PMIx Init failed: %s\n", PMIx_Error_string(rc));
            exit(1);
        }
        PMIX_INFO_FREE(info, ninfo);

        /* register a handler specifically for when models declare */
        active = -1;
        ninfo = 1;
        PMIX_INFO_CREATE(info, ninfo);
        PMIX_INFO_LOAD(&info[0], PMIX_EVENT_HDLR_NAME, "APP-MODEL", PMIX_STRING);
        PMIx_Register_event_handler(&code, 1, info, ninfo,
                                    model_callback, model_registration_callback, (void*)&active);
        while (-1 == active) {
            usleep(10);
        }
        PMIX_INFO_FREE(info, ninfo);
        if (0 != active) {
            exit(active);
        }
    }

    /* initialize the MPI library - it will declare itself */
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (argc <= 1) {
        fprintf(stderr, "Registering handler\n");
        /* register a handler specifically for when models declare */
        active = -1;
        ninfo = 1;
        PMIX_INFO_CREATE(info, ninfo);
        PMIX_INFO_LOAD(&info[0], PMIX_EVENT_HDLR_NAME, "APP-MODEL", PMIX_STRING);

        PMIx_Register_event_handler(&code, 1, info, ninfo,
                                    model_callback, model_registration_callback, (void*)&active);
        while (-1 == active) {
            usleep(10);
        }
        PMIX_INFO_FREE(info, ninfo);
        if (0 != active) {
            exit(active);
        }
    }

    fprintf(stderr, "Rank %d has cleared MPI_Init\n", rank);

    next = (rank + 1) % size;
    prev = (rank + size - 1) % size;
    send_array = malloc(sizeof(int) * SIZE);
    recv_array = malloc(sizeof(int) * SIZE);

    for (i = 0; i < array_size; ++i) {
        send_array[i] = 17;
        recv_array[i] = -1;
    }

    if (0 == rank) {
        send_array[pos] = INITIAL_VALUE;
        MPI_Send(send_array, array_size, MPI_INT, next, tag,
                 MPI_COMM_WORLD);
    }

    /* if we didn't already do it, declare another model now */
    if (argc <= 1) {
        fprintf(stderr, "Declaring ourselves\n");
        /* declare ourselves as a non-MPI library after MPI_Init */
        ninfo = 4;
        PMIX_INFO_CREATE(info, ninfo);
        PMIX_INFO_LOAD(&info[0], PMIX_PROGRAMMING_MODEL, "EXAMPLE", PMIX_STRING);
        PMIX_INFO_LOAD(&info[1], PMIX_MODEL_LIBRARY_NAME, "FOOL", PMIX_STRING);
        PMIX_INFO_LOAD(&info[2], PMIX_MODEL_LIBRARY_VERSION, "1.2.3", PMIX_STRING);
        PMIX_INFO_LOAD(&info[3], PMIX_THREADING_MODEL, "NONE", PMIX_STRING);

        if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, info, ninfo))) {
            fprintf(stderr, "PMIx Init failed: %s\n", PMIx_Error_string(rc));
            exit(1);
        }
        PMIX_INFO_FREE(info, ninfo);
    }

    while (1) {
        recv_array[pos] = -1;
        MPI_Recv(recv_array, array_size, MPI_INT, prev, tag,
                 MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        send_array[pos] = recv_array[pos];
        if (rank == 0) {
            --send_array[pos];
        }
        MPI_Send(send_array, array_size, MPI_INT, next, tag, MPI_COMM_WORLD);
        if (0 == send_array[pos]) {
            break;
        }
    }

    if (rank == 0) {
        MPI_Recv(recv_array, array_size, MPI_INT, prev, tag,
                 MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    }

    fprintf(stderr, "Rank %d has completed ring\n", rank);
    MPI_Barrier(MPI_COMM_WORLD);
    fprintf(stderr, "Rank %d has completed MPI_Barrier\n", rank);

    /* decrement the PMIx refcount */
    PMIx_Finalize(NULL, 0);
    MPI_Finalize();
    return 0;
}
