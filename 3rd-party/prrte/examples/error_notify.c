/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include <pmix.h>

static pmix_proc_t myproc;
static bool completed;
struct timeval start, end;
double sec;

static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    gettimeofday(&end, NULL);
    fprintf(stderr, "New notification comes\n");
    if ((info[0].value.data.proc != NULL)
        && strcmp(info[0].value.data.proc->nspace, myproc.nspace) == 0) {
        fprintf(stderr, "Client %s:%d NOTIFIED with status %d and error proc %s:%d key %s \n",
                myproc.nspace, myproc.rank, status, info[0].value.data.proc->nspace,
                info[0].value.data.proc->rank, info[0].key);
        completed = true;
        if (NULL != cbfunc) {
            cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
        }
    } else
        fprintf(stderr, "Not from my namespace \n");
}

static void op_callbk(pmix_status_t status, void *cbdata)
{
    fprintf(stderr, "Client %s:%d OP CALLBACK CALLED WITH STATUS %d\n", myproc.nspace, myproc.rank,
            status);
}

static void errhandler_reg_callbk(pmix_status_t status, size_t errhandler_ref, void *cbdata)
{
    fprintf(stderr,
            "Client %s:%d ERRHANDLER REGISTRATION CALLBACK CALLED WITH STATUS %d, ref=%lu\n",
            myproc.nspace, myproc.rank, status, (unsigned long) errhandler_ref);
}

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_proc_t proc;
    uint32_t nprocs;
    pid_t pid;

    char name[255];

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        exit(0);
    }
    gethostname(name, 255);
    fprintf(stderr, "%s Client ns %s rank %d: Running\n", name, myproc.nspace, myproc.rank);

    PMIX_PROC_CONSTRUCT(&proc);
    (void) strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    /* get our universe size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get universe size failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    // fprintf(stderr, "Client %s:%d universe size %d\n", myproc.nspace, myproc.rank, nprocs);
    completed = false;

    pmix_status_t status;
    status = PMIX_ERR_PROC_ABORTED;
    /* register our errhandler */
    PMIx_Register_event_handler(&status, 1, NULL, 0, notification_fn, errhandler_reg_callbk, NULL);

    /* call fence to sync */
    PMIX_PROC_CONSTRUCT(&proc);
    (void) strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    gethostname(name, 255);
    sleep(3);
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        goto done;
    }
    gettimeofday(&start, NULL);
    if (myproc.rank == 2) // || myproc.rank == 4)
    {
        fprintf(stderr, "\nClient ns %s:%d kill self \n", myproc.nspace, myproc.rank);
        completed = true;
        pid = getpid();
        kill(pid, 1);
    }
    while (!completed) {
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }
done:
    /* finalize us */
    sec = end.tv_sec + (double) end.tv_usec / 1000000.0 - start.tv_sec
          - (double) start.tv_usec / 1000000.0;
    fprintf(stderr, "Client ns %s rank %d takes %f: Finalizing\n", myproc.nspace, myproc.rank, sec);
    PMIx_Deregister_event_handler(1, op_callbk, NULL);

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace,
                myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n",
                myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return (0);
}
