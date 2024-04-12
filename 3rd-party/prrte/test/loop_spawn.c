/*file .c : spawned  the file Exe*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>
#include <errno.h>
#include <getopt.h>

#include <pmix.h>

#include "test.h"

#define     EXE_TEST             "./loop_child"

static bool verbose = false;

static void regcbfunc(pmix_status_t status, size_t ref, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;

    DEBUG_WAKEUP_THREAD(lock);
}

static void evhandler(size_t evhdlr_registration_id, pmix_status_t status,
                      const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                      pmix_info_t *results, size_t nresults,
                      pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    mylock_t *lock = NULL;
    pmix_status_t jobstatus = 0;
    pmix_nspace_t jobid = {0};
    size_t n;
    char *msg = NULL;

    /* we should always have info returned to us - if not, there is
     * nothing we can do */
    if (NULL != info) {
        for (n = 0; n < ninfo; n++) {
            if (0 == strncmp(info[n].key, PMIX_JOB_TERM_STATUS, PMIX_MAX_KEYLEN)) {
                jobstatus = info[n].value.data.status;
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_AFFECTED_PROC, PMIX_MAX_KEYLEN)) {
                PMIX_LOAD_NSPACE(jobid, info[n].value.data.proc->nspace);
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
                lock = (mylock_t *) info[n].value.data.ptr;
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_TEXT_MESSAGE, PMIX_MAX_KEYLEN)) {
                msg = info[n].value.data.string;
            }
        }
        if (verbose) {
            fprintf(stdout, "JOB %s COMPLETED WITH STATUS %d MSG %s\n",
                    jobid, jobstatus,  (NULL == msg) ? "NONE" : msg);
        }
    }
    if (NULL != lock) {
        /* save the status */
        lock->status = jobstatus;
        if (NULL != msg) {
            lock->msg = strdup(msg);
        }
        /* release the lock */
        DEBUG_WAKEUP_THREAD(lock);
    }

    /* we _always_ have to execute the evhandler callback or
     * else the event progress engine will hang */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
}

int main(int argc, char **argv)
{
    int iter, itermax = 100;
    bool sync = false;
    pmix_status_t rc;
    pmix_proc_t myproc;
    pmix_proc_t procs[2];
    pmix_app_t app;
    pmix_nspace_t nspace;
    pmix_proc_t pname;
    mylock_t lock, rellock;
    pmix_info_t iptr[3];
    static struct option myoptions[] = {
        {"iters", required_argument, NULL, 'i'},
        {"sync", no_argument, NULL, 's'},
        {"verbose", no_argument, NULL, 'v'},
        {"help", no_argument, NULL, 'h'},
        {"report", required_argument, NULL, 'r'},
        {0, 0, 0, 0}
    };
    int option_index;
    int opt;
    int freq = 100;

    while ((opt = getopt_long(argc, argv, "hsvi:r:", myoptions, &option_index)) != -1) {
        switch (opt) {
            case 'i':
                itermax = strtol(optarg, NULL, 10);
                break;
            case 's':
                sync = true;
                break;
            case 'v':
                verbose = true;
                break;
            case 'r':
                freq = strtol(optarg, NULL, 10);
                break;
            case 'h':
                fprintf(stderr,
                        "Usage: %s\n    Options:\n"
                        "        [-i N] [number of iterations]\n"
                        "        [-s] [Sync mode - wait for termination before spawning next child]\n"
                        "        [-v] [Verbose]\n"
                        "        [-r N] [Report progress every N iterations]\n",
                        argv[0]);
                exit(1);
            default:
                fprintf(stderr,
                        "Usage: %s\n    Options:\n"
                        "        [-i N] [number of iterations]\n"
                        "        [-s] [Sync mode - wait for termination before spawning next child]\n"
                        "        [-v] [Verbose]\n"
                        "        [-r N] [Report progress every N iterations]\n",
                        argv[0]);
                exit(1);
        }
    }

    if (verbose) {
        printf("parent*******************************\n");
        printf("parent: Launching Child*\n");
    }


    rc = PMIx_Init(&myproc, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Failed to init\n");
        exit(1);
    }

    PMIX_APP_CONSTRUCT(&app);
    app.cmd = strdup(EXE_TEST);
    PMIX_ARGV_APPEND(rc, app.argv, EXE_TEST);
    if (verbose) {
        PMIX_ARGV_APPEND(rc, app.argv, "--verbose");
    }
    app.maxprocs = 1;

    PMIX_XFER_PROCID(&procs[0], &myproc);

    for (iter = 0; iter < itermax; ++iter) {
        rc = PMIx_Spawn(NULL, 0, &app, 1, nspace);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "Failed to spawn iteration %d: %s\n", iter, PMIx_Error_string(rc));
            goto done;
        }
        if (sync) {
            DEBUG_CONSTRUCT_LOCK(&rellock);
            rc = PMIX_EVENT_JOB_END;
            /* give the handler a name */
            PMIX_INFO_LOAD(&iptr[0], PMIX_EVENT_HDLR_NAME, "JOB_TERMINATION_EVENT", PMIX_STRING);
            /* specify we only want to be notified when our
             * child job terminates */
            PMIX_LOAD_PROCID(&pname, nspace, PMIX_RANK_WILDCARD);
            PMIX_INFO_LOAD(&iptr[1], PMIX_EVENT_AFFECTED_PROC, &pname, PMIX_PROC);
            /* request that they return our lock object */
            PMIX_INFO_LOAD(&iptr[2], PMIX_EVENT_RETURN_OBJECT, &rellock, PMIX_POINTER);
            /* do the registration */
            DEBUG_CONSTRUCT_LOCK(&lock);
            PMIx_Register_event_handler(&rc, 1, iptr, 3, evhandler, regcbfunc, &lock);
            DEBUG_WAIT_THREAD(&lock);
            DEBUG_DESTRUCT_LOCK(&lock);
            PMIX_INFO_DESTRUCT(&iptr[0]);
            PMIX_INFO_DESTRUCT(&iptr[1]);
            PMIX_INFO_DESTRUCT(&iptr[2]);
        }

        PMIX_LOAD_PROCID(&procs[1], nspace, 0);
        rc = PMIx_Connect(procs, 2, NULL, 0);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "[%s.%u]: Failed to connect\n", myproc.nspace, myproc.rank);
            exit(1);
        }

        PMIx_Disconnect(procs, 2, NULL, 0);

        if (sync) {
            DEBUG_WAIT_THREAD(&rellock);
            DEBUG_DESTRUCT_LOCK(&rellock);
        }
        if (!verbose && 0 == (iter % freq)) {
            fprintf(stderr, "Completed iteration %d\n", iter);
        }
    }

done:
    PMIx_Finalize(NULL, 0);
    if (verbose) {
        printf("parent: End .\n" );
    }
    return 0;
}
