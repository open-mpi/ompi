/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <pmix_tool.h>
#include "examples.h"

static pmix_proc_t myproc;


static void notification_fn(size_t evhdlr_registration_id,
                            pmix_status_t status,
                            const pmix_proc_t *source,
                            pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc,
                            void *cbdata)
{
    myrel_t *lock = NULL;
    size_t n;
    pmix_status_t jobstatus = 0;
    pmix_proc_t affected;
    char *msg = NULL;

    memset(&affected, 0, sizeof(pmix_proc_t));

    /* we should always have info returned to us - if not, there is
     * nothing we can do */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strncmp(info[n].key, PMIX_JOB_TERM_STATUS, PMIX_MAX_KEYLEN)) {
                jobstatus = info[n].value.data.status;
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_AFFECTED_PROC, PMIX_MAX_KEYLEN)) {
                memcpy(&affected, info[n].value.data.proc, sizeof(pmix_proc_t));
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
                lock = (myrel_t*)info[n].value.data.ptr;
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_TEXT_MESSAGE, PMIX_MAX_KEYLEN)) {
                msg = info[n].value.data.string;
            }
        }
    }
    if (NULL == lock) {
        fprintf(stderr, "LOCK WAS NOT RETURNED IN EVENT NOTIFICATION\n");
        goto done;
    }
    /* save the status */
    lock->lock.status = jobstatus;
    if (NULL != msg) {
        lock->nspace = strdup(msg);
    }
    /* release the lock */
    DEBUG_WAKEUP_THREAD(&lock->lock);

  done:
    /* we _always_ have to execute the evhandler callback or
     * else the event progress engine will hang */
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
static void evhandler_reg_callbk(pmix_status_t status,
                                 size_t evhandler_ref,
                                 void *cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                   myproc.nspace, myproc.rank, status, (unsigned long)evhandler_ref);
    }
    lock->status = status;
    lock->evhandler_ref = evhandler_ref;
    DEBUG_WAKEUP_THREAD(lock);
}


int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_info_t info;
    pmix_app_t *app;
    size_t ninfo, napps;
    bool flag;
    myrel_t myrel;
    mylock_t mylock;
    pmix_status_t code[6] = {PMIX_ERR_PROC_ABORTING, PMIX_ERR_PROC_ABORTED,
                             PMIX_ERR_PROC_REQUESTED_ABORT, PMIX_ERR_JOB_TERMINATED,
                             PMIX_ERR_UNREACH, PMIX_ERR_LOST_CONNECTION_TO_SERVER};
    pmix_nspace_t appspace;

    /* we need to attach to a "system" PMIx server so we
     * can ask it to spawn applications for us. There can
     * only be one such connection on a node, so we will
     * instruct the tool library to only look for it */
    flag = true;
    PMIX_INFO_LOAD(&info, PMIX_CONNECT_TO_SYSTEM, &flag, PMIX_BOOL);

    /* initialize the library and make the connection */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, &info, 1))) {
        fprintf(stderr, "PMIx_tool_init failed: %d\n", rc);
        exit(rc);
    }

    DEBUG_CONSTRUCT_MYREL(&myrel);

    /* register an event handler so we can be notified when
     * our spawned job completes, or if it fails (even at launch) */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIX_INFO_LOAD(&info, PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
    PMIx_Register_event_handler(code, 6, &info, 1,
                                notification_fn, evhandler_reg_callbk, (void*)&mylock);
    DEBUG_WAIT_THREAD(&mylock);
    rc = mylock.status;
    DEBUG_DESTRUCT_LOCK(&mylock);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "[%s:%d] Default handler registration failed\n", myproc.nspace, myproc.rank);
        goto done;
    }

    /* parse the cmd line and create our array of app structs
     * describing the application we want launched */
    napps = 1;
    PMIX_APP_CREATE(app, napps);
    /* setup the executable */
    app[0].cmd = strdup("app");
    app[0].argv = (char**)malloc(2*sizeof(char*));
    app[0].argv[0] = strdup("app");
    app[0].argv[1] = NULL;
    app[0].maxprocs = 128;
    /* can also provide environmental params in the app.env field */

    /* provide directives so the apps do what the user requested - just
     * some random examples provided here*/
    app[0].ninfo = 2;
    PMIX_INFO_CREATE(app[0].info, app[0].ninfo);
    PMIX_INFO_LOAD(&app[0].info[0], PMIX_MAPBY, "slot", PMIX_STRING);
    /* include a directive that we be notified upon completion of the job */
    PMIX_INFO_LOAD(&app[0].info[1], PMIX_NOTIFY_COMPLETION, &flag, PMIX_BOOL);

    /* spawn the application */
    PMIx_Spawn(NULL, 0, app, napps, appspace);
    /* cleanup */
    PMIX_APP_FREE(app, napps);

    DEBUG_WAIT_THREAD(&myrel.lock);
    DEBUG_DESTRUCT_MYREL(&myrel);

  done:
    PMIx_tool_finalize();

    return(0);
}
