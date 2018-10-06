/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include "orte_config.h"

#include <stdio.h>
#include "mpi.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/argv.h"
#include "opal/util/printf.h"
#include "orte/runtime/runtime.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

static int rank, size;
static volatile bool wait_for_release = true;
#define MEMPROBE_RELEASE 12345

static void _release_fn(int status,
                        const opal_process_name_t *source,
                        opal_list_t *info, opal_list_t *results,
                        opal_pmix_notification_complete_fn_t cbfunc,
                        void *cbdata)
{
    /* must let the notifier know we are done */
    if (NULL != cbfunc) {
        cbfunc(OPAL_ERR_HANDLERS_COMPLETE, NULL, NULL, NULL, cbdata);
    }
    /* flag that the debugger is complete so we can exit */
    wait_for_release = false;
}

static void _register_fn(int status,
                         size_t evhandler_ref,
                         void *cbdata)
{
    volatile int *active = (volatile int*)cbdata;

    if (0 != status) {
        fprintf(stderr, "Client EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                   status, (unsigned long)evhandler_ref);
    }
    *active = status;
}

static void qcbfunc(int status,
                    opal_list_t *info,
                    void *cbdata,
                    opal_pmix_release_cbfunc_t release_fn,
                    void *release_cbdata)
{
    opal_list_t *results = (opal_list_t*)cbdata;
    opal_value_t *kv;

    if (NULL != info) {
        while (NULL != (kv = (opal_value_t*)opal_list_remove_first(info))) {
            opal_list_append(results, &kv->super);
        }
    }
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    wait_for_release = false;
}

static void notifycbfunc(int status, void *cbdata)
{
    volatile int *active = (volatile int*)cbdata;
    *active = status;
}

static void sample(void)
{
    opal_value_t *kv, *ival;
    opal_pmix_query_t *q;
    opal_list_t query, response, *lt;
    volatile int active;
    char **answer = NULL, *tmp, *msg;

    OBJ_CONSTRUCT(&query, opal_list_t);
    OBJ_CONSTRUCT(&response, opal_list_t);
    q = OBJ_NEW(opal_pmix_query_t);
    opal_list_append(&query, &q->super);
    opal_argv_append_nosize(&q->keys, OPAL_PMIX_QUERY_MEMORY_USAGE);
    /* qualify that we just want local avg, min/max values reported */
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_QUERY_LOCAL_ONLY);
    kv->type = OPAL_BOOL;
    kv->data.flag = true;
    opal_list_append(&q->qualifiers, &kv->super);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_QUERY_REPORT_AVG);
    kv->type = OPAL_BOOL;
    kv->data.flag = true;
    opal_list_append(&q->qualifiers, &kv->super);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_QUERY_REPORT_MINMAX);
    kv->type = OPAL_BOOL;
    kv->data.flag = true;
    opal_list_append(&q->qualifiers, &kv->super);
    /* issue the request */
    wait_for_release = true;
    opal_pmix.query(&query, qcbfunc, (void*)&response);
    /* wait for the query to complete */
    while (wait_for_release) {
        usleep(10);
    }
    wait_for_release = true;
    /* log my own results as a single string so the output
     * doesn't get garbled on the other end */
    opal_asprintf(&tmp, "Data for node %s", orte_process_info.nodename);
    opal_argv_append_nosize(&answer, tmp);
    free(tmp);
    OPAL_LIST_FOREACH(kv, &response, opal_value_t) {
        lt = (opal_list_t*)kv->data.ptr;
        if (NULL != lt) {
            OPAL_LIST_FOREACH(ival, lt, opal_value_t) {
                if (0 == strcmp(ival->key, OPAL_PMIX_DAEMON_MEMORY)) {
                    opal_asprintf(&tmp, "\tDaemon: %f", ival->data.fval);
                    opal_argv_append_nosize(&answer, tmp);
                    free(tmp);
                } else if (0 == strcmp(ival->key, OPAL_PMIX_CLIENT_AVG_MEMORY)) {
                    opal_asprintf(&tmp, "\tClient: %f", ival->data.fval);
                    opal_argv_append_nosize(&answer, tmp);
                    free(tmp);
                } else {
                    fprintf(stderr, "\tUnknown key: %s", ival->key);
                }
            }
        }
    }
    opal_argv_append_nosize(&answer, "\n");
    OPAL_LIST_DESTRUCT(&response);

    /* construct the log output */
    OBJ_CONSTRUCT(&response, opal_list_t);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_LOG_STDOUT);
    kv->type = OPAL_STRING;
    kv->data.string = opal_argv_join(answer, '\n');
    opal_list_append(&response, &kv->super);
    opal_argv_free(answer);
    active = -1;
    opal_pmix.log(&response, notifycbfunc, (void*)&active);
    while (-1 == active) {
        usleep(10);
    }
    OPAL_LIST_DESTRUCT(&response);

    if (0 == rank) {
        /* send the notification to release the other procs */
        wait_for_release = true;
        OBJ_CONSTRUCT(&response, opal_list_t);
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_EVENT_NON_DEFAULT);
        kv->type = OPAL_BOOL;
        kv->data.flag = true;
        opal_list_append(&response, &kv->super);
        active = -1;
        if (OPAL_SUCCESS != opal_pmix.notify_event(MEMPROBE_RELEASE, NULL,
                                                   OPAL_PMIX_RANGE_GLOBAL, &response,
                                                   NULL, NULL)) {
            fprintf(stderr, "Notify event failed\n");
            exit(1);
        }
    } else {
        /* now wait for notification */
        while (wait_for_release) {
            usleep(10);
        }
    }
}

int main(int argc, char* argv[])
{
    opal_list_t *codes;
    opal_value_t *kv;
    volatile int active;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (0 == rank) {
        fprintf(stderr, "Sampling memory usage after MPI_Init\n");
    }

    /* everyone registers their event handler */
    codes = OBJ_NEW(opal_list_t);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup("errorcode");
    kv->type = OPAL_INT;
    kv->data.integer = MEMPROBE_RELEASE;
    opal_list_append(codes, &kv->super);

    active = -1;
    opal_pmix.register_evhandler(codes, NULL, _release_fn, _register_fn, (void*)&active);
    while (-1 == active) {
        usleep(10);
    }

    /* if I am the local leader (i.e., local_rank=0), then I ask
     * my daemon to report the local memory usage, and send it
     * to rank=0 */
    if (0 == orte_process_info.my_local_rank) {
        sample();
    } else {
        /* now wait for notification */
        while (wait_for_release) {
            usleep(10);
        }
    }
    wait_for_release = true;

    /* perform a barrier so some communication will occur, thus
     * requiring exchange of endpoint info */
    MPI_Barrier(MPI_COMM_WORLD);

    if (0 == rank) {
        fprintf(stderr, "\n\nSampling memory usage after MPI_Barrier\n");
    }

    if (0 == orte_process_info.my_local_rank) {
        if (0 != rank) {
            /* wait a little */
            usleep(1000);
        }
        sample();
    } else {
        /* wait again while memory is sampled */
        while (wait_for_release) {
            usleep(10);
        }
    }

    MPI_Finalize();
    return 0;
}
