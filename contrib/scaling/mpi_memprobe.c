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
#include "orte/runtime/runtime.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

static int rank, size;
static volatile int active;
static volatile bool wait_for_release = true;
#define MEMPROBE_RELEASE 12345

static void _release_fn(int status,
                        const opal_process_name_t *source,
                        opal_list_t *info, opal_list_t *results,
                        opal_pmix_notification_complete_fn_t cbfunc,
                        void *cbdata)
{
    fprintf(stderr, "Rank %d: Release recvd\n", rank);
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

    fprintf(stderr, "Rank %d: Query returned status %d\n", rank, status);
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

int main(int argc, char* argv[])
{
    opal_list_t *codes;
    opal_value_t *kv;
    opal_pmix_query_t *q;
    opal_list_t query, response;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

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

    /* rank 0 asks for memory to be sampled, while everyone else waits */
    if (0 == rank) {
        fprintf(stderr, "Sampling memory usage after MPI_Init\n");
        OBJ_CONSTRUCT(&query, opal_list_t);
        OBJ_CONSTRUCT(&response, opal_list_t);
        q = OBJ_NEW(opal_pmix_query_t);
        opal_list_append(&query, &q->super);
        opal_argv_append_nosize(&q->keys, OPAL_PMIX_QUERY_MEMORY_USAGE);
        /* qualify that we just want avg, min/max values reported */
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
        while (wait_for_release) {
            usleep(10);
        }
        /* output the results */
        OPAL_LIST_FOREACH(kv, &response, opal_value_t) {
            fprintf(stderr, "\tResults: %s\n", kv->key);
        }
        OPAL_LIST_DESTRUCT(&response);
        /* send the notification to release the other procs */
        wait_for_release = true;
        OBJ_CONSTRUCT(&response, opal_list_t);
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_EVENT_NON_DEFAULT);
        kv->type = OPAL_BOOL;
        kv->data.flag = true;
        opal_list_append(&response, &kv->super);
        if (OPAL_SUCCESS != opal_pmix.notify_event(MEMPROBE_RELEASE, NULL,
                                                   OPAL_PMIX_RANGE_GLOBAL, &response,
                                                   NULL, NULL)) {
            fprintf(stderr, "Notify event failed\n");
            exit(1);
        }
        while (wait_for_release) {
            usleep(10);
        }
        OPAL_LIST_DESTRUCT(&response);
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
        OBJ_CONSTRUCT(&query, opal_list_t);
        OBJ_CONSTRUCT(&response, opal_list_t);
        q = OBJ_NEW(opal_pmix_query_t);
        opal_list_append(&query, &q->super);
        opal_argv_append_nosize(&q->keys, OPAL_PMIX_QUERY_MEMORY_USAGE);
        /* qualify that we just want avg, min/max values reported */
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
        while (wait_for_release) {
            usleep(10);
        }
        /* output the results */
        OPAL_LIST_FOREACH(kv, &response, opal_value_t) {
            fprintf(stderr, "\tResults: %s\n", kv->key);
        }
        OPAL_LIST_DESTRUCT(&response);
        /* send the notification to release the other procs */
        wait_for_release = true;
        OBJ_CONSTRUCT(&response, opal_list_t);
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_EVENT_NON_DEFAULT);
        kv->type = OPAL_BOOL;
        kv->data.flag = true;
        opal_list_append(&response, &kv->super);
        if (OPAL_SUCCESS != opal_pmix.notify_event(MEMPROBE_RELEASE, NULL,
                                                   OPAL_PMIX_RANGE_GLOBAL, &response,
                                                   NULL, NULL)) {
            fprintf(stderr, "Notify event failed\n");
            exit(1);
        }
        while (wait_for_release) {
            usleep(10);
        }
        OPAL_LIST_DESTRUCT(&response);
    } else {
        /* wait again while memory is sampled */
        while (wait_for_release) {
            usleep(10);
        }
    }

    fprintf(stderr, "%d: FINALIZING\n", rank);
    fflush(stderr);
    MPI_Finalize();
    return 0;
}
