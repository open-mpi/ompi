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

static volatile int active;
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
        cbfunc(0, NULL, NULL, NULL, cbdata);
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

int main(int argc, char* argv[])
{
    int rank, size;
    opal_list_t *codes;
    opal_value_t *kv;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (0 == rank) {
        fprintf(stderr, "Sampling memory usage after MPI_Init\n");
    }

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

    /* now wait for notification */
    while (wait_for_release) {
        usleep(10);
    }
    wait_for_release = true;

    /* perform a barrier so some communication will occur, thus
     * requiring exchange of endpoint info */
    MPI_Barrier(MPI_COMM_WORLD);

    if (0 == rank) {
        fprintf(stderr, "\n\nSampling memory usage after MPI_Barrier\n");
    }

    /* wait again while memory is sampled */
    while (wait_for_release) {
        usleep(10);
    }

    MPI_Finalize();
    return 0;
}
