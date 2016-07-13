/* -*- C -*-
 *
 * $HEADER$
 *
 * A program that just spins - provides mechanism for testing user-driven
 * abnormal program termination
 */

#include <stdio.h>
#include "mpi.h"

#include "opal/dss/dss.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/output.h"
#include "orte/util/name_fns.h"
#include "orte/constants.h"

static volatile bool register_active = false;

static void _event_fn(int status,
                      const opal_process_name_t *source,
                      opal_list_t *info, opal_list_t *results,
                      opal_pmix_notification_complete_fn_t cbfunc,
                      void *cbdata)
{
    opal_value_t *kv;
    orte_process_name_t proc;

    /* the name of the terminating proc should be on the info list */
    proc.jobid = ORTE_JOBID_INVALID;
    proc.vpid = ORTE_VPID_INVALID;
    OPAL_LIST_FOREACH(kv, info, opal_value_t) {
        if (0 == strcmp(kv->key, OPAL_PMIX_EVENT_AFFECTED_PROC)) {
            proc.jobid = kv->data.name.jobid;
            proc.vpid = kv->data.name.vpid;
            break;
        }
    }

    opal_output(0, "NOTIFIED OF TERMINATION OF PROC %s",
                ORTE_NAME_PRINT(&proc));

    /* must let the notifier know we are done */
    if (NULL != cbfunc) {
        cbfunc(ORTE_SUCCESS, NULL, NULL, NULL, cbdata);
    }
}

static void _register_fn(int status,
                         size_t evhandler_ref,
                         void *cbdata)
{
    opal_list_t *codes = (opal_list_t*)cbdata;

    OPAL_LIST_RELEASE(codes);
    register_active = false;
}


int main(int argc, char* argv[])
{

    int i;
    double pi;
    opal_list_t *codes;
    opal_value_t *kv;

    MPI_Init(&argc, &argv);

    /* register an event handler for the OPAL_ERR_PROC_ABORTED event */
    codes = OBJ_NEW(opal_list_t);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup("errorcode");
    kv->type = OPAL_INT;
    kv->data.integer = OPAL_ERR_PROC_ABORTED;
    opal_list_append(codes, &kv->super);

    register_active = true;
    opal_pmix.register_evhandler(codes, NULL, _event_fn, _register_fn, codes);

    i = 0;
    while (1) {
        i++;
        pi = i / 3.14159256;
        if (i > 100) i = 0;
    }

    MPI_Finalize();

    return 0;
}
