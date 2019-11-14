/* -*- C -*-
 *
 * $HEADER$
 *
 */

#include "orte_config.h"

#include <stdio.h>
#include <unistd.h>

#include "opal/runtime/opal.h"
#include "opal/mca/pmix/pmix.h"
#include "orte/runtime/runtime.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

static pid_t pid;
static char hostname[OPAL_MAXHOSTNAMELEN];

static void notification_fn(int status,
                            const opal_process_name_t *source,
                            opal_list_t *info, opal_list_t *results,
                            opal_pmix_notification_complete_fn_t cbfunc,
                            void *cbdata)
 {
    int peer_rank;

    fprintf(stderr, "orte_notify: Name %s Host: %s Pid %ld status %d source %s\n",
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
            hostname, (long)pid, status, ORTE_NAME_PRINT(source));

    /** let the notifier know we are done */
    if (cbfunc) {
       cbfunc(OPAL_ERR_HANDLERS_COMPLETE, NULL, NULL, NULL, cbdata);
    }

}

static void errhandler_reg_callbk(int status,
                                  size_t evhdlr_ref,
                                  void *cbdata)
{
    return;
}

int main(int argc, char* argv[])
{
    int rc;
    opal_value_t *kv;
    opal_list_t info;
    const char *local_hostname;

    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_abort: couldn't init orte - error code %d\n", rc);
        return rc;
    }
    pid = getpid();

    /* Because hostname variable is static, we need to copy it from another variable that gets the
       return of opal_gethostname. Truncate it if it is longer than OPAL_MAXHOSTNAMELEN. 
     */
    local_hostname = opal_gethostname();
    strncpy(hostname, local_hostname, OPAL_MAXHOSTNAMELEN - 1);
    local_hostname[OPAL_MAXHOSTNAMELEN - 1] = '\0';

    printf("orte_notify: Name %s Host: %s Pid %ld\n",
           ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
           hostname, (long)pid);
    fflush(stdout);

    /* register the event handler */
    OBJ_CONSTRUCT(&info, opal_list_t);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_EVENT_ORDER_PREPEND);
    kv->type = OPAL_BOOL;
    kv->data.flag = true;
    opal_list_append(&info, &kv->super);

    opal_pmix.register_evhandler(NULL, &info,
                                notification_fn,
                                NULL, NULL);

    while (1) {
        usleep(100);
    }

    return 0;
}
