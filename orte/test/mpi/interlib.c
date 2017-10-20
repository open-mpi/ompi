/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include <pthread.h>

#include "opal/mca/hwloc/base/base.h"
#include "mpi.h"

#include "orte/util/proc_info.h"
#include "opal/mca/pmix/base/base.h"

static size_t interlibhandler_id = SIZE_MAX;
static opal_pmix_lock_t thread_complete;

static void model_registration_callback(int status,
                                        size_t errhandler_ref,
                                        void *cbdata)
{
    opal_pmix_lock_t *lock = (opal_pmix_lock_t*)cbdata;

    interlibhandler_id = errhandler_ref;
    OPAL_PMIX_WAKEUP_THREAD(lock);
}
static void model_callback(int status,
                           const opal_process_name_t *source,
                           opal_list_t *info, opal_list_t *results,
                           opal_pmix_notification_complete_fn_t cbfunc,
                           void *cbdata)
{
    opal_value_t *val;

    /* we can ignore our own callback as we obviously
     * know that we are MPI */
    if (NULL != info) {
        OPAL_LIST_FOREACH(val, info, opal_value_t) {
            if (OPAL_STRING == val->type) {
#if 1
                opal_output(0, "Thread Model Callback Key: %s Val %s", val->key, val->data.string);
#else
                if (0 == strcmp(val->key, OPAL_PMIX_MODEL_LIBRARY_NAME) &&
                    0 == strcmp(val->data.string, "OpenMPI")) {
                    goto cback;
                }
#endif
            }
        }
    }
    /* otherwise, do something clever here */

  cback:
    /* we must NOT tell the event handler state machine that we
     * are the last step as that will prevent it from notifying
     * anyone else that might be listening for declarations */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, NULL, NULL, NULL, cbdata);
    }
    OPAL_PMIX_WAKEUP_THREAD(&thread_complete);
}

static void *mylib(void *ptr)
{
    opal_list_t info, directives;
    opal_value_t *kv;
    int ret;
    opal_pmix_lock_t lock;

    OPAL_PMIX_CONSTRUCT_LOCK(&thread_complete);

    /* declare that we are present and active */
    OBJ_CONSTRUCT(&info, opal_list_t);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_PROGRAMMING_MODEL);
    kv->type = OPAL_STRING;
    kv->data.string = strdup("OpenMP");
    opal_list_append(&info, &kv->super);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_MODEL_LIBRARY_NAME);
    kv->type = OPAL_STRING;
    kv->data.string = strdup("foobar");
    opal_list_append(&info, &kv->super);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_MODEL_LIBRARY_VERSION);
    kv->type = OPAL_STRING;
    kv->data.string = strdup("1.2.3.4");
    opal_list_append(&info, &kv->super);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_THREADING_MODEL);
    kv->type = OPAL_STRING;
    kv->data.string = strdup("PTHREAD");
    opal_list_append(&info, &kv->super);

    /* call pmix to initialize these values */
    ret = opal_pmix.init(&info);
    OPAL_LIST_DESTRUCT(&info);

    /* register to receive model callbacks */

    /* give it a name so we can distinguish it */
    OBJ_CONSTRUCT(&directives, opal_list_t);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_EVENT_HDLR_NAME);
    kv->type = OPAL_STRING;
    kv->data.string = strdup("My-Declarations");
    opal_list_append(&directives, &kv->super);
    /* specify the event code */
    OBJ_CONSTRUCT(&info, opal_list_t);
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup("status");   // the key here is irrelevant
    kv->type = OPAL_INT;
    kv->data.integer = OPAL_ERR_MODEL_DECLARED;
    opal_list_append(&info, &kv->super);
    /* we could constrain the range to proc_local - technically, this
     * isn't required so long as the code that generates
     * the event stipulates its range as proc_local. We rely
     * on that here */
    OPAL_PMIX_CONSTRUCT_LOCK(&lock);
    opal_pmix.register_evhandler(&info, &directives, model_callback,
                                 model_registration_callback,
                                 (void*)&lock);
    OPAL_PMIX_WAIT_THREAD(&lock);
    OPAL_PMIX_DESTRUCT_LOCK(&lock);

    /* wait for the model callback */
    OPAL_PMIX_WAIT_THREAD(&thread_complete);

    /* finalize */
    opal_pmix.finalize();

    /* done */
    return NULL;
}

int main(int argc, char* argv[])
{
    int rank, size, rc;
    hwloc_cpuset_t cpus;
    char *bindings = NULL;
    pid_t pid;
    pthread_t mythread;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    pid = getpid();

    /* spin up a thread */
    if (pthread_create(&mythread, NULL, mylib, NULL)) {
        fprintf(stderr, "Error creating thread\n");
        goto done;
    }

    printf("[%lu] Rank %d: getting topology\n", (unsigned long)pid, rank);
    fflush(stdout);
    if (OPAL_SUCCESS == opal_hwloc_base_get_topology()) {
        cpus = hwloc_bitmap_alloc();
        rc = hwloc_get_cpubind(opal_hwloc_topology, cpus, HWLOC_CPUBIND_PROCESS);
        hwloc_bitmap_list_asprintf(&bindings, cpus);
    }

    printf("Hello, World, I am %d of %d [%d local peers]: get_cpubind: %d bitmap %s\n",
           rank, size, orte_process_info.num_local_peers, rc,
           (NULL == bindings) ? "NULL" : bindings);

    /* wait for the thread to finish */
    if (pthread_join(mythread, NULL)) {
        fprintf(stderr, "Error joining thread\n");
    }

  done:
    MPI_Finalize();
    return 0;
}
