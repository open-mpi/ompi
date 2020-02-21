/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include <pthread.h>
#include "mpi.h"
#include "pmix.h"

typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    volatile bool active;
    pmix_status_t status;
} mylock_t;

#define MY_CONSTRUCT_LOCK(l)                     \
    do {                                            \
        pthread_mutex_init(&(l)->mutex, NULL);      \
        pthread_cond_init(&(l)->cond, NULL);        \
        (l)->active = true;                         \
        (l)->status = PMIX_SUCCESS;                 \
    } while(0)

#define MY_DESTRUCT_LOCK(l)              \
    do {                                    \
        pthread_mutex_destroy(&(l)->mutex); \
        pthread_cond_destroy(&(l)->cond);   \
    } while(0)

#define MY_WAIT_THREAD(lck)                                      \
    do {                                                            \
        pthread_mutex_lock(&(lck)->mutex);                          \
        while ((lck)->active) {                                     \
            pthread_cond_wait(&(lck)->cond, &(lck)->mutex);         \
        }                                                           \
        pthread_mutex_unlock(&(lck)->mutex);                        \
    } while(0)

#define MY_WAKEUP_THREAD(lck)                        \
    do {                                                \
        pthread_mutex_lock(&(lck)->mutex);              \
        (lck)->active = false;                          \
        pthread_cond_broadcast(&(lck)->cond);           \
        pthread_mutex_unlock(&(lck)->mutex);            \
    } while(0)


static size_t interlibhandler_id = SIZE_MAX;
static mylock_t thread_complete;
static pmix_proc_t myproc;

static void model_registration_callback(pmix_status_t status,
                                        size_t errhandler_ref,
                                        void *cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;

    interlibhandler_id = errhandler_ref;
    MY_WAKEUP_THREAD(lock);
}
static void model_callback(size_t evhdlr_registration_id,
                           pmix_status_t status,
                           const pmix_proc_t *source,
                           pmix_info_t info[], size_t ninfo,
                           pmix_info_t *results, size_t nresults,
                           pmix_event_notification_cbfunc_fn_t cbfunc,
                           void *cbdata)
{
    size_t n;

    /* we can ignore our own callback as we obviously
     * know that we are OpenMP */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strcmp(info[n].key, PMIX_PROGRAMMING_MODEL) &&
                0 == strcmp(info[n].value.data.string, "OpenMP")) {
                goto cback;
            }
            if (PMIX_STRING == info[n].value.type) {
                fprintf(stderr, "Thread Model Callback Key: %s Val %s\n", info[n].key, info[n].value.data.string);
            }
        }
    }
    /* otherwise, do something clever here */

  cback:
    /* we must NOT tell the event handler state machine that we
     * are the last step as that will prevent it from notifying
     * anyone else that might be listening for declarations */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
    MY_WAKEUP_THREAD(&thread_complete);
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;
    MY_WAKEUP_THREAD(lock);
}

static void infocb(pmix_status_t status,
                   pmix_info_t *info, size_t ninfo,
                   void *cbdata,
                   pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;
    size_t n;

    for (n=0; n < ninfo; n++) {
        fprintf(stderr, "QUERY DATA KEY: %s VALUE %s\n", info[n].key, info[n].value.data.string);
    }
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    MY_WAKEUP_THREAD(lock);
}

static void *mylib(void *ptr)
{
    pmix_info_t *info, *directives;
    pmix_status_t ret;
    mylock_t lock;
    bool init = false, flag;
    pmix_query_t *query;
    pmix_pdata_t *pdata;
    pmix_status_t code = PMIX_MODEL_DECLARED;
    pmix_value_t *val;
    int wait = 0;

    MY_CONSTRUCT_LOCK(&thread_complete);

    /* declare that we are present and active */
    PMIX_INFO_CREATE(info, 5);
    PMIX_INFO_LOAD(&info[0], PMIX_PROGRAMMING_MODEL, "OpenMP", PMIX_STRING);
    PMIX_INFO_LOAD(&info[1], PMIX_MODEL_LIBRARY_NAME, "foobar", PMIX_STRING);
    PMIX_INFO_LOAD(&info[2], PMIX_MODEL_LIBRARY_VERSION, "1.2.3.4", PMIX_STRING);
    PMIX_INFO_LOAD(&info[3], PMIX_THREADING_MODEL, "PTHREAD", PMIX_STRING);
    /* mark that this isn't to go to any default event handler - pmix_init
     * takes care of that for us, but we have to explicitly do it here */
    flag = true;
    PMIX_INFO_LOAD(&info[4], PMIX_EVENT_NON_DEFAULT, &flag, PMIX_BOOL);

    /* see if pmix is already initialized - note that if we
     * don't know our process identifier at this point (e.g.,
     * we don't store it in some global location), then we
     * could always call PMIx_Init anyway as it is just
     * reference counted. */
    if (PMIx_Initialized()) {
        /* it is, so let's just use the event notification
         * API to let everyone know we are here */
        MY_CONSTRUCT_LOCK(&lock);
        ret = PMIx_Notify_event(code, &myproc,
                                PMIX_RANGE_PROC_LOCAL,
                                info, 5,
                                opcbfunc, (void*)&lock);
        MY_WAIT_THREAD(&lock);
        MY_DESTRUCT_LOCK(&lock);
    } else {
        /* call pmix to initialize these values */
        ret = PMIx_Init(&myproc, info, 5);
        init = true;
    }
    PMIX_INFO_FREE(info, 5);

    /* register to receive model callbacks */
    PMIX_INFO_CREATE(directives, 1);
    /* give the event a name so we can distinguish it */
    PMIX_INFO_LOAD(&directives[0], PMIX_EVENT_HDLR_NAME, "My-Declarations", PMIX_STRING);

    /* we could constrain the range to proc_local - technically, this
     * isn't required so long as the code that generates
     * the event stipulates its range as proc_local. We rely
     * on that here */
    MY_CONSTRUCT_LOCK(&lock);
    PMIx_Register_event_handler(&code, 1, directives, 1,
                                model_callback,
                                model_registration_callback,
                                (void*)&lock);
    MY_WAIT_THREAD(&lock);
    MY_DESTRUCT_LOCK(&lock);
    PMIX_INFO_FREE(directives, 1);

    /* wait for the model callback */
    MY_WAIT_THREAD(&thread_complete);

    /* let's do a couple of operations just to verify we can,
     * starting with a query */
    PMIX_QUERY_CREATE(query, 1);
    PMIX_ARGV_APPEND(ret, query->keys, PMIX_QUERY_NAMESPACES);

    MY_CONSTRUCT_LOCK(&lock);
    PMIx_Query_info_nb(query, 1, infocb, &lock);
    MY_WAIT_THREAD(&lock);
    MY_DESTRUCT_LOCK(&lock);
    PMIX_QUERY_FREE(query, 1);

    /* Get something */
    val = NULL;
    PMIx_Get(&myproc, "WASSUP", NULL, 0, &val);
    if (NULL == val) {
        fprintf(stderr, "ERROR GETTING WASSUP\n");
    } else {
        fprintf(stderr, "THREAD WASSUP: %s\n", val->data.string);
        PMIX_VALUE_FREE(val, 1);
    }

    /* lookup something published by the main thread */
    PMIX_PDATA_CREATE(pdata, 1);
    PMIX_PDATA_LOAD(&pdata[0], &myproc, "SOMETHING", NULL, PMIX_BOOL);

    /* tell the call to wait for the data to be published */
    PMIX_INFO_CREATE(directives, 1);
    PMIX_INFO_LOAD(&directives[0], PMIX_WAIT, &wait, PMIX_INT);

    if (PMIX_SUCCESS != PMIx_Lookup(pdata, 1, directives, 1)) {
        fprintf(stderr, "LOOKUP FAILED\n");
    } else {
        fprintf(stderr, "LOOKUP RETURNED %s\n", pdata[0].value.data.string);
    }
    PMIX_PDATA_FREE(pdata, 1);
    PMIX_INFO_FREE(directives, 1);

    if (init) {
        /* need to finalize to maintain refcount */
        PMIx_Finalize(NULL, 0);
    }

    /* done */
    return NULL;
}

int main(int argc, char* argv[])
{
    int rank, size, rc;
    pid_t pid;
    pthread_t mythread;
    bool before = false;
    pmix_info_t *info;
    pmix_value_t value;
    char *valstring;
    pmix_data_range_t range = PMIX_RANGE_LOCAL;

    if (1 < argc) {
        if (0 == strcmp(argv[1], "-b") || 0 == strcmp(argv[1], "--before")) {
            before = true;
        }
    }

    if (before) {
        /* spin up a thread */
        if (pthread_create(&mythread, NULL, mylib, NULL)) {
            fprintf(stderr, "Error creating thread\n");
            goto done;
        }
    }

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    pid = getpid();

    if (!before) {
        /* spin up a thread */
        if (pthread_create(&mythread, NULL, mylib, NULL)) {
            fprintf(stderr, "Error creating thread\n");
            goto done;
        }
    }

    /* push something the thread can recognize */
    PMIX_VALUE_CONSTRUCT(&value);
    value.type = PMIX_STRING;
    value.data.string = strdup("nothing");
    PMIx_Put(PMIX_LOCAL, "WASSUP", &value);
    PMIX_VALUE_DESTRUCT(&value);
    /* no need to commit it as this is strictly within ourselves */

    printf("Hello, World, I am %d of %d\n", rank, size);

    /* publish something */
    PMIX_INFO_CREATE(info, 2);
    PMIX_INFO_LOAD(&info[0], "SOMETHING", "foobar", PMIX_STRING);
    PMIX_INFO_LOAD(&info[1], PMIX_RANGE, &range, PMIX_DATA_RANGE);
    PMIx_Publish(info, 2);
    PMIX_INFO_FREE(info, 2);

    /* wait for the thread to finish */
    if (pthread_join(mythread, NULL)) {
        fprintf(stderr, "Error joining thread\n");
    }

  done:
    MPI_Finalize();
    return 0;
}
