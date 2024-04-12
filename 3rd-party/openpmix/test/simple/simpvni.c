#include <pmix_server.h>
#include <pmix_tool.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <stdarg.h>

static void hide_unused_params(int x, ...)
{
    va_list ap;

    va_start(ap, x);
    va_end(ap);
}

typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    volatile bool active;
    pmix_status_t status;
} mylock_t;

typedef struct {
    mylock_t lock;
    pmix_info_t *info;
    size_t ninfo;
} myxfer_t;

#define DEBUG_CONSTRUCT_LOCK(l)                \
    do {                                       \
        pthread_mutex_init(&(l)->mutex, NULL); \
        pthread_cond_init(&(l)->cond, NULL);   \
        (l)->active = true;                    \
        (l)->status = PMIX_SUCCESS;            \
    } while (0)

#define DEBUG_DESTRUCT_LOCK(l)              \
    do {                                    \
        pthread_mutex_destroy(&(l)->mutex); \
        pthread_cond_destroy(&(l)->cond);   \
    } while (0)

#define DEBUG_WAIT_THREAD(lck)                              \
    do {                                                    \
        pthread_mutex_lock(&(lck)->mutex);                  \
        while ((lck)->active) {                             \
            pthread_cond_wait(&(lck)->cond, &(lck)->mutex); \
        }                                                   \
        pthread_mutex_unlock(&(lck)->mutex);                \
    } while (0)

#define DEBUG_WAKEUP_THREAD(lck)              \
    do {                                      \
        pthread_mutex_lock(&(lck)->mutex);    \
        (lck)->active = false;                \
        pthread_cond_broadcast(&(lck)->cond); \
        pthread_mutex_unlock(&(lck)->mutex);  \
    } while (0)

static void setup_cbfunc(pmix_status_t status, pmix_info_t info[], size_t ninfo,
                         void *provided_cbdata, pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    myxfer_t *x = (myxfer_t *) provided_cbdata;
    size_t n;

    /* transfer it to the caddy for return to the main thread */
    x->lock.status = status;
    if (0 < ninfo) {
        PMIX_INFO_CREATE(x->info, ninfo);
        x->ninfo = ninfo;
        for (n = 0; n < ninfo; n++) {
            PMIX_INFO_XFER(&x->info[n], &info[n]);
        }
    }

    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }

    DEBUG_WAKEUP_THREAD(&x->lock);
}

int main(int argc, char **argv)
{
    pmix_status_t rc=0;
    myxfer_t x;
    pmix_proc_t myproc;
    pmix_info_t *info;
    size_t n, ninfo;
    char *myvni = NULL;
    hide_unused_params(rc, argc, argv);

    /* setup the PMIx tool library - don't need to connect
     * to a PMIx server */
    ninfo = 2;
    PMIX_INFO_CREATE(info, ninfo);
    n = 0;
    PMIX_INFO_LOAD(&info[n], PMIX_TOOL_DO_NOT_CONNECT, NULL, PMIX_BOOL);
    ++n;
    PMIX_INFO_LOAD(&info[n], PMIX_LAUNCHER, NULL, PMIX_BOOL);
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, info, ninfo))) {
        fprintf(stderr, "Init failed with error %s\n", PMIx_Error_string(rc));
        return rc;
    }
    PMIX_INFO_FREE(info, ninfo);

    /* ask for a security credential */
    DEBUG_CONSTRUCT_LOCK(&x.lock);
    x.info = NULL;
    x.ninfo = 0;
    ninfo = 1;
    PMIX_INFO_CREATE(info, ninfo);
    n = 0;
    PMIX_INFO_LOAD(&info[n], PMIX_ALLOC_FABRIC_SEC_KEY, NULL, PMIX_BOOL);
    rc = PMIx_server_setup_application(myproc.nspace, info, ninfo, setup_cbfunc, &x);
    if (PMIX_SUCCESS != rc) {
        /* the thread won't be called back */
        PMIX_INFO_FREE(info, ninfo);
        goto done;
    }
    DEBUG_WAIT_THREAD(&x.lock);
    PMIX_INFO_FREE(info, ninfo);
    if (PMIX_SUCCESS != x.lock.status) {
        /* couldn't allocate it */
        rc = x.lock.status;
        goto done;
    }

    /* find the VNI in the returned data - shouldn't be anything else */
    for (n = 0; n < x.ninfo; n++) {
        if (PMIX_CHECK_KEY(&x.info[n], PMIX_CREDENTIAL)) {
            myvni = strdup(x.info[n].value.data.string);
            break;
        }
    }
    fprintf(stderr, "VNI: %s\n", (NULL == myvni) ? "NULL" : myvni);

done:
    rc = PMIx_tool_finalize();
    return rc;
}
