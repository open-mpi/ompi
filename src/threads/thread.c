/*
 * $HEADER$
 */

#ifdef HAVE_CONFIG_H
#include "ompi_config.h"
#endif

#include "include/constants.h"
#include "threads/thread.h"


static void ompi_thread_construct(ompi_thread_t *t)
{
    t->t_run = 0;
#ifdef __WINDOWS__
    t->t_handle = (HANDLE) -1;
#elif OMPI_HAVE_POSIX_THREADS
    t->t_handle = (pthread_t) -1;
#endif
}


static void ompi_thread_destruct(ompi_thread_t *t)
{
}


OBJ_CLASS_INSTANCE(ompi_thread_t,
                   ompi_object_t,
                   ompi_thread_construct,
                   ompi_thread_destruct);



#ifdef __WINDOWS__

#error Windows code is untested

int ompi_thread_start(ompi_thread_t *t)
{
    DWORD tid;

    if (OMPI_ENABLE_DEBUG) {
        if (NULL == t->t_run || t->t_handle != (HANDLE) -1L) {
            return OMPI_ERR_BAD_PARAM;
        }
    }

    t->t_handle = CreateThread(NULL,    /* default security attributes */
                               0,       /* default stack size */
                               (LPVOID) t->t_run,
                               t,       /* argument */
                               0,       /* default creation flags */
                               &tid);

    if (t->t_handle == NULL) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int ompi_thread_join(ompi_thread_t *t, void **thr_return)
{
    DWORD rc;

    if (WaitForSingleObject(t->t_handle, INFINITE) != WAIT_OBJECT_0) {
        return OMPI_ERROR;
    }
    if (!GetExitCodeThread(t->t_handle, &rc)) {
        return OMPI_ERROR;
    }

    *thr_return = (void *) rc;

    return OMPI_SUCCESS;
}


#elif OMPI_HAVE_POSIX_THREADS


int ompi_thread_start(ompi_thread_t *t)
{
    int rc;

    if (OMPI_ENABLE_DEBUG) {
        if (NULL == t->t_run || t->t_handle != (pthread_t) -1) {
            return OMPI_ERR_BAD_PARAM;
        }
    }

    rc = pthread_create(&t->t_handle, NULL, (void*(*)(void*)) t->t_run, t);

    return (rc == 0) ? OMPI_SUCCESS : OMPI_ERROR;
}


int ompi_thread_join(ompi_thread_t *t, void **thr_return)
{
    int rc = pthread_join(t->t_handle, thr_return);
    return (rc == 0) ? OMPI_SUCCESS : OMPI_ERROR;
}

bool ompi_thread_self(ompi_thread_t *t)
{
    return t->t_handle == pthread_self();
}

#else


int ompi_thread_start(ompi_thread_t *t)
{
    return OMPI_ERROR;
}


int ompi_thread_join(ompi_thread_t *t, void **thr_return)
{
    return OMPI_ERROR;
}

bool ompi_thread_self(ompi_thread_t *t)
{
    return true;
}

#endif
