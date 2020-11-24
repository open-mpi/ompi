/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

/* This file currently implements these as a preprocessor if/elif/else sequence.
 * This has the upside of not doing #includes for .c files or (poorly
 * named) .i files.  It has the downside of making this file large-ish
 * and a little harder to read in some cases.  If this becomes
 * unmanagable at some point these should be separated back out into
 * header files and included as needed. [goodell@ 2009-06-24] */

/* Implementation specific function definitions (usually in the form of macros) */

#if MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_WIN

/*
 * struct MPLI_thread_info
 *
 * Structure used to pass the user function and data to the intermediate function, MPLI_thread_start.  See comment in
 * MPLI_thread_start() header for more information.
 */
struct MPLI_thread_info {
    MPL_thread_func_t func;
    void *data;
};


DWORD WINAPI MPLI_thread_start(LPVOID arg);

/*
 * MPL_thread_create()
 */
void MPL_thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * idp, int *errp)
{
    struct MPLI_thread_info *thread_info;
    int err = MPL_THREAD_SUCCESS;

    thread_info =
        (struct MPLI_thread_info *) MPL_malloc(sizeof(struct MPLI_thread_info), MPL_MEM_THREAD);
    if (thread_info != NULL) {
        thread_info->func = func;
        thread_info->data = data;
        *idp = CreateThread(NULL, 0, MPLI_thread_start, thread_info, 0, NULL);
        if (*idp == NULL) {
            err = GetLastError();
        }
    } else {
        err = 1000000000;
    }

    if (errp != NULL) {
        *errp = err;
    }
}


/*
 * MPLI_thread_start()
 *
 * Start functions in Windows are expected to return a DWORD.  Since our start functions do not return a value we must
 * use an intermediate function to perform the call to the user's start function and then return a value of 0.
 */
DWORD WINAPI MPLI_thread_start(LPVOID arg)
{
    struct MPLI_thread_info *thread_info = (struct MPLI_thread_info *) arg;
    MPL_thread_func_t func = thread_info->func;
    void *data = thread_info->data;

    MPL_free(arg);

    func(data);

    return 0;
}

void MPL_thread_exit()
{
    ExitThread(0);
}

void MPL_thread_self(MPL_thread_id_t * id)
{
    *id = GetCurrentThread();
}

void MPL_thread_same(MPL_thread_id_t * id1, MPL_thread_id_t * id2, int *same)
{
    *same = (*id1 == *id2) ? TRUE : FALSE;
}

void MPL_thread_yield(void)
{
    Sleep(0);
}

/*
 *    Mutexes
 */

void MPL_thread_mutex_create(MPL_thread_mutex_t * mutex, int *err)
{
    *mutex = CreateMutex(NULL, FALSE, NULL);
    if (err != NULL) {
        if (*mutex == NULL) {
            *err = GetLastError();
        } else {
            *err = MPL_THREAD_SUCCESS;
        }
    }
}

void MPL_thread_mutex_destroy(MPL_thread_mutex_t * mutex, int *err)
{
    BOOL result;

    result = CloseHandle(*mutex);
    if (err != NULL) {
        if (result) {
            *err = MPL_THREAD_SUCCESS;
        } else {
            *err = GetLastError();
        }
    }
}

void MPL_thread_mutex_lock(MPL_thread_mutex_t * mutex, int *err)
{
    DWORD result;

    result = WaitForSingleObject(*mutex, INFINITE);
    if (err != NULL) {
        if (result == WAIT_OBJECT_0) {
            *err = MPL_THREAD_SUCCESS;
        } else {
            if (result == WAIT_FAILED) {
                *err = GetLastError();
            } else {
                *err = result;
            }
        }
    }
}

void MPL_thread_mutex_unlock(MPL_thread_mutex_t * mutex, int *err)
{
    BOOL result;

    result = ReleaseMutex(*mutex);
    if (err != NULL) {
        if (result) {
            *err = MPL_THREAD_SUCCESS;
        } else {
            *err = GetLastError();
        }
    }
}


/*
 * Condition Variables
 */

void MPL_thread_cond_create(MPL_thread_cond_t * cond, int *err)
{
    /* Create a tls slot to store the events used to wakeup each thread in cond_bcast or cond_signal */
    MPL_thread_tls_create(NULL, &cond->tls, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    /* Create a mutex to protect the fifo queue.  This is required because the mutex passed in to the
     * cond functions need not be the same in each thread. */
    MPL_thread_mutex_create(&cond->fifo_mutex, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    cond->fifo_head = NULL;
    cond->fifo_tail = NULL;
    if (err != NULL) {
        *err = MPL_THREAD_SUCCESS;
    }
}

void MPL_thread_cond_destroy(MPL_thread_cond_t * cond, int *err)
{
    MPLI_win_thread_cond_fifo_t *iter;

    while (cond->fifo_head) {
        iter = cond->fifo_head;
        cond->fifo_head = cond->fifo_head->next;
        MPL_free(iter);
    }
    MPL_thread_mutex_destroy(&cond->fifo_mutex, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    MPL_thread_tls_destroy(&cond->tls, err);
    /*
     * if (err != NULL)
     * {
     * *err = MPL_THREAD_SUCCESS;
     * }
     */
}

void MPL_thread_cond_wait(MPL_thread_cond_t * cond, MPL_thread_mutex_t * mutex, int *err)
{
    HANDLE event;
    DWORD result;
    MPL_thread_tls_get(&cond->tls, &event, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    if (event == NULL) {
        event = CreateEvent(NULL, TRUE, FALSE, NULL);
        if (event == NULL) {
            if (err != NULL) {
                *err = GetLastError();
            }
            return;
        }
        MPL_thread_tls_set(&cond->tls, event, err);
        if (err != NULL && *err != MPL_THREAD_SUCCESS) {
            return;
        }
    }
    MPL_thread_mutex_lock(&cond->fifo_mutex, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    if (cond->fifo_tail == NULL) {
        cond->fifo_tail =
            (MPLI_win_thread_cond_fifo_t *) MPL_malloc(sizeof(MPLI_win_thread_cond_fifo_t),
                                                       MPL_MEM_THREAD);
        cond->fifo_head = cond->fifo_tail;
    } else {
        cond->fifo_tail->next =
            (MPLI_win_thread_cond_fifo_t *) MPL_malloc(sizeof(MPLI_win_thread_cond_fifo_t),
                                                       MPL_MEM_THREAD);
        cond->fifo_tail = cond->fifo_tail->next;
    }
    if (cond->fifo_tail == NULL) {
        if (err != NULL) {
            *err = -1;
        }
        return;
    }
    cond->fifo_tail->event = event;
    cond->fifo_tail->next = NULL;
    MPL_thread_mutex_unlock(&cond->fifo_mutex, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    MPL_thread_mutex_unlock(mutex, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    result = WaitForSingleObject(event, INFINITE);
    if (err != NULL) {
        if (result != WAIT_OBJECT_0) {
            if (result == WAIT_FAILED) {
                *err = GetLastError();
            } else {
                *err = result;
            }
            return;
        }
    }
    result = ResetEvent(event);
    if (!result && err != NULL) {
        *err = GetLastError();
        return;
    }
    MPL_thread_mutex_lock(mutex, err);
    /*
     * if (err != NULL)
     * {
     * *err = MPL_THREAD_SUCCESS;
     * }
     */
}

void MPL_thread_cond_broadcast(MPL_thread_cond_t * cond, int *err)
{
    MPLI_win_thread_cond_fifo_t *fifo, *temp;
    MPL_thread_mutex_lock(&cond->fifo_mutex, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    /* remove the fifo queue from the cond variable */
    fifo = cond->fifo_head;
    cond->fifo_head = cond->fifo_tail = NULL;
    MPL_thread_mutex_unlock(&cond->fifo_mutex, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    /* signal each event in the fifo queue */
    while (fifo) {
        if (!SetEvent(fifo->event) && err != NULL) {
            *err = GetLastError();
            /* lost memory */
            return;
        }
        temp = fifo;
        fifo = fifo->next;
        MPL_free(temp);
    }
    if (err != NULL) {
        *err = MPL_THREAD_SUCCESS;
    }
}

void MPL_thread_cond_signal(MPL_thread_cond_t * cond, int *err)
{
    MPLI_win_thread_cond_fifo_t *fifo;
    MPL_thread_mutex_lock(&cond->fifo_mutex, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    fifo = cond->fifo_head;
    if (fifo) {
        cond->fifo_head = cond->fifo_head->next;
        if (cond->fifo_head == NULL)
            cond->fifo_tail = NULL;
    }
    MPL_thread_mutex_unlock(&cond->fifo_mutex, err);
    if (err != NULL && *err != MPL_THREAD_SUCCESS) {
        return;
    }
    if (fifo) {
        if (!SetEvent(fifo->event) && err != NULL) {
            *err = GetLastError();
            MPL_free(fifo);
            return;
        }
        MPL_free(fifo);
    }
    if (err != NULL) {
        *err = MPL_THREAD_SUCCESS;
    }
}

#endif
