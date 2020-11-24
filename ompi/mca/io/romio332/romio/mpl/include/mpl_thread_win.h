/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_THREAD_WIN_H_INCLUDED
#define MPL_THREAD_WIN_H_INCLUDED

#define WIN32_LEAN_AND_MEAN

#include <windows.h>

typedef HANDLE MPL_thread_mutex_t;
typedef HANDLE MPL_thread_id_t;
typedef DWORD MPL_thread_tls_t;

typedef struct MPLI_win_thread_cond_fifo_t {
    HANDLE event;
    struct MPLI_win_thread_cond_fifo_t *next;
} MPLI_win_thread_cond_fifo_t;
typedef struct MPL_thread_cond_t {
    MPL_thread_tls_t tls;
    MPL_thread_mutex_t fifo_mutex;
    MPLI_win_thread_cond_fifo_t *fifo_head, *fifo_tail;
} MPL_thread_cond_t;

typedef void (*MPL_thread_func_t) (void *data);

void MPL_thread_create(MPL_thread_func_t func, void *data, MPL_thread_id_t * id, int *err);
void MPL_thread_exit(void);
void MPL_thread_self(MPL_thread_id_t * id);
void MPL_thread_same(MPL_thread_id_t * id1, MPL_thread_id_t * id2, int *same);
void MPL_thread_yield();

void MPL_thread_mutex_create(MPL_thread_mutex_t * mutex, int *err);
void MPL_thread_mutex_destroy(MPL_thread_mutex_t * mutex, int *err);
void MPL_thread_mutex_lock(MPL_thread_mutex_t * mutex, int *err);
void MPL_thread_mutex_unlock(MPL_thread_mutex_t * mutex, int *err);

void MPL_thread_cond_create(MPL_thread_cond_t * cond, int *err);
void MPL_thread_cond_destroy(MPL_thread_cond_t * cond, int *err);
void MPL_thread_cond_wait(MPL_thread_cond_t * cond, MPL_thread_mutex_t * mutex, int *err);
void MPL_thread_cond_broadcast(MPL_thread_cond_t * cond, int *err);
void MPL_thread_cond_signal(MPL_thread_cond_t * cond, int *err);

/*
 * Thread Local Storage
 */

#define MPL_thread_tls_create(exit_func_ptr_, tls_ptr_, err_ptr_)       \
    do {                                                                \
        *(tls_ptr_) = TlsAlloc();                                       \
        if ((err_ptr_) != NULL) {                                       \
            if (*(tls_ptr_) == TLS_OUT_OF_INDEXES) {                    \
                *(int *)(err_ptr_) = GetLastError();                    \
            }                                                           \
            else {                                                      \
                *(int *)(err_ptr_) = MPL_THREAD_SUCCESS;                \
            }                                                           \
        }                                                               \
    } while (0)

#define MPL_thread_tls_destroy(tls_ptr_, err_ptr_)              \
    do {                                                        \
        BOOL result__;                                          \
        result__ = TlsFree(*(tls_ptr_));                        \
        if ((err_ptr_) != NULL) {                               \
            if (result__) {                                     \
                *(int *)(err_ptr_) = MPL_THREAD_SUCCESS;        \
            }                                                   \
            else {                                              \
                *(int *)(err_ptr_) = GetLastError();            \
            }                                                   \
        }                                                       \
    } while (0)

#define MPL_thread_tls_set(tls_ptr_, value_, err_ptr_)          \
    do {                                                        \
        BOOL result__;                                          \
        result__ = TlsSetValue(*(tls_ptr_), (value_));          \
        if ((err_ptr_) != NULL) {                               \
            if (result__) {                                     \
                *(int *)(err_ptr_) = MPL_THREAD_SUCCESS;        \
            }                                                   \
            else {                                              \
                *(int *)(err_ptr_) = GetLastError();            \
            }                                                   \
        }                                                       \
    } while (0)

#define MPL_thread_tls_get(tls_ptr_, value_ptr_, err_ptr_)              \
    do {                                                                \
        *((void **)value_ptr_) = TlsGetValue(*(tls_ptr_));              \
        if ((err_ptr_) != NULL) {                                       \
            if (*(value_ptr_) == 0 && GetLastError() != NO_ERROR) {     \
                *(int *)(err_ptr_) = GetLastError();                    \
            }                                                           \
            else {                                                      \
                *(int *)(err_ptr_) = MPL_THREAD_SUCCESS;                \
            }                                                           \
        }                                                               \
    } while (0)

#endif /* MPL_THREAD_WIN_H_INCLUDED */
