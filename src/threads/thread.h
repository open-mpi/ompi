/*
 * $HEADER$
 */

#ifndef OMPI_THREAD_H
#define OMPI_THREAD_H 1

#ifdef WIN32
#include <windows.h>
#elif OMPI_HAVE_POSIX_THREADS
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#endif

#include "class/ompi_object.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

typedef void *(*ompi_thread_fn_t) (ompi_object_t *);


struct ompi_thread_t {
    ompi_object_t super;
    ompi_thread_fn_t t_run;
    void* t_arg;
#ifdef WIN32
    HANDLE t_handle;
#elif OMPI_HAVE_POSIX_THREADS
    pthread_t t_handle;
#endif
};

typedef struct ompi_thread_t ompi_thread_t;


OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_thread_t);


int  ompi_thread_start(ompi_thread_t *);
int  ompi_thread_join(ompi_thread_t *, void **thread_return);
bool ompi_thread_self(ompi_thread_t*);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_THREAD_H */
