/*
 * $HEADER$
 */

#ifndef OMPI_THREAD_H
#define OMPI_THREAD_H 1

#ifdef __WINDOWS__
#include <windows.h>
#elif OMPI_HAVE_POSIX_THREADS
#include <pthread.h>
#endif

#include "class/ompi_object.h"

typedef void *(*ompi_thread_fn_t) (ompi_object_t *);


struct ompi_thread_t {
    ompi_object_t super;
    ompi_thread_fn_t t_run;
#ifdef __WINDOWS__
    HANDLE t_handle;
#elif OMPI_HAVE_POSIX_THREADS
    pthread_t t_handle;
#endif
};

typedef struct ompi_thread_t ompi_thread_t;


OBJ_CLASS_DECLARATION(ompi_thread_t);


int  ompi_thread_start(ompi_thread_t *);
int  ompi_thread_join(ompi_thread_t *, void **thread_return);
bool ompi_thread_self(ompi_thread_t*);

#endif /* OMPI_THREAD_H */
