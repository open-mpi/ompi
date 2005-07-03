/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_THREAD_H
#define OMPI_THREAD_H 1

#ifdef WIN32
#include <windows.h>
#elif OMPI_HAVE_POSIX_THREADS
#include <pthread.h>
#elif OMPI_HAVE_SOLARIS_THREADS
#include <thread.h>
#endif

#include "opal/class/opal_object.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

typedef void *(*ompi_thread_fn_t) (opal_object_t *);


struct ompi_thread_t {
    opal_object_t super;
    ompi_thread_fn_t t_run;
    void* t_arg;
#ifdef WIN32
    HANDLE t_handle;
#elif OMPI_HAVE_POSIX_THREADS
    pthread_t t_handle;
#elif OMPI_HAVE_SOLARIS_THREADS
    thread_t t_handle;
#endif
};

typedef struct ompi_thread_t ompi_thread_t;


OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_thread_t);


int  ompi_thread_start(ompi_thread_t *);
int  ompi_thread_join(ompi_thread_t *, void **thread_return);
bool ompi_thread_self_compare(ompi_thread_t*);
ompi_thread_t *ompi_thread_get_self(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_THREAD_H */
