/*
 * $HEADER$
 */

#ifndef OMPI_THREAD_H
#define OMPI_THREAD_H

#include <pthread.h>
#include "class/ompi_object.h"

typedef void* (*ompi_thread_fn_t)(ompi_object_t*);


struct ompi_thread_t
{
    ompi_object_t super;
    ompi_thread_fn_t t_run;
    pthread_t t_handle;
};
typedef struct ompi_thread_t ompi_thread_t;


OBJ_CLASS_DECLARATION(ompi_thread_t);


int ompi_thread_start(ompi_thread_t*);
int ompi_thread_join(ompi_thread_t*, void** thread_return);

#endif /* OMPI_THREAD_H */
