/*
 * $HEADER$
 */

#include "include/constants.h"
#include "threads/thread.h"


static void ompi_thread_construct(ompi_thread_t* t)
{
    t->t_run = 0;
    t->t_handle = (pthread_t) -1;
}
                                                                                                             
                                                                                                             
static void ompi_thread_destruct(ompi_thread_t* t)
{
}

                                                                                                             
OBJ_CLASS_INSTANCE(
    ompi_thread_t,
    ompi_object_t,
    ompi_thread_construct,
    ompi_thread_destruct
);

typedef void* (*pthread_start_fn_t)(void*);


int ompi_thread_start(ompi_thread_t* t)
{
    int rc;
#if OMPI_ENABLE_DEBUG
    if(NULL == t->t_run || t->t_handle != -1)
        return OMPI_ERR_BAD_PARAM;
#endif
    rc = pthread_create(&t->t_handle, NULL, (pthread_start_fn_t)t->t_run, t);
    return (rc == 0) ? OMPI_SUCCESS: OMPI_ERROR;
}

int ompi_thread_join(ompi_thread_t* t, void** thr_return)
{
    int rc = pthread_join(t->t_handle, thr_return);
    return (rc == 0) ? OMPI_SUCCESS: OMPI_ERROR;
}

