/*
 * $HEADER$
 */

#include "include/constants.h"
#include "threads/thread.h"


static void lam_thread_construct(lam_thread_t* t)
{
    t->t_run = 0;
    t->t_handle = (pthread_t) -1;
}
                                                                                                             
                                                                                                             
static void lam_thread_destruct(lam_thread_t* t)
{
}

                                                                                                             
OBJ_CLASS_INSTANCE(
    lam_thread_t,
    lam_object_t,
    lam_thread_construct,
    lam_thread_destruct
);

typedef void* (*pthread_start_fn_t)(void*);


int lam_thread_start(lam_thread_t* t)
{
    int rc;
#if LAM_ENABLE_DEBUG
    if(NULL == t->t_run || t->t_handle != -1)
        return LAM_ERR_BAD_PARAM;
#endif
    rc = pthread_create(&t->t_handle, NULL, (pthread_start_fn_t)t->t_run, t);
    return (rc == 0) ? LAM_SUCCESS: LAM_ERROR;
}

int lam_thread_join(lam_thread_t* t, void** thr_return)
{
    int rc = pthread_join(t->t_handle, thr_return);
    return (rc == 0) ? LAM_SUCCESS: LAM_ERROR;
}

