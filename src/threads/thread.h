/*
 * $HEADER$
 */

#ifndef LAM_THREAD_H
#define LAM_THREAD_H

#include <pthread.h>
#include "lfc/lam_object.h"

typedef void* (*lam_thread_fn_t)(lam_object_t*);


struct lam_thread_t
{
    lam_object_t super;
    lam_thread_fn_t t_run;
    pthread_t t_handle;
};
typedef struct lam_thread_t lam_thread_t;


OBJ_CLASS_DECLARATION(lam_thread_t);


int lam_thread_start(lam_thread_t*);
int lam_thread_join(lam_thread_t*, void** thread_return);

#endif /* LAM_THREAD_H */
