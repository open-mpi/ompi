/*
 * $HEADER$
 */

#ifndef LAM_THREAD_H
#define LAM_THREAD_H

#include "lam/lfc/lam_object.h"

typedef struct lam_thread
{
    lam_object_t super;
    void (*thr_run)(lam_object_t*);
} lam_thread_t;


void lam_thr_construct(lam_thread_t *a_thread);
lam_thread_t *lam_thr_create(lam_object_t *arg);

#endif /* LAM_THREAD_H */
