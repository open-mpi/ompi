/* 
 * $HEADER$
 */
#ifndef LAM_CONDITION_SPINLOCK_H
#define LAM_CONDITION_SPINLOCK_H

#include "threads/condition.h"


struct lam_condition_t {
    volatile int c_waiting;
};
typedef struct lam_condition_t lam_condition_t;

OBJ_CLASS_DECLARATION(lam_condition_t);


static inline int lam_condition_wait(lam_condition_t* c, lam_mutex_t* m)
{
    return 0;
}

static inline int lam_condition_timedwait(lam_condition_t* c, lam_mutex_t* m, const struct timespec *abstime)
{
    return 0;
}

static inline int lam_condition_signal(lam_condition_t* c)
{
    return 0;
}

static inline int lam_condition_broadcast(lam_condition_t* c)
{
    return 0;
}

#endif

