/* 
 * $HEADER$
 */
#ifndef LAM_CONDITION_SPINLOCK_H
#define LAM_CONDITION_SPINLOCK_H

#include "threads/condition.h"
#include "threads/mutex_spinlock.h"
#include "runtime/lam_progress.h"


struct lam_condition_t {
    volatile int c_waiting;
    volatile int c_signaled;
};
typedef struct lam_condition_t lam_condition_t;

OBJ_CLASS_DECLARATION(lam_condition_t);


static inline int lam_condition_wait(lam_condition_t* c, lam_mutex_t* m)
{
    c->c_waiting++;
    while(c->c_signaled == 0) {
        lam_mutex_unlock(m);
        lam_progress();
        lam_mutex_lock(m);
    }
    c->c_signaled--;
    c->c_waiting--;
    return 0;
}

static inline int lam_condition_timedwait(lam_condition_t* c, lam_mutex_t* m, const struct timespec *abstime)
{
    return 0;
}

static inline int lam_condition_signal(lam_condition_t* c)
{
    if(c->c_waiting) {
       c->c_signaled++;
    }
    return 0;
}

static inline int lam_condition_broadcast(lam_condition_t* c)
{
    c->c_signaled += c->c_waiting;
    return 0;
}

#endif

