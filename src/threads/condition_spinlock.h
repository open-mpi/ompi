/* 
 * $HEADER$
 */
#ifndef OMPI_CONDITION_SPINOMPI_LOCK_H
#define OMPI_CONDITION_SPINOMPI_LOCK_H

#include "threads/condition.h"
#include "threads/mutex.h"
#include "threads/mutex_spinlock.h"
#include "runtime/ompi_progress.h"


struct ompi_condition_t {
    volatile int c_waiting;
    volatile int c_signaled;
};
typedef struct ompi_condition_t ompi_condition_t;

OBJ_CLASS_DECLARATION(ompi_condition_t);


static inline int ompi_condition_wait(ompi_condition_t* c, ompi_mutex_t* m)
{
    c->c_waiting++;
    if(ompi_using_threads()) {
        while(c->c_signaled == 0) {
            ompi_mutex_unlock(m);
            ompi_progress();
            ompi_mutex_lock(m);
        }
    } else {
        while(c->c_signaled == 0) {
            ompi_progress();
        }
    }
    c->c_signaled--;
    c->c_waiting--;
    return 0;
}

static inline int ompi_condition_timedwait(ompi_condition_t* c, ompi_mutex_t* m, const struct timespec *abstime)
{
    return 0;
}

static inline int ompi_condition_signal(ompi_condition_t* c)
{
    if(c->c_waiting) {
       c->c_signaled++;
    }
    return 0;
}

static inline int ompi_condition_broadcast(ompi_condition_t* c)
{
    c->c_signaled += c->c_waiting;
    return 0;
}

#endif

