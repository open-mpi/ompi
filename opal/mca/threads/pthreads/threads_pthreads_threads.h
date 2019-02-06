
#ifndef  OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_THREADS_H
#define  OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_THREADS_H 1

#include <pthread.h>
#include <signal.h>

struct opal_thread_t {
    opal_object_t super;
    opal_thread_fn_t t_run;
    void* t_arg;
    pthread_t t_handle;
};

#endif /* OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_THREADS_H */
