
#ifndef  OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_THREADS_H
#define  OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_THREADS_H 1

#include <abt.h>
#include <signal.h>

struct opal_thread_t {
    opal_object_t super;
    opal_thread_fn_t t_run;
    void* t_arg;
    ABT_thread t_handle;
    void* t_ret;
};

#endif /* OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_THREADS_H */
