/*
 * Test program for memory consistency in a thread shifting design
 *
 *
 * Run:
 *  ./threads ITERATIONS [MODE]
 *  ./threads 9000000 3
 *
 * Example:
 *  ./threads 9000000 0 --> Will fail, no memory barriers
 *  ./threads 9000000 1 --> Will fail, no WMB
 *  ./threads 9000000 2 --> Will fail, no RMB
 *  ./threads 9000000 3 --> Success
 *  ./threads 9000000 4 --> Success
 *  ./threads 9000000 5 --> N/A
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>
#include <stdint.h>
#include <hwloc.h>
#include <sys/time.h>

#include "opal/sys/atomic.h"


// Max value for an int16_t
#define MAX_VAL 32767

typedef struct {
    int type;
    union {
        bool flag;
        int integer;
        int8_t int8;
        int16_t int16;
        int32_t int32;
        int64_t int64;
        //char padding[1];
    } data;
} my_value_t;

// Structure to handoff work to the peer thread
typedef struct {
    volatile bool working;
    void *ptr; // Note that adding a volatile here has no effect
} thread_handoff_t;

// Shared object to handoff work
thread_handoff_t handoff;

// Indicates if the test has finished
bool time_to_stop = false;

// Progress reporting
#define PERC_INC 10.0
double perc_report_after = PERC_INC;
double perc_current = 0.0;

// Memory barrier modes
#define MB_MODE_NONE 0x0
#define MB_MODE_RMB  0x1
#define MB_MODE_WMB  0x2
#define MB_MODE_MB   0x4
#define MB_MODE_XMB  0x8
#define MB_MODE_ALL (MB_MODE_RMB | MB_MODE_WMB)
int mb_mode = MB_MODE_ALL;


// Shared hwloc topology (so we only have to read it once)
static hwloc_topology_t topo;
// Which object we are binding to
//  4 - sockets with 5 cores each
// 20 - cores with 8 PUs each
//#define OBJ_TYPE HWLOC_OBJ_SOCKET
#define OBJ_TYPE HWLOC_OBJ_CORE

/*
 * Some basic timing support
 */
double acc_time, start_time, stop_time, delta;
static double get_ts_gettimeofday(void) {
    double ret;
    struct timeval tv;
    gettimeofday(&tv, NULL);
    ret = tv.tv_sec;
    ret += (double)tv.tv_usec / 1000000.0;
    return ret;
}

/*
 * Bind either the main or support thread far away from each other
 */
void bind_me_to(bool main_thread);

/*
 * Support thread to do the memory allocation and xfer
 */
void *value_xfer_thread(void *arg);

/*
 * Main thread
 */
int main(int argc, char **argv) {
    pthread_t support_thread;
    int rc, i, max_iters = 10, cur_iter;
    my_value_t *val = NULL;
    int mode;

    /*
     * Parse command line arguments
     */
    if( argc > 1 ) {
        max_iters = atoi(argv[1]);
    }
    if( argc > 2 ) {
        mode = atoi(argv[2]);
        if( 0 > mode || mode > 5 ) {
            printf("Error: Invalid mode %d\n"
                   "\tNone = 0\n"
                   "\tRMB = 1\n"
                   "\tWMB = 2\n"
                   "\tBoth = 3\n"
                   "\tMB Only = 4\n",
                   "\tXMB Only = 5\n",
                   mode);
            exit(-1);
        }
    }
    else {
        mode = 3;
    }
    switch(mode) {
    case 0:
        mb_mode = MB_MODE_NONE;
        break;
    case 1:
        mb_mode = MB_MODE_RMB;
        break;
    case 2:
        mb_mode = MB_MODE_WMB;
        break;
    case 3:
        mb_mode = MB_MODE_ALL;
        break;
    case 4:
        mb_mode = MB_MODE_MB;
        break;
    case 5:
        mb_mode = MB_MODE_XMB;
        break;
    }

    // Load hwloc topology
    hwloc_topology_init(&topo);
    hwloc_topology_load(topo);

    // Display banner
    printf("---------------------------\n");
    printf("Iterations: %10d\n", max_iters);
    printf("Mode R MB : %10s\n", (mb_mode & MB_MODE_RMB ? "Enabled" : "Disabled") );
    printf("Mode W MB : %10s\n", (mb_mode & MB_MODE_WMB ? "Enabled" : "Disabled") );
    printf("Mode - MB : %10s\n", (mb_mode & MB_MODE_MB  ? "Enabled" : "Disabled") );
    printf("Mode X MB : %10s\n", (mb_mode & MB_MODE_XMB ? "Enabled" : "Disabled") );
    printf("---------------------------\n");

    bind_me_to(true);
    handoff.working = false;

    /*
     * Launch supporting thread
     */
    rc = pthread_create(&support_thread, NULL, value_xfer_thread, NULL);
    if( 0 != rc ) {
        printf("Error: Failed to create a thread! %d\n", rc);
        exit(-1);
    }

    /*
     * Main work loop
     */
    acc_time = 0.0;
    for(cur_iter = 0; cur_iter < max_iters; ++cur_iter) {
        perc_current = (cur_iter / ((double)max_iters)) * 100.0;
        if( perc_current > perc_report_after ) {
            delta = (acc_time / cur_iter) * 1000000;
            printf("%6.1f %% complete : Iteration %10d / %10d : %6.1f usec / iter\n",
                   perc_current, cur_iter+1, max_iters, delta);
            perc_report_after += PERC_INC;
        }

        start_time = get_ts_gettimeofday();
        // Initialize values
        val = NULL;
        handoff.ptr = &val;
        if( mb_mode & MB_MODE_RMB ) {
            opal_atomic_rmb();
        }
        if( mb_mode & MB_MODE_MB ) {
            opal_atomic_mb();
        }
        handoff.working = true;

        // Wait for work to finish
        while( handoff.working ) {
            usleep(1);
        }
        if( mb_mode & MB_MODE_WMB ) {
            opal_atomic_wmb();
        }
        if( mb_mode & MB_MODE_MB ) {
            opal_atomic_mb();
        }

        // Inspect values for correctness
        if( NULL == val ) {
            printf("[%10d / %10d] Error: val = %s\n", cur_iter+1, max_iters,
                   (NULL == val ? "NULL" : "Valid") );
            exit(-1);
        }
        else if( 999 != val->type ) {
            printf("[%10d / %10d] Error: val->type = %d\n", cur_iter+1, max_iters, val->type);
            exit(-1);
        }
        else if( (cur_iter+1)%MAX_VAL != val->data.int16 ) {
            printf("[%10d / %10d] Error: val->data.int16 = %d\n", cur_iter+1, max_iters, val->data.int16);
            exit(-1);
        }

        stop_time = get_ts_gettimeofday();
        acc_time += (stop_time - start_time);

        // Yes, this is a memory leak!
        // I need to make sure that the supporting thread is not reusing a
        // previous storage location when it calls malloc. This is to emulate
        // a program that calls malloc after the value was acquired, possibly
        // reusing this memory location.
        //free(val);
        val = NULL;
    }
    delta = (acc_time / max_iters) * 1000000;

    /*
     * All done - Cleanup
     */
    time_to_stop = true;

    rc = pthread_join(support_thread, NULL);
    if( 0 != rc ) {
        printf("Error: Failed to join a thread! %d\n", rc);
        exit(-1);
    }

    hwloc_topology_destroy(topo);

    printf("Success - %6.1f usec / iter\n", delta);

    return 0;
}

void *value_xfer_thread(void *arg) {
    my_value_t **val = NULL;
    static int var = 0;

    // Bind this thread away from the main thread
    bind_me_to(false);

    while( !time_to_stop ) {
        if( handoff.working ) {
            // Make sure I have the right pointer
            if( mb_mode & MB_MODE_WMB ) {
                opal_atomic_wmb();
            }
            if( mb_mode & MB_MODE_MB ) {
                opal_atomic_mb();
            }

            // Allocate and set the value
            val = (my_value_t**)handoff.ptr;
            (*val) = malloc(sizeof(my_value_t));
            (*val)->type = 999;
            (*val)->data.int16 = (++var)%MAX_VAL;

            // Make sure main thread can see the value
            // See 'Examples' -> 'Global thread flag' discussion here:
            // https://www.ibm.com/developerworks/systems/articles/powerpc.html
            if( mb_mode & MB_MODE_RMB ) {
                opal_atomic_rmb();
            }
            if( mb_mode & MB_MODE_MB ) {
                opal_atomic_mb();
            }
            // Release main thread
            handoff.working = false;
        }
        else {
            // wait for work
            usleep(1);
        }
    }
    pthread_exit(NULL);
}

void bind_me_to(bool main_thread) {
    int num_objs;
    hwloc_cpuset_t set;
    char *buffer = NULL;
    hwloc_obj_t obj;

    num_objs = hwloc_get_nbobjs_by_type(topo, OBJ_TYPE);

    if( main_thread ) {
        obj = hwloc_get_obj_by_type(topo, OBJ_TYPE, 0);
    }
    else {
        obj = hwloc_get_obj_by_type(topo, OBJ_TYPE, num_objs-1);
    }

    if( obj->type == OBJ_TYPE ) {
        hwloc_set_cpubind(topo, obj->cpuset, HWLOC_CPUBIND_THREAD);
    }
    else {
        printf("Error: Invalid object\n");
        exit(-1);
    }

    set = hwloc_bitmap_alloc();
    hwloc_get_cpubind(topo, set, HWLOC_CPUBIND_THREAD);
    hwloc_bitmap_opal_asprintf(&buffer, set);
    printf("%s : [objs = %d] : cpuset is %s\n", (main_thread ? "Main" : "Peer"), num_objs, buffer);
    free(buffer);
    hwloc_bitmap_free(set);
}
