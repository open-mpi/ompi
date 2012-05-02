#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifndef WIN32
#include <unistd.h>
#include <sys/time.h>
#endif
#include <errno.h>

#include "opal/util/fd.h"
#include "opal/threads/threads.h"
#include "opal/runtime/opal.h"
#include "opal/mca/event/event.h"

static opal_event_base_t *my_base=NULL;
static opal_thread_t progress_thread;
static bool progress_thread_stop=false;
static int progress_thread_pipe[2];
static opal_mutex_t lock;
static opal_condition_t cond;
static bool active=false;
static opal_event_t write_event;
static int my_fd;
static bool fd_written=false;

static void* progress_engine(opal_object_t *obj);
static void send_handler(int sd, short flags, void *arg);

int main(int argc, char **argv)
{
    char byte='a';
    struct timespec tp={0, 100};
    int count=0;

    /* Initialize the event library */
    opal_init(&argc, &argv);

    /* setup for threads */
    opal_event_use_threads();

    /* create a new base */
    my_base = opal_event_base_create();

    /* launch a progress thread on that base*/
    pipe(progress_thread_pipe);
    OBJ_CONSTRUCT(&lock, opal_mutex_t);
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    OBJ_CONSTRUCT(&progress_thread, opal_thread_t);
    progress_thread.t_run = progress_engine;
    if (OPAL_SUCCESS != opal_thread_start(&progress_thread)) {
        fprintf(stderr, "Unable to start progress thread\n");
        exit(1);
    }

    /* wait a little while - reflects reality in an async system */
    while (count < 100) {
        nanosleep(&tp, NULL);
        count++;
    }
    count=0;

    /* define a file descriptor event - looks like an incoming socket
     * connection being created
     */
    if (0 > (my_fd = open("foo", O_CREAT | O_TRUNC | O_RDWR, 0644))) {
        fprintf(stderr, "Couldnt open file\n");
        exit(1);
    }
    opal_event_set(my_base,
                   &write_event,
                   my_fd,
                   OPAL_EV_WRITE|OPAL_EV_PERSIST,
                   send_handler,
                   NULL);
    opal_event_add(&write_event, 0);
    /*    opal_fd_write(progress_thread_pipe[1], 1, &byte); */

    /* wait for it to trigger */
    while (count < 1000) {
        OPAL_ACQUIRE_THREAD(&lock, &cond, &active);
        if (fd_written) {
            OPAL_RELEASE_THREAD(&lock, &cond, &active);
            break;
        }
        OPAL_RELEASE_THREAD(&lock, &cond, &active);
        if (0 == (count % 100)) {
            fprintf(stderr, "Waiting...\n");
        }
        nanosleep(&tp, NULL);
        count++;
    }
    fprintf(stderr, "Done waiting\n");

    /* stop the thread */
    OPAL_ACQUIRE_THREAD(&lock, &cond, &active);
    progress_thread_stop = true;
    OPAL_RELEASE_THREAD(&lock, &cond, &active);
    opal_fd_write(progress_thread_pipe[1], 1, &byte);
    opal_thread_join(&progress_thread, NULL);

    opal_finalize();
    return 0;
}

static opal_event_t stop_event;
static void stop_handler(int sd, short flags, void* cbdata)
{
    char byte;
    opal_fd_read(progress_thread_pipe[0], 1, &byte);
    fprintf(stderr, "Stop handler called\n");
    /* reset the event */
    opal_event_add(&stop_event, 0);
    return;
}

static void* progress_engine(opal_object_t *obj)
{
    /* define an event that will be used to kick us out of a blocking
     * situation when we want to exit
     */
    opal_event_set(my_base, &stop_event,
                   progress_thread_pipe[0], OPAL_EV_READ, stop_handler, NULL);
    opal_event_add(&stop_event, 0);

    while (1) {
        OPAL_ACQUIRE_THREAD(&lock, &cond, &active);
        if (progress_thread_stop) {
            fprintf(stderr, "Thread stopping\n");
            OPAL_RELEASE_THREAD(&lock, &cond, &active);
            opal_event_del(&stop_event);
            return OPAL_THREAD_CANCELLED;
        }
        OPAL_RELEASE_THREAD(&lock, &cond, &active);
        fprintf(stderr, "Looping...\n");
        opal_event_loop(my_base, OPAL_EVLOOP_ONCE);
    }
}

static void send_handler(int sd, short flags, void *arg)
{
    char *bytes="This is an output string\n";

    fprintf(stderr, "Write event fired\n");
    opal_fd_write(my_fd, strlen(bytes), bytes);
    opal_event_del(&write_event);
    OPAL_ACQUIRE_THREAD(&lock, &cond, &active);
    fd_written = true;
    OPAL_RELEASE_THREAD(&lock, &cond, &active);
}
