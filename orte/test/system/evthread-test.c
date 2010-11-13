#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#ifndef WIN32
#include <unistd.h>
#include <sys/time.h>
#endif
#include <errno.h>

#include <pthread.h>

#include <event2/util.h>
#include <event2/thread.h>
#include <event2/event.h>
#include <event2/event_struct.h>

static struct event_base *my_base=NULL;
static pthread_t progress_thread;
static bool progress_thread_stop=false;
static int progress_thread_pipe[2];
static pthread_mutex_t lock;
static struct event write_event;
static int my_fd;
static bool fd_written=false;

static void* progress_engine(void *obj);
static void send_handler(int sd, short flags, void *arg);

int main(int argc, char **argv)
{
    char byte='a';
    struct timespec tp={0, 100};
    int count=0;

    /* setup for threads */
    evthread_use_pthreads();

    /* create a new base */
    my_base = event_base_new();

    /* launch a progress thread on that base*/
    pipe(progress_thread_pipe);

    if (pthread_mutex_init(&lock, NULL)) {
     fprintf(stderr, "pthread_mutex_init failed\n");
      exit(1);
    }
    if (pthread_create(&progress_thread, NULL, progress_engine,
                       NULL)) {
     fprintf(stderr, "pthread_create failed\n");
      exit(1);
    }
    /*
      pthread starts the thread running itself; no need to do anything to
      launch it.
    */

    /* wait a little while - reflects reality in an async system */
    while (count < 100) {
        nanosleep(&tp, NULL);
        count++;
    }
    count=0;

#ifdef WAKE_WITH_EVENT
    /* make a dummy event */
    fprintf(stderr, "activating the write_event");
    event_assign(&write_event,
                 my_base,
                 -1,
                 0,
                 send_handler,
                 NULL);
    /* activate it. */
    event_active(&write_event, EV_WRITE, 1);
#else
    fprintf(stderr, "opening the file");
    /* define a file descriptor event - looks like an incoming socket
     * connection being created, if we're lucky.
     */
    my_fd = open("foo", O_CREAT | O_TRUNC | O_RDWR, 0644);
    if (my_fd <0) {
      perror("open");
      exit(1);
    }
    event_assign(&write_event,
                 my_base,
                 my_fd,
                 EV_WRITE|EV_PERSIST,
                 send_handler,
                 NULL);
    event_add(&write_event, NULL);
    if (write(progress_thread_pipe[1], &byte, 1) < 0) {
      perror("write");
      exit(1);
    }
#endif

    /* wait for it to trigger */
    while (!fd_written && count < 1000) {
        if (0 == (count % 100)) {
            fprintf(stderr, "Waiting...\n");
        }
        nanosleep(&tp, NULL);
        count++;
    }

    /* stop the thread */
    pthread_mutex_lock(&lock);
    progress_thread_stop = true;
    pthread_mutex_unlock(&lock);

    write(progress_thread_pipe[1], &byte, 1);
    pthread_join(progress_thread, NULL);

    return 0;
}

static struct event stop_event;
static void stop_handler(int sd, short flags, void* cbdata)
{
    char byte;
    int n;
    if ((n = read(progress_thread_pipe[0], &byte, 1)) <= 0) {
      if (n == 0)
        fprintf(stderr, "got a close\n");
      else
        perror("read");
    }

    /* reset the event */
    event_add(&stop_event, NULL);
    return;
}

static void* progress_engine(void *obj)
{
    /* define an event that will be used to kick us out of a blocking
     * situation when we want to exit
     */
    event_assign(&stop_event, my_base,
                 progress_thread_pipe[0], EV_READ, stop_handler, NULL);
    event_add(&stop_event, NULL);

    while (1) {
        pthread_mutex_lock(&lock);
        if (progress_thread_stop) {
            fprintf(stderr, "Thread stopping\n");
            pthread_mutex_unlock(&lock); /* moved this */
            event_del(&stop_event);
            return (void*)1;
        }
        pthread_mutex_unlock(&lock);
        fprintf(stderr, "Looping...\n");
        event_base_loop(my_base, EVLOOP_ONCE);
    }
}

static void send_handler(int sd, short flags, void *arg)
{
#ifdef WAKE_WITH_EVENT
    fprintf(stderr, "Write event fired\n");
#else
    char *bytes="This is an output string\n";

    fprintf(stderr, "Write event fired\n");
    if (write(my_fd, bytes, strlen(bytes)) < 0) {
      perror("write");
      exit(1);
    }
    event_del(&write_event);
#endif
    fd_written = true; /* This needs a lock around it if you are reading it
                        * in the main thread and changing it here XXX */
}
