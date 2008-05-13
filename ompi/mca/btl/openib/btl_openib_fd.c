/*
 * Copyright (c) 2008 Cisco, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "orte/util/output.h"

#include "ompi/constants.h"

#include "btl_openib_fd.h"


/*
 * Data for each registered item
 */
typedef struct {
    opal_list_item_t super;
    bool ri_event_used;
    opal_event_t ri_event;
    int ri_fd;
    int ri_flags;
    union {
        ompi_btl_openib_fd_callback_fn_t *fd;
        ompi_btl_openib_schedule_callback_fn_t *schedule;
    } ri_callback;
    void *ri_context;
} registered_item_t;

static OBJ_CLASS_INSTANCE(registered_item_t, opal_list_item_t, NULL, NULL);

/*
 * Command types
 */
typedef enum {
    CMD_TIME_TO_QUIT,
    CMD_ADD_FD,
    CMD_REMOVE_FD,
    CMD_MAX
} cmd_type_t;

/*
 * Commands.  Fields ordered to avoid memory holes (and valgrind warnings).
 */
typedef struct {
    ompi_btl_openib_fd_callback_fn_t *pc_callback;
    void *pc_context;
    int pc_fd;
    int pc_flags;
    cmd_type_t pc_cmd;
    char end;
} cmd_t;

static bool initialized = false;
static int cmd_size = 0;
static fd_set read_fds, write_fds;
static int max_fd;
static opal_list_t registered_items;

/* These items are only used in the threaded version */
static pthread_t thread;
static int pipe_fd[2] = { -1, -1 };


static void libevent_fd_callback(int fd, short event, void *context)
{
    registered_item_t *ri = (registered_item_t*) context;
    ri->ri_callback.fd(fd, event, ri->ri_context);
}


static void libevent_event_callback(int fd, short event, void *context)
{
    registered_item_t *ri = (registered_item_t*) context;
    ri->ri_callback.schedule(ri->ri_context);
    /* JMS Can I free ri now?  It contains the event... */
#if 0
    OBJ_RELEASE(ri);
#endif    
}


/*
 * Add an fd to the listening set
 */
static int local_pipe_cmd_add_fd(bool use_libevent, cmd_t *cmd)
{
    registered_item_t *ri = OBJ_NEW(registered_item_t);
    if (NULL == ri) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    ri->ri_event_used = false;
    ri->ri_fd = cmd->pc_fd;
    ri->ri_flags = cmd->pc_flags;
    ri->ri_callback.fd = cmd->pc_callback;
    ri->ri_context = cmd->pc_context;

    if (use_libevent) {
        /* Make an event for this fd */
        ri->ri_event_used = true;
        memset(&ri->ri_event, 0, sizeof(ri->ri_event));
        opal_event_set(&ri->ri_event, ri->ri_fd, 
                       ri->ri_flags | OPAL_EV_PERSIST, libevent_fd_callback,
                       ri);
        opal_event_add(&ri->ri_event, 0);
    } else {
        /* Add the fd to the relevant fd local sets and update max_fd */
        if (OPAL_EV_READ & ri->ri_flags) {
            FD_SET(ri->ri_fd, &read_fds);
        }
        if (OPAL_EV_WRITE & cmd->pc_flags) {
            FD_SET(ri->ri_fd, &write_fds);
        }
        max_fd = (max_fd > ri->ri_fd) ? max_fd : ri->ri_fd + 1;
    }

    opal_list_append(&registered_items, &ri->super);
    return OMPI_SUCCESS;
}


/*
 * Remove an fd from the listening set
 */
static int local_pipe_cmd_remove_fd(cmd_t *cmd)
{
    int i;
    opal_list_item_t *item;
    registered_item_t *ri;

    /* Go through the list of registered fd's and find the fd to
       remove */
    for (item = opal_list_get_first(&registered_items);
         NULL != opal_list_get_end(&registered_items);
         item = opal_list_get_next(item)) {
        ri = (registered_item_t*) item;
        if (cmd->pc_fd == ri->ri_fd) {
            /* Found it.  The item knows if it was used as a libevent
               event or an entry in the local fd sets. */
            if (ri->ri_event_used) {
                /* Remove this event from libevent */
                opal_event_del(&ri->ri_event);
            } else {
                /* Remove this item from the fd_sets and recalculate
                   max_fd */
                FD_CLR(cmd->pc_fd, &read_fds);
                FD_CLR(cmd->pc_fd, &write_fds);
                for (max_fd = i = pipe_fd[0]; i < FD_SETSIZE; ++i) {
                    if (FD_ISSET(i, &read_fds) || FD_ISSET(i, &write_fds)) {
                        max_fd = i + 1;
                    }
                }
            }
            
            /* Let the caller know that we have stopped monitoring
               this fd (if they care) */
            if (NULL != cmd->pc_callback) {
                cmd->pc_callback(cmd->pc_fd, 0, cmd->pc_context);
            }
    
            /* Remove this item from the list of registered items and
               release it */
            opal_list_remove_item(&registered_items, item);
            OBJ_RELEASE(item);
            return OMPI_SUCCESS;
        }
    }

    /* This shouldn't happen */
    return OMPI_ERR_NOT_FOUND;
}

/*
 * Simple loop over reading from a fd
 */
static int read_fd(int fd, int len, void *buffer)
{
    int rc;
    char *b = buffer;

    while (len > 0) {
        rc = read(fd, b, len);
        if (rc < 0 && EAGAIN == errno) {
            continue;
        } else if (rc > 0) {
            len -= rc;
            b += rc;
        } else {
            return OMPI_ERROR;
        }
    }
    return OMPI_SUCCESS;
}


/*
 * Simple loop over writing to an fd
 */
static int write_fd(int fd, int len, void *buffer)
{
    int rc;
    char *b = buffer;

    while (len > 0) {
        rc = write(fd, b, len);
        if (rc < 0 && EAGAIN == errno) {
            continue;
        } else if (rc > 0) {
            len -= rc;
            b += rc;
        } else {
            return OMPI_ERROR;
        }
    }
    return OMPI_SUCCESS;
}


/*
 * Act on pipe commands
 */
static bool local_pipe_cmd(void)
{
    bool ret = false;
    cmd_t cmd;

    read_fd(pipe_fd[0], cmd_size, &cmd);
    switch (cmd.pc_cmd) {
    case CMD_ADD_FD:
        if (OMPI_SUCCESS != local_pipe_cmd_add_fd(false, &cmd)) {
            ret = true;
        }
        break;

    case CMD_REMOVE_FD:
        if (OMPI_SUCCESS != local_pipe_cmd_remove_fd(&cmd)) {
            ret = true;
        }
        break;

    case CMD_TIME_TO_QUIT:
        orte_output(-1, "fd listener thread: time to quit");
        ret = true;
        break;

    default:
        orte_output(-1, "fd listener thread: unknown pipe command!");
        break;
    }

    return ret;
}


/*
 * Main thread logic
 */
static void *thread_main(void *context)
{
    int rc, flags;
    fd_set read_fds_copy, write_fds_copy;
    opal_list_item_t *item;
    registered_item_t *ri;

    /* Make an fd set that we can select() on */
    FD_ZERO(&write_fds);
    FD_ZERO(&read_fds);
    FD_SET(pipe_fd[0], &read_fds);
    max_fd = pipe_fd[0] + 1;

    orte_output(-1, "fd listener thread running");

    /* Main loop waiting for commands over the fd's */
    while (1) {
        memcpy(&read_fds_copy, &read_fds, sizeof(read_fds));
        memcpy(&write_fds_copy, &write_fds, sizeof(write_fds));
        orte_output(-1, "fd listener thread blocking on select...");
        rc = select(max_fd, &read_fds_copy, &write_fds_copy, NULL, NULL);
        if (0 != rc && EAGAIN == errno) {
            continue;
        }

        orte_output(-1, "fd listener thread woke up!");
        if (rc > 0) {
            if (FD_ISSET(pipe_fd[0], &read_fds_copy)) {
                orte_output(-1, "fd listener thread: pipe command");
                if (local_pipe_cmd()) {
                    orte_output(-1, "fd listener thread: exiting");
                    break;
                }
            } 

            /* Go through all the registered events and see who had
               activity */
            if (!opal_list_is_empty(&registered_items)) {
                for (item = opal_list_get_first(&registered_items);
                     item != opal_list_get_end(&registered_items);
                     item = opal_list_get_next(item)) {
                    ri = (registered_item_t*) item;
                    flags = 0;

                    /* See if this fd was ready for reading or writing
                       (fd's will only be in the read_fds or write_fds
                       set depending on what they registered for) */
                    if (FD_ISSET(ri->ri_fd, &read_fds_copy)) {
                        flags |= OPAL_EV_READ;
                    }
                    if (FD_ISSET(ri->ri_fd, &write_fds_copy)) {
                        flags |= OPAL_EV_WRITE;
                    }

                    /* If either was ready, invoke the callback */
                    if (0 != flags) {
                        orte_output(-1, "fd listener thread: invoking callback for registered fd %d", ri->ri_fd);
                        ri->ri_callback.fd(ri->ri_fd, flags, 
                                           ri->ri_context);
                    }
                }
            }
        }
    }

    /* All done */
    return NULL;
}


/*
 * Initialize
 */
int ompi_btl_openib_fd_init(void)
{
    if (!initialized) {
        cmd_t bogus;

        OBJ_CONSTRUCT(&registered_items, opal_list_t);

        if (OMPI_HAVE_THREAD_SUPPORT) {
            /* Create a pipe to communicate with the thread */
            if (0 != pipe(pipe_fd)) {
                return OMPI_ERR_IN_ERRNO;
            }

            if (0 != pthread_create(&thread, NULL, thread_main, NULL)) {
                return OMPI_ERR_IN_ERRNO;
            }
        }

        /* Calculate the real size of the cmd struct */
        cmd_size = (int) (&(bogus.end) - ((char*) &bogus));
        initialized = true;
    }
    return OMPI_SUCCESS;
}


/*
 * Start monitoring an fd
 */
int ompi_btl_openib_fd_monitor(int fd, int flags, 
                               ompi_btl_openib_fd_callback_fn_t *callback,
                               void *context)
{
    cmd_t cmd;

    /* Sanity check */
    if (fd < 0 || 0 == flags || NULL == callback) {
        return OMPI_ERR_BAD_PARAM;
    }

    cmd.pc_cmd = CMD_ADD_FD;
    cmd.pc_fd = fd;
    cmd.pc_flags = flags;
    cmd.pc_callback = callback;
    cmd.pc_context = context;
    if (OMPI_HAVE_THREAD_SUPPORT) {
        /* For the threaded version, write a command down the pipe */
        write_fd(pipe_fd[1], cmd_size, &cmd);
    } else {
        /* Otherwise, add it directly */
        local_pipe_cmd_add_fd(true, &cmd);
    }

    return OMPI_SUCCESS;
}


/*
 * Stop monitoring an fd
 */
int ompi_btl_openib_fd_unmonitor(int fd, 
                                 ompi_btl_openib_fd_callback_fn_t *callback,
                                 void *context)
{
    cmd_t cmd;

    /* Sanity check */
    if (fd < 0) {
        return OMPI_ERR_BAD_PARAM;
    }
    
    cmd.pc_cmd = CMD_REMOVE_FD;
    cmd.pc_fd = fd;
    cmd.pc_flags = 0;
    cmd.pc_callback = callback;
    cmd.pc_context = context;
    if (OMPI_HAVE_THREAD_SUPPORT) {
        /* For the threaded version, write a command down the pipe */
        write_fd(pipe_fd[1], cmd_size, &cmd);
    } else {
        /* Otherwise, remove it directly */
        local_pipe_cmd_remove_fd(&cmd);
    }

    return OMPI_SUCCESS;
}

/*
 * Run a function in the main thread
 */
int ompi_btl_openib_fd_schedule(ompi_btl_openib_schedule_callback_fn_t *callback,
                                void *context)
{
    if (OMPI_HAVE_THREAD_SUPPORT) {
        /* For the threaded version, schedule an event for "now" */
        registered_item_t *ri;
        struct timeval now;

        ri = OBJ_NEW(registered_item_t);
        if (NULL == ri) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* Create an event that will run in the main thread */
        ri->ri_fd = ri->ri_flags = -1;
        ri->ri_callback.schedule = callback;
        ri->ri_context = context;
        ri->ri_event_used = true;
        opal_evtimer_set(&ri->ri_event, libevent_event_callback, ri);
        now.tv_sec = 0;
        now.tv_usec = 0;
        opal_evtimer_add(&ri->ri_event, &now);
    } else {
        /* For the non-threaded version, just call the function */
        callback(context);
    }

    return OMPI_SUCCESS;
}

/*
 * Finalize
 */
int ompi_btl_openib_fd_finalize(void)
{
    if (initialized) {
        if (OMPI_HAVE_THREAD_SUPPORT) {
            /* For the threaded version, send a command down the pipe */
            cmd_t cmd;
            memset(&cmd, 0, cmd_size);
            cmd.pc_cmd = CMD_TIME_TO_QUIT;
            write_fd(pipe_fd[1], cmd_size, &cmd);
            
            pthread_join(thread, NULL);
            close(pipe_fd[0]);
            close(pipe_fd[1]);
        }
    }
    initialized = false;

    return OMPI_SUCCESS;
}
