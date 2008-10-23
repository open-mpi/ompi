/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/constants.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "orte/util/show_help.h"

#include "orte/mca/oob/base/base.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/iof/base/base.h"
#include "iof_hnp.h"

/* LOCAL FUNCTIONS */
static void stdin_write_handler(int fd, short event, void *cbdata);


/* API FUNCTIONS */
static int hnp_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd);

static int hnp_pull(const orte_process_name_t* src_name,
                orte_iof_tag_t src_tag,
                int fd);

static int hnp_close(const orte_process_name_t* peer,
                 orte_iof_tag_t source_tag);

static int hnp_ft_event(int state);

/* The API's in this module are solely used to support LOCAL
 * procs - i.e., procs that are co-located to the HNP. Remote
 * procs interact with the HNP's IOF via the HNP's receive function,
 * which operates independently and is in the iof_hnp_receive.c file
 */

orte_iof_base_module_t orte_iof_hnp_module = {
    hnp_push,
    hnp_pull,
    hnp_close,
    hnp_ft_event
};


/* Setup to read local data. If the tag is other than STDIN,
 * then this is output being pushed from one of my child processes
 * and I'll write the data out myself. If the tag is STDIN,
 * then I need to setup to read from my stdin, and send anything
 * I get to the specified dst_name. The dst_name in this case tells
 * us which procs are to get stdin - only two options are supported:
 *
 * (a) a specific name, usually vpid=0; or
 *
 * (b) all procs, specified by vpid=ORTE_VPID_WILDCARD
 *
 * The orte_plm_base_launch_apps function calls iof.push after
 * the procs are launched and tells us how to distribute stdin. This
 * ensures that the procs are started -before- we begin reading stdin
 * and attempting to send it to remote procs
 */
static int hnp_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd)
{
    orte_job_t *jdata;
    orte_proc_t **procs;
    orte_iof_sink_t *sink;
    
    /* don't do this if the dst vpid is invalid */
    if (ORTE_VPID_INVALID == dst_name->vpid) {
        return ORTE_SUCCESS;
    }
    
    if (!(src_tag & ORTE_IOF_STDIN)) {
        /* if we are not after stdin. then define a read event and activate it */
        ORTE_IOF_READ_EVENT(dst_name, fd, src_tag,
                            orte_iof_hnp_read_local_handler,
                            &mca_iof_hnp_component.read_events, true);
        return ORTE_SUCCESS;
    }

    /* if we are pushing stdin, this is happening only during launch - setup
     * a target for this destination if it is going somewhere other than me
     */
    if (ORTE_VPID_WILDCARD == dst_name->vpid) {
        /* if wildcard, define a sink with that info so it gets sent out */
        ORTE_IOF_SINK_DEFINE(&sink, dst_name, -1, src_tag,
                             stdin_write_handler,
                             &mca_iof_hnp_component.sinks);
    } else {
        /* no - lookup the proc's daemon and set that into sink */
        if (NULL == (jdata = orte_get_job_data_object(dst_name->jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return ORTE_ERR_BAD_PARAM;
        }
        procs = (orte_proc_t**)jdata->procs->addr;
        /* if it is me, then don't set this up - we'll get it on the pull */
        if (ORTE_PROC_MY_NAME->vpid != procs[dst_name->vpid]->node->daemon->name.vpid) {
            ORTE_IOF_SINK_DEFINE(&sink, dst_name, -1, src_tag,
                                 stdin_write_handler,
                                 &mca_iof_hnp_component.sinks);
            sink->daemon.jobid = ORTE_PROC_MY_NAME->jobid;
            sink->daemon.vpid = procs[dst_name->vpid]->node->daemon->name.vpid;
        }
    }
    
    /* now setup the read - but check to only do this once */
    if (NULL == mca_iof_hnp_component.stdinev) {
        /* Since we are the HNP, we don't want to set nonblocking on our
         * file descriptors.  If we do so, we set the file descriptor to
         * non-blocking for everyone that has that file descriptor, which
         * includes everyone else in our shell pipeline chain.  (See
         * http://lists.freebsd.org/pipermail/freebsd-hackers/2005-January/009742.html).
         * This causes things like "mpirun -np 1 big_app | cat" to lose
         * output, because cat's stdout is then ALSO non-blocking and cat
         * isn't built to deal with that case (same with almost all other
         * unix text utils). 
         */
        
        if (isatty(fd)) {
            /* We should avoid trying to read from stdin if we
             * have a terminal, but are backgrounded.  Catch the
             * signals that are commonly used when we switch
             * between being backgrounded and not.  If the
             * filedescriptor is not a tty, don't worry about it
             * and always stay connected.
             */
            opal_signal_set(&mca_iof_hnp_component.stdinsig,
                            SIGCONT, orte_iof_hnp_stdin_cb,
                            NULL);
            
            /* setup a read event to read stdin, but don't activate it yet. The
             * dst_name indicates who should receive the stdin. If that recipient
             * doesn't do a corresponding pull, however, then the stdin will
             * be dropped upon receipt at the local daemon
             */
            ORTE_IOF_READ_EVENT(dst_name, fd, src_tag,
                                orte_iof_hnp_read_local_handler,
                                &mca_iof_hnp_component.read_events, false);
            /* save it somewhere convenient */
            mca_iof_hnp_component.stdinev =
            (orte_iof_read_event_t*)opal_list_get_first(&mca_iof_hnp_component.read_events);
            
            /* check to see if we want the stdin read event to be
             * active - we will always at least define the event,
             * but may delay its activation
             */
            if (!(src_tag & ORTE_IOF_STDIN) || orte_iof_hnp_stdin_check(fd)) {
                mca_iof_hnp_component.stdinev->active = true;
                opal_event_add(&(mca_iof_hnp_component.stdinev->ev), 0);
            }
        } else{
            /* if we are not looking at a tty, just setup a read event
             * and activate it
             */
            ORTE_IOF_READ_EVENT(dst_name, fd, src_tag,
                                orte_iof_hnp_read_local_handler,
                                &mca_iof_hnp_component.read_events, false);
            
            /* save it somewhere convenient */
            mca_iof_hnp_component.stdinev =
            (orte_iof_read_event_t*)opal_list_get_first(&mca_iof_hnp_component.read_events);
            /* flag that it is operational */
            mca_iof_hnp_component.stdinev->active = true;
            /* activate it */
            opal_event_add(&(mca_iof_hnp_component.stdinev->ev), 0);
        }
    }
        
    return ORTE_SUCCESS;
}


/*
 * Since we are the HNP, the only "pull" call comes from a local
 * process so we can record the file descriptor for its stdin.
 */

static int hnp_pull(const orte_process_name_t* dst_name,
                    orte_iof_tag_t src_tag,
                    int fd)
{
    orte_iof_sink_t *sink;
    
    /* this is a local call - only stdin is supported */
    if (ORTE_IOF_STDIN != src_tag) {
        return ORTE_ERR_NOT_SUPPORTED;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s hnp:pull setting up %s to pass stdin",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(dst_name)));

    ORTE_IOF_SINK_DEFINE(&sink, dst_name, fd, src_tag,
                         stdin_write_handler,
                         &mca_iof_hnp_component.sinks);
    sink->daemon.jobid = ORTE_PROC_MY_NAME->jobid;
    sink->daemon.vpid = ORTE_PROC_MY_NAME->vpid;

    return ORTE_SUCCESS;
}

/*
 * One of our local procs wants us to close the specifed
 * stream(s), thus terminating any potential io to/from it.
 */
static int hnp_close(const orte_process_name_t* peer,
                 orte_iof_tag_t source_tag)
{
    return ORTE_SUCCESS;
}

int hnp_ft_event(int state) {
    /*
     * Replica doesn't need to do anything for a checkpoint
     */
    return ORTE_SUCCESS;
}


static void stdin_write_handler(int fd, short event, void *cbdata)
{
    orte_iof_write_event_t *wev = (orte_iof_write_event_t*)cbdata;
    opal_list_item_t *item;
    orte_iof_write_output_t *output;
    int num_written;
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s hnp:stdin:write:handler writing data to %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         wev->fd));
    
    /* lock us up to protect global operations */
    OPAL_THREAD_LOCK(&mca_iof_hnp_component.lock);
    
    while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
        output = (orte_iof_write_output_t*)item;
        if (0 == output->numbytes) {
            /* this indicates we are to close the fd - there is
             * nothing to write
             */
            close(wev->fd);
            /* be sure to delete the write event */
            opal_event_del(&wev->ev);
            wev->pending = false;
            /* just leave - we don't want to restart the
             * read event!
             */
            goto DEPART;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        if (num_written < output->numbytes) {
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                                 "incomplete write %d - adjusting data", num_written));
            /* incomplete write - adjust data to avoid duplicate output */
            memmove(output->data, &output->data[num_written], output->numbytes - num_written);
            /* push this item back on the front of the list */
            opal_list_prepend(&wev->outputs, item);
            /* leave the write event running so it will call us again
             * when the fd is ready. 
             */
            goto CHECK;
        }
        OBJ_RELEASE(output);
    }
    opal_event_del(&wev->ev);
    wev->pending = false;
    
CHECK:
    if (!mca_iof_hnp_component.stdinev->active) {
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                            "read event is off - checking if okay to restart"));
        /* if we have turned off the read event, check to
         * see if the output list has shrunk enough to
         * turn it back on
         *
         * RHC: Note that when multiple procs want stdin, we
         * can get into a fight between a proc turnin stdin
         * back "on" and other procs turning it "off". There
         * is no clear way to resolve this as different procs
         * may take input at different rates.
         */
        if (opal_list_get_size(&wev->outputs) < ORTE_IOF_MAX_INPUT_BUFFERS) {
            /* restart the read */
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                                 "restarting read event"));
            mca_iof_hnp_component.stdinev->active = true;
            opal_event_add(&(mca_iof_hnp_component.stdinev->ev), 0);
        }
    }

DEPART:
    /* unlock and go */
    OPAL_THREAD_UNLOCK(&mca_iof_hnp_component.lock);
}
