/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "opal/util/output.h"
#include "orte/constants.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#else
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#endif

#include "opal/mca/event/event.h"

#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/mca/odls/odls_types.h"

#include "orte/mca/iof/base/base.h"
#include "iof_mrhnp.h"

/* LOCAL FUNCTIONS */
static void stdin_write_handler(int fd, short event, void *cbdata);

/* API FUNCTIONS */
static int init(void);

static int mrhnp_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd);

static int mrhnp_pull(const orte_process_name_t* src_name,
                      orte_iof_tag_t src_tag,
                      int fd);

static int mrhnp_close(const orte_process_name_t* peer,
                       orte_iof_tag_t source_tag);

static void mrhnp_complete(const orte_job_t *jdata);

static int finalize(void);

static int mrhnp_ft_event(int state);

/* The API's in this module are solely used to support LOCAL
 * procs - i.e., procs that are co-located to the HNP. Remote
 * procs interact with the HNP's IOF via the HNP's receive function,
 * which operates independently and is in the iof_mrhnp_receive.c file
 */

orte_iof_base_module_t orte_iof_mrhnp_module = {
    init,
    mrhnp_push,
    mrhnp_pull,
    mrhnp_close,
    mrhnp_complete,
    finalize,
    mrhnp_ft_event
};

/* Initialize the module */
static int init(void)
{
    /* post non-blocking recv to catch forwarded IO from
     * the orteds
     */    
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_IOF_HNP,
                            ORTE_RML_PERSISTENT,
                            orte_iof_mrhnp_recv,
                            NULL);

    OBJ_CONSTRUCT(&mca_iof_mr_hnp_component.sinks, opal_list_t);
    OBJ_CONSTRUCT(&mca_iof_mr_hnp_component.procs, opal_list_t);
    mca_iof_mr_hnp_component.stdinev = NULL;
    OBJ_CONSTRUCT(&mca_iof_mr_hnp_component.stdin_jobs, opal_pointer_array_t);
    opal_pointer_array_init(&mca_iof_mr_hnp_component.stdin_jobs, 1, INT_MAX, 1);
    return ORTE_SUCCESS;
}

/* Setup to read from stdin. 
 */
static int mrhnp_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd)
{
    orte_job_t *jdata;
    orte_iof_sink_t *sink;
    orte_iof_proc_t *proct;
    opal_list_item_t *item;
    int flags;
    char *outfile;
    int fdout;
    int np, numdigs;
    orte_ns_cmp_bitmask_t mask;
    orte_iof_job_t *jptr;
    int j;
    bool found;

    /* don't do this if the dst vpid is invalid or the fd is negative! */
    if (ORTE_VPID_INVALID == dst_name->vpid || fd < 0) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s iof:mrhnp pushing fd %d for process %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         fd, ORTE_NAME_PRINT(dst_name)));
    
    /* we get a push for stdout, stderr, and stddiag on every LOCAL process, so
     * setup to read those streams and forward them to the next app_context
     */
    if (!(src_tag & ORTE_IOF_STDIN)) {
        /* set the file descriptor to non-blocking - do this before we setup
         * and activate the read event in case it fires right away
         */
        if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
            opal_output(orte_iof_base_framework.framework_output, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", 
                        __FILE__, __LINE__, errno);
        } else {
            flags |= O_NONBLOCK;
            fcntl(fd, F_SETFL, flags);
        }
        /* do we already have this process in our list? */
        for (item = opal_list_get_first(&mca_iof_mr_hnp_component.procs);
             item != opal_list_get_end(&mca_iof_mr_hnp_component.procs);
             item = opal_list_get_next(item)) {
            proct = (orte_iof_proc_t*)item;
            mask = ORTE_NS_CMP_ALL;
            if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &proct->name, dst_name)) {
                /* found it */
                goto SETUP;
            }
        }
        /* if we get here, then we don't yet have this proc in our list */
        proct = OBJ_NEW(orte_iof_proc_t);
        proct->name.jobid = dst_name->jobid;
        proct->name.vpid = dst_name->vpid;
        opal_list_append(&mca_iof_mr_hnp_component.procs, &proct->super);
        /* see if we are to output to a file */
        if (NULL != orte_output_filename) {
            /* get the jobdata for this proc */
            if (NULL == (jdata = orte_get_job_data_object(dst_name->jobid))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                return ORTE_ERR_NOT_FOUND;
            }
            np = jdata->num_procs / 10;
            /* determine the number of digits required for max vpid */
            numdigs = 1;
            while (np > 0) {
                numdigs++;
                np = np / 10;
            }
            /* construct the filename */
            asprintf(&outfile, "%s.%d.%0*lu", orte_output_filename,
                     (int)ORTE_LOCAL_JOBID(proct->name.jobid),
                     numdigs, (unsigned long)proct->name.vpid);
            /* create the file */
            fdout = open(outfile, O_CREAT|O_RDWR|O_TRUNC, 0644);
            free(outfile);
            if (fdout < 0) {
                /* couldn't be opened */
                ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
                return ORTE_ERR_FILE_OPEN_FAILURE;
            }
            /* define a sink to that file descriptor */
            ORTE_IOF_SINK_DEFINE(&sink, dst_name, fdout, ORTE_IOF_STDOUTALL,
                                 orte_iof_base_write_handler,
                                 &mca_iof_mr_hnp_component.sinks);
        }
        
    SETUP:
        /* define a read event but don't activate it */
        if (src_tag & ORTE_IOF_STDOUT) {
            ORTE_IOF_READ_EVENT(&proct->revstdout, dst_name, fd, ORTE_IOF_STDOUT,
                                orte_iof_mrhnp_read_local_handler, false);
        } else if (src_tag & ORTE_IOF_STDERR) {
            ORTE_IOF_READ_EVENT(&proct->revstderr, dst_name, fd, ORTE_IOF_STDERR,
                                orte_iof_mrhnp_read_local_handler, false);
        } else if (src_tag & ORTE_IOF_STDDIAG) {
            ORTE_IOF_READ_EVENT(&proct->revstddiag, dst_name, fd, ORTE_IOF_STDDIAG,
                                orte_iof_mrhnp_read_local_handler, false);
        }
        /* if -all- of the readevents for this proc have been defined, then
         * activate them. Otherwise, we can think that the proc is complete
         * because one of the readevents fires -prior- to all of them having been defined!
         */
        if (NULL != proct->revstdout && NULL != proct->revstderr && NULL != proct->revstddiag) {
            /* now activate read events */
            proct->revstdout->active = true;
            opal_event_add(proct->revstdout->ev, 0);
            proct->revstderr->active = true;
            opal_event_add(proct->revstderr->ev, 0);
            proct->revstddiag->active = true;
            opal_event_add(proct->revstddiag->ev, 0);
        }
        return ORTE_SUCCESS;
    }

    /*** HANDLE STDIN PUSH ***/

    /* get the job object for this proc and check to see if it
     * is a mapper - if so, add it to the jobs that receive
     * our stdin
     */
    jdata = orte_get_job_data_object(dst_name->jobid);
    if (ORTE_JOB_CONTROL_MAPPER & jdata->controls) {
        /* see if we already have it */
        found = false;
        for (j=0; j < mca_iof_mr_hnp_component.stdin_jobs.size; j++) {
            if (NULL == (jptr = (orte_iof_job_t*)opal_pointer_array_get_item(&mca_iof_mr_hnp_component.stdin_jobs, j))) {
                continue;
            }
            if (jptr->jdata->jobid == jdata->jobid) {
                found = true;
                break;
            }
        }
        if (!found) {
            jptr = OBJ_NEW(orte_iof_job_t);
            OBJ_RETAIN(jdata);
            jptr->jdata = jdata;
            opal_bitmap_init(&jptr->xoff, jdata->num_procs);
            opal_pointer_array_add(&mca_iof_mr_hnp_component.stdin_jobs, jptr);
        }
    }

    /* now setup the read - but check to only do this once */
    if (NULL == mca_iof_mr_hnp_component.stdinev) {
        /* Since we are the HNP, we don't want to set nonblocking on our
         * stdio stream.  If we do so, we set the file descriptor to
         * non-blocking for everyone that has that file descriptor, which
         * includes everyone else in our shell pipeline chain.  (See
         * http://lists.freebsd.org/pipermail/freebsd-hackers/2005-January/009742.html).
         * This causes things like "mpirun -np 1 big_app | cat" to lose
         * output, because cat's stdout is then ALSO non-blocking and cat
         * isn't built to deal with that case (same with almost all other
         * unix text utils). 
         */
        if (0 != fd) {
            if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
                opal_output(orte_iof_base_framework.framework_output, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", 
                            __FILE__, __LINE__, errno);
            } else {
                flags |= O_NONBLOCK;
                fcntl(fd, F_SETFL, flags);
            }            
        }
        if (isatty(fd)) {
            /* We should avoid trying to read from stdin if we
             * have a terminal, but are backgrounded.  Catch the
             * signals that are commonly used when we switch
             * between being backgrounded and not.  If the
             * filedescriptor is not a tty, don't worry about it
             * and always stay connected.
             */
            opal_event_signal_set(orte_event_base, &mca_iof_mr_hnp_component.stdinsig,
                                  SIGCONT, orte_iof_mrhnp_stdin_cb,
                                  NULL);
            
            /* setup a read event to read stdin, but don't activate it yet. The
             * dst_name indicates who should receive the stdin. If that recipient
             * doesn't do a corresponding pull, however, then the stdin will
             * be dropped upon receipt at the local daemon
             */
            ORTE_IOF_READ_EVENT(&mca_iof_mr_hnp_component.stdinev,
                                dst_name, fd, ORTE_IOF_STDIN,
                                orte_iof_mrhnp_read_local_handler, false);
            
            /* check to see if we want the stdin read event to be
             * active - we will always at least define the event,
             * but may delay its activation
             */
            if (!(src_tag & ORTE_IOF_STDIN) || orte_iof_mrhnp_stdin_check(fd)) {
                mca_iof_mr_hnp_component.stdinev->active = true;
                opal_event_add(mca_iof_mr_hnp_component.stdinev->ev, 0);
            }
        } else {
            /* if we are not looking at a tty, just setup a read event
             * and activate it
             */
            ORTE_IOF_READ_EVENT(&mca_iof_mr_hnp_component.stdinev,
                                dst_name, fd, ORTE_IOF_STDIN,
                                orte_iof_mrhnp_read_local_handler, true);
        }
    }
    return ORTE_SUCCESS;
}


/*
 * Since we are the HNP, the only "pull" call comes from a local
 * process so we can record the file descriptor for its stdin.
 */

static int mrhnp_pull(const orte_process_name_t* dst_name,
                    orte_iof_tag_t src_tag,
                    int fd)
{
    orte_iof_sink_t *sink;
    int flags, j;
    orte_iof_proc_t *ptr, *proct;
    opal_list_item_t *item;
    orte_job_t *jdata;
    orte_iof_job_t *jptr;
    bool found;

    /* this is a local call - only stdin is supported */
    if (ORTE_IOF_STDIN != src_tag) {
        return ORTE_ERR_NOT_SUPPORTED;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s iof:mrhnp pulling fd %d for process %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         fd, ORTE_NAME_PRINT(dst_name)));
    
    /* get the job object for this proc and check to see if it
     * is a mapper - if so, add it to the jobs that receive
     * our stdin
     */
    jdata = orte_get_job_data_object(dst_name->jobid);
    if (ORTE_JOB_CONTROL_MAPPER & jdata->controls) {
        /* see if we already have it */
        found = false;
        for (j=0; j < mca_iof_mr_hnp_component.stdin_jobs.size; j++) {
            if (NULL == (jptr = (orte_iof_job_t*)opal_pointer_array_get_item(&mca_iof_mr_hnp_component.stdin_jobs, j))) {
                continue;
            }
            if (jptr->jdata->jobid == jdata->jobid) {
                found = true;
                break;
            }
        }
        if (!found) {
            jptr = OBJ_NEW(orte_iof_job_t);
            OBJ_RETAIN(jdata);
            jptr->jdata = jdata;
            opal_bitmap_init(&jptr->xoff, jdata->num_procs);
            opal_pointer_array_add(&mca_iof_mr_hnp_component.stdin_jobs, jptr);
        }
    }

    /* set the file descriptor to non-blocking - do this before we setup
     * the sink in case it fires right away
     */
    if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        opal_output(orte_iof_base_framework.framework_output, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", 
                    __FILE__, __LINE__, errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }
    
    ORTE_IOF_SINK_DEFINE(&sink, dst_name, fd, ORTE_IOF_STDIN,
                         stdin_write_handler, NULL);
    sink->daemon.jobid = ORTE_PROC_MY_NAME->jobid;
    sink->daemon.vpid = ORTE_PROC_MY_NAME->vpid;

    /* find the proct for this proc */
    proct = NULL;
    for (item = opal_list_get_first(&mca_iof_mr_hnp_component.procs);
         item != opal_list_get_end(&mca_iof_mr_hnp_component.procs);
         item = opal_list_get_next(item)) {
        ptr = (orte_iof_proc_t*)item;
        if (ptr->name.jobid == dst_name->jobid &&
            ptr->name.vpid == dst_name->vpid) {
            proct = ptr;
            break;
        }
    }
    if (NULL == proct) {
        /* we don't yet have this proc in our list */
        proct = OBJ_NEW(orte_iof_proc_t);
        proct->name.jobid = dst_name->jobid;
        proct->name.vpid = dst_name->vpid;
        opal_list_append(&mca_iof_mr_hnp_component.procs, &proct->super);
    }
    proct->sink = sink;

    return ORTE_SUCCESS;
}

/*
 * One of our local procs wants us to close the specifed
 * stream(s), thus terminating any potential io to/from it.
 */
static int mrhnp_close(const orte_process_name_t* peer,
                     orte_iof_tag_t source_tag)
{
    opal_list_item_t *item, *next_item;
    orte_iof_sink_t* sink;
    orte_ns_cmp_bitmask_t mask;

    for (item = opal_list_get_first(&mca_iof_mr_hnp_component.sinks);
        item != opal_list_get_end(&mca_iof_mr_hnp_component.sinks);
        item = next_item ) {
        sink = (orte_iof_sink_t*)item;
        next_item = opal_list_get_next(item);

        mask = ORTE_NS_CMP_ALL;
        
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &sink->name, peer) &&
           (source_tag & sink->tag)) {
            
            /* No need to delete the event or close the file
             * descriptor - the destructor will automatically
             * do it for us.
             */
            opal_list_remove_item(&mca_iof_mr_hnp_component.sinks, item);
            OBJ_RELEASE(item);
            break;
        }
    }
    return ORTE_SUCCESS;
}

static void send_data(orte_process_name_t *name, orte_iof_tag_t tag,
                      orte_jobid_t jobid,
                      unsigned char *data, int32_t nbytes)
{
    opal_buffer_t *buf;
    int rc;

    buf = OBJ_NEW(opal_buffer_t);

    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, data, nbytes, OPAL_BYTE))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    if (0 > (rc = orte_rml.send_buffer_nb(name, buf, ORTE_RML_TAG_IOF_PROXY,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
    }
}

static void mrhnp_complete(const orte_job_t *jdata)
{
    orte_job_t *jptr;
    orte_job_map_t *map;
    orte_proc_t *daemon;
    orte_iof_proc_t *proct;
    unsigned char data[1];
    opal_list_item_t *item;
    int i;
    orte_node_t *node;

    if (ORTE_JOBID_INVALID == jdata->stdout_target) {
        /* nothing to do */
        return;
    }

    /* the job is complete - close out the stdin
     * of any procs it was feeding
     */
    jptr = orte_get_job_data_object(jdata->stdout_target);
    map = jptr->map;
    /* cycle thru the map to find any node that has at least
     * one proc from this job
     */
    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        daemon = node->daemon;
        if (daemon->name.vpid == ORTE_PROC_MY_NAME->vpid) {
            for (item = opal_list_get_first(&mca_iof_mr_hnp_component.procs);
                 item != opal_list_get_end(&mca_iof_mr_hnp_component.procs);
                 item = opal_list_get_next(item)) {
                proct = (orte_iof_proc_t*)item;
                if (proct->name.jobid == jptr->jobid) {
                    if (NULL != proct->sink) {
                        /* need to write a 0-byte event to clear the stream and close it */
                        orte_iof_base_write_output(&proct->name, ORTE_IOF_STDIN, data, 0, proct->sink->wev);
                        proct->sink = NULL;
                    }
                }
            }
        } else {
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                 "%s sending close stdin to daemon %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&daemon->name)));
                
            /* need to send a 0-byte message to clear the stream and close it */
            send_data(&daemon->name, ORTE_IOF_STDIN, jptr->jobid, data, 0);
        }
    }
}

static int finalize(void)
{
    opal_list_item_t* item;
    orte_iof_write_output_t *output;
    orte_iof_write_event_t *wev;
    int num_written;
    bool dump;
    int i;
    orte_job_t *jdata;

    /* check if anything is still trying to be written out */
    wev = orte_iof_base.iof_write_stdout->wev;
    if (!opal_list_is_empty(&wev->outputs)) {
        dump = false;
        /* make one last attempt to write this out */
        while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
            output = (orte_iof_write_output_t*)item;
            if (!dump) {
                num_written = write(wev->fd, output->data, output->numbytes);
                if (num_written < output->numbytes) {
                    /* don't retry - just cleanout the list and dump it */
                    dump = true;
                }
            }
            OBJ_RELEASE(output);
        }
    }
    if (!orte_xml_output) {
        /* we only opened stderr channel if we are NOT doing xml output */
        wev = orte_iof_base.iof_write_stderr->wev;
        if (!opal_list_is_empty(&wev->outputs)) {
            dump = false;
            /* make one last attempt to write this out */
            while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
                output = (orte_iof_write_output_t*)item;
                if (!dump) {
                    num_written = write(wev->fd, output->data, output->numbytes);
                    if (num_written < output->numbytes) {
                        /* don't retry - just cleanout the list and dump it */
                        dump = true;
                    }
                }
                OBJ_RELEASE(output);
            }
        }
    }
    
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_IOF_HNP);

    /* clear our stdin job array */
    for (i=0; i < mca_iof_mr_hnp_component.stdin_jobs.size; i++) {
        if (NULL == (jdata = (orte_job_t*)opal_pointer_array_get_item(&mca_iof_mr_hnp_component.stdin_jobs, i))) {
            continue;
        }
        OBJ_RELEASE(jdata);
    }
    OBJ_DESTRUCT(&mca_iof_mr_hnp_component.stdin_jobs);

    return ORTE_SUCCESS;
}

int mrhnp_ft_event(int state) {
    /*
     * Replica doesn't need to do anything for a checkpoint
     */
    return ORTE_SUCCESS;
}


static void stdin_write_handler(int fd, short event, void *cbdata)
{
    orte_iof_sink_t *sink = (orte_iof_sink_t*)cbdata;
    orte_iof_write_event_t *wev = sink->wev;
    opal_list_item_t *item;
    orte_iof_write_output_t *output;
    int num_written;
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s mrhnp:stdin:write:handler writing data to %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         wev->fd));
    
    wev->pending = false;
    
    while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
        output = (orte_iof_write_output_t*)item;
        /* if an abnormal termination has occurred, just dump
         * this data as we are aborting
         */
        if (orte_abnormal_term_ordered) {
            OBJ_RELEASE(output);
            continue;
        }
        if (0 == output->numbytes) {
            /* this indicates we are to close the fd - there is
             * nothing to write
             */
            OPAL_OUTPUT_VERBOSE((20, orte_iof_base_framework.framework_output,
                                 "%s iof:mrhnp closing fd %d on write event due to zero bytes output",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wev->fd));
            OBJ_RELEASE(wev);
            sink->wev = NULL;
            /* just leave - we don't want to restart the
             * read event!
             */
            return;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                             "%s mrhnp:stdin:write:handler wrote %d bytes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             num_written));
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                opal_list_prepend(&wev->outputs, item);
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                wev->pending = true;
                opal_event_add(wev->ev, 0);
                goto CHECK;
            }            
            /* otherwise, something bad happened so all we can do is declare an
             * error and abort
             */
            OBJ_RELEASE(output);
            OPAL_OUTPUT_VERBOSE((20, orte_iof_base_framework.framework_output,
                                 "%s iof:mrhnp closing fd %d on write event due to negative bytes written",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wev->fd));
            OBJ_RELEASE(wev);
            sink->wev = NULL;
            return;
        } else if (num_written < output->numbytes) {
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                 "%s mrhnp:stdin:write:handler incomplete write %d - adjusting data",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_written));
            /* incomplete write - adjust data to avoid duplicate output */
            memmove(output->data, &output->data[num_written], output->numbytes - num_written);
            /* push this item back on the front of the list */
            opal_list_prepend(&wev->outputs, item);
            /* leave the write event running so it will call us again
             * when the fd is ready. 
             */
            wev->pending = true;
            opal_event_add(wev->ev, 0);
            goto CHECK;
        }
        OBJ_RELEASE(output);
    }

CHECK:
    if (NULL != mca_iof_mr_hnp_component.stdinev &&
        !orte_abnormal_term_ordered &&
        !mca_iof_mr_hnp_component.stdinev->active) {
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
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
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                 "restarting read event"));
            mca_iof_mr_hnp_component.stdinev->active = true;
            opal_event_add(mca_iof_mr_hnp_component.stdinev->ev, 0);
        }
    }
}
