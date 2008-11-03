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

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#else
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#endif

#include "orte/util/show_help.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"

#include "iof_orted.h"


/* LOCAL FUNCTIONS */
static void stdin_write_handler(int fd, short event, void *cbdata);


/* API FUNCTIONS */
static int orted_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd);

static int orted_pull(const orte_process_name_t* src_name,
                      orte_iof_tag_t src_tag,
                      int fd);

static int orted_close(const orte_process_name_t* peer,
                       orte_iof_tag_t source_tag);

static int orted_ft_event(int state);

/* The API's in this module are solely used to support LOCAL
 * procs - i.e., procs that are co-located to the daemon. Output
 * from local procs is automatically sent to the HNP for output
 * and possible forwarding to other requestors. The HNP automatically
 * determines and wires up the stdin configuration, so we don't
 * have to do anything here.
 */

orte_iof_base_module_t orte_iof_orted_module = {
    orted_push,
    orted_pull,
    orted_close,
    orted_ft_event
};


/**
 * Push data from the specified file descriptor
 * to the HNP
 */

static int orted_push(const orte_process_name_t* dst_name, orte_iof_tag_t src_tag, int fd)
{
    int flags;

    /* set the file descriptor to non-blocking - do this before we setup
     * and activate the read event in case it fires right away
     */
    if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        opal_output(orte_iof_base.iof_output, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", 
                    __FILE__, __LINE__, errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }

    /* setup to read from the specified file descriptor and
     * forward anything we get to the HNP
     */
    ORTE_IOF_READ_EVENT(dst_name, fd, src_tag,
                        orte_iof_orted_read_handler,
                        &mca_iof_orted_component.read_events, true);
    
    return ORTE_SUCCESS;
}


/**
 * Pull for a daemon tells
 * us that any info we receive from the HNP that is targeted
 * for stdin of the specified process should be fed down the
 * indicated file descriptor. Thus, all we need to do here
 * is define a local endpoint so we know where to feed anything
 * that comes to us
 */

static int orted_pull(const orte_process_name_t* dst_name,
                      orte_iof_tag_t src_tag,
                      int fd)
{
    orte_iof_sink_t *sink;
    int flags;
    
    /* this is a local call - only stdin is supported */
    if (ORTE_IOF_STDIN != src_tag) {
        return ORTE_ERR_NOT_SUPPORTED;
    }
    
    /* set the file descriptor to non-blocking - do this before we setup
     * the sink in case it fires right away
     */
    if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
        opal_output(orte_iof_base.iof_output, "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n", 
                    __FILE__, __LINE__, errno);
    } else {
        flags |= O_NONBLOCK;
        fcntl(fd, F_SETFL, flags);
    }

    ORTE_IOF_SINK_DEFINE(&sink, dst_name, fd, src_tag,
                         stdin_write_handler,
                         &mca_iof_orted_component.sinks);
    
    return ORTE_SUCCESS;
}


/*
 * One of our local procs wants us to close the specifed
 * stream(s), thus terminating any potential io to/from it.
 * For the orted, this just means closing the local fd
 */
static int orted_close(const orte_process_name_t* peer,
                 orte_iof_tag_t source_tag)
{
    return ORTE_SUCCESS;
}


/*
 * FT event
 */

static int orted_ft_event(int state)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
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
    OPAL_THREAD_LOCK(&mca_iof_orted_component.lock);
    
    while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
        output = (orte_iof_write_output_t*)item;
        if (0 == output->numbytes) {
            /* this indicates we are to close the fd - there is
             * nothing to write
             */
            close(wev->fd);
            /* be sure to delete the write event */
            opal_event_del(&wev->ev);
            /* set the fd to -1 to indicate that this channel is closed */
            wev->fd = -1;
            goto DEPART;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                opal_list_prepend(&wev->outputs, item);
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                goto CHECK;
            }            
            /* otherwise, something bad happened so all we can do is abort
             * this attempt
             */
            OBJ_RELEASE(output);
            goto ABORT;
        } else if (num_written < output->numbytes) {
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
ABORT:
    opal_event_del(&wev->ev);
    wev->pending = false;
    
CHECK:
    if (mca_iof_orted_component.xoff) {
        /* if we have told the HNP to stop reading stdin, see if
         * the proc has absorbed enough to justify restart
         *
         * RHC: Note that when multiple procs want stdin, we
         * can get into a fight between a proc turnin stdin
         * back "on" and other procs turning it "off". There
         * is no clear way to resolve this as different procs
         * may take input at different rates.
         */
        if (opal_list_get_size(&wev->outputs) < ORTE_IOF_MAX_INPUT_BUFFERS) {
            /* restart the read */
            mca_iof_orted_component.xoff = false;
            orte_iof_orted_send_xonxoff(ORTE_IOF_XON);
        }
    }
    
DEPART:
    /* unlock and go */
    OPAL_THREAD_UNLOCK(&mca_iof_orted_component.lock);
}
