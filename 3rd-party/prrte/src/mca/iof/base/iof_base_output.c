/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "prte_config.h"
#include "constants.h"

#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <errno.h>
#include <time.h>

#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/state/state.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"

#include "src/mca/iof/base/base.h"

int prte_iof_base_write_output(const pmix_proc_t *name, prte_iof_tag_t stream,
                               const unsigned char *data, int numbytes,
                               prte_iof_write_event_t *channel)
{
    prte_iof_write_output_t *output;
    int num_buffered;
    PRTE_HIDE_UNUSED_PARAMS(stream);

    PMIX_OUTPUT_VERBOSE(
        (1, prte_iof_base_framework.framework_output,
         "%s write:output setting up to write %d bytes to stdin for %s on fd %d",
         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), numbytes,
         PRTE_NAME_PRINT(name), (NULL == channel) ? -1 : channel->fd));

    if (NULL == channel) {
        return 0;
    }

    /* setup output object */
    output = PMIX_NEW(prte_iof_write_output_t);

    /* copy over the data to be written */
    if (0 < numbytes) {
        /* don't copy 0 bytes - we just need to pass
         * the zero bytes so the fd can be closed
         * after it writes everything out
         */
        memcpy(output->data, data, numbytes);
    }
    output->numbytes = numbytes;
    /* add this data to the write list for this fd */
    pmix_list_append(&channel->outputs, &output->super);

    /* record how big the buffer is */
    num_buffered = pmix_list_get_size(&channel->outputs);

    /* is the write event issued? */
    if (!channel->pending) {
        /* issue it */
        PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                             "%s write:output adding write event",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        PRTE_IOF_SINK_ACTIVATE(channel);
    }

    return num_buffered;
}

void prte_iof_base_write_handler(int _fd, short event, void *cbdata)
{
    prte_iof_sink_t *sink = (prte_iof_sink_t *) cbdata;
    prte_iof_write_event_t *wev = sink->wev;
    pmix_list_item_t *item;
    prte_iof_write_output_t *output;
    int num_written, total_written = 0;
    PRTE_HIDE_UNUSED_PARAMS(_fd, event);

    PMIX_ACQUIRE_OBJECT(sink);

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s write:handler writing data to %d", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         wev->fd));

    while (NULL != (item = pmix_list_remove_first(&wev->outputs))) {
        output = (prte_iof_write_output_t *) item;
        if (0 == output->numbytes) {
            /* indicates we are to close this stream */
            PMIX_RELEASE(sink);
            return;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                pmix_list_prepend(&wev->outputs, item);
                /* if the list is getting too large, abort */
                if (prte_iof_base_output_limit < (int)pmix_list_get_size(&wev->outputs)) {
                    pmix_output(0, "IO Forwarding is running too far behind - something is "
                                   "blocking us from writing");
                    PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                    goto ABORT;
                }
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                goto NEXT_CALL;
            }
            /* otherwise, something bad happened so all we can do is abort
             * this attempt
             */
            PMIX_RELEASE(output);
            goto ABORT;
        } else if (num_written < output->numbytes) {
            /* incomplete write - adjust data to avoid duplicate output */
            memmove(output->data, &output->data[num_written], output->numbytes - num_written);
            /* adjust the number of bytes remaining to be written */
            output->numbytes -= num_written;
            /* push this item back on the front of the list */
            pmix_list_prepend(&wev->outputs, item);
            /* if the list is getting too large, abort */
            if (prte_iof_base_output_limit < (int)pmix_list_get_size(&wev->outputs)) {
                pmix_output(0, "IO Forwarding is running too far behind - something is blocking us "
                               "from writing");
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                goto ABORT;
            }
            /* leave the write event running so it will call us again
             * when the fd is ready
             */
            goto NEXT_CALL;
        }
        PMIX_RELEASE(output);

        total_written += num_written;
        if (wev->always_writable && (PRTE_IOF_SINK_BLOCKSIZE <= total_written)) {
            /* If this is a regular file it will never tell us it will block
             * Write no more than PRTE_IOF_REGULARF_BLOCK at a time allowing
             * other fds to progress
             */
            goto NEXT_CALL;
        }
    }
ABORT:
    wev->pending = false;
    PMIX_POST_OBJECT(wev);
    return;
NEXT_CALL:
    PRTE_IOF_SINK_ACTIVATE(wev);
}
