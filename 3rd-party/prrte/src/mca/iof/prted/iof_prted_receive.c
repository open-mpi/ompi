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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <string.h>

#include "src/pmix/pmix-internal.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/rml/rml.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"

#include "src/mca/iof/base/base.h"
#include "src/mca/iof/iof_types.h"

#include "iof_prted.h"

void prte_iof_prted_send_xonxoff(prte_iof_tag_t tag)
{
    pmix_data_buffer_t *buf;
    int rc;

    PMIX_DATA_BUFFER_CREATE(buf);

    /* pack the tag - we do this first so that flow control messages can
     * consist solely of the tag
     */
    rc = PMIx_Data_pack(NULL, buf, &tag, 1, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        return;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output, "%s sending %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         (PRTE_IOF_XON == tag) ? "xon" : "xoff"));

    /* send the buffer to the HNP */
    PRTE_RML_SEND(rc, PRTE_PROC_MY_HNP->rank, buf, PRTE_RML_TAG_IOF_HNP);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
    }
}

/*
 * The only messages coming to an prted are either:
 *
 * (a) stdin, which is to be copied to whichever local
 *     procs "pull'd" a copy
 *
 * (b) flow control messages
 */
void prte_iof_prted_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                         prte_rml_tag_t tag, void *cbdata)
{
    unsigned char data[PRTE_IOF_BASE_MSG_MAX];
    prte_iof_tag_t stream;
    int32_t count, numbytes;
    pmix_proc_t target;
    prte_iof_proc_t *proct;
    int rc;
    PRTE_HIDE_UNUSED_PARAMS(status, sender, tag, cbdata);

    /* see what stream generated this data */
    count = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &stream, &count, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    /* if this isn't stdin, then we have an error */
    if (PRTE_IOF_STDIN != stream) {
        PRTE_ERROR_LOG(PRTE_ERR_COMM_FAILURE);
        return;
    }

    /* unpack the intended target */
    count = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &target, &count, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    /* unpack the data */
    numbytes = PRTE_IOF_BASE_MSG_MAX;
    rc = PMIx_Data_unpack(NULL, buffer, data, &numbytes, PMIX_BYTE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    /* numbytes will contain the actual #bytes that were sent */

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s unpacked %d bytes for local proc %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), numbytes, PRTE_NAME_PRINT(&target)));

    /* cycle through our list of procs */
    PMIX_LIST_FOREACH(proct, &prte_mca_iof_prted_component.procs, prte_iof_proc_t)
    {
        /* is this intended for this jobid? */
        if (PMIX_CHECK_NSPACE(target.nspace, proct->name.nspace)) {
            /* yes - is this intended for all vpids or this vpid? */
            if (PMIX_CHECK_RANK(target.rank, proct->name.rank)) {
                PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                                     "%s writing data to local proc %s",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     PRTE_NAME_PRINT(&proct->name)));
                if (NULL == proct->stdinev) {
                    continue;
                }
                /* send the bytes down the pipe - we even send 0 byte events
                 * down the pipe so it forces out any preceding data before
                 * closing the output stream
                 */
                if (PRTE_IOF_MAX_INPUT_BUFFERS < prte_iof_base_write_output(&target, stream, data,
                                                                            numbytes,
                                                                            proct->stdinev->wev)) {
                    /* getting too backed up - tell the HNP to hold off any more input if we
                     * haven't already told it
                     */
                    if (!prte_mca_iof_prted_component.xoff) {
                        prte_mca_iof_prted_component.xoff = true;
                        prte_iof_prted_send_xonxoff(PRTE_IOF_XOFF);
                    }
                }
            }
        }
    }
}
