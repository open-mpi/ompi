/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies. All rights reserved.
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
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#else
#    ifdef HAVE_SYS_FCNTL_H
#        include <sys/fcntl.h>
#    endif
#endif

#include "src/pmix/pmix-internal.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/rml/rml.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"

#include "src/mca/iof/base/base.h"
#include "src/mca/iof/iof.h"

#include "iof_hnp.h"

static void lkcbfunc(pmix_status_t status, void *cbdata)
{
    prte_iof_deliver_t *p = (prte_iof_deliver_t*)cbdata;

    /* nothing to do here - we use this solely to
     * ensure that IOF_deliver doesn't block */
    if (PMIX_SUCCESS != status) {
        PMIX_ERROR_LOG(status);
    }
    PMIX_RELEASE(p);
}

void prte_iof_hnp_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                       prte_rml_tag_t tag, void *cbdata)
{
    pmix_proc_t origin;
    prte_iof_tag_t stream;
    int32_t count, numbytes;
    int rc;
    prte_iof_proc_t *proct;
    pmix_iof_channel_t pchan;
    prte_iof_deliver_t *p;
    pmix_status_t prc;
    PRTE_HIDE_UNUSED_PARAMS(status, tag, cbdata);

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s received IOF msg from proc %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         PRTE_NAME_PRINT(sender)));

    /* unpack the stream first as this may be flow control info */
    count = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &stream, &count, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }

    /* get name of the process whose io we are discussing */
    count = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &origin, &count, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s received IOF cmd for source %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         PRTE_NAME_PRINT(&origin)));

    /* this must have come from a daemon forwarding output - unpack the data */
    count = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &numbytes, &count, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    if (0 == numbytes) {
        /* nothing to do - shouldn't have been sent */
        goto CLEAN_RETURN;
    }
    p = PMIX_NEW(prte_iof_deliver_t);
    PMIX_XFER_PROCID(&p->source, &origin);
    p->bo.bytes = (char*)malloc(numbytes);
    rc = PMIx_Data_unpack(NULL, buffer, p->bo.bytes, &numbytes, PMIX_BYTE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(p);
        goto CLEAN_RETURN;
    }
    p->bo.size = numbytes;

    PMIX_OUTPUT_VERBOSE((1, prte_iof_base_framework.framework_output,
                         "%s unpacked %d bytes from remote proc %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), numbytes, PRTE_NAME_PRINT(&origin)));

    /* do we already have this process in our list? */
    PMIX_LIST_FOREACH(proct, &prte_mca_iof_hnp_component.procs, prte_iof_proc_t)
    {
        if (PMIX_CHECK_PROCID(&proct->name, &origin)) {
            /* found it */
            goto NSTEP;
        }
    }

    /* if we get here, then we don't yet have this proc in our list */
    proct = PMIX_NEW(prte_iof_proc_t);
    PMIX_XFER_PROCID(&proct->name, &origin);
    pmix_list_append(&prte_mca_iof_hnp_component.procs, &proct->super);

NSTEP:
    pchan = 0;
    if (PRTE_IOF_STDOUT & stream) {
        pchan |= PMIX_FWD_STDOUT_CHANNEL;
    }
    if (PRTE_IOF_STDERR & stream) {
        pchan |= PMIX_FWD_STDERR_CHANNEL;
    }
    if (PRTE_IOF_STDDIAG & stream) {
        pchan |= PMIX_FWD_STDDIAG_CHANNEL;
    }
    /* output this thru our PMIx server */
    prc = PMIx_server_IOF_deliver(&p->source, pchan, &p->bo, NULL, 0, lkcbfunc, (void*)p);
    if (PMIX_SUCCESS != prc) {
        PMIX_ERROR_LOG(prc);
        PMIX_RELEASE(p);
    }

CLEAN_RETURN:
    return;
}
