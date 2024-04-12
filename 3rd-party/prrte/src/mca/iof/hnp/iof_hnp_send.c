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
 * Copyright (c) 2012      Los Alamos National Security, LLC
 *                         All rights reserved
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
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

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/grpcomm/grpcomm.h"
#include "src/rml/rml.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"

#include "src/mca/iof/base/base.h"
#include "src/mca/iof/iof.h"

#include "iof_hnp.h"

int prte_iof_hnp_send_data_to_endpoint(const pmix_proc_t *host,
                                       const pmix_proc_t *target,
                                       prte_iof_tag_t tag,
                                       unsigned char *data, int numbytes)
{
    pmix_data_buffer_t *buf;
    int rc;
    prte_grpcomm_signature_t sig;

    /* if the host is a daemon and we are in the process of aborting,
     * then ignore this request. We leave it alone if the host is not
     * a daemon because it might be a tool that wants to watch the
     * output from an abort procedure
     */
    if (PMIX_CHECK_NSPACE(PRTE_JOB_FAMILY_PRINT(host->nspace),
                          PRTE_JOB_FAMILY_PRINT(PRTE_PROC_MY_NAME->nspace))
        && prte_dvm_abort_ordered) {
        return PRTE_SUCCESS;
    }

    PMIX_DATA_BUFFER_CREATE(buf);

    /* pack the tag - we do this first so that flow control messages can
     * consist solely of the tag
     */
    rc = PMIx_Data_pack(NULL, buf, &tag, 1, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        return rc;
    }
    /* pack the name of the target - this is either the intended
     * recipient (if the tag is stdin and we are sending to a daemon),
     * or the source (if we are sending to anyone else)
     */
    rc = PMIx_Data_pack(NULL, buf, (void*)target, 1, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        return rc;
    }

    /* pack the data - if numbytes is zero, we will pack zero bytes */
    rc = PMIx_Data_pack(NULL, buf, data, numbytes, PMIX_BYTE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        return rc;
    }

    /* if the target is wildcard, then this needs to go to everyone - xcast it */
    if (PMIX_CHECK_NSPACE(PRTE_PROC_MY_NAME->nspace, host->nspace)
        && PMIX_RANK_WILDCARD == host->rank) {
        /* xcast this to everyone - the local daemons will know how to handle it */
        PMIX_PROC_CREATE(sig.signature, 1);
        sig.sz = 1;
        PMIX_LOAD_PROCID(&sig.signature[0], PRTE_PROC_MY_NAME->nspace, PMIX_RANK_WILDCARD);
        (void) prte_grpcomm.xcast(&sig, PRTE_RML_TAG_IOF_PROXY, buf);
        PMIX_DATA_BUFFER_RELEASE(buf);
        PMIX_PROC_FREE(sig.signature, 1);
        return PRTE_SUCCESS;
    }

    /* send the buffer to the host - this is either a daemon or
     * a tool that requested IOF
     */
    PRTE_RML_SEND(rc, host->rank, buf, PRTE_RML_TAG_IOF_PROXY);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        return rc;
    }

    return PRTE_SUCCESS;
}
