/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <unistd.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <pmix_common.h>

#include "src/include/pmix_globals.h"
#include "src/util/error.h"
#include "src/util/output.h"

#include "src/mca/psec/base/base.h"
#include "psec_dummy_handshake.h"

#include "src/mca/ptl/base/base.h"

#define PMIX_PSEC_DUMMY_HNDSHK_STR "PMIX_PSEC_DUMMY_HANDSHAKE_STRING"

static pmix_status_t simple_init(void);
static void simple_finalize(void);
static pmix_status_t create_cred(struct pmix_peer_t *peer,
                                 const pmix_info_t directives[], size_t ndirs,
                                 pmix_info_t **info, size_t *ninfo,
                                 pmix_byte_object_t *cred);
static pmix_status_t client_hndshk(int sd);
static pmix_status_t server_hndshk(int sd);

pmix_psec_module_t pmix_dummy_handshake_module = {
    .name = "dummy_handshake",
    /** init/finalize */
    .init = simple_init,
    .finalize = simple_finalize,
    /** Client-side */
    .create_cred = create_cred,
    .client_handshake = client_hndshk,
    /** Server-side */
    .validate_cred = NULL,
    .server_handshake = server_hndshk
};

static pmix_status_t simple_init(void)
{
    pmix_output_verbose(2, pmix_psec_base_framework.framework_output,
                        "psec: simple init");
    return PMIX_SUCCESS;
}

static void simple_finalize(void)
{
    pmix_output_verbose(2, pmix_psec_base_framework.framework_output,
                        "psec: simple finalize");
}

static pmix_status_t create_cred(struct pmix_peer_t *peer,
                                 const pmix_info_t directives[], size_t ndirs,
                                 pmix_info_t **info, size_t *ninfo,
                                 pmix_byte_object_t *cred)
{
    char mycred[] = "dymmy_cred";

    pmix_output_verbose(2, pmix_psec_base_framework.framework_output,
                        "psec: simple create_cred");

    /* ensure initialization */
    PMIX_BYTE_OBJECT_CONSTRUCT(cred);

    cred->bytes = strdup(mycred);
    cred->size = strlen(mycred) + 1;

    return PMIX_SUCCESS;
}

static pmix_status_t server_hndshk(int sd)
{
    pmix_status_t rc, status = PMIX_SUCCESS;
    char *hndshk_msg = NULL;
    size_t size;

    pmix_output_verbose(2, pmix_psec_base_framework.framework_output,
                        "psec: simple server_hndshk");

    asprintf(&hndshk_msg, "%s", PMIX_PSEC_DUMMY_HNDSHK_STR);
    size = strlen(hndshk_msg);

    /* send size of handshake message */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(sd, (char*)&size,
                                                          sizeof(size)))) {
        goto exit;
    }
    /* send handshake message */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(sd, hndshk_msg,
                                                          size))) {
        goto exit;
    }
    /* recv hadshake status from client */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_recv_blocking(sd, (char*)&status,
                                                          sizeof(status)))) {
        goto exit;
    }
    rc = status;
    pmix_output(0, "[%s:%d] psec handshake status %d recv from client",
                __FILE__, __LINE__, status);

exit:
    if (NULL != hndshk_msg) {
        free(hndshk_msg);
    }

    return rc;
}

static pmix_status_t client_hndshk(int sd)
{
    char *hndshk_msg = NULL;
    size_t size;
    pmix_status_t rc, status = PMIX_SUCCESS;

    pmix_output_verbose(2, pmix_psec_base_framework.framework_output,
                        "psec: simple client_hndshk");

    /* recv size of handshake message */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_recv_blocking(sd, (char*)&size,
                                                          sizeof(size_t)))) {
        return rc;
    }
    hndshk_msg = (char*)malloc(size);
    /* recv handshake message */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_recv_blocking(sd, (char*)hndshk_msg,
                                                          size))) {
        free(hndshk_msg);
        return rc;
    }
    /* verifying handshake data */
    if (size != strlen(PMIX_PSEC_DUMMY_HNDSHK_STR)) {
        rc = PMIX_ERR_HANDSHAKE_FAILED;
        goto exit;
    }
    if (0 != strncmp(hndshk_msg, PMIX_PSEC_DUMMY_HNDSHK_STR, size)) {
        rc = PMIX_ERR_HANDSHAKE_FAILED;
        goto exit;
    }

    /* send hadshake status to the server */
    status = PMIX_SUCCESS;
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(sd, (char*)&status,
                                                          sizeof(status)))) {
        goto exit;
    }
    pmix_output(0, "[%s:%d] psec handshake status %d sent to server",
                __FILE__, __LINE__, status);
exit:
    if (NULL != hndshk_msg) {
        free(hndshk_msg);
    }
    return rc;
}
