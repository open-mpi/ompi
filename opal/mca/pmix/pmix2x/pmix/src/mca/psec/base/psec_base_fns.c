/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <pmix_common.h>
#include "src/include/pmix_globals.h"

#include "src/class/pmix_list.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/mca/ptl/base/base.h"

#include "src/mca/psec/base/base.h"


char* pmix_psec_base_get_available_modules(void)
{
    pmix_psec_base_active_module_t *active;
    char **tmp=NULL, *reply=NULL;

    if (!pmix_psec_globals.initialized) {
        return NULL;
    }

    PMIX_LIST_FOREACH(active, &pmix_psec_globals.actives, pmix_psec_base_active_module_t) {
        pmix_argv_append_nosize(&tmp, active->component->base.pmix_mca_component_name);
    }
    if (NULL != tmp) {
        reply = pmix_argv_join(tmp, ',');
        pmix_argv_free(tmp);
    }
    return reply;
}

pmix_status_t pmix_psec_base_assign_module(struct pmix_peer_t *peer,
                                           const char *options)
{
    pmix_peer_t *pr = (pmix_peer_t*)peer;
    pmix_psec_base_active_module_t *active;
    pmix_psec_module_t *mod;
    char **tmp=NULL;
    int i;

    if (!pmix_psec_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    if (NULL != options) {
        tmp = pmix_argv_split(options, ',');
    }

    PMIX_LIST_FOREACH(active, &pmix_psec_globals.actives, pmix_psec_base_active_module_t) {
        if (NULL == tmp) {
            if (NULL != (mod = active->component->assign_module())) {
                pr->compat.psec = mod;
                return PMIX_SUCCESS;
            }
        } else {
            for (i=0; NULL != tmp[i]; i++) {
                if (0 == strcmp(tmp[i], active->component->base.pmix_mca_component_name)) {
                    if (NULL != (mod = active->component->assign_module())) {
                        pmix_argv_free(tmp);
                        pr->compat.psec = mod;
                        return PMIX_SUCCESS;
                    }
                }
            }
        }
    }

    /* we only get here if nothing was found */
    if (NULL != tmp) {
        pmix_argv_free(tmp);
    }
    return PMIX_ERR_NOT_AVAILABLE;
}

pmix_status_t pmix_psec_base_create_cred(struct pmix_peer_t *peer,
                                         pmix_listener_protocol_t protocol,
                                         char **cred, size_t *len)
{
    pmix_peer_t *pr = (pmix_peer_t*)peer;

    if (NULL == pr->compat.psec->create_cred) {
        return PMIX_ERR_NOT_SUPPORTED;
    }
    return pr->compat.psec->create_cred(protocol, cred, len);
}

pmix_status_t pmix_psec_base_client_handshake(struct pmix_peer_t *peer, int sd)
{
    pmix_peer_t *pr = (pmix_peer_t*)peer;

    if (NULL == pr->compat.psec->client_handshake) {
        return PMIX_ERR_NOT_SUPPORTED;
    }
    return pr->compat.psec->client_handshake(sd);
}

pmix_status_t pmix_psec_base_validate_connection(struct pmix_peer_t *peer,
                                                 pmix_listener_protocol_t protocol,
                                                 char *cred, size_t len)
{
    pmix_peer_t *pr = (pmix_peer_t*)peer;
    pmix_status_t rc;

    /* if a credential is available, then check it */
    if (NULL != pr->compat.psec->validate_cred) {
        if (PMIX_SUCCESS != (rc = pr->compat.psec->validate_cred(peer, protocol, cred, len))) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "validation of credential failed: %s",
                                PMIx_Error_string(rc));
            return rc;
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "credential validated");
        /* send them success */
        rc = PMIX_SUCCESS;
        if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(pr->sd, (char*)&rc, sizeof(int)))) {
            PMIX_ERROR_LOG(rc);
        }
        return rc;
    } else if (NULL != pr->compat.psec->server_handshake) {
        /* execute the handshake if the security mode calls for it */
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "executing handshake");
        rc = PMIX_ERR_READY_FOR_HANDSHAKE;
        if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(pr->sd, (char*)&rc, sizeof(int)))) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pr->compat.psec->server_handshake(peer))) {
            PMIX_ERROR_LOG(rc);
        }
        return rc;
    } else {
        /* this is not allowed */
        return PMIX_ERR_NOT_SUPPORTED;
    }
}
