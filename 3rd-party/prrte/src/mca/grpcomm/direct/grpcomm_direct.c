/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include <string.h>

#include "src/class/pmix_list.h"
#include "src/pmix/pmix-internal.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/rml/rml.h"
#include "src/mca/state/state.h"
#include "src/util/name_fns.h"
#include "src/util/nidmap.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_show_help.h"

#include "grpcomm_direct.h"
#include "src/mca/grpcomm/base/base.h"

/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(pmix_rank_t *vpids, size_t nprocs, pmix_data_buffer_t *buf);
static int allgather(prte_grpcomm_coll_t *coll,
                     prte_pmix_mdx_caddy_t *cd);

/* Module def */
prte_grpcomm_base_module_t prte_grpcomm_direct_module = {
    .init = init,
    .finalize = finalize,
    .xcast = xcast,
    .allgather = allgather
};

/* internal functions */
static void xcast_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                       prte_rml_tag_t tag, void *cbdata);
static void allgather_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                           prte_rml_tag_t tag, void *cbdata);
static void barrier_release(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                            prte_rml_tag_t tag, void *cbdata);

/* internal variables */
static pmix_list_t tracker;

/**
 * Initialize the module
 */
static int init(void)
{
    PMIX_CONSTRUCT(&tracker, pmix_list_t);

    /* post the receives */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_XCAST,
                  PRTE_RML_PERSISTENT, xcast_recv, NULL);
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_ALLGATHER_DIRECT,
                  PRTE_RML_PERSISTENT, allgather_recv, NULL);
    /* setup recv for barrier release */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_COLL_RELEASE,
                  PRTE_RML_PERSISTENT, barrier_release, NULL);

    return PRTE_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    PMIX_LIST_DESTRUCT(&tracker);
    return;
}

static int xcast(pmix_rank_t *vpids, size_t nprocs, pmix_data_buffer_t *buf)
{
    int rc;
    PRTE_HIDE_UNUSED_PARAMS(vpids, nprocs);

    /* send it to the HNP (could be myself) for relay */
    PRTE_RML_SEND(rc, PRTE_PROC_MY_HNP->rank, buf, PRTE_RML_TAG_XCAST);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        return rc;
    }
    return PRTE_SUCCESS;
}

static int allgather(prte_grpcomm_coll_t *coll,
                     prte_pmix_mdx_caddy_t *cd)
{
    int rc;
    pmix_data_buffer_t *relay;

    PMIX_OUTPUT_VERBOSE((1, prte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct: allgather",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    /* the base functions pushed us into the event library
     * before calling us, so we can safely access global data
     * at this point */

    PMIX_DATA_BUFFER_CREATE(relay);
    /* pack the signature */
    rc = PMIx_Data_pack(NULL, relay, &coll->sig->sz, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(relay);
        return rc;
    }
    rc = PMIx_Data_pack(NULL, relay, coll->sig->signature, coll->sig->sz, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(relay);
        return rc;
    }

    /* pack the ctrls */
    rc = PMIx_Data_pack(NULL, relay, &cd->ctrls, 1, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(relay);
        return rc;
    }

    /* pass along the payload */
    rc = PMIx_Data_copy_payload(relay, cd->buf);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(relay);
        return rc;
    }

    /* send this to ourselves for processing */
    PMIX_OUTPUT_VERBOSE((1, prte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct:allgather sending to ourself",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    /* send the info to ourselves for tracking */
    PRTE_RML_SEND(rc, PRTE_PROC_MY_NAME->rank, relay,
                  PRTE_RML_TAG_ALLGATHER_DIRECT);
    return rc;
}

static void allgather_recv(int status, pmix_proc_t *sender,
                           pmix_data_buffer_t *buffer,
                           prte_rml_tag_t tag, void *cbdata)
{
    int32_t cnt;
    int rc, timeout;
    size_t n, ninfo, memsize, m;
    bool assignID = false;
    pmix_proc_t *addmembers = NULL;
    size_t num_members = 0;
    prte_namelist_t *nm;
    pmix_data_array_t darray;
    pmix_status_t st;
    pmix_info_t *info, infostat;
    prte_grpcomm_signature_t sig;
    pmix_byte_object_t ctrlsbo;
    pmix_data_buffer_t ctrlbuf;
    pmix_data_buffer_t *reply;
    prte_grpcomm_coll_t *coll;
    pmix_status_t local_status;
    PRTE_HIDE_UNUSED_PARAMS(status, tag, cbdata);

    PMIX_OUTPUT_VERBOSE((1, prte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct allgather recvd from %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender)));

    /* unpack the signature */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &sig.sz, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    PMIX_PROC_CREATE(sig.signature, sig.sz);
    cnt = sig.sz;
    rc = PMIx_Data_unpack(NULL, buffer, sig.signature, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    /* check for the tracker and create it if not found */
    if (NULL == (coll = prte_grpcomm_base_get_tracker(&sig, true))) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        PMIX_PROC_FREE(sig.signature, sig.sz);
        return;
    }

    /* unpack the ctrls from this contributor */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &ctrlsbo, &cnt, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_PROC_FREE(sig.signature, sig.sz);
        return;
    }
    PMIX_DATA_BUFFER_CONSTRUCT(&ctrlbuf);
    rc = PMIx_Data_load(&ctrlbuf, &ctrlsbo);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_PROC_FREE(sig.signature, sig.sz);
        PMIX_BYTE_OBJECT_DESTRUCT(&ctrlsbo);
        return;
    }
    PMIX_BYTE_OBJECT_DESTRUCT(&ctrlsbo);

    /* unpack the number of info's in the ctrls */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, &ctrlbuf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_PROC_FREE(sig.signature, sig.sz);
        PMIX_DATA_BUFFER_DESTRUCT(&ctrlbuf);
        return;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        cnt = ninfo;
        rc = PMIx_Data_unpack(NULL, &ctrlbuf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_PROC_FREE(sig.signature, sig.sz);
            PMIX_DATA_BUFFER_DESTRUCT(&ctrlbuf);
            return;
        }
    }
    PMIX_DATA_BUFFER_DESTRUCT(&ctrlbuf);

    /* cycle thru the ctrls to look for keys we support */
    for (n=0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_TIMEOUT)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, timeout, int);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
            if (coll->timeout < timeout) {
                coll->timeout = timeout;
            }
            /* update the info with the collected value */
            info[n].value.type = PMIX_INT;
            info[n].value.data.integer = coll->timeout;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_LOCAL_COLLECTIVE_STATUS)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, st, pmix_status_t);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
            if (PMIX_SUCCESS != st &&
                PMIX_SUCCESS == coll->status) {
                coll->status = st;
            }
            /* update the info with the collected value */
            info[n].value.type = PMIX_STATUS;
            info[n].value.data.status = coll->status;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_GROUP_ASSIGN_CONTEXT_ID)) {
            assignID = PMIX_INFO_TRUE(&info[n]);
            if (assignID) {
                coll->assignID = true;
            }
            /* update the info with the collected value */
            info[n].value.type = PMIX_BOOL;
            info[n].value.data.flag = coll->assignID;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_GROUP_ADD_MEMBERS)) {
            addmembers = (pmix_proc_t*)info[n].value.data.darray->array;
            num_members = info[n].value.data.darray->size;
            for (m=0; m < num_members; m++) {
                nm = PMIX_NEW(prte_namelist_t);
                PMIX_XFER_PROCID(&nm->name, &addmembers[m]);
                pmix_list_append(&coll->addmembers, &nm->super);
            }
        }
    }

    /* increment nprocs reported for collective */
    coll->nreported++;
    /* capture any provided content */
    rc = PMIx_Data_copy_payload(&coll->bucket, buffer);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct allgather recv nexpected %d nrep %d",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (int) coll->nexpected,
                         (int) coll->nreported));

    /* see if everyone has reported */
    if (coll->nreported == coll->nexpected) {
        if (PRTE_PROC_IS_MASTER) {
            PMIX_OUTPUT_VERBOSE((1, prte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:direct allgather HNP reports complete",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            /* the allgather is complete - send the xcast */
            PMIX_DATA_BUFFER_CREATE(reply);
            /* pack the signature */
            rc = PMIx_Data_pack(NULL, reply, &sig.sz, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
            rc = PMIx_Data_pack(NULL, reply, sig.signature, sig.sz, PMIX_PROC);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
            /* pack the status */
            rc = PMIx_Data_pack(NULL, reply, &coll->status, 1, PMIX_INT32);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
            /* add some values to the payload in the bucket */
            PMIX_DATA_BUFFER_CONSTRUCT(&ctrlbuf);

            /* if we were asked to provide a context id, do so */
            if (assignID) {
                size_t sz;
                sz = prte_grpcomm_base.context_id;
                --prte_grpcomm_base.context_id;
                PMIX_INFO_LOAD(&infostat, PMIX_GROUP_CONTEXT_ID, &sz, PMIX_SIZE);
                rc = PMIx_Data_pack(NULL, &ctrlbuf, &infostat, 1, PMIX_INFO);
                PMIX_INFO_DESTRUCT(&infostat);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DATA_BUFFER_RELEASE(reply);
                    PMIX_DATA_BUFFER_DESTRUCT(&ctrlbuf);
                    PMIX_PROC_FREE(sig.signature, sig.sz);
                    return;
                }
            }
            /* if we added members, add them here */
            if (0 < pmix_list_get_size(&coll->addmembers)) {
                num_members = pmix_list_get_size(&coll->addmembers);
                PMIX_PROC_CREATE(addmembers, num_members);
                n=0;
                PMIX_LIST_FOREACH(nm, &coll->addmembers, prte_namelist_t) {
                    memcpy(&addmembers[n], &nm->name, sizeof(pmix_proc_t));
                    ++n;
                }
                darray.type = PMIX_PROC;
                darray.array = addmembers;
                darray.size = num_members;
                PMIX_INFO_LOAD(&infostat, PMIX_GROUP_ADD_MEMBERS, &darray, PMIX_DATA_ARRAY);
                PMIX_PROC_FREE(addmembers, num_members);
                rc = PMIx_Data_pack(NULL, &ctrlbuf, &infostat, 1, PMIX_INFO);
                PMIX_INFO_DESTRUCT(&infostat);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DATA_BUFFER_RELEASE(reply);
                    PMIX_DATA_BUFFER_DESTRUCT(&ctrlbuf);
                    PMIX_PROC_FREE(sig.signature, sig.sz);
                    return;
                }
            }
            PMIX_DATA_BUFFER_UNLOAD(&ctrlbuf, ctrlsbo.bytes, ctrlsbo.size);
            PMIX_DATA_BUFFER_DESTRUCT(&ctrlbuf);
            rc = PMIx_Data_pack(NULL, reply, &ctrlsbo, 1, PMIX_BYTE_OBJECT);
            PMIX_BYTE_OBJECT_DESTRUCT(&ctrlsbo);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }

            /* transfer the collected bucket */
            rc = PMIx_Data_copy_payload(reply, &coll->bucket);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
            /* send the release via xcast */
            (void) prte_grpcomm.xcast(&sig, PRTE_RML_TAG_COLL_RELEASE, reply);
        } else {
            PMIX_OUTPUT_VERBOSE((1, prte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:direct allgather rollup complete - sending to %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_PARENT)));
            PMIX_DATA_BUFFER_CREATE(reply);
            /* pack the signature */
            rc = PMIx_Data_pack(NULL, reply, &sig.sz, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
            rc = PMIx_Data_pack(NULL, reply, sig.signature, sig.sz, PMIX_PROC);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
            /* pass along the ctrls - we have updated the values
             * we collected along the way */
            rc = prte_pack_ctrl_options(&ctrlsbo, info, ninfo);
            if (PRTE_SUCCESS != rc) {
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
            rc = PMIx_Data_pack(NULL, reply, &ctrlsbo, 1, PMIX_BYTE_OBJECT);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                PMIx_Byte_object_destruct(&ctrlsbo);
                return;
            }
            PMIx_Byte_object_destruct(&ctrlsbo);

            /* transfer the collected bucket */
            rc = PMIx_Data_copy_payload(reply, &coll->bucket);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
            /* send the info to our parent */
            PRTE_RML_SEND(rc, PRTE_PROC_MY_PARENT->rank, reply,
                          PRTE_RML_TAG_ALLGATHER_DIRECT);
            if (PRTE_SUCCESS != rc) {
                PRTE_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                PMIX_PROC_FREE(sig.signature, sig.sz);
                return;
            }
        }
    }
    PMIX_PROC_FREE(sig.signature, sig.sz);
}

static void xcast_recv(int status, pmix_proc_t *sender,
                       pmix_data_buffer_t *buffer,
                       prte_rml_tag_t tg, void *cbdata)
{
    prte_routed_tree_t *nm;
    int ret, cnt;
    pmix_data_buffer_t *relay = NULL, *rly, *rlycopy;
    pmix_data_buffer_t datbuf, *data;
    bool compressed;
    prte_job_t *daemons;
    pmix_list_t coll;
    prte_grpcomm_signature_t sig;
    prte_rml_tag_t tag;
    pmix_byte_object_t bo, pbo;
    pmix_value_t val;
    pmix_proc_t dmn;
    PRTE_HIDE_UNUSED_PARAMS(status, sender, tg, cbdata);

    PMIX_OUTPUT_VERBOSE((1, prte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct:xcast:recv: with %d bytes",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (int) buffer->bytes_used));

    /* we need a passthru buffer to send to our children - we leave it
     * as compressed data */
    PMIX_DATA_BUFFER_CREATE(rly);
    ret = PMIx_Data_copy_payload(rly, buffer);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PMIX_DATA_BUFFER_RELEASE(rly);
        return;
    }
    PMIX_DATA_BUFFER_CONSTRUCT(&datbuf);
    /* setup the relay list */
    PMIX_CONSTRUCT(&coll, pmix_list_t);

    /* unpack the flag to see if this payload is compressed */
    cnt = 1;
    ret = PMIx_Data_unpack(NULL, buffer, &compressed, &cnt, PMIX_BOOL);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
        PMIX_DESTRUCT(&coll);
        PMIX_DATA_BUFFER_RELEASE(rly);
        return;
    }
    /* unpack the data blob */
    cnt = 1;
    ret = PMIx_Data_unpack(NULL, buffer, &pbo, &cnt, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DESTRUCT(&coll);
        PMIX_DATA_BUFFER_RELEASE(rly);
        return;
    }
    if (compressed) {
        /* decompress the data */
        if (PMIx_Data_decompress((uint8_t *) pbo.bytes, pbo.size,
                                 (uint8_t **) &bo.bytes, &bo.size)) {
            /* the data has been uncompressed */
            ret = PMIx_Data_load(&datbuf, &bo);
            if (PMIX_SUCCESS != ret) {
                PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
                PMIX_DESTRUCT(&coll);
                PMIX_DATA_BUFFER_RELEASE(rly);
                return;
            }
        } else {
            pmix_show_help("help-prte-runtime.txt", "failed-to-uncompress",
                           true, prte_process_info.nodename);
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
            PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
            PMIX_DESTRUCT(&coll);
            PMIX_DATA_BUFFER_RELEASE(rly);
            return;
        }
    } else {
        ret = PMIx_Data_load(&datbuf, &pbo);
        if (PMIX_SUCCESS != ret) {
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
            PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
            PMIX_DESTRUCT(&coll);
            PMIX_DATA_BUFFER_RELEASE(rly);
            return;
        }
    }
    PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
    data = &datbuf;

    /* get the signature that we do not need */
    cnt = 1;
    ret = PMIx_Data_unpack(NULL, data, &sig.sz, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
        PMIX_DESTRUCT(&coll);
        PMIX_DATA_BUFFER_RELEASE(rly);
        return;
    }
    PMIX_PROC_CREATE(sig.signature, sig.sz);
    cnt = sig.sz;
    ret = PMIx_Data_unpack(NULL, data, sig.signature, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
        PMIX_DESTRUCT(&coll);
        PMIX_DATA_BUFFER_RELEASE(rly);
        PMIX_PROC_FREE(sig.signature, sig.sz);
        return;
    }
    PMIX_PROC_FREE(sig.signature, sig.sz);

    /* get the target tag */
    cnt = 1;
    ret = PMIx_Data_unpack(NULL, data, &tag, &cnt, PMIX_UINT32);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
        PMIX_DESTRUCT(&coll);
        PMIX_DATA_BUFFER_RELEASE(rly);
        return;
    }

    /* copy the msg for relay to ourselves */
    PMIX_DATA_BUFFER_CREATE(relay);
    ret = PMIx_Data_copy_payload(relay, data);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
        PMIX_DESTRUCT(&coll);
        PMIX_DATA_BUFFER_RELEASE(rly);
        PMIX_DATA_BUFFER_RELEASE(relay);
        return;
    }

    if (PRTE_RML_TAG_WIREUP == tag && !PRTE_PROC_IS_MASTER) {
        if (PRTE_SUCCESS != (ret = prte_util_decode_nidmap(data))) {
            PRTE_ERROR_LOG(ret);
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
            PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
            PMIX_DESTRUCT(&coll);
            PMIX_DATA_BUFFER_RELEASE(rly);
            PMIX_DATA_BUFFER_RELEASE(relay);
            return;
        }
        /* unpack the wireup info */
        cnt = 1;
        while (PMIX_SUCCESS == (ret = PMIx_Data_unpack(NULL, data, &dmn, &cnt, PMIX_PROC))) {
            PMIX_VALUE_CONSTRUCT(&val);
            val.type = PMIX_STRING;
            cnt = 1;
            ret = PMIx_Data_unpack(NULL, data, &val.data.string, &cnt, PMIX_STRING);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
                PMIX_DESTRUCT(&coll);
                PMIX_DATA_BUFFER_RELEASE(rly);
                PMIX_DATA_BUFFER_RELEASE(relay);
                return;
            }

            if (!PMIX_CHECK_PROCID(&dmn, PRTE_PROC_MY_HNP) &&
                !PMIX_CHECK_PROCID(&dmn, PRTE_PROC_MY_NAME) &&
                !PMIX_CHECK_PROCID(&dmn, PRTE_PROC_MY_PARENT)) {
                /* store it locally */
                ret = PMIx_Store_internal(&dmn, PMIX_PROC_URI, &val);
                PMIX_VALUE_DESTRUCT(&val);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                    PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
                    PMIX_DESTRUCT(&coll);
                    PMIX_DATA_BUFFER_RELEASE(rly);
                    PMIX_DATA_BUFFER_RELEASE(relay);
                    return;
                }
            }
        }
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != ret) {
            PMIX_ERROR_LOG(ret);
        }
    }

    daemons = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    if (!prte_get_attribute(&daemons->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
        /* send the message to each of our children */
        PMIX_LIST_FOREACH(nm, &prte_rml_base.children, prte_routed_tree_t)
        {
            PMIX_OUTPUT_VERBOSE((5, prte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:direct:send_relay sending relay msg of %d bytes to %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (int) rly->bytes_used,
                                 PRTE_VPID_PRINT(nm->rank)));
            /* copy the buffer for send */
            PMIX_DATA_BUFFER_CREATE(rlycopy);
            ret = PMIx_Data_copy_payload(rlycopy, rly);
            if (PMIX_SUCCESS != ret) {
                PRTE_ERROR_LOG(ret);
                PMIX_DATA_BUFFER_RELEASE(rlycopy);
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                continue;
            }
            PRTE_RML_SEND(ret, nm->rank, rlycopy, PRTE_RML_TAG_XCAST);
            if (PRTE_SUCCESS != ret) {
                PRTE_ERROR_LOG(ret);
                PMIX_DATA_BUFFER_RELEASE(rlycopy);
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                continue;
            }
        }
    }

CLEANUP:
    /* cleanup */
    PMIX_LIST_DESTRUCT(&coll);
    PMIX_DATA_BUFFER_RELEASE(rly); // retain accounting

    /* now pass the relay buffer to myself for processing IFF it
     * wasn't just a wireup message - don't
     * inject it into the RML system via send as that will compete
     * with the relay messages down in the OOB. Instead, pass it
     * directly to the RML message processor */
    if (PRTE_RML_TAG_WIREUP != tag) {
        PRTE_RML_POST_MESSAGE(PRTE_PROC_MY_NAME, tag, 1, relay->base_ptr, relay->bytes_used);
        relay->base_ptr = NULL;
        relay->bytes_used = 0;
    }
    if (NULL != relay) {
        PMIX_DATA_BUFFER_RELEASE(relay);
    }
    PMIX_DATA_BUFFER_DESTRUCT(&datbuf);
}

static void barrier_release(int status, pmix_proc_t *sender,
                            pmix_data_buffer_t *buffer,
                            prte_rml_tag_t tag, void *cbdata)
{
    int32_t cnt;
    int rc, ret;
    prte_grpcomm_signature_t sig;
    prte_grpcomm_coll_t *coll;
    PRTE_HIDE_UNUSED_PARAMS(status, sender, tag, cbdata);

    PMIX_OUTPUT_VERBOSE((5, prte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct: barrier release called with %d bytes",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (int) buffer->bytes_used));

    /* unpack the signature */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &sig.sz, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    PMIX_PROC_CREATE(sig.signature, sig.sz);
    cnt = sig.sz;
    rc = PMIx_Data_unpack(NULL, buffer, sig.signature, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    /* unpack the return status */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &ret, &cnt, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    /* check for the tracker - it is not an error if not
     * found as that just means we wre not involved
     * in the collective */
    if (NULL == (coll = prte_grpcomm_base_get_tracker(&sig, false))) {
        PMIX_PROC_FREE(sig.signature, sig.sz);
        return;
    }

    /* execute the callback */
    if (NULL != coll->cbfunc) {
        coll->cbfunc(ret, buffer, coll->cbdata);
    }
    pmix_list_remove_item(&prte_grpcomm_base.ongoing, &coll->super);
    PMIX_RELEASE(coll);
    PMIX_PROC_FREE(sig.signature, sig.sz);
}
