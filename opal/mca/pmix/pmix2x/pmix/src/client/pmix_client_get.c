/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <src/include/types.h>
#include <src/include/pmix_stdint.h>

#include <pmix.h>
#include <pmix_rename.h>

#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if PMIX_HAVE_ZLIB
#include <zlib.h>
#endif
#include PMIX_EVENT_HEADER

#include "src/class/pmix_list.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/util/argv.h"
#include "src/util/compress.h"
#include "src/util/error.h"
#include "src/util/hash.h"
#include "src/util/output.h"
#include "src/mca/gds/gds.h"
#include "src/mca/ptl/ptl.h"

#include "pmix_client_ops.h"

static pmix_buffer_t* _pack_get(char *nspace, pmix_rank_t rank,
                               const pmix_info_t info[], size_t ninfo,
                               pmix_cmd_t cmd);

static void _getnbfn(int sd, short args, void *cbdata);

static void _getnb_cbfunc(struct pmix_peer_t *pr,
                          pmix_ptl_hdr_t *hdr,
                          pmix_buffer_t *buf, void *cbdata);

static void _value_cbfunc(pmix_status_t status, pmix_value_t *kv, void *cbdata);


PMIX_EXPORT pmix_status_t PMIx_Get(const pmix_proc_t *proc, const char key[],
                                   const pmix_info_t info[], size_t ninfo,
                                   pmix_value_t **val)
{
    pmix_cb_t *cb;
    pmix_status_t rc;

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* check for bozo case - we do not support this call for a NULL key */
    if (NULL == key || 0 == strlen(key)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;
    if (PMIX_SUCCESS != (rc = PMIx_Get_nb(proc, key, info, ninfo, _value_cbfunc, cb))) {
        PMIX_RELEASE(cb);
        return rc;
    }

    /* wait for the data to return */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = cb->status;
    *val = cb->value;
    PMIX_RELEASE(cb);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client get completed");

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Get_nb(const pmix_proc_t *proc, const char *key,
                                      const pmix_info_t info[], size_t ninfo,
                                      pmix_value_cbfunc_t cbfunc, void *cbdata)
{
    pmix_cb_t *cb;
    int rank;
    char *nm;

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

     /* check for bozo case - we do not support this call
      * for a NULL key */
     if (NULL == key || 0 == strlen(key)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
     }

    /* if the given proc param is NULL, or the nspace is
     * empty, then the caller is referencing our own nspace */
    if (NULL == proc || 0 == strlen(proc->nspace)) {
        nm = pmix_globals.myid.nspace;
    } else {
        nm = (char*)proc->nspace;
    }

    /* if the proc param is NULL, then we are seeking a key that
     * must be globally unique, so communicate this UNDEF rank */
    if (NULL == proc) {
        rank = PMIX_RANK_UNDEF;
    } else {
        rank = proc->rank;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: get_nb value for proc %s:%u key %s",
                        nm, rank, (NULL == key) ? "NULL" : key);

    /* threadshift this request so we can access global structures */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;
    cb->pname.nspace = strdup(nm);
    cb->pname.rank = rank;
    cb->key = (char*)key;
    cb->info = (pmix_info_t*)info;
    cb->ninfo = ninfo;
    cb->cbfunc.valuefn = cbfunc;
    cb->cbdata = cbdata;
    PMIX_THREADSHIFT(cb, _getnbfn);

    return PMIX_SUCCESS;
}

static void _value_cbfunc(pmix_status_t status, pmix_value_t *kv, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;

    cb->status = status;
    if (PMIX_SUCCESS == status) {
        PMIX_BFROPS_COPY(rc, &pmix_client_globals.myserver,
                         (void**)&cb->value, kv, PMIX_VALUE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }
    cb->active = false;
}

static pmix_buffer_t* _pack_get(char *nspace, pmix_rank_t rank,
                               const pmix_info_t info[], size_t ninfo,
                               pmix_cmd_t cmd)
{
    pmix_buffer_t *msg;
    pmix_status_t rc;

    /* nope - see if we can get it */
    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the get cmd */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    /* pack the request information - we'll get the entire blob
     * for this proc, so we don't need to pass the key */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &nspace, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &rank, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    /* pack the number of info structs */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msg, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return NULL;
        }
    }
    return msg;
}

/* this callback is coming from the PTL recv, and thus
 * is occurring inside of our progress thread - hence, no
 * need to thread shift */
static void _getnb_cbfunc(struct pmix_peer_t *pr,
                          pmix_ptl_hdr_t *hdr,
                          pmix_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_cb_t *cb2;
    pmix_status_t rc, ret;
    pmix_value_t *val = NULL;
    int32_t cnt;
    pmix_proc_t proc, proct;
    pmix_byte_object_t bo;
    pmix_buffer_t pbkt;
    pmix_kval_t *kv;
    pmix_peer_t *peer;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: get_nb callback recvd");

    if (NULL == cb) {
        /* nothing we can do */
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return;
    }
    /* cache the proc id */
    (void)strncpy(proc.nspace, cb->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = cb->pname.rank;

    /* unpack the status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &ret, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        pmix_list_remove_item(&pmix_client_globals.pending_requests, &cb->super);
        PMIX_RELEASE(cb);
        return;
    }

    if (PMIX_SUCCESS != ret) {
        goto done;
    }

    /* the incoming payload is provided as a set of packed
     * byte objects, one for each rank. A pmix_proc_t is the first
     * entry in the byte object. If the rank=PMIX_RANK_WILDCARD,
     * then that byte object contains job level info
     * for the provided nspace. Otherwise, the byte
     * object contains the pmix_kval_t's that were "put" by the
     * referenced process */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &bo, &cnt, PMIX_BYTE_OBJECT);
    while (PMIX_SUCCESS == rc) {
        /* setup the byte object for unpacking */
        PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
        PMIX_LOAD_BUFFER(&pmix_client_globals.myserver,
                         &pbkt, bo.bytes, bo.size);
        /* unpack the id of the providing process */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                           &pbkt, &proct, &cnt, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto done;
        }
        /* if the rank is WILDCARD, then the byte object contains
         * job-level data. Note that we can get job-level data
         * for a "get" request that referenced a specific non-wildcard
         * rank - this happens in the case where the nspace is
         * different than that of the requestor. We may also be
         * in a situation where the data for -all- ranks on a
         * remote node is being returned by a request for data
         * from only one of them - this can occur as an optimization.
         * So we have to check the rank here as it may not match the rank of
         * the requestor */
        if (PMIX_RANK_WILDCARD == proct.rank) {
            peer = &pmix_client_globals.myserver;  // job-level data is accessed via the server module
        } else {
            peer = pmix_globals.mypeer;   // all other data is stored on my peer module
        }
        cnt = 1;
        kv = PMIX_NEW(pmix_kval_t);
        PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                           &pbkt, kv, &cnt, PMIX_KVAL);
        while (PMIX_SUCCESS == rc) {
            /* let the GDS component for this peer store it - if
             * the kval contains shmem connection info, then the
             * component will know what to do about it (or else
             * we selected the wrong component for this peer!) */
            PMIX_GDS_STORE_KV(rc, peer, &proct, PMIX_INTERNAL, kv);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kv);
                PMIX_DESTRUCT(&pbkt);
                goto done;
            }
            PMIX_RELEASE(kv);  // maintain accounting
            /* get the next one */
            kv = PMIX_NEW(pmix_kval_t);
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                               &pbkt, kv, &cnt, PMIX_KVAL);
        }
        PMIX_RELEASE(kv);  // maintain accounting
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&pbkt);
            goto done;
        }
        PMIX_DESTRUCT(&pbkt);
        /* get the next one */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                           buf, &bo, &cnt, PMIX_BYTE_OBJECT);
        }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
    }

  done:
    /* now search any pending requests (including the one this was in
     * response to) to see if they can be met. Note that this function
     * will only be called if the user requested a specific key - we
     * don't support calls to "get" for a NULL key */
    PMIX_LIST_FOREACH_SAFE(cb, cb2, &pmix_client_globals.pending_requests, pmix_cb_t) {
        if (0 == strncmp(proc.nspace, cb->pname.nspace, PMIX_MAX_NSLEN) &&
            cb->pname.rank == proc.rank) {
           /* we have the data for this proc - see if we can find the key */
            cb->proc = &proc;
            cb->scope = PMIX_SCOPE_UNDEF;
            /* fetch the data from my peer module - since we are passing
             * it back to the user, we need a copy of it */
            cb->copy = true;
            PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, cb);
            if (PMIX_SUCCESS == rc) {
                if (1 != pmix_list_get_size(&cb->kvs)) {
                    rc = PMIX_ERR_INVALID_VAL;
                    val = NULL;
                } else {
                    kv = (pmix_kval_t*)pmix_list_remove_first(&cb->kvs);
                    val = kv->value;
                    kv->value = NULL; // protect the value
                    PMIX_RELEASE(kv);
                }
            }
            cb->cbfunc.valuefn(rc, val, cb->cbdata);
            pmix_list_remove_item(&pmix_client_globals.pending_requests, &cb->super);
            PMIX_RELEASE(cb);
        }
    }
}

static void timeout(int fd, short flags, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    /* let them know that we timed out */
    cb->cbfunc.valuefn(PMIX_ERR_TIMEOUT, NULL, cb->cbdata);
    cb->timer_running = false;

    /* remove this request */
    pmix_list_remove_item(&pmix_client_globals.pending_requests, &cb->super);
    PMIX_RELEASE(cb);
}

static void _getnbfn(int fd, short flags, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_cb_t *cbret;
    pmix_buffer_t *msg;
    pmix_value_t *val = NULL;
    pmix_status_t rc;
    size_t n;
    char *tmp;
    pmix_proc_t proc;
    pmix_list_t kvs;
    pmix_kval_t *kv;
    bool optional = false;
    struct timeval tv;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: getnbfn value for proc %s:%u key %s",
                        cb->pname.nspace, cb->pname.rank,
                        (NULL == cb->key) ? "NULL" : cb->key);

    /* set the proc object identifier */
    (void)strncpy(proc.nspace, cb->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = cb->pname.rank;

    /* scan the incoming directives */
    if (NULL != cb->info) {
        for (n=0; n < cb->ninfo; n++) {
            if (0 == strcmp(cb->info[n].key, PMIX_OPTIONAL) ||
                0 == strcmp(cb->info[n].key, PMIX_IMMEDIATE)) {
                if (PMIX_UNDEF == cb->info[n].value.type ||
                    cb->info[n].value.data.flag) {
                    optional = true;
                }
            } else if (0 == strcmp(cb->info[n].key, PMIX_TIMEOUT)) {
                /* set a timer to kick us out if we don't
                 * have an answer within their window */
                if (0 < cb->info[n].value.data.integer) {
                    tv.tv_sec = cb->info[n].value.data.integer;
                    tv.tv_usec = 0;
                    pmix_event_evtimer_set(pmix_globals.evbase, &cb->ev,
                                           timeout, cb);
                    pmix_event_evtimer_add(&cb->ev, &tv);
                    cb->timer_running = true;
                }
            } else if (0 == strcmp(cb->info[n].key, PMIX_DATA_SCOPE)) {
                cb->scope = cb->info[n].value.data.scope;
            }
        }
    }

    /* prep the response list */
    PMIX_CONSTRUCT(&kvs, pmix_list_t);

    /* if the key starts with "pmix", then they are looking for data
     * that was provided by the server at startup */
    if (0 == strncmp(cb->key, "pmix", 4)) {
        cb->proc = &proc;
        /* fetch the data from my server's module - since we are passing
         * it back to the user, we need a copy of it */
        cb->copy = true;
        PMIX_GDS_FETCH_KV(rc, &pmix_client_globals.myserver, cb);
        if (PMIX_SUCCESS != rc) {
            if (0 != strncmp(cb->pname.nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN)) {
                /* we are asking about the job-level info from another
                 * namespace. It seems that we don't have it - go and
                 * ask server
                 */
                goto request;
            } else {
                /* we should have had this info, so respond with the error */
                val = NULL;
                goto respond;
            }
        } else if (1 != pmix_list_get_size(&cb->kvs)) {
            /* not allowed */
            val = NULL;
            rc = PMIX_ERR_INVALID_VAL;
            goto respond;
        }
        /* return whatever we found */
        kv = (pmix_kval_t*)pmix_list_get_first(&cb->kvs);
        val = kv->value;
        kv->value = NULL;  // protect the value
        goto respond;
    } else {
        /* fetch the data from my peer module - since we are passing
         * it back to the user, we need a copy of it */
        cb->proc = &proc;
        cb->copy = true;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, cb);
        if (PMIX_SUCCESS != rc) {
            /* if this was a request for info about ourselves, then
             * there is no place to go for it - we just need to return
             * an error */
            if (0 == strncmp(cb->pname.nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN) &&
                cb->pname.rank == pmix_globals.myid.rank) {
                val = NULL;
                goto respond;
            } else {
                /* ask the server to try and get it for us */
                goto request;
            }
        } else if (1 != pmix_list_get_size(&cb->kvs)) {
            /* not allowed */
            val = NULL;
            rc = PMIX_ERR_INVALID_VAL;
            goto respond;
        }
        /* return whatever we found */
        kv = (pmix_kval_t*)pmix_list_get_first(&cb->kvs);
        val = kv->value;
        kv->value = NULL;  // protect the value
        goto respond;
    }

  respond:
    /* if a callback was provided, execute it */
    if (NULL != cb->cbfunc.valuefn) {
      if (NULL != val)  {
          /* if this is a compressed string, then uncompress it */
          if (PMIX_COMPRESSED_STRING == val->type) {
              pmix_util_uncompress_string(&tmp, (uint8_t*)val->data.bo.bytes, val->data.bo.size);
              if (NULL == tmp) {
                  PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                  rc = PMIX_ERR_NOMEM;
                  PMIX_VALUE_RELEASE(val);
                  val = NULL;
              } else {
                  PMIX_VALUE_DESTRUCT(val);
                  PMIX_VAL_ASSIGN(val, string, tmp);
              }
          }
      }
      cb->cbfunc.valuefn(rc, val, cb->cbdata);
    }
    if (NULL != val) {
      PMIX_VALUE_RELEASE(val);
    }
    PMIX_RELEASE(cb);
    return;

  request:
    /* if we got here, then we don't have the data for this proc. If we
     * are a server, or we are a client and not connected, then there is
     * nothing more we can do */
    if (PMIX_PROC_SERVER == pmix_globals.proc_type ||
        (PMIX_PROC_SERVER != pmix_globals.proc_type && !pmix_globals.connected)) {
        rc = PMIX_ERR_NOT_FOUND;
        goto respond;
    }

    /* we also have to check the user's directives to see if they do not want
     * us to attempt to retrieve it from the server */
    if (optional) {
        /* they don't want us to try and retrieve it */
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "PMIx_Get key=%s for rank = %u, namespace = %s was not found - request was optional",
                            cb->key, cb->pname.rank, cb->pname.nspace);
        rc = PMIX_ERR_NOT_FOUND;
        goto respond;
    }

    /* see if we already have a request in place with the server for data from
     * this nspace:rank. If we do, then no need to ask again as the
     * request will return _all_ data from that proc */
    PMIX_LIST_FOREACH(cbret, &pmix_client_globals.pending_requests, pmix_cb_t) {
        if (0 == strncmp(cbret->pname.nspace, cb->pname.nspace, PMIX_MAX_NSLEN) &&
            cbret->pname.rank == cb->pname.rank) {
            /* we do have a pending request, but we still need to track this
             * outstanding request so we can satisfy it once the data is returned */
            pmix_list_append(&pmix_client_globals.pending_requests, &cb->super);
            return;
        }
    }

    /* we don't have a pending request, so let's create one - don't worry
     * about packing the key as we return everything from that proc */
    msg = _pack_get(cb->pname.nspace, cb->pname.rank, cb->info, cb->ninfo, PMIX_GETNB_CMD);
    if (NULL == msg) {
        rc = PMIX_ERROR;
        goto respond;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "%s:%d REQUESTING DATA FROM SERVER FOR %s:%d KEY %s",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank,
                        cb->pname.nspace, cb->pname.rank, cb->key);

    /* track the callback object */
    pmix_list_append(&pmix_client_globals.pending_requests, &cb->super);
    /* send to the server */
    PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver, msg, _getnb_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        pmix_list_remove_item(&pmix_client_globals.pending_requests, &cb->super);
        rc = PMIX_ERROR;
        goto respond;
    }

    return;
}
