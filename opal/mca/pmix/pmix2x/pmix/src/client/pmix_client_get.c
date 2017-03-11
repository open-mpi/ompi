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
#include "src/include/pmix_jobdata.h"

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
#include "src/buffer_ops/buffer_ops.h"
#include "src/util/argv.h"
#include "src/util/compress.h"
#include "src/util/error.h"
#include "src/util/hash.h"
#include "src/util/output.h"
#include "src/mca/ptl/ptl.h"
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
#include "src/dstore/pmix_dstore.h"
#endif /* PMIX_ENABLE_DSTORE */

#include "pmix_client_ops.h"
#include "src/include/pmix_jobdata.h"

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

    /* if the proc is NULL, then the caller is assuming
     * that the key is universally unique within the caller's
     * own nspace. This most likely indicates that the code
     * was originally written for a legacy version of PMI.
     *
     * If the key is NULL, then the caller wants all
     * data from the specified proc. Again, this likely
     * indicates use of a legacy version of PMI.
     *
     * Either case is supported. However, we don't currently
     * support the case where -both- values are NULL */
    if (NULL == proc && NULL == key) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* if the key is NULL, the rank cannot be WILDCARD as
     * we cannot return all info from every rank */
    if (NULL != proc && PMIX_RANK_WILDCARD == proc->rank && NULL == key) {
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
     * must be globally unique, so communicate this to the hash
     * functions with the UNDEF rank */
    if (NULL == proc) {
        rank = PMIX_RANK_UNDEF;
    } else {
        rank = proc->rank;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: get_nb value for proc %s:%u key %s",
                        nm, rank, (NULL == key) ? "NULL" : key);

    /* thread-shift so we can check global objects */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;
    (void)strncpy(cb->nspace, nm, PMIX_MAX_NSLEN);
    cb->rank = rank;
    cb->key = (char*)key;
    cb->info = (pmix_info_t*)info;
    cb->ninfo = ninfo;
    cb->value_cbfunc = cbfunc;
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
        if (PMIX_SUCCESS != (rc = pmix_bfrop.copy((void**)&cb->value, kv, PMIX_VALUE))) {
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
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    /* pack the request information - we'll get the entire blob
     * for this proc, so we don't need to pass the key */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &nspace, 1, PMIX_STRING))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &rank, 1, PMIX_PROC_RANK))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    /* pack the number of info structs */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    if (0 < ninfo) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return NULL;
        }
    }
    return msg;
}

/* this callback is coming from the usock recv, and thus
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
    pmix_nspace_t *ns, *nptr;
    pmix_rank_t rank;
#if (PMIX_ENABLE_DSTORE != 1)
    pmix_rank_t cur_rank;
#endif
    char *tmp;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: get_nb callback recvd");

    if (NULL == cb) {
        /* nothing we can do */
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return;
    }
    /* cache the rank */
    rank = cb->rank;

    /* unpack the status */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ret, &cnt, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    /* look up the nspace object for this proc */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strncmp(cb->nspace, ns->nspace, PMIX_MAX_NSLEN)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        /* new nspace - setup a record for it */
        nptr = PMIX_NEW(pmix_nspace_t);
        (void)strncpy(nptr->nspace, cb->nspace, PMIX_MAX_NSLEN);
        pmix_list_append(&pmix_globals.nspaces, &nptr->super);
    }

    if (PMIX_SUCCESS != ret) {
        goto done;
    }

#if (defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1))
    if (PMIX_SUCCESS != (rc = pmix_dstore_fetch(nptr->nspace, cb->rank, cb->key, &val))){
        /* DO NOT error log this status - it is perfectly okay
         * for a key not to be found */
        goto done;
    }
#else
    /* we received the entire blob for this process, so
     * unpack and store it in the modex - this could consist
     * of buffers from multiple scopes */
    cur_rank = rank;
    cnt = 1;
    while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(buf, &cur_rank, &cnt, PMIX_PROC_RANK))) {
        pmix_kval_t *cur_kval;
        pmix_buffer_t *bptr;

        cnt = 1;
        if (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(buf, &bptr, &cnt, PMIX_BUFFER))) {
            /* if the rank is WILDCARD, then this is an nspace blob */
            if (PMIX_RANK_WILDCARD == cur_rank) {
                char *nspace;
                /* unpack the nspace - we don't really need it, but have to
                 * unpack it to maintain sequence */
                cnt = 1;
                if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(bptr, &nspace, &cnt, PMIX_STRING))) {
                    PMIX_ERROR_LOG(rc);
                    return;
                }
                free(nspace);
                pmix_job_data_htable_store(cb->nspace, bptr);

                /* Check if the key is in this blob */

                pmix_hash_fetch(&nptr->internal, cb->rank, cb->key, &val);

            } else {
                cnt = 1;
                cur_kval = PMIX_NEW(pmix_kval_t);
                while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(bptr, cur_kval, &cnt, PMIX_KVAL))) {
                    pmix_output_verbose(2, pmix_globals.debug_output,
                                        "pmix: unpacked key %s", cur_kval->key);
                    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nptr->modex, cur_rank, cur_kval))) {
                        PMIX_ERROR_LOG(rc);
                    }
                    if (NULL != cb->key && 0 == strcmp(cb->key, cur_kval->key)) {
                        pmix_output_verbose(2, pmix_globals.debug_output,
                                            "pmix: found requested value");
                        if (PMIX_SUCCESS != (rc = pmix_bfrop.copy((void**)&val, cur_kval->value, PMIX_VALUE))) {
                            PMIX_ERROR_LOG(rc);
                            PMIX_RELEASE(cur_kval);
                            val = NULL;
                            goto done;
                        }
                    }
                    PMIX_RELEASE(cur_kval); // maintain acctg - hash_store does a retain
                    cnt = 1;
                    cur_kval = PMIX_NEW(pmix_kval_t);
                }
                cnt = 1;
                PMIX_RELEASE(cur_kval);
            }
        }
        PMIX_RELEASE(bptr);  // free's the data region
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc &&
            PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            rc = PMIX_ERR_SILENT; // avoid error-logging twice
            break;
        }
    }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc &&
        PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    } else {
        rc = PMIX_SUCCESS;
    }
#endif /* PMIX_ENABLE_DSTORE */

done:
    /* if a callback was provided, execute it */
    if (NULL != cb && NULL != cb->value_cbfunc) {
        if (NULL == val) {
            rc = PMIX_ERR_NOT_FOUND;
        } else {
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
        cb->value_cbfunc(rc, val, cb->cbdata);
    }
    if (NULL != val) {
        PMIX_VALUE_RELEASE(val);
    }
    /* we obviously processed this one, so remove it from the
     * list of pending requests */
    pmix_list_remove_item(&pmix_client_globals.pending_requests, &cb->super);
    PMIX_RELEASE(cb);

    /* now search any pending requests to see if they can be met */
    PMIX_LIST_FOREACH_SAFE(cb, cb2, &pmix_client_globals.pending_requests, pmix_cb_t) {
        if (0 == strncmp(nptr->nspace, cb->nspace, PMIX_MAX_NSLEN) && cb->rank == rank) {
           /* we have the data - see if we can find the key */
            val = NULL;
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
            rc = pmix_dstore_fetch(nptr->nspace, rank, cb->key, &val);
#else
            rc = pmix_hash_fetch(&nptr->modex, rank, cb->key, &val);
#endif /* PMIX_ENABLE_DSTORE */
            cb->value_cbfunc(rc, val, cb->cbdata);
            if (NULL != val) {
                PMIX_VALUE_RELEASE(val);
            }
            pmix_list_remove_item(&pmix_client_globals.pending_requests, &cb->super);
            PMIX_RELEASE(cb);
        }
    }
}

static pmix_status_t process_val(pmix_value_t *val,
                                 size_t *num_vals,
                                 pmix_pointer_array_t *results)
{
    pmix_info_t *info;
    size_t n, nsize, nvals;
    pmix_status_t rc;

    if (NULL == val) {
        /* this is an error */
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }
    /* since we didn't provide them with a key, the hash function
     * must return the results in the pmix_data_array field of the
     * value */
    /* must account for the deprecated pmix_info_array_t */
    if (PMIX_DATA_ARRAY != val->type &&
        PMIX_INFO_ARRAY != val->type) {
        /* this is an error */
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }
    /* save the results */
    if (PMIX_DATA_ARRAY == val->type) {
        info = (pmix_info_t*)val->data.darray->array;
        nsize = val->data.darray->size;
    } else {
        info = (pmix_info_t*)val->data.array->array;
        nsize = val->data.array->size;
    }
    nvals = 0;
    for (n=0; n < nsize; n++) {
        if (PMIX_SUCCESS != (rc = pmix_pointer_array_add(results, &info[n]))) {
            return rc;
        }
        ++nvals;
    }
    if (PMIX_DATA_ARRAY == val->type) {
        val->data.darray->array = NULL;  // protect the data
        val->data.darray->size = 0;
    } else {
        val->data.array->array = NULL;
        val->data.array->size = 0;
    }
    /* increment the number of values */
    (*num_vals) += nvals;
    return PMIX_SUCCESS;
}

static void _getnbfn(int fd, short flags, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_cb_t *cbret;
    pmix_buffer_t *msg;
    pmix_value_t *val = NULL;
    pmix_info_t *info, *iptr;
    pmix_pointer_array_t results;
    pmix_status_t rc;
    pmix_nspace_t *ns, *nptr;
    size_t n, nvals;
    char *tmp;
    bool my_nspace = false, my_rank = false;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: getnbfn value for proc %s:%d key %s",
                        cb->nspace, cb->rank,
                        (NULL == cb->key) ? "NULL" : cb->key);

    /* find the nspace object */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(cb->nspace, ns->nspace)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        /* we are asking for info about a new nspace - give us
         * a chance to learn about it from the server. If the
         * server has never heard of it, the server will return
         * an error */
         nptr = PMIX_NEW(pmix_nspace_t);
         (void)strncpy(nptr->nspace, cb->nspace, PMIX_MAX_NSLEN);
         pmix_list_append(&pmix_globals.nspaces, &nptr->super);
         /* there is no point in looking for data in this nspace
          * object, so let's just go generate the request */
         goto request;
    }

    /* The NULL==key scenario only pertains to cases where legacy
     * PMI methods are being employed. In this case, we have to check
     * both the job-data  and the modex tables. If we don't yet have
     * the modex data, then we are going to have to go get it. So let's
     * check that case first */
    if (NULL == cb->key) {
        PMIX_CONSTRUCT(&results, pmix_pointer_array_t);
        pmix_pointer_array_init(&results, 2, INT_MAX, 1);
        nvals = 0;
        /* if the rank is WILDCARD, then they want all the job-level info,
         * so no need to check the modex */
        if (PMIX_RANK_WILDCARD != cb->rank) {
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
            if (PMIX_SUCCESS == (rc = pmix_dstore_fetch(nptr->nspace, cb->rank, NULL, &val))) {
#else
            if (PMIX_SUCCESS == (rc = pmix_hash_fetch(&nptr->modex, cb->rank, NULL, &val))) {
#endif /* PMIX_ENABLE_DSTORE */
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "pmix_get[%d]: value retrieved from dstore", __LINE__);
                if (PMIX_SUCCESS != (rc = process_val(val, &nvals, &results))) {
                    cb->value_cbfunc(rc, NULL, cb->cbdata);
                    /* cleanup */
                    if (NULL != val) {
                        PMIX_VALUE_RELEASE(val);
                    }
                    PMIX_RELEASE(cb);
                    return;
                }
                /* cleanup */
                PMIX_VALUE_RELEASE(val);
            } else {
                /* if we didn't find a modex for this rank, then we need
                 * to go get it. Thus, the caller wants -all- information for
                 * the specified rank, not just the job-level info. */
                 goto request;
            }
        }
        /* now get any data from the job-level info */
        if (PMIX_SUCCESS == (rc = pmix_hash_fetch(&nptr->internal, PMIX_RANK_WILDCARD, NULL, &val))) {
            if (PMIX_SUCCESS != (rc = process_val(val, &nvals, &results))) {
                cb->value_cbfunc(rc, NULL, cb->cbdata);
                /* cleanup */
                if (NULL != val) {
                    PMIX_VALUE_RELEASE(val);
                }
                PMIX_RELEASE(cb);
                return;
            }
            /* cleanup */
            PMIX_VALUE_RELEASE(val);
        }
        /* now let's package up the results */
        PMIX_VALUE_CREATE(val, 1);
        val->type = PMIX_DATA_ARRAY;
        val->data.darray->type = PMIX_INFO;
        val->data.darray->size = nvals;
        PMIX_INFO_CREATE(iptr, nvals);
        val->data.darray->array = (void*)iptr;
        for (n=0; n < (size_t)results.size && n < nvals; n++) {
            if (NULL != (info = (pmix_info_t*)pmix_pointer_array_get_item(&results, n))) {
                (void)strncpy(iptr[n].key, info->key, PMIX_MAX_KEYLEN);
                /* if this is a compressed string, then uncompress it */
                if (PMIX_COMPRESSED_STRING == info->value.type) {
                    iptr[n].value.type = PMIX_STRING;
                    pmix_util_uncompress_string(&iptr[n].value.data.string,
                                                (uint8_t*)info->value.data.bo.bytes,
                                                info->value.data.bo.size);
                    if (NULL == iptr[n].value.data.string) {
                        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                    }
                } else {
                    pmix_value_xfer(&iptr[n].value, &info->value);
                }
                PMIX_INFO_FREE(info, 1);
            }
        }
        /* done with results array */
        PMIX_DESTRUCT(&results);
        /* return the result to the caller */
        cb->value_cbfunc(PMIX_SUCCESS, val, cb->cbdata);
        PMIX_VALUE_FREE(val, 1);
        PMIX_RELEASE(cb);
        return;
    }

    /* check the internal storage first */
    rc = pmix_hash_fetch(&nptr->internal, cb->rank, cb->key, &val);
    if(PMIX_SUCCESS == rc) {
        goto respond;
    }

    my_nspace = (0 == strncmp(cb->nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN));
    my_rank = (pmix_globals.myid.rank == cb->rank);

    /* if the key starts from "pmix", then they are looking for data
     * that was provided at startup */
    if (0 == strncmp(cb->key, "pmix", 4)) {
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
        /* if this is a dstore - check there */
        rc = pmix_dstore_fetch(cb->nspace, cb->rank, cb->key, &val);
#endif
        if( PMIX_SUCCESS != rc && !my_nspace ){
            /* we are asking about the job-level info from other
             * namespace. It seems tha we don't have it - go and
             * ask server
             */
            goto request;
        }
        /* we supposed to already have all local namespace data */
        goto respond;
    }

    /* if we were asked about this rank */
    if ( my_nspace && my_rank ){
        /* if we asking the data about this rank - check local hash table.
         * All the data passed through PMIx_Put settle down there
         * if there is nothing there - it's nothing else we can do
         */
        rc = pmix_hash_fetch(&nptr->modex, pmix_globals.myid.rank, cb->key, &val);
        if( PMIX_SUCCESS != rc ){
            rc = PMIX_ERR_NOT_FOUND;
            goto respond;
        }
    }

    /* otherwise, the data must be something they "put" */
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    rc = PMIX_ERR_PROC_ENTRY_NOT_FOUND;
    /* if rank is undefined - check local table first */
    if ( my_nspace && (PMIX_RANK_UNDEF == cb->rank)){
        /* if we asking about undefined process - check local hash table first
         * local rank may have submitted this key. */
        rc = pmix_hash_fetch(&nptr->modex, pmix_globals.myid.rank, cb->key, &val);
    }
    /* try to take it from dstore */
    if( PMIX_ERR_PROC_ENTRY_NOT_FOUND == rc ){
        /* Two option possible here:
           - we asking the key from UNDEF process and local proc
             haven't pushed this data
           - we askin the key from the particular process which is not us.
         */
        rc = pmix_dstore_fetch(nptr->nspace, cb->rank, cb->key, &val);
    }
#else
    rc = pmix_hash_fetch(&nptr->modex, cb->rank, cb->key, &val);
#endif /* PMIX_ENABLE_DSTORE */

    if ( PMIX_SUCCESS == rc ) {
        goto respond;
    } else if ( PMIX_ERR_PROC_ENTRY_NOT_FOUND == rc ){
        goto request;
    }else if (PMIX_ERR_NOT_FOUND == rc) {
        /* we have the modex data from this proc, but didn't find the key
         * the user requested. It's possible someone pushed something since
         * we got this data, so let's ask the server for an update. However,
         * we do have to protect against an infinite loop! */
        if (cb->checked) {
            goto respond;
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "Unable to locally satisfy request for key=%s for rank = %d, namespace = %s",
                            cb->key, cb->rank, cb->nspace);
        cb->checked = true; // flag that we are going to check this again
        goto request;
    } else if (PMIX_ERR_PROC_ENTRY_NOT_FOUND != rc) {
        /* errors are fatal */
        goto respond;
    }

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
    for (n=0; n < cb->ninfo; n++) {
        if (0 == strcmp(cb->info[n].key, PMIX_OPTIONAL) &&
            (PMIX_UNDEF == cb->info[n].value.type || cb->info[n].value.data.flag)) {
            /* they don't want us to try and retrieve it */
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "PMIx_Get key=%s for rank = %d, namespace = %s was not found - request was optional",
                                cb->key, cb->rank, cb->nspace);
            rc = PMIX_ERR_NOT_FOUND;
            goto respond;
        }
    }

    /* see if we already have a request in place with the server for data from
     * this nspace:rank. If we do, then no need to ask again as the
     * request will return _all_ data from that proc */
    PMIX_LIST_FOREACH(cbret, &pmix_client_globals.pending_requests, pmix_cb_t) {
        if (0 == strncmp(cbret->nspace, cb->nspace, PMIX_MAX_NSLEN) &&
            cbret->rank == cb->rank) {
            /* we do have a pending request, but we still need to track this
             * outstanding request so we can satisfy it once the data is returned */
            pmix_list_append(&pmix_client_globals.pending_requests, &cb->super);
            return;
        }
    }

    /* we don't have a pending request, so let's create one - don't worry
     * about packing the key as we return everything from that proc */
    msg = _pack_get(cb->nspace, cb->rank, cb->info, cb->ninfo, PMIX_GETNB_CMD);
    if (NULL == msg) {
        rc = PMIX_ERROR;
        goto respond;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "%s:%d REQUESTING DATA FROM SERVER FOR %s:%d KEY %s",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank,
                        cb->nspace, cb->rank, cb->key);

    /* track the callback object */
    pmix_list_append(&pmix_client_globals.pending_requests, &cb->super);
    /* send to the server */
    if (PMIX_SUCCESS != (rc = pmix_ptl.send_recv(&pmix_client_globals.myserver, msg, _getnb_cbfunc, (void*)cb))){
        pmix_list_remove_item(&pmix_client_globals.pending_requests, &cb->super);
        rc = PMIX_ERROR;
        goto respond;
    }

    return;

respond:

    /* if a callback was provided, execute it */
    if (NULL != cb->value_cbfunc) {
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
        cb->value_cbfunc(rc, val, cb->cbdata);
    }
    if (NULL != val) {
        PMIX_VALUE_RELEASE(val);
    }
    PMIX_RELEASE(cb);
    return;

}
