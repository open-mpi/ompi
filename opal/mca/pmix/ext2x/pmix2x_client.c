/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/types.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/hash_string.h"
#include "opal/threads/threads.h"
#include "opal/util/argv.h"
#include "opal/util/proc.h"

#include "opal/mca/pmix/base/base.h"
#include "pmix2x.h"
#include "pmix.h"

static pmix_proc_t my_proc;
static char *dbgvalue=NULL;
static volatile bool regactive;
static bool initialized = false;

#define PMIX_WAIT_FOR_COMPLETION(a)             \
    do {                                        \
        while ((a)) {                           \
            usleep(10);                         \
        }                                       \
        OPAL_ACQUIRE_OBJECT(a);                 \
    } while (0)


static void errreg_cbfunc (pmix_status_t status,
                           size_t errhandler_ref,
                           void *cbdata)
{
    opal_pmix2x_event_t *event = (opal_pmix2x_event_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(event);

    event->index = errhandler_ref;
    opal_output_verbose(5, opal_pmix_base_framework.framework_output,
                        "PMIX client errreg_cbfunc - error handler registered status=%d, reference=%lu",
                        status, (unsigned long)errhandler_ref);
    regactive = false;
    OPAL_POST_OBJECT(regactive);
}

int pmix2x_client_init(opal_list_t *ilist)
{
    opal_process_name_t pname;
    pmix_status_t rc;
    int dbg;
    opal_pmix2x_jobid_trkr_t *job;
    opal_pmix2x_event_t *event;
    pmix_info_t *pinfo;
    size_t ninfo, n;
    opal_value_t *ival;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client init");

    if (!initialized) {
        if (0 < (dbg = opal_output_get_verbosity(opal_pmix_base_framework.framework_output))) {
            asprintf(&dbgvalue, "PMIX_DEBUG=%d", dbg);
            putenv(dbgvalue);
        }
    }

    /* convert the incoming list to info structs */
    if (NULL != ilist) {
        ninfo = opal_list_get_size(ilist);
        if (0 < ninfo) {
            PMIX_INFO_CREATE(pinfo, ninfo);
            n=0;
            OPAL_LIST_FOREACH(ival, ilist, opal_value_t) {
                (void)strncpy(pinfo[n].key, ival->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&pinfo[n].value, ival);
                ++n;
            }
        } else {
            pinfo = NULL;
        }
    } else {
        pinfo = NULL;
        ninfo = 0;
    }

    rc = PMIx_Init(&my_proc, pinfo, ninfo);
    if (PMIX_SUCCESS != rc) {
        return pmix2x_convert_rc(rc);
    }
    if (0 < ninfo) {
        PMIX_INFO_FREE(pinfo, ninfo);

    }
    if (initialized) {
        return OPAL_SUCCESS;
    }
    initialized = true;

    /* store our jobid and rank */
    if (NULL != getenv(OPAL_MCA_PREFIX"orte_launch")) {
        /* if we were launched by the OMPI RTE, then
         * the jobid is in a special format - so get it */
        mca_pmix_ext2x_component.native_launch = true;
        opal_convert_string_to_jobid(&pname.jobid, my_proc.nspace);
    } else {
        /* we were launched by someone else, so make the
         * jobid just be the hash of the nspace */
        OPAL_HASH_JOBID(my_proc.nspace, pname.jobid);
    }
    /* insert this into our list of jobids - it will be the
     * first, and so we'll check it first */
    job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
    (void)strncpy(job->nspace, my_proc.nspace, PMIX_MAX_NSLEN);
    job->jobid = pname.jobid;
    opal_list_append(&mca_pmix_ext2x_component.jobids, &job->super);

    pname.vpid = pmix2x_convert_rank(my_proc.rank);
    opal_proc_set_name(&pname);

    /* register the default event handler */
    event = OBJ_NEW(opal_pmix2x_event_t);
    opal_list_append(&mca_pmix_ext2x_component.events, &event->super);
    PMIX_INFO_CREATE(pinfo, 1);
    PMIX_INFO_LOAD(&pinfo[0], PMIX_EVENT_HDLR_NAME, "OPAL-PMIX-2X-DEFAULT", PMIX_STRING);
    regactive = true;
    PMIx_Register_event_handler(NULL, 0, pinfo, 1, pmix2x_event_hdlr, errreg_cbfunc, event);
    PMIX_WAIT_FOR_COMPLETION(regactive);
    PMIX_INFO_FREE(pinfo, 1);

    return OPAL_SUCCESS;

}

int pmix2x_client_finalize(void)
{
    pmix_status_t rc;
    opal_pmix2x_event_t *event;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client finalize");

    /* deregister all event handlers */
    OPAL_LIST_FOREACH(event, &mca_pmix_ext2x_component.events, opal_pmix2x_event_t) {
        PMIx_Deregister_event_handler(event->index, NULL, NULL);
    }
    /* the list will be destructed when the component is finalized */

    rc = PMIx_Finalize(NULL, 0);
    return pmix2x_convert_rc(rc);
}

int pmix2x_initialized(void)
{
    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client initialized");

    return initialized;
}

int pmix2x_abort(int flag, const char *msg,
                 opal_list_t *procs)
{
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client abort");

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
            /* look thru our list of jobids and find the
             * corresponding nspace */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
                if (jptr->jobid == ptr->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                PMIX_PROC_FREE(parray, cnt);
                return OPAL_ERR_NOT_FOUND;
            }
            (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
            parray[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
            ++n;
        }
    }

    /* call the library abort */
    rc = PMIx_Abort(flag, msg, parray, cnt);

    /* release the array */
    PMIX_PROC_FREE(parray, cnt);

    return pmix2x_convert_rc(rc);
}

int pmix2x_store_local(const opal_process_name_t *proc, opal_value_t *val)
{
    pmix_value_t kv;
    pmix_status_t rc;
    pmix_proc_t p;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */

    if (NULL != proc) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (jptr->jobid == proc->jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            return OPAL_ERR_NOT_FOUND;
        }
        (void)strncpy(p.nspace, job->nspace, PMIX_MAX_NSLEN);
        p.rank = pmix2x_convert_opalrank(proc->vpid);
    } else {
        /* use our name */
        (void)strncpy(p.nspace, my_proc.nspace, PMIX_MAX_NSLEN);
        p.rank = pmix2x_convert_opalrank(OPAL_PROC_MY_NAME.vpid);
    }

    PMIX_VALUE_CONSTRUCT(&kv);
    pmix2x_value_load(&kv, val);

    rc = PMIx_Store_internal(&p, val->key, &kv);
    PMIX_VALUE_DESTRUCT(&kv);

    return pmix2x_convert_rc(rc);
}

int pmix2x_commit(void)
{
    pmix_status_t rc;

    rc = PMIx_Commit();
    return pmix2x_convert_rc(rc);
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;

    OPAL_ACQUIRE_OBJECT(op);
    if (NULL != op->opcbfunc) {
        op->opcbfunc(pmix2x_convert_rc(status), op->cbdata);
    }
    OBJ_RELEASE(op);
}

int pmix2x_fence(opal_list_t *procs, int collect_data)
{
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix_info_t info, *iptr;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client fence");

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
            /* look thru our list of jobids and find the
             * corresponding nspace */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
                if (jptr->jobid == ptr->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                PMIX_PROC_FREE(parray, cnt);
                return OPAL_ERR_NOT_FOUND;
            }
            (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
            parray[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
            ++n;
        }
    }
    if (collect_data) {
        PMIX_INFO_CONSTRUCT(&info);
        (void)strncpy(info.key, PMIX_COLLECT_DATA, PMIX_MAX_KEYLEN);
        info.value.type = PMIX_BOOL;
        info.value.data.flag = true;
        iptr = &info;
        n = 1;
    } else {
        iptr = NULL;
        n = 0;
    }

    /* call the library function */
    rc = PMIx_Fence(parray, cnt, iptr, n);

    /* release the array */
    PMIX_PROC_FREE(parray, cnt);
    if (NULL != iptr) {
        PMIX_INFO_DESTRUCT(&info);
    }

    return pmix2x_convert_rc(rc);

}

int pmix2x_fencenb(opal_list_t *procs, int collect_data,
                   opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix2x_opcaddy_t *op;
    pmix_info_t info, *iptr;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client fence_nb");

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
            /* look thru our list of jobids and find the
             * corresponding nspace */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
                if (jptr->jobid == ptr->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                PMIX_PROC_FREE(parray, cnt);
                return OPAL_ERR_NOT_FOUND;
            }
            (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
            parray[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
            ++n;
        }
    }

    if (collect_data) {
        PMIX_INFO_CONSTRUCT(&info);
        (void)strncpy(info.key, PMIX_COLLECT_DATA, PMIX_MAX_KEYLEN);
        info.value.type = PMIX_BOOL;
        info.value.data.flag = true;
        iptr = &info;
        n = 1;
    } else {
        iptr = NULL;
        n = 0;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->procs = parray;
    op->nprocs = cnt;

    /* call the library function */
    rc = PMIx_Fence_nb(parray, cnt, iptr, n, opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }

    return pmix2x_convert_rc(rc);

}

int pmix2x_put(opal_pmix_scope_t opal_scope,
               opal_value_t *val)
{
    pmix_value_t kv;
    pmix_scope_t pmix_scope = pmix2x_convert_opalscope(opal_scope);
    pmix_status_t rc;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client put");

    PMIX_VALUE_CONSTRUCT(&kv);
    pmix2x_value_load(&kv, val);

    rc = PMIx_Put(pmix_scope, val->key, &kv);
    PMIX_VALUE_DESTRUCT(&kv);
    return pmix2x_convert_rc(rc);
}

int pmix2x_get(const opal_process_name_t *proc, const char *key,
               opal_list_t *info, opal_value_t **val)
{
    int ret;
    pmix_value_t *kv;
    pmix_status_t rc;
    pmix_proc_t p, *pptr;
    size_t ninfo, n;
    pmix_info_t *pinfo;
    opal_value_t *ival;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "%s PMIx_client get on proc %s key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        (NULL == proc) ? "NULL" : OPAL_NAME_PRINT(*proc), key);

    /* prep default response */
    *val = NULL;
    if (NULL != proc) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (jptr->jobid == proc->jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            return OPAL_ERR_NOT_FOUND;
        }
        (void)strncpy(p.nspace, job->nspace, PMIX_MAX_NSLEN);
        p.rank = pmix2x_convert_opalrank(proc->vpid);
        pptr = &p;
    } else {
        /* if they are asking for our jobid, then return it */
        if (0 == strcmp(key, OPAL_PMIX_JOBID)) {
            (*val) = OBJ_NEW(opal_value_t);
            (*val)->type = OPAL_UINT32;
            (*val)->data.uint32 = OPAL_PROC_MY_NAME.jobid;
            return OPAL_SUCCESS;
        } else if (0 == strcmp(key, OPAL_PMIX_RANK)) {
            (*val) = OBJ_NEW(opal_value_t);
            (*val)->type = OPAL_INT;
            (*val)->data.integer = pmix2x_convert_rank(my_proc.rank);
            return OPAL_SUCCESS;
        }
        pptr = NULL;
    }

    if (NULL != info) {
        ninfo = opal_list_get_size(info);
        if (0 < ninfo) {
            PMIX_INFO_CREATE(pinfo, ninfo);
            n=0;
            OPAL_LIST_FOREACH(ival, info, opal_value_t) {
                (void)strncpy(pinfo[n].key, ival->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&pinfo[n].value, ival);
                ++n;
            }
        } else {
            pinfo = NULL;
        }
    } else {
        pinfo = NULL;
        ninfo = 0;
    }

    /* pass the request down */
    rc = PMIx_Get(pptr, key, pinfo, ninfo, &kv);
    if (PMIX_SUCCESS == rc) {
        if (NULL == kv) {
            ret = OPAL_SUCCESS;
        } else {
            *val = OBJ_NEW(opal_value_t);
            ret = pmix2x_value_unload(*val, kv);
            PMIX_VALUE_FREE(kv, 1);
        }
    } else {
        ret = pmix2x_convert_rc(rc);
    }
    PMIX_INFO_FREE(pinfo, ninfo);
    return ret;
}

static void val_cbfunc(pmix_status_t status,
                       pmix_value_t *kv, void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;
    int rc;
    opal_value_t val, *v=NULL;

    OPAL_ACQUIRE_OBJECT(op);

    rc = pmix2x_convert_opalrc(status);
    if (PMIX_SUCCESS == status && NULL != kv) {
        rc = pmix2x_value_unload(&val, kv);
        v = &val;
    }

    if (NULL != op->valcbfunc) {
        op->valcbfunc(rc, v, op->cbdata);
    }
    OBJ_RELEASE(op);
}

int pmix2x_getnb(const opal_process_name_t *proc, const char *key,
                 opal_list_t *info,
                 opal_pmix_value_cbfunc_t cbfunc, void *cbdata)
{
    pmix2x_opcaddy_t *op;
    pmix_status_t rc;
    size_t n;
    opal_value_t *ival;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access shared lists/objects */

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "%s PMIx_client get_nb on proc %s key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        (NULL == proc) ? "NULL" : OPAL_NAME_PRINT(*proc), key);

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->valcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != proc) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (jptr->jobid == proc->jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            return OPAL_ERR_NOT_FOUND;
        }
        (void)strncpy(op->p.nspace, job->nspace, PMIX_MAX_NSLEN);
        op->p.rank = pmix2x_convert_opalrank(proc->vpid);
    } else {
        (void)strncpy(op->p.nspace, my_proc.nspace, PMIX_MAX_NSLEN);
        op->p.rank = pmix2x_convert_rank(PMIX_RANK_WILDCARD);
    }

    if (NULL != info) {
        op->sz = opal_list_get_size(info);
        if (0 < op->sz) {
            PMIX_INFO_CREATE(op->info, op->sz);
            n=0;
            OPAL_LIST_FOREACH(ival, info, opal_value_t) {
                (void)strncpy(op->info[n].key, ival->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&op->info[n].value, ival);
                ++n;
            }
        }
    }

    /* call the library function */
    rc = PMIx_Get_nb(&op->p, key, op->info, op->sz, val_cbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }

    return pmix2x_convert_rc(rc);
}

int pmix2x_publish(opal_list_t *info)
{
    pmix_info_t *pinfo;
    pmix_status_t ret;
    opal_value_t *iptr;
    size_t sz, n;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client publish");

    if (NULL == info) {
        return OPAL_ERR_BAD_PARAM;
    }

    sz = opal_list_get_size(info);
    if (0 < sz) {
        PMIX_INFO_CREATE(pinfo, sz);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&pinfo[n].value, iptr);
            ++n;
        }
    } else {
        pinfo = NULL;
    }

    ret = PMIx_Publish(pinfo, sz);
    if (0 < sz) {
        PMIX_INFO_FREE(pinfo, sz);
    }

    return pmix2x_convert_rc(ret);
}

int pmix2x_publishnb(opal_list_t *info,
                     opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    opal_value_t *iptr;
    size_t n;
    pmix2x_opcaddy_t *op;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client publish_nb");

    if (NULL == info) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    op->sz = opal_list_get_size(info);
    if (0 < op->sz) {
        PMIX_INFO_CREATE(op->info, op->sz);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
            (void)strncpy(op->info[n].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&op->info[n].value, iptr);
            ++n;
        }
    }

    ret = PMIx_Publish_nb(op->info, op->sz, opcbfunc, op);
    if (0 < op->sz) {
        PMIX_INFO_FREE(op->info, op->sz);
    }

    return pmix2x_convert_rc(ret);
}

int pmix2x_lookup(opal_list_t *data, opal_list_t *info)
{
    pmix_pdata_t *pdata;
    pmix_info_t *pinfo;
    size_t sz, ninfo, n;
    int rc;
    pmix_status_t ret;
    opal_pmix_pdata_t *d;
    opal_value_t *iptr;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access shared lists/objects */
    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client lookup");

    if (NULL == data) {
        return OPAL_ERR_BAD_PARAM;
    }

    sz = opal_list_get_size(data);
    PMIX_PDATA_CREATE(pdata, sz);
    n=0;
    OPAL_LIST_FOREACH(d, data, opal_pmix_pdata_t) {
        (void)strncpy(pdata[n++].key, d->value.key, PMIX_MAX_KEYLEN);
    }

    if (NULL != info) {
        ninfo = opal_list_get_size(info);
        PMIX_INFO_CREATE(pinfo, ninfo);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&pinfo[n].value, iptr);
            ++n;
        }
    } else {
        pinfo = NULL;
        ninfo = 0;
    }

    ret = PMIx_Lookup(pdata, sz, pinfo, ninfo);
    PMIX_INFO_FREE(pinfo, ninfo);

    if (PMIX_SUCCESS == ret) {
        /* transfer the data back */
        n=0;
        OPAL_LIST_FOREACH(d, data, opal_pmix_pdata_t) {
            if (mca_pmix_ext2x_component.native_launch) {
                /* if we were launched by the OMPI RTE, then
                 * the jobid is in a special format - so get it */
                opal_convert_string_to_jobid(&d->proc.jobid, pdata[n].proc.nspace);
            } else {
                /* we were launched by someone else, so make the
                 * jobid just be the hash of the nspace */
                OPAL_HASH_JOBID(pdata[n].proc.nspace, d->proc.jobid);
            }
            /* if we don't already have it, add this to our jobid tracker */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
                if (jptr->jobid == d->proc.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
                (void)strncpy(job->nspace, pdata[n].proc.nspace, PMIX_MAX_NSLEN);
                job->jobid = d->proc.jobid;
                opal_list_append(&mca_pmix_ext2x_component.jobids, &job->super);
            }
            d->proc.vpid = pmix2x_convert_rank(pdata[n].proc.rank);
            rc = pmix2x_value_unload(&d->value, &pdata[n].value);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                PMIX_PDATA_FREE(pdata, sz);
                return OPAL_ERR_BAD_PARAM;
            }
            ++n;
        }
    }

    return pmix2x_convert_rc(ret);
}

static void lk_cbfunc(pmix_status_t status,
                      pmix_pdata_t data[], size_t ndata,
                      void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;
    opal_pmix_pdata_t *d;
    opal_list_t results, *r = NULL;
    int rc;
    size_t n;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    OPAL_ACQUIRE_OBJECT(op);

    /* this is in the PMIx local thread - need to threadshift to
     * our own thread as we will be accessing framework-global
     * lists and objects */

    if (NULL == op->lkcbfunc) {
        OBJ_RELEASE(op);
        return;
    }

    rc = pmix2x_convert_rc(status);
    if (OPAL_SUCCESS == rc) {
        OBJ_CONSTRUCT(&results, opal_list_t);
        for (n=0; n < ndata; n++) {
            d = OBJ_NEW(opal_pmix_pdata_t);
            opal_list_append(&results, &d->super);
            if (mca_pmix_ext2x_component.native_launch) {
                /* if we were launched by the OMPI RTE, then
                 * the jobid is in a special format - so get it */
                opal_convert_string_to_jobid(&d->proc.jobid, data[n].proc.nspace);
            } else {
                /* we were launched by someone else, so make the
                 * jobid just be the hash of the nspace */
                OPAL_HASH_JOBID(data[n].proc.nspace, d->proc.jobid);
            }
            /* if we don't already have it, add this to our jobid tracker */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
                if (jptr->jobid == d->proc.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
                (void)strncpy(job->nspace, data[n].proc.nspace, PMIX_MAX_NSLEN);
                job->jobid = d->proc.jobid;
                opal_list_append(&mca_pmix_ext2x_component.jobids, &job->super);
            }
            d->proc.vpid = pmix2x_convert_rank(data[n].proc.rank);
            d->value.key = strdup(data[n].key);
            rc = pmix2x_value_unload(&d->value, &data[n].value);
            if (OPAL_SUCCESS != rc) {
                rc = OPAL_ERR_BAD_PARAM;
                OPAL_ERROR_LOG(rc);
                goto release;
            }
        }
        r = &results;
    }
  release:
    /* execute the callback */
    op->lkcbfunc(rc, r, op->cbdata);

    if (NULL != r) {
        OPAL_LIST_DESTRUCT(&results);
    }
    OBJ_RELEASE(op);
}

int pmix2x_lookupnb(char **keys, opal_list_t *info,
                    opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    pmix2x_opcaddy_t *op;
    opal_value_t *iptr;
    size_t n;


    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client lookup_nb");

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->lkcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != info) {
        op->sz = opal_list_get_size(info);
        if (0 < op->sz) {
            PMIX_INFO_CREATE(op->info, op->sz);
            n=0;
            OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
                (void)strncpy(op->info[n].key, iptr->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&op->info[n].value, iptr);
                ++n;
            }
        }
    }

    ret = PMIx_Lookup_nb(keys, op->info, op->sz, lk_cbfunc, op);

    return pmix2x_convert_rc(ret);
}

int pmix2x_unpublish(char **keys, opal_list_t *info)
{
    pmix_status_t ret;
    size_t ninfo, n;
    pmix_info_t *pinfo;
    opal_value_t *iptr;

    if (NULL != info) {
        ninfo = opal_list_get_size(info);
        PMIX_INFO_CREATE(pinfo, ninfo);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&pinfo[n].value, iptr);
            ++n;
        }
    } else {
        pinfo = NULL;
        ninfo = 0;
    }

    ret = PMIx_Unpublish(keys, pinfo, ninfo);
    PMIX_INFO_FREE(pinfo, ninfo);

    return pmix2x_convert_rc(ret);
}

int pmix2x_unpublishnb(char **keys, opal_list_t *info,
                       opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    pmix2x_opcaddy_t *op;
    opal_value_t *iptr;
    size_t n;

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != info) {
        op->sz = opal_list_get_size(info);
        if (0 < op->sz) {
            PMIX_INFO_CREATE(op->info, op->sz);
            n=0;
            OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
                (void)strncpy(op->info[n].key, iptr->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&op->info[n].value, iptr);
                ++n;
            }
        }
    }

    ret = PMIx_Unpublish_nb(keys, op->info, op->sz, opcbfunc, op);

    return pmix2x_convert_rc(ret);
}

int pmix2x_spawn(opal_list_t *job_info, opal_list_t *apps, opal_jobid_t *jobid)
{
    pmix_status_t ret;
    pmix_info_t *pinfo = NULL;
    pmix_app_t *papps;
    size_t napps, n, m, ninfo = 0;
    char nspace[PMIX_MAX_NSLEN+1];
    opal_value_t *info;
    opal_pmix_app_t *app;
    opal_pmix2x_jobid_trkr_t *job;

    if (NULL != job_info && 0 < (ninfo = opal_list_get_size(job_info))) {
        PMIX_INFO_CREATE(pinfo, ninfo);
        n=0;
        OPAL_LIST_FOREACH(info, job_info, opal_value_t) {
            (void)strncpy(pinfo[n].key, info->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&pinfo[n].value, info);
            ++n;
        }
    }

    napps = opal_list_get_size(apps);
    PMIX_APP_CREATE(papps, napps);
    n=0;
    OPAL_LIST_FOREACH(app, apps, opal_pmix_app_t) {
        papps[n].cmd = strdup(app->cmd);
        papps[n].argv = opal_argv_copy(app->argv);
        papps[n].env = opal_argv_copy(app->env);
        papps[n].maxprocs = app->maxprocs;
        if (0 < (papps[n].ninfo = opal_list_get_size(&app->info))) {
            PMIX_INFO_CREATE(papps[n].info, papps[n].ninfo);
            m=0;
            OPAL_LIST_FOREACH(info, &app->info, opal_value_t) {
                (void)strncpy(papps[n].info[m].key, info->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&papps[n].info[m].value, info);
                ++m;
            }
        }
        ++n;
    }

    ret = PMIx_Spawn(pinfo, ninfo, papps, napps, nspace);
    if (PMIX_SUCCESS == ret) {
        if (mca_pmix_ext2x_component.native_launch) {
            /* if we were launched by the OMPI RTE, then
             * the jobid is in a special format - so get it */
            opal_convert_string_to_jobid(jobid, nspace);
        } else {
            /* we were launched by someone else, so make the
             * jobid just be the hash of the nspace */
            OPAL_HASH_JOBID(nspace, *jobid);
        }
        /* add this to our jobid tracker */
        job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
        (void)strncpy(job->nspace, nspace, PMIX_MAX_NSLEN);
        job->jobid = *jobid;
        opal_list_append(&mca_pmix_ext2x_component.jobids, &job->super);
    }
    if (0 < ninfo) {
        PMIX_INFO_FREE(pinfo, ninfo);
    }
    PMIX_APP_FREE(papps, napps);

    return pmix2x_convert_rc(ret);
}

static void spcbfunc(pmix_status_t status,
                     char *nspace, void *cbdata)
{
    pmix2x_opcaddy_t *op = (pmix2x_opcaddy_t*)cbdata;
    int rc;
    opal_jobid_t jobid=OPAL_JOBID_INVALID;
    opal_pmix2x_jobid_trkr_t *job;

    OPAL_ACQUIRE_OBJECT(op);

    /* this is in the PMIx local thread - need to threadshift to
     * our own thread as we will be accessing framework-global
     * lists and objects */

    rc = pmix2x_convert_rc(status);
    if (PMIX_SUCCESS == status) {
        if (mca_pmix_ext2x_component.native_launch) {
            /* if we were launched by the OMPI RTE, then
             * the jobid is in a special format - so get it */
            opal_convert_string_to_jobid(&jobid, nspace);
        } else {
            /* we were launched by someone else, so make the
             * jobid just be the hash of the nspace */
            OPAL_HASH_JOBID(nspace, jobid);
        }
        /* add this to our jobid tracker */
        job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
        (void)strncpy(job->nspace, nspace, PMIX_MAX_NSLEN);
        job->jobid = jobid;
        opal_list_append(&mca_pmix_ext2x_component.jobids, &job->super);
    }

    op->spcbfunc(rc, jobid, op->cbdata);
    OBJ_RELEASE(op);
}

int pmix2x_spawnnb(opal_list_t *job_info, opal_list_t *apps,
                   opal_pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    pmix2x_opcaddy_t *op;
    size_t n, m;
    opal_value_t *info;
    opal_pmix_app_t *app;

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->spcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != job_info && 0 < (op->ninfo = opal_list_get_size(job_info))) {
        PMIX_INFO_CREATE(op->info, op->ninfo);
        n=0;
        OPAL_LIST_FOREACH(info, job_info, opal_value_t) {
            (void)strncpy(op->info[n].key, info->key, PMIX_MAX_KEYLEN);
            pmix2x_value_load(&op->info[n].value, info);
            ++n;
        }
    }

    op->sz = opal_list_get_size(apps);
    PMIX_APP_CREATE(op->apps, op->sz);
    n=0;
    OPAL_LIST_FOREACH(app, apps, opal_pmix_app_t) {
        op->apps[n].cmd = strdup(app->cmd);
        op->apps[n].argv = opal_argv_copy(app->argv);
        op->apps[n].env = opal_argv_copy(app->env);
        op->apps[n].maxprocs = app->maxprocs;
        if (0 < (op->apps[n].ninfo = opal_list_get_size(&app->info))) {
            PMIX_INFO_CREATE(op->apps[n].info, op->apps[n].ninfo);
            m=0;
            OPAL_LIST_FOREACH(info, &app->info, opal_value_t) {
                (void)strncpy(op->apps[n].info[m].key, info->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&op->apps[n].info[m].value, info);
                ++m;
            }
        }
        ++n;
    }

    ret = PMIx_Spawn_nb(op->info, op->ninfo, op->apps, op->sz, spcbfunc, op);

    return pmix2x_convert_rc(ret);
}

int pmix2x_connect(opal_list_t *procs)
{
    pmix_status_t ret;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(parray, cnt);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (jptr->jobid == ptr->name.jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
            PMIX_PROC_FREE(parray, cnt);
            return OPAL_ERR_NOT_FOUND;
        }
        (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
        parray[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
        ++n;
    }

    ret = PMIx_Connect(parray, cnt, NULL, 0);
    PMIX_PROC_FREE(parray, cnt);

    return pmix2x_convert_rc(ret);
}

int pmix2x_connectnb(opal_list_t *procs,
                     opal_pmix_op_cbfunc_t cbfunc,
                     void *cbdata)
{
    pmix_status_t ret;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix2x_opcaddy_t *op;
    opal_pmix2x_jobid_trkr_t *job;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->nprocs = cnt;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(op->procs, op->nprocs);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        OPAL_LIST_FOREACH(job, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (job->jobid == ptr->name.jobid) {
                (void)strncpy(op->procs[n].nspace, job->nspace, PMIX_MAX_NSLEN);
                break;
            }
        }
        op->procs[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
        ++n;
    }

    ret = PMIx_Connect_nb(op->procs, op->nprocs, NULL, 0, opcbfunc, op);

    return pmix2x_convert_rc(ret);
}

int pmix2x_disconnect(opal_list_t *procs)
{
    pmix_status_t ret;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    opal_pmix2x_jobid_trkr_t *job;

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(parray, cnt);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        OPAL_LIST_FOREACH(job, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (job->jobid == ptr->name.jobid) {
                (void)strncpy(parray[n].nspace, job->nspace, PMIX_MAX_NSLEN);
                break;
            }
        }
        parray[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
        ++n;
    }

    ret = PMIx_Disconnect(parray, cnt, NULL, 0);
    PMIX_PROC_FREE(parray, cnt);

    return pmix2x_convert_rc(ret);
}

int pmix2x_disconnectnb(opal_list_t *procs,
                        opal_pmix_op_cbfunc_t cbfunc,
                        void *cbdata)
{
    pmix_status_t ret;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix2x_opcaddy_t *op;
    opal_pmix2x_jobid_trkr_t *job;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix2x_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->nprocs = cnt;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(op->procs, op->nprocs);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        OPAL_LIST_FOREACH(job, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (job->jobid == ptr->name.jobid) {
                (void)strncpy(op->procs[n].nspace, job->nspace, PMIX_MAX_NSLEN);
                break;
            }
        }
        op->procs[n].rank = pmix2x_convert_opalrank(ptr->name.vpid);
        ++n;
    }

    ret = PMIx_Disconnect_nb(op->procs, op->nprocs, NULL, 0, opcbfunc, op);

    return pmix2x_convert_rc(ret);
}


int pmix2x_resolve_peers(const char *nodename, opal_jobid_t jobid,
                         opal_list_t *procs)
{
    char *nspace;
    pmix_proc_t *array=NULL;
    size_t nprocs, n;
    opal_namelist_t *nm;
    int rc;
    pmix_status_t ret;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */

    if (OPAL_JOBID_WILDCARD == jobid) {
        nspace = NULL;
    } else {
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (jptr->jobid == jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            return OPAL_ERR_NOT_FOUND;
        }
        nspace = job->nspace;
    }

    ret = PMIx_Resolve_peers(nodename, nspace, &array, &nprocs);
    rc = pmix2x_convert_rc(ret);

    if (NULL != array && 0 < nprocs) {
        for (n=0; n < nprocs; n++) {
            nm = OBJ_NEW(opal_namelist_t);
            opal_list_append(procs, &nm->super);
            if (mca_pmix_ext2x_component.native_launch) {
                /* if we were launched by the OMPI RTE, then
                 * the jobid is in a special format - so get it */
                opal_convert_string_to_jobid(&nm->name.jobid, array[n].nspace);
            } else {
                /* we were launched by someone else, so make the
                 * jobid just be the hash of the nspace */
                OPAL_HASH_JOBID(array[n].nspace, nm->name.jobid);
            }
            /* if we don't already have it, add this to our jobid tracker */
            job = NULL;
            OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
                if (jptr->jobid == nm->name.jobid) {
                    job = jptr;
                    break;
                }
            }
            if (NULL == job) {
                job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
                (void)strncpy(job->nspace, nspace, PMIX_MAX_NSLEN);
                job->jobid = jobid;
                opal_list_append(&mca_pmix_ext2x_component.jobids, &job->super);
            }
            nm->name.vpid = pmix2x_convert_rank(array[n].rank);
        }
    }
    PMIX_PROC_FREE(array, nprocs);

    return rc;
}

int pmix2x_resolve_nodes(opal_jobid_t jobid, char **nodelist)
{
    pmix_status_t ret;
    char *nspace=NULL;
    opal_pmix2x_jobid_trkr_t *job, *jptr;

    /* we must threadshift this request as we might not be in an event
     * and we are going to access framework-global lists/objects */

    if (OPAL_JOBID_WILDCARD != jobid) {
        /* look thru our list of jobids and find the
         * corresponding nspace */
        job = NULL;
        OPAL_LIST_FOREACH(jptr, &mca_pmix_ext2x_component.jobids, opal_pmix2x_jobid_trkr_t) {
            if (jptr->jobid == jobid) {
                job = jptr;
                break;
            }
        }
        if (NULL == job) {
            return OPAL_ERR_NOT_FOUND;
        }
        nspace = job->nspace;
    }

    ret = PMIx_Resolve_nodes(nspace, nodelist);

    return pmix2x_convert_rc(ret);;
}
