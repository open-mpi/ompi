/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies Ltd. All rights reserved.
 * Copyright (c) 2017-2020 IBM Corporation.  All rights reserved.
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

#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif
#include <pmix.h>
#include <pmix_server.h>
#include <signal.h>
#include <time.h>

#include "prte_stdint.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_environ.h"
#include "src/util/sys_limits.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/base/base.h"
#include "src/mca/filem/filem.h"
#include "src/mca/grpcomm/base/base.h"
#include "src/mca/iof/base/iof_base_setup.h"
#include "src/mca/iof/iof.h"
#include "src/mca/plm/base/base.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/base/rmaps_private.h"
#include "src/mca/rmaps/rmaps_types.h"
#include "src/rml/rml_contact.h"
#include "src/rml/rml.h"
#include "src/mca/rtc/rtc.h"
#include "src/mca/schizo/base/base.h"
#include "src/mca/state/state.h"

#include "src/prted/pmix/pmix_server.h"
#include "src/prted/prted.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_wait.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_context_fns.h"
#include "src/util/name_fns.h"
#include "src/util/nidmap.h"
#include "src/util/proc_info.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/odls/base/base.h"

typedef struct {
    prte_job_t *jdata;
    pmix_info_t *info;
    size_t ninfo;
    prte_pmix_lock_t lock;
} prte_odls_jcaddy_t;

static void setup_cbfunc(pmix_status_t status, pmix_info_t info[], size_t ninfo,
                         void *provided_cbdata, pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    prte_odls_jcaddy_t *cd = (prte_odls_jcaddy_t *) provided_cbdata;
    prte_job_t *jdata = cd->jdata;
    pmix_data_buffer_t pbuf;
    pmix_byte_object_t pbo;
    int rc = PRTE_SUCCESS;
    PRTE_HIDE_UNUSED_PARAMS(status);

    /* release any info */
    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }

    PMIX_BYTE_OBJECT_CONSTRUCT(&pbo);
    if (NULL != info) {
        PMIX_DATA_BUFFER_CONSTRUCT(&pbuf);
        /* pack the provided info */
        if (PMIX_SUCCESS != (rc = PMIx_Data_pack(NULL, &pbuf, &ninfo, 1, PMIX_SIZE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
            goto done;
        }
        if (PMIX_SUCCESS != (rc = PMIx_Data_pack(NULL, &pbuf, info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
            goto done;
        }
        /* unload it */
        rc = PMIx_Data_unload(&pbuf, &pbo);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }
    /* add the results */
    rc = PMIx_Data_pack(NULL, &jdata->launch_msg, &pbo, 1, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }

done:
    PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
    /* release our caller */
    if (NULL != cbfunc) {
        cbfunc(rc, cbdata);
    }

    /* move to next stage */
    PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_SEND_LAUNCH_MSG);

    /* release the original thread */
    PRTE_PMIX_WAKEUP_THREAD(&cd->lock);
}

/* IT IS CRITICAL THAT ANY CHANGE IN THE ORDER OF THE INFO PACKED IN
 * THIS FUNCTION BE REFLECTED IN THE CONSTRUCT_CHILD_LIST PARSER BELOW
 */
int prte_odls_base_default_get_add_procs_data(pmix_data_buffer_t *buffer, pmix_nspace_t job)
{
    int rc, n;
    prte_job_t *jdata = NULL, *jptr;
    prte_job_map_t *map = NULL;
    pmix_data_buffer_t jobdata, priorjob;
    int8_t flag;
    prte_proc_t *proc;
    pmix_status_t ret;
    prte_node_t *node;
    int i, k;
    char **list, **procs, **micro, *tmp, *regex;
    prte_odls_jcaddy_t cd = {0};
    prte_proc_t *pptr;
    uint32_t uid;
    uint32_t gid;
    pmix_byte_object_t pbo;
    void *ilist, *mlist;
    pmix_data_array_t darray;

    /* get the job data pointer */
    if (NULL == (jdata = prte_get_job_data_object(job))) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }

    /* get a pointer to the job map */
    map = jdata->map;
    /* if there is no map, just return */
    if (NULL == map) {
        return PRTE_SUCCESS;
    }

    /* we need to ensure that any new daemons get a complete
     * copy of all active jobs so the grpcomm collectives can
     * properly work should a proc from one of the other jobs
     * interact with this one */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_LAUNCHED_DAEMONS, NULL, PMIX_BOOL)) {
        flag = 1;
        rc = PMIx_Data_pack(NULL, buffer, &flag, 1, PMIX_INT8);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        PMIX_DATA_BUFFER_CONSTRUCT(&jobdata);
        for (i = 1; i < prte_job_data->size; i++) {
            jptr = pmix_pointer_array_get_item(prte_job_data, i);
            if (NULL == jptr) {
                continue;
            }
            /* skip the one we are launching now */
            if (jptr != jdata) {
                PMIX_DATA_BUFFER_CONSTRUCT(&priorjob);
                /* pack the job struct */
                rc = prte_job_pack(&priorjob, jptr);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DATA_BUFFER_DESTRUCT(&jobdata);
                    PMIX_DATA_BUFFER_DESTRUCT(&priorjob);
                    return rc;
                }
                /* pack the location of each proc */
                for (n = 0; n < jptr->procs->size; n++) {
                    if (NULL
                        == (proc = (prte_proc_t *) pmix_pointer_array_get_item(jptr->procs, n))) {
                        continue;
                    }
                    rc = PMIx_Data_pack(NULL, &priorjob, &proc->parent, 1, PMIX_PROC_RANK);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DATA_BUFFER_DESTRUCT(&jobdata);
                        PMIX_DATA_BUFFER_DESTRUCT(&priorjob);
                        return rc;
                    }
                }
                /* unload the buffer */
                rc = PMIx_Data_unload(&priorjob, &pbo);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DATA_BUFFER_DESTRUCT(&priorjob);
                    PMIX_DATA_BUFFER_DESTRUCT(&jobdata);
                    return rc;
                }
                /* add it to the jobdata buffer */
                rc = PMIx_Data_pack(NULL, &jobdata, &pbo, 1, PMIX_BYTE_OBJECT);
                PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DATA_BUFFER_DESTRUCT(&jobdata);
                    return rc;
                }
            }
        }
        /* unload the buffer */
        rc = PMIx_Data_unload(&jobdata, &pbo);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_DESTRUCT(&jobdata);
            return rc;
        }
        /* add it to the message */
        rc = PMIx_Data_pack(NULL, buffer, &pbo, 1, PMIX_BYTE_OBJECT);
        PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    } else {
        flag = 0;
        rc = PMIx_Data_pack(NULL, buffer, &flag, 1, PMIX_INT8);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    }

    /* pack the job struct */
    rc = prte_job_pack(buffer, jdata);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* assemble the node and proc map info */
    list = NULL;
    procs = NULL;
    PMIX_INFO_LIST_START(ilist);
    for (i = 0; i < map->nodes->size; i++) {
        micro = NULL;
        if (NULL != (node = (prte_node_t *) pmix_pointer_array_get_item(map->nodes, i))) {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&list, node->name);
            /* assemble all the ranks for this job that are on this node */
            for (k = 0; k < node->procs->size; k++) {
                if (NULL != (pptr = (prte_proc_t *) pmix_pointer_array_get_item(node->procs, k))) {
                    if (PMIX_CHECK_NSPACE(jdata->nspace, pptr->name.nspace)) {
                        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&micro, PRTE_VPID_PRINT(pptr->name.rank));
                    }
                }
            }
            /* assemble the rank/node map */
            if (NULL != micro) {
                tmp = PMIX_ARGV_JOIN_COMPAT(micro, ',');
                PMIX_ARGV_FREE_COMPAT(micro);
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&procs, tmp);
                free(tmp);
            }
        }
    }

    /* let the PMIx server generate the nodemap regex */
    if (NULL != list) {
        tmp = PMIX_ARGV_JOIN_COMPAT(list, ',');
        PMIX_ARGV_FREE_COMPAT(list);
        list = NULL;
        if (PMIX_SUCCESS != (ret = PMIx_generate_regex(tmp, &regex))) {
            PMIX_ERROR_LOG(ret);
            free(tmp);
            PMIX_INFO_FREE(cd.info, cd.ninfo);
            return prte_pmix_convert_status(ret);
        }
        free(tmp);
        PMIX_INFO_LIST_ADD(ret, ilist, PMIX_NODE_MAP, regex, PMIX_REGEX);
        free(regex);
    }

    /* let the PMIx server generate the procmap regex */
    if (NULL != procs) {
        tmp = PMIX_ARGV_JOIN_COMPAT(procs, ';');
        PMIX_ARGV_FREE_COMPAT(procs);
        procs = NULL;
        if (PMIX_SUCCESS != (ret = PMIx_generate_ppn(tmp, &regex))) {
            PMIX_ERROR_LOG(ret);
            free(tmp);
            PMIX_INFO_FREE(cd.info, cd.ninfo);
            return prte_pmix_convert_status(ret);
        }
        free(tmp);
        PMIX_INFO_LIST_ADD(ret, ilist, PMIX_PROC_MAP, regex, PMIX_REGEX);
        free(regex);
    }

    /* add in the personality */
    if (NULL != jdata->personality) {
        tmp = PMIX_ARGV_JOIN_COMPAT(jdata->personality, ',');
        PMIX_INFO_LIST_ADD(ret, ilist, PMIX_PERSONALITY, tmp, PMIX_STRING);
        free(tmp);
    }

    /* construct the actual request - we just let them pick the
     * default transport for now. Someday, we will add to prun
     * the ability for transport specifications */
    PMIX_INFO_LIST_START(mlist);

    asprintf(&tmp, "%s.net", jdata->nspace);
    PMIX_INFO_LIST_ADD(ret, mlist, PMIX_ALLOC_NETWORK_ID, tmp, PMIX_STRING);
    free(tmp);
    PMIX_INFO_LIST_ADD(ret, mlist, PMIX_ALLOC_NETWORK_SEC_KEY, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_CONVERT(ret, mlist, &darray);
    PMIX_INFO_LIST_ADD(ret, ilist, PMIX_ALLOC_NETWORK, &darray, PMIX_DATA_ARRAY);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    PMIX_INFO_LIST_RELEASE(mlist);

    /* add in the user's uid and gid */
    uid = geteuid();
    PMIX_INFO_LIST_ADD(ret, ilist, PMIX_USERID, &uid, PMIX_UINT32);
    gid = getegid();
    PMIX_INFO_LIST_ADD(ret, ilist, PMIX_GRPID, &gid, PMIX_UINT32);

    /* if they haven't harvested envars, do so now */
    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_ENVARS_HARVESTED, NULL, PMIX_BOOL)) {
        PMIX_INFO_LIST_ADD(ret, ilist, PMIX_SETUP_APP_ENVARS, NULL, PMIX_BOOL);
    }
    /* convert the job info into an array */
    PMIX_INFO_LIST_CONVERT(ret, ilist, &darray);
    cd.info = (pmix_info_t *) darray.array;
    cd.ninfo = darray.size;
    PMIX_INFO_LIST_RELEASE(ilist);

    /* we don't want to block here because it could
     * take some indeterminate time to get the info */
    rc = PRTE_SUCCESS;
    cd.jdata = jdata;
    PRTE_PMIX_CONSTRUCT_LOCK(&cd.lock);
    ret = PMIx_server_setup_application(jdata->nspace, cd.info, cd.ninfo,
                                        setup_cbfunc, &cd);
    if (PMIX_SUCCESS != ret) {
        pmix_output(0, "[%s:%d] PMIx_server_setup_application failed: %s", __FILE__, __LINE__,
                    PMIx_Error_string(ret));
        rc = PRTE_ERROR;
    } else {
        PRTE_PMIX_WAIT_THREAD(&cd.lock);
    }
    PRTE_PMIX_DESTRUCT_LOCK(&cd.lock);
    return rc;
}

static void ls_cbunc(pmix_status_t status, void *cbdata)
{
    prte_pmix_lock_t *lock = (prte_pmix_lock_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(status);
    PRTE_PMIX_WAKEUP_THREAD(lock);
}

int prte_odls_base_default_construct_child_list(pmix_data_buffer_t *buffer, pmix_nspace_t *job)
{
    int rc;
    int32_t cnt;
    prte_job_t *jdata = NULL, *daemons;
    prte_node_t *node;
    pmix_rank_t dmnvpid, v;
    int32_t n;
    pmix_data_buffer_t dbuf, jdbuf;
    prte_proc_t *pptr, *dmn;
    prte_app_context_t *app;
    int8_t flag;
    prte_pmix_lock_t lock;
    pmix_info_t *info = NULL;
    size_t ninfo = 0;
    pmix_status_t ret;
    pmix_data_buffer_t pbuf;
    pmix_byte_object_t bo, pbo;
    size_t m;
    pmix_envar_t envt;
    char *tmp;

    PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                         "%s odls:constructing child list", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    /* set a default response */
    PMIX_LOAD_NSPACE(*job, NULL);

    /* get the daemon job object */
    daemons = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    PRTE_PMIX_CONSTRUCT_LOCK(&lock);

    /* unpack the flag to see if new daemons were launched */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &flag, &cnt, PMIX_INT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        rc = prte_pmix_convert_status(rc);
        goto REPORT_ERROR;
    }

    if (0 != flag) {
        /* unpack the buffer containing the info */
        cnt = 1;
        rc = PMIx_Data_unpack(NULL, buffer, &bo, &cnt, PMIX_BYTE_OBJECT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            rc = prte_pmix_convert_status(rc);
            goto REPORT_ERROR;
        }
        if (PRTE_PROC_IS_MASTER) {
            /* we don't need this */
            PMIX_BYTE_OBJECT_DESTRUCT(&bo);
            goto next;
        }
        PMIX_DATA_BUFFER_CONSTRUCT(&dbuf);
        rc = PMIx_Data_load(&dbuf, &bo);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            rc = prte_pmix_convert_status(rc);
            goto REPORT_ERROR;
        }
        cnt = 1;
        rc = PMIx_Data_unpack(NULL, &dbuf, &pbo, &cnt, PMIX_BYTE_OBJECT);
        while (PMIX_SUCCESS == rc) {
            PMIX_DATA_BUFFER_CONSTRUCT(&jdbuf);
            rc = PMIx_Data_load(&jdbuf, &pbo);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                rc = prte_pmix_convert_status(rc);
                goto REPORT_ERROR;
            }
            /* unpack each job and add it to the local prte_job_data array */
            cnt = 1;
            rc = prte_job_unpack(&jdbuf, &jdata);
            if (PRTE_SUCCESS != rc) {
                PRTE_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_DESTRUCT(&dbuf);
                PMIX_DATA_BUFFER_DESTRUCT(&jdbuf);
                goto REPORT_ERROR;
            }
            /* check to see if we already have this one */
            if (NULL != prte_get_job_data_object(jdata->nspace)) {
                /* yep - so we can drop this copy */
                jdata->index = -1;
                PMIX_RELEASE(jdata);
            } else {
                /* nope - add it */
                prte_set_job_data_object(jdata);
                /* unpack the location of each proc in this job */
                for (v = 0; v < jdata->num_procs; v++) {
                    if (NULL
                        == (pptr = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, v))) {
                        pptr = PMIX_NEW(prte_proc_t);
                        PMIX_LOAD_PROCID(&pptr->name, jdata->nspace, v);
                        pmix_pointer_array_set_item(jdata->procs, v, pptr);
                    }
                    cnt = 1;
                    rc = PMIx_Data_unpack(NULL, &jdbuf, &dmnvpid, &cnt, PMIX_PROC_RANK);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DATA_BUFFER_DESTRUCT(&dbuf);
                        PMIX_DATA_BUFFER_DESTRUCT(&jdbuf);
                        goto REPORT_ERROR;
                    }
                    /* lookup the daemon */
                    if (NULL
                        == (dmn = (prte_proc_t *) pmix_pointer_array_get_item(daemons->procs,
                                                                              dmnvpid))) {
                        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
                        rc = PRTE_ERR_NOT_FOUND;
                        PMIX_DATA_BUFFER_DESTRUCT(&dbuf);
                        PMIX_DATA_BUFFER_DESTRUCT(&jdbuf);
                        goto REPORT_ERROR;
                    }
                    /* connect the two */
                    PMIX_RETAIN(dmn->node);
                    pptr->node = dmn->node;

                    /* add the node to the job map, if needed */
                    if (!PRTE_FLAG_TEST(pptr->node, PRTE_NODE_FLAG_MAPPED)) {
                        PMIX_RETAIN(pptr->node);
                        pmix_pointer_array_add(jdata->map->nodes, pptr->node);
                        jdata->map->num_nodes++;
                        PRTE_FLAG_SET(pptr->node, PRTE_NODE_FLAG_MAPPED);
                    }
                    /* add this proc to that node */
                    PMIX_RETAIN(pptr);
                    pmix_pointer_array_add(pptr->node->procs, pptr);
                    pptr->node->num_procs++;
                }
                /* reset the mapped flags */
                for (n = 0; n < jdata->map->nodes->size; n++) {
                    if (NULL
                        != (node = (prte_node_t *) pmix_pointer_array_get_item(jdata->map->nodes, n))) {
                        PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_MAPPED);
                    }

                }
            }
            /* release the buffer */
            PMIX_DATA_BUFFER_DESTRUCT(&jdbuf);
            cnt = 1;
            rc = PMIx_Data_unpack(NULL, &dbuf, &pbo, &cnt, PMIX_BYTE_OBJECT);
        }
        PMIX_DATA_BUFFER_DESTRUCT(&dbuf);
        if (PMIX_SUCCESS != rc && PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            PMIX_ERROR_LOG(rc);
            rc = prte_pmix_convert_status(rc);
            goto REPORT_ERROR;
        }
    }

next:
    /* unpack the job we are to launch */
    rc = prte_job_unpack(buffer, &jdata);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    if (PMIX_NSPACE_INVALID(jdata->nspace)) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        rc = PRTE_ERR_BAD_PARAM;
        goto REPORT_ERROR;
    }
    PMIX_LOAD_NSPACE(*job, jdata->nspace);

    PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                         "%s odls:construct_child_list unpacking data to launch job %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(*job)));

    /* if we are the HNP, we don't need to unpack this buffer - we already
     * have all the required info in our local job array. So just build the
     * array of local children
     */
    if (PRTE_PROC_IS_MASTER) {
        /* we don't want/need the extra copy of the prte_job_t, but
         * we can't just release it as that will NULL the location in
         * the prte_job_data array. So set its index to -1 to
         * protect the array, and then release the object to free
         * the storage */
        jdata->index = -1;
        PMIX_RELEASE(jdata);
        /* get the correct job object - it will be completely filled out */
        if (NULL == (jdata = prte_get_job_data_object(*job))) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            rc = PRTE_ERR_NOT_FOUND;
            goto REPORT_ERROR;
        }
        if (NULL == jdata->schizo) {
            pmix_show_help("help-schizo-base.txt", "no-proxy", true,
                           prte_tool_basename, "NULL");
            return 1;
        }
    } else {
        prte_set_job_data_object(jdata);

        /* ensure the map object is present */
        if (NULL == jdata->map) {
            jdata->map = PMIX_NEW(prte_job_map_t);
        }
        /* get the associated schizo module */
        if (NULL != jdata->personality) {
            tmp = PMIX_ARGV_JOIN_COMPAT(jdata->personality, ',');
        } else {
            tmp = NULL;
        }
        jdata->schizo = (struct prte_schizo_base_module_t*)prte_schizo_base_detect_proxy(tmp);
        if (NULL == jdata->schizo) {
            pmix_show_help("help-schizo-base.txt", "no-proxy", true,
                           prte_tool_basename, (NULL == tmp) ? "NULL" : tmp);
            if (NULL != tmp) {
                free(tmp);
            }
            return 1;
        }
        if (NULL != tmp) {
            free(tmp);
        }
    }

    /* unpack the byte object containing any application setup info - there
     * might not be any, so it isn't an error if we don't find things */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &bo, &cnt, PMIX_BYTE_OBJECT);
    if (PRTE_SUCCESS == rc && 0 < bo.size) {
        /* there was setup data - process it */
        PMIX_DATA_BUFFER_CONSTRUCT(&pbuf);
        rc = PMIx_Data_load(&pbuf, &bo);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto REPORT_ERROR;
        }
        PMIX_BYTE_OBJECT_DESTRUCT(&bo);
        /* unpack the number of info structs */
        cnt = 1;
        ret = PMIx_Data_unpack(NULL, &pbuf, &ninfo, &cnt, PMIX_SIZE);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
            rc = PRTE_ERROR;
            goto REPORT_ERROR;
        }
        PMIX_INFO_CREATE(info, ninfo);
        cnt = ninfo;
        ret = PMIx_Data_unpack(NULL, &pbuf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_INFO_FREE(info, ninfo);
            PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
            rc = PRTE_ERROR;
            goto REPORT_ERROR;
        }
        PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
        /* add any cache'd values to the front of the job attributes  */
        for (m = 0; m < ninfo; m++) {
            if (0 == strcmp(info[m].key, PMIX_SET_ENVAR)) {
                envt.envar = strdup(info[m].value.data.envar.envar);
                envt.value = strdup(info[m].value.data.envar.value);
                envt.separator = info[m].value.data.envar.separator;
                prte_prepend_attribute(&jdata->attributes, PRTE_JOB_SET_ENVAR, PRTE_ATTR_GLOBAL,
                                       &envt, PMIX_ENVAR);
            } else if (0 == strcmp(info[m].key, PMIX_ADD_ENVAR)) {
                envt.envar = info[m].value.data.envar.envar;
                envt.value = info[m].value.data.envar.value;
                envt.separator = info[m].value.data.envar.separator;
                prte_prepend_attribute(&jdata->attributes, PRTE_JOB_ADD_ENVAR, PRTE_ATTR_GLOBAL,
                                       &envt, PMIX_ENVAR);
            } else if (0 == strcmp(info[m].key, PMIX_UNSET_ENVAR)) {
                prte_prepend_attribute(&jdata->attributes, PRTE_JOB_UNSET_ENVAR, PRTE_ATTR_GLOBAL,
                                       info[m].value.data.string, PMIX_STRING);
            } else if (0 == strcmp(info[m].key, PMIX_PREPEND_ENVAR)) {
                envt.envar = info[m].value.data.envar.envar;
                envt.value = info[m].value.data.envar.value;
                envt.separator = info[m].value.data.envar.separator;
                prte_prepend_attribute(&jdata->attributes, PRTE_JOB_PREPEND_ENVAR, PRTE_ATTR_GLOBAL,
                                       &envt, PMIX_ENVAR);
            } else if (0 == strcmp(info[m].key, PMIX_APPEND_ENVAR)) {
                envt.envar = info[m].value.data.envar.envar;
                envt.value = info[m].value.data.envar.value;
                envt.separator = info[m].value.data.envar.separator;
                prte_prepend_attribute(&jdata->attributes, PRTE_JOB_APPEND_ENVAR, PRTE_ATTR_GLOBAL,
                                       &envt, PMIX_ENVAR);
            }
        }
    }

    /* now that the node array in the job map and jdata are completely filled out,.
     * we need to "wireup" the procs to their nodes so other utilities can
     * locate them */
    for (n = 0; n < jdata->procs->size; n++) {
        if (NULL == (pptr = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, n))) {
            continue;
        }
        if (PRTE_PROC_STATE_UNDEF == pptr->state) {
            /* not ready for use yet */
            continue;
        }
        if (!PRTE_PROC_IS_MASTER) {
            /* connect the proc to its node here */
            pmix_output_verbose(5, prte_odls_base_framework.framework_output,
                                "%s GETTING DAEMON FOR PROC %s WITH PARENT %s",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&pptr->name),
                                PRTE_VPID_PRINT(pptr->parent));
            if (PMIX_RANK_INVALID == pptr->parent) {
                PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
                rc = PRTE_ERR_BAD_PARAM;
                goto REPORT_ERROR;
            }
            /* connect the proc to its node object */
            if (NULL
                == (dmn = (prte_proc_t *) pmix_pointer_array_get_item(daemons->procs,
                                                                      pptr->parent))) {
                PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
                rc = PRTE_ERR_NOT_FOUND;
                goto REPORT_ERROR;
            }
            PMIX_RETAIN(dmn->node);
            pptr->node = dmn->node;
            /* add the node to the job map, if needed */
            if (!PRTE_FLAG_TEST(pptr->node, PRTE_NODE_FLAG_MAPPED)) {
                PMIX_RETAIN(pptr->node);
                pmix_pointer_array_add(jdata->map->nodes, pptr->node);
                jdata->map->num_nodes++;
                PRTE_FLAG_SET(pptr->node, PRTE_NODE_FLAG_MAPPED);
            }
            /* add this proc to that node */
            PMIX_RETAIN(pptr);
            pmix_pointer_array_add(pptr->node->procs, pptr);
            pptr->node->num_procs++;
        }
        /* see if it belongs to us */
        if (pptr->parent == PRTE_PROC_MY_NAME->rank) {
            /* is this child on our current list of children */
            if (!PRTE_FLAG_TEST(pptr, PRTE_PROC_FLAG_LOCAL)) {
                /* not on the local list */
                PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                     "%s[%s:%d] adding proc %s to my local list",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__,
                                     PRTE_NAME_PRINT(&pptr->name)));
                /* keep tabs of the number of local procs */
                jdata->num_local_procs++;
                /* add this proc to our child list */
                PMIX_RETAIN(pptr);
                PRTE_FLAG_SET(pptr, PRTE_PROC_FLAG_LOCAL);
                pmix_pointer_array_add(prte_local_children, pptr);
            }

            /* if the job is in restart mode, the child must not barrier when launched */
            if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_RESTART)) {
                prte_set_attribute(&pptr->attributes, PRTE_PROC_NOBARRIER, PRTE_ATTR_LOCAL, NULL,
                                   PMIX_BOOL);
            }
            /* mark that this app_context is being used on this node */
            app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, pptr->app_idx);
            PRTE_FLAG_SET(app, PRTE_APP_FLAG_USED_ON_NODE);
        }
    }

    /* reset the mapped flags */
    for (n = 0; n < jdata->map->nodes->size; n++) {
        if (NULL
            != (node = (prte_node_t *) pmix_pointer_array_get_item(jdata->map->nodes, n))) {
            PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_MAPPED);
        }
    }

    /* register this job with the PMIx server - need to wait until after we
     * have computed the #local_procs before calling the function */
    if (PRTE_SUCCESS != (rc = prte_pmix_server_register_nspace(jdata))) {
        PRTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }

    /* if we have local support setup info, then execute it here - we
     * have to do so AFTER we register the nspace so the PMIx server
     * has the nspace info it needs */
    if (0 < ninfo) {
        if (PMIX_SUCCESS
            != (ret = PMIx_server_setup_local_support(jdata->nspace, info, ninfo, ls_cbunc,
                                                      &lock))) {
            PMIX_ERROR_LOG(ret);
            rc = PRTE_ERROR;
            goto REPORT_ERROR;
        }
    } else {
        lock.active = false; // we won't get a callback
    }

    /* load any controls into the job */
    prte_rtc.assign(jdata);

    /* spin up the spawn threads */
    prte_odls_base_start_threads(jdata);

    /* to save memory, purge the job map of all procs other than
     * our own - for daemons, this will completely release the
     * proc structures. For the HNP, the proc structs will
     * remain in the prte_job_t array */

    /* wait here until the local support has been setup */
    PRTE_PMIX_WAIT_THREAD(&lock);
    PRTE_PMIX_DESTRUCT_LOCK(&lock);
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    return PRTE_SUCCESS;

REPORT_ERROR:
    PRTE_PMIX_DESTRUCT_LOCK(&lock);
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    /* we have to report an error back to the HNP so we don't just
     * hang. Although there shouldn't be any errors once this is
     * all debugged, it is still good practice to have a way
     * for it to happen - especially so developers don't have to
     * deal with the hang!
     */
    PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_NEVER_LAUNCHED);
    return rc;
}

static int setup_path(prte_app_context_t *app, char **wdir)
{
    int rc = PRTE_SUCCESS;
    char dir[MAXPATHLEN];
    char *session_dir;
    bool usercwd = false;
    prte_job_t *job;

    if (prte_get_attribute(&app->attributes, PRTE_APP_SSNDIR_CWD, NULL, PMIX_BOOL)) {
        /* move us to that location */
        job = (prte_job_t*)app->job;
        session_dir = job->session_dir;
        if (NULL == session_dir) {
            // cannot do it
            return PRTE_ERROR;
        }
        if (0 != chdir(session_dir)) {
            return PRTE_ERROR;
        }
        /* NOTE: if a user's program does a chdir(), then $PWD will once
         * again not match getcwd! This is beyond our control - we are only
         * ensuring they start out matching.
         */
        if (NULL == getcwd(dir, sizeof(dir))) {
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        *wdir = strdup(dir);
        PMIX_SETENV_COMPAT("PWD", dir, true, &app->env);
    } else {
        /* Try to change to the app's cwd and check that the app
           exists and is executable The function will
           take care of outputting a pretty error message, if required
        */
        if (prte_get_attribute(&app->attributes, PRTE_APP_USER_CWD, NULL, PMIX_BOOL)) {
            usercwd = true;
        }
        rc = pmix_util_check_context_cwd(&app->cwd, true, usercwd);
        if (PMIX_SUCCESS != rc) {
            /* do not ERROR_LOG - it will be reported elsewhere */
            goto CLEANUP;
        }

        /* The prior function will have done a chdir() to jump us to
         * wherever the app is to be executed. This could be either where
         * the user specified (via -wdir), or to the user's home directory
         * on this node if nothing was provided. It seems that chdir doesn't
         * adjust the $PWD enviro variable when it changes the directory. This
         * can cause a user to get a different response when doing getcwd vs
         * looking at the enviro variable. To keep this consistent, we explicitly
         * ensure that the PWD enviro variable matches the CWD we moved to.
         *
         * NOTE: if a user's program does a chdir(), then $PWD will once
         * again not match getcwd! This is beyond our control - we are only
         * ensuring they start out matching.
         */
        if (NULL == getcwd(dir, sizeof(dir))) {
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        *wdir = strdup(dir);
        PMIX_SETENV_COMPAT("PWD", dir, true, &app->env);
    }

CLEANUP:
    return rc;
}

/* define a timer release point so that we can wait for
 * file descriptors to come available, if necessary
 */
static void timer_cb(int fd, short event, void *cbdata)
{
    prte_timer_t *tm = (prte_timer_t *) cbdata;
    prte_odls_launch_local_t *ll = (prte_odls_launch_local_t *) tm->payload;
    PRTE_HIDE_UNUSED_PARAMS(fd, event);

    PMIX_ACQUIRE_OBJECT(tm);

    /* increment the number of retries */
    ll->retries++;

    /* re-attempt the launch */
    prte_event_active(ll->ev, PRTE_EV_WRITE, 1);

    /* release the timer event */
    PMIX_RELEASE(tm);
}

static int compute_num_procs_alive(pmix_nspace_t job)
{
    int i;
    prte_proc_t *child;
    int num_procs_alive = 0;

    for (i = 0; i < prte_local_children->size; i++) {
        if (NULL == (child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i))) {
            continue;
        }
        if (!PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_ALIVE)) {
            continue;
        }
        /* do not include members of the specified job as they
         * will be added later, if required
         */
        if (PMIX_CHECK_NSPACE(job, child->name.nspace)) {
            continue;
        }
        num_procs_alive++;
    }
    return num_procs_alive;
}

void prte_odls_base_spawn_proc(int fd, short sd, void *cbdata)
{
    prte_odls_spawn_caddy_t *cd = (prte_odls_spawn_caddy_t *) cbdata;
    prte_job_t *jobdat = cd->jdata;
    prte_app_context_t *app = cd->app;
    prte_proc_t *child = cd->child;
    int rc, i;
    bool found;
    prte_proc_state_t state;
    pmix_proc_t pproc;
    pmix_status_t ret;
    char *ptr;
    pmix_value_t pidval = PMIX_VALUE_STATIC_INIT;

    PRTE_HIDE_UNUSED_PARAMS(fd, sd);

    PMIX_ACQUIRE_OBJECT(cd);

    /* thread-protect common values */
    cd->env = PMIX_ARGV_COPY_COMPAT(prte_launch_environ);
    if (NULL != app->env) {
        for (i = 0; NULL != app->env[i]; i++) {
            /* find the '=' sign.
             * strdup the env string to a tmp variable,
             * since it is shared among apps.
             */
            char *tmp = strdup(app->env[i]);
            ptr = strchr(tmp, '=');
            if (NULL == ptr) {
                PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
                rc = PRTE_ERR_BAD_PARAM;
                state = PRTE_PROC_STATE_FAILED_TO_LAUNCH;
                free(tmp);
                goto errorout;
            }
            *ptr = '\0';
            ++ptr;
            PMIX_SETENV_COMPAT(tmp, ptr, true, &cd->env);
            free(tmp);
        }
    }

    /* ensure we clear any prior info regarding state or exit status in
     * case this is a restart
     */
    child->exit_code = 0;
    PRTE_FLAG_UNSET(child, PRTE_PROC_FLAG_WAITPID);

    /* setup the pmix environment */
    PMIX_LOAD_PROCID(&pproc, child->name.nspace, child->name.rank);
    if (PMIX_SUCCESS != (ret = PMIx_server_setup_fork(&pproc, &cd->env))) {
        PMIX_ERROR_LOG(ret);
        rc = PRTE_ERROR;
        state = PRTE_PROC_STATE_FAILED_TO_LAUNCH;
        goto errorout;
    }

    /* if we are not forwarding output for this job, then
     * flag iof as complete
     */
    if (PRTE_FLAG_TEST(jobdat, PRTE_JOB_FLAG_FORWARD_OUTPUT)) {
        PRTE_FLAG_UNSET(child, PRTE_PROC_FLAG_IOF_COMPLETE);
    } else {
        PRTE_FLAG_SET(child, PRTE_PROC_FLAG_IOF_COMPLETE);
    }
    child->pid = 0;
    if (NULL != child->rml_uri) {
        free(child->rml_uri);
        child->rml_uri = NULL;
    }

    /* did the user request we display output in xterms? */
    if (NULL != prte_xterm) {
        pmix_list_item_t *nmitem;
        prte_namelist_t *nm;
        /* see if this rank is one of those requested */
        found = false;
        for (nmitem = pmix_list_get_first(&prte_odls_globals.xterm_ranks);
             nmitem != pmix_list_get_end(&prte_odls_globals.xterm_ranks);
             nmitem = pmix_list_get_next(nmitem)) {
            nm = (prte_namelist_t *) nmitem;
            if (PMIX_RANK_WILDCARD == nm->name.rank || child->name.rank == nm->name.rank) {
                /* we want this one - modify the app's command to include
                 * the prte xterm cmd that starts with the xtermcmd */
                cd->argv = PMIX_ARGV_COPY_COMPAT(prte_odls_globals.xtermcmd);
                /* insert the rank into the correct place as a window title */
                free(cd->argv[2]);
                pmix_asprintf(&cd->argv[2], "Rank %s", PRTE_VPID_PRINT(child->name.rank));
                /* add in the argv from the app */
                for (i = 0; NULL != app->argv[i]; i++) {
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cd->argv, app->argv[i]);
                }
                /* use the xterm cmd as the app string */
                cd->cmd = strdup(prte_odls_globals.xtermcmd[0]);
                found = true;
                break;
            } else if (jobdat->num_procs <= nm->name.rank) { /* check for bozo case */
                /* can't be done! */
                pmix_show_help("help-prte-odls-base.txt", "prte-odls-base:xterm-rank-out-of-bounds",
                               true, prte_process_info.nodename, nm->name.rank, jobdat->num_procs);
                state = PRTE_PROC_STATE_FAILED_TO_LAUNCH;
                goto errorout;
            }
        }
        if (!found) {
            cd->cmd = strdup(app->app);
            cd->argv = PMIX_ARGV_COPY_COMPAT(app->argv);
        }
    } else if (prte_get_attribute(&jobdat->attributes, PRTE_JOB_EXEC_AGENT, (void**)&ptr, PMIX_STRING)) {
        /* we were given a fork agent - use it */
        cd->argv = PMIX_ARGV_SPLIT_COMPAT(ptr, ' ');
        /* add in the argv from the app */
        for (i = 0; NULL != app->argv[i]; i++) {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cd->argv, app->argv[i]);
        }
        cd->cmd = pmix_path_findv(cd->argv[0], X_OK, prte_launch_environ, NULL);
        if (NULL == cd->cmd) {
            pmix_show_help("help-prte-odls-base.txt", "prte-odls-base:fork-agent-not-found", true,
                           prte_process_info.nodename, ptr);
            state = PRTE_PROC_STATE_FAILED_TO_LAUNCH;
            free(ptr);
            goto errorout;
        }
        free(ptr);
    } else if (NULL != prte_odls_globals.exec_agent) {
        /* we were given a fork agent - use it */
        cd->argv = PMIX_ARGV_SPLIT_COMPAT(prte_odls_globals.exec_agent, ' ');
        /* add in the argv from the app */
        for (i = 0; NULL != app->argv[i]; i++) {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cd->argv, app->argv[i]);
        }
        cd->cmd = pmix_path_findv(cd->argv[0], X_OK, prte_launch_environ, NULL);
        if (NULL == cd->cmd) {
            pmix_show_help("help-prte-odls-base.txt", "prte-odls-base:fork-agent-not-found", true,
                           prte_process_info.nodename, cd->argv[0]);
            state = PRTE_PROC_STATE_FAILED_TO_LAUNCH;
            goto errorout;
        }
    } else {
        cd->cmd = strdup(app->app);
        cd->argv = PMIX_ARGV_COPY_COMPAT(app->argv);
    }

    /* if we are indexing the argv by rank, do so now */
    if (cd->index_argv) {
        char *param;
        pmix_asprintf(&param, "%s-%u", cd->argv[0], child->name.rank);
        free(cd->argv[0]);
        cd->argv[0] = param;
    }

    pmix_output_verbose(5, prte_odls_base_framework.framework_output,
                        "%s odls:launch spawning child %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PRTE_NAME_PRINT(&child->name));

    if (15 < pmix_output_get_verbosity(prte_odls_base_framework.framework_output)) {
        /* dump what is going to be exec'd */
        char *output = NULL;
        prte_app_print(&output, jobdat, app);
        pmix_output(prte_odls_base_framework.framework_output, "%s", output);
        free(output);
    }

    if (PRTE_SUCCESS != (rc = cd->fork_local(cd))) {
        /* error message already output */
        state = PRTE_PROC_STATE_FAILED_TO_START;
        goto errorout;
    }
    if (PRTE_PROC_IS_MASTER) {
        /* locally store the pid */
        pidval.type = PMIX_PID;
        pidval.data.pid = child->pid;
        /* store the PID for later retrieval */
        rc = PMIx_Store_internal(&child->name, PMIX_PROC_PID, &pidval);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }
    PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_RUNNING);
    PMIX_RELEASE(cd);
    return;

errorout:
    PRTE_FLAG_UNSET(child, PRTE_PROC_FLAG_ALIVE);
    child->exit_code = rc;
    PRTE_ACTIVATE_PROC_STATE(&child->name, state);
    PMIX_RELEASE(cd);
}

void prte_odls_base_default_launch_local(int fd, short sd, void *cbdata)
{
    prte_app_context_t *app;
    prte_proc_t *child = NULL;
    int rc = PRTE_SUCCESS;
    char basedir[MAXPATHLEN];
    int j, idx;
    int total_num_local_procs = 0;
    prte_odls_launch_local_t *caddy = (prte_odls_launch_local_t *) cbdata;
    prte_job_t *jobdat;
    pmix_nspace_t job;
    prte_odls_base_fork_local_proc_fn_t fork_local = caddy->fork_local;
    bool index_argv;
    char *msg;
    prte_odls_spawn_caddy_t *cd;
    prte_event_base_t *evb;
    prte_schizo_base_module_t *schizo;

    PRTE_HIDE_UNUSED_PARAMS(fd, sd);

    PMIX_ACQUIRE_OBJECT(caddy);

    pmix_output_verbose(5, prte_odls_base_framework.framework_output,
                        "%s local:launch",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    PMIX_LOAD_NSPACE(job, caddy->job);

    /* establish our baseline working directory - we will be potentially
     * bouncing around as we execute various apps, but we will always return
     * to this place as our default directory
     */
    if (NULL == getcwd(basedir, sizeof(basedir))) {
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FAILED_TO_LAUNCH);
        goto ERROR_OUT;
    }
    /* find the jobdat for this job */
    if (NULL == (jobdat = prte_get_job_data_object(job))) {
        /* not much we can do here - the most likely explanation
         * is that a job that didn't involve us already completed
         * and was removed. This isn't an error so just move along */
        goto ERROR_OUT;
    }
    schizo = (prte_schizo_base_module_t*)jobdat->schizo;

    /* do we have any local procs to launch? */
    if (0 == jobdat->num_local_procs) {
        /* indicate that we are done trying to launch them */
        pmix_output_verbose(5, prte_odls_base_framework.framework_output,
                            "%s local:launch no local procs",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        goto GETOUT;
    }

    /* track if we are indexing argvs so we don't check every time */
    index_argv = prte_get_attribute(&jobdat->attributes, PRTE_JOB_INDEX_ARGV, NULL, PMIX_BOOL);

    /* compute the total number of local procs currently alive and about to be launched */
    total_num_local_procs = compute_num_procs_alive(job) + jobdat->num_local_procs;

    /* check the system limits - if we are at our max allowed children, then
     * we won't be allowed to do this anyway, so we may as well abort now.
     * According to the documentation, num_procs = 0 is equivalent to
     * no limit, so treat it as unlimited here.
     */
    if (0 < prte_sys_limits.num_procs) {
        PMIX_OUTPUT_VERBOSE((10, prte_odls_base_framework.framework_output,
                             "%s checking limit on num procs %d #children needed %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), prte_sys_limits.num_procs,
                             total_num_local_procs));
        if (prte_sys_limits.num_procs < total_num_local_procs) {
            if (2 < caddy->retries) {
                /* if we have already tried too many times, then just give up */
                PRTE_ODLS_SET_ERROR(job, PMIX_ERR_SYS_LIMITS_CHILDREN, UINT_MAX);
                PRTE_ACTIVATE_JOB_STATE(jobdat, PRTE_JOB_STATE_FAILED_TO_LAUNCH);
                goto ERROR_OUT;
            }
            /* set a timer event so we can retry later - this
             * gives the system a chance to let other procs
             * terminate, thus creating room for new ones
             */
            PRTE_DETECT_TIMEOUT(1000, 1000, -1, timer_cb, caddy);
            return;
        }
    }

    /* check to see if we have enough available file descriptors
     * to launch these children - if not, then let's wait a little
     * while to see if some come free. This can happen if we are
     * in a tight loop over comm_spawn
     */
    if (0 < prte_sys_limits.num_files) {
        int limit;
        limit = 4 * total_num_local_procs + 6 * jobdat->num_local_procs;
        PMIX_OUTPUT_VERBOSE((10, prte_odls_base_framework.framework_output,
                             "%s checking limit on file descriptors %d need %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), prte_sys_limits.num_files, limit));
        if (prte_sys_limits.num_files < limit) {
            if (2 < caddy->retries) {
                /* tried enough - give up */
                PRTE_ODLS_SET_ERROR(job, PMIX_ERR_SYS_LIMITS_FILES, UINT_MAX);
                PRTE_ACTIVATE_JOB_STATE(jobdat, PRTE_JOB_STATE_FAILED_TO_LAUNCH);
                goto ERROR_OUT;
            }
            /* don't have enough - wait a little time */
            PRTE_DETECT_TIMEOUT(1000, 1000, -1, timer_cb, caddy);
            return;
        }
    }

    for (j = 0; j < jobdat->apps->size; j++) {
        app = (prte_app_context_t *) pmix_pointer_array_get_item(jobdat->apps, j);
        if (NULL == app) {
            continue;
        }

        /* if this app isn't being used on our node, skip it */
        if (!PRTE_FLAG_TEST(app, PRTE_APP_FLAG_USED_ON_NODE)) {
            pmix_output_verbose(5, prte_odls_base_framework.framework_output,
                                "%s app %d not used on node",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), j);
            continue;
        }

        /* setup the working directory for this app - will jump us
         * to that directory
         */
        if (PRTE_SUCCESS != (rc = setup_path(app, &app->cwd))) {
            PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                 "%s odls:launch:setup_path failed with error %s(%d)",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_ERROR_NAME(rc), rc));
            /* do not ERROR_LOG this failure - it will be reported
             * elsewhere. The launch is going to fail. Since we could have
             * multiple app_contexts, we need to ensure that we flag only
             * the correct one that caused this operation to fail. We then have
             * to flag all the other procs from the app_context as having "not failed"
             * so we can report things out correctly
             */
            PRTE_ODLS_SET_ERROR(job, PMIX_ERR_SYS_LIMITS_FILES, j);
            PRTE_ACTIVATE_JOB_STATE(jobdat, PRTE_JOB_STATE_FAILED_TO_LAUNCH);
            goto GETOUT;
        }

        /* setup the environment for this app */
        if (PRTE_SUCCESS != (rc = schizo->setup_fork(jobdat, app))) {

            PMIX_OUTPUT_VERBOSE((10, prte_odls_base_framework.framework_output,
                                 "%s odls:launch:setup_fork failed with error %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_ERROR_NAME(rc)));

            /* do not ERROR_LOG this failure - it will be reported
             * elsewhere. The launch is going to fail. Since we could have
             * multiple app_contexts, we need to ensure that we flag only
             * the correct one that caused this operation to fail. We then have
             * to flag all the other procs from the app_context as having "not failed"
             * so we can report things out correctly
             */
            /* cycle through children to find those for this jobid */
            for (idx = 0; idx < prte_local_children->size; idx++) {
                child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, idx);
                if (NULL == child) {
                    continue;
                }
                if (PMIX_CHECK_NSPACE(job, child->name.nspace) && j == (int) child->app_idx) {
                    child->exit_code = PRTE_PROC_STATE_FAILED_TO_LAUNCH;
                    PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_FAILED_TO_LAUNCH);
                }
            }
            goto GETOUT;
        }

        /* setup any local files that were prepositioned for us */
        if (PRTE_SUCCESS != (rc = prte_filem.link_local_files(jobdat, app))) {
            /* cycle through children to find those for this jobid */
            for (idx = 0; idx < prte_local_children->size; idx++) {
                child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, idx);
                if (NULL == child) {
                    continue;
                }
                if (PMIX_CHECK_NSPACE(job, child->name.nspace) && j == (int) child->app_idx) {
                    child->exit_code = rc;
                    PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_FAILED_TO_LAUNCH);
                }
            }
            goto GETOUT;
        }

        rc = pmix_util_check_context_app(&app->app, app->cwd, app->env);
        /* do not ERROR_LOG - it will be reported elsewhere */
        if (PMIX_SUCCESS != rc) {
            /* cycle through children to find those for this jobid */
            for (idx = 0; idx < prte_local_children->size; idx++) {
                child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, idx);
                if (NULL == child) {
                    continue;
                }
                if (PMIX_CHECK_NSPACE(job, child->name.nspace) && j == (int) child->app_idx) {
                    child->exit_code = rc;
                    PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_FAILED_TO_LAUNCH);
                }
            }
            goto GETOUT;
        }

        /* if the user requested it, set the system resource limits */
        if (PRTE_SUCCESS != (rc = prte_util_init_sys_limits(&msg))) {
            pmix_show_help("help-prte-odls-default.txt", "set limit", true,
                           prte_process_info.nodename, app, __FILE__, __LINE__, msg);
            /* cycle through children to find those for this jobid */
            for (idx = 0; idx < prte_local_children->size; idx++) {
                child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, idx);
                if (NULL == child) {
                    continue;
                }
                if (PMIX_CHECK_NSPACE(job, child->name.nspace) && j == (int) child->app_idx) {
                    child->exit_code = rc;
                    PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_FAILED_TO_LAUNCH);
                }
            }
            goto GETOUT;
        }

        /* reset our working directory back to our default location - if we
         * don't do this, then we will be looking for relative paths starting
         * from the last wdir option specified by the user. Thus, we would
         * be requiring that the user keep track on the cmd line of where
         * each app was located relative to the prior app, instead of relative
         * to their current location
         */
        if (0 != chdir(basedir)) {
            PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_FAILED_TO_LAUNCH);
            goto GETOUT;
        }

        /* okay, now let's launch all the local procs for this app using the provided fork_local fn
         */
        for (idx = 0; idx < prte_local_children->size; idx++) {
            child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, idx);
            if (NULL == child) {
                continue;
            }
            /* does this child belong to this app? */
            if (j != (int) child->app_idx) {
                continue;
            }

            /* is this child already alive? This can happen if
             * we are asked to launch additional processes.
             * If it has been launched, then do nothing
             */
            if (PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_ALIVE)) {

                PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                     "%s odls:launch child %s has already been launched",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     PRTE_NAME_PRINT(&child->name)));

                continue;
            }
            /* is this child a candidate to start? it may not be alive
             * because it already executed
             */
            if (PRTE_PROC_STATE_INIT != child->state &&
                PRTE_PROC_STATE_RESTART != child->state) {
                continue;
            }
            /* do we have a child from the specified job. Because the
             * job could be given as a WILDCARD value, we must use
             * the dss.compare function to check for equality.
             */
            if (!PMIX_CHECK_NSPACE(job, child->name.nspace)) {

                PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                     "%s odls:launch child %s is not in job %s being launched",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     PRTE_NAME_PRINT(&child->name), PRTE_JOBID_PRINT(job)));

                continue;
            }

            PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                 "%s odls:launch working child %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 PRTE_NAME_PRINT(&child->name)));

            /* determine the thread that will handle this child */
            ++prte_odls_globals.next_base;
            if (prte_odls_globals.num_threads <= prte_odls_globals.next_base) {
                prte_odls_globals.next_base = 0;
            }
            evb = prte_odls_globals.ev_bases[prte_odls_globals.next_base];

            /* set the waitpid callback here for thread protection and
             * to ensure we can capture the callback on shortlived apps */
            PRTE_FLAG_SET(child, PRTE_PROC_FLAG_ALIVE);
            prte_wait_cb(child, prte_odls_base_default_wait_local_proc, NULL);

            /* dispatch this child to the next available launch thread */
            cd = PMIX_NEW(prte_odls_spawn_caddy_t);
            cd->jdata = jobdat;
            cd->app = app;
            cd->wdir = strdup(app->cwd);
            cd->child = child;
            cd->fork_local = fork_local;
            cd->index_argv = index_argv;
            /* setup any IOF */
            cd->opts.usepty = PRTE_ENABLE_PTY_SUPPORT;

            /* do we want to setup stdin? */
            if (jobdat->stdin_target == PMIX_RANK_WILDCARD ||
                child->name.rank == jobdat->stdin_target) {
                cd->opts.connect_stdin = true;
            } else {
                cd->opts.connect_stdin = false;
            }
            if (PRTE_SUCCESS != (rc = prte_iof_base_setup_prefork(&cd->opts))) {
                PRTE_ERROR_LOG(rc);
                child->exit_code = rc;
                PMIX_RELEASE(cd);
                PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_FAILED_TO_LAUNCH);
                goto GETOUT;
            }
            if (PRTE_FLAG_TEST(jobdat, PRTE_JOB_FLAG_FORWARD_OUTPUT)) {
                /* connect endpoints IOF */
                rc = prte_iof_base_setup_parent(&child->name, &cd->opts);
                if (PRTE_SUCCESS != rc) {
                    PRTE_ERROR_LOG(rc);
                    PMIX_RELEASE(cd);
                    PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_FAILED_TO_LAUNCH);
                    goto GETOUT;
                }
            }
            pmix_output_verbose(1, prte_odls_base_framework.framework_output,
                                "%s odls:dispatch %s to thread %d",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&child->name),
                                prte_odls_globals.next_base);
            prte_event_set(evb, &cd->ev, -1, PRTE_EV_WRITE, prte_odls_base_spawn_proc, cd);
            prte_event_active(&cd->ev, PRTE_EV_WRITE, 1);
        }
    }

GETOUT:

ERROR_OUT:
    /* ensure we reset our working directory back to our default location  */
    if (0 != chdir(basedir)) {
        PRTE_ERROR_LOG(PRTE_ERROR);
    }
    /* release the event */
    PMIX_RELEASE(caddy);
}

/**
 *  Pass a signal to my local procs
 */

int prte_odls_base_default_signal_local_procs(const pmix_proc_t *proc, int32_t signal,
                                              prte_odls_base_signal_local_fn_t signal_local)
{
    int rc, i;
    prte_proc_t *child;

    PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                         "%s odls: signaling proc %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         (NULL == proc) ? "NULL" : PRTE_NAME_PRINT(proc)));

    /* if procs is NULL, then we want to signal all
     * of the local procs, so just do that case
     */
    if (NULL == proc) {
        rc = PRTE_SUCCESS; /* pre-set this as an empty list causes us to drop to bottom */
        for (i = 0; i < prte_local_children->size; i++) {
            child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i);
            if (NULL == child) {
                continue;
            }
            if (0 == child->pid || !PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_ALIVE)) {
                /* skip this one as the child isn't alive */
                continue;
            }
            if (PRTE_SUCCESS != (rc = signal_local(child->pid, (int) signal))) {
                PRTE_ERROR_LOG(rc);
            }
        }
        return rc;
    }

    /* we want it sent to some specified process, so find it */
    for (i = 0; i < prte_local_children->size; i++) {
        child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i);
        if (NULL == child) {
            continue;
        }
        if (PMIX_CHECK_PROCID(&child->name, proc)) {
            if (PRTE_SUCCESS != (rc = signal_local(child->pid, (int) signal))) {
                PRTE_ERROR_LOG(rc);
            }
            return rc;
        }
    }

    /* only way to get here is if we couldn't find the specified proc.
     * report that as an error and return it
     */
    PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
    return PRTE_ERR_NOT_FOUND;
}

/*
 *  Wait for a callback indicating the child has completed.
 */

void prte_odls_base_default_wait_local_proc(int fd, short sd, void *cbdata)
{
    prte_wait_tracker_t *t2 = (prte_wait_tracker_t *) cbdata;
    prte_proc_t *proc = t2->child;
    int i;
    prte_job_t *jobdat;
    prte_proc_state_t state = PRTE_PROC_STATE_WAITPID_FIRED;
    prte_proc_t *cptr;
    bool flag = false;
    PRTE_HIDE_UNUSED_PARAMS(fd, sd);

    PMIX_ACQUIRE_OBJECT(t2);

    pmix_output_verbose(5, prte_odls_base_framework.framework_output,
                        "%s odls:wait_local_proc child process %s pid %ld terminated",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name),
                        (long) proc->pid);

    /* if the child was previously flagged as dead, then just
     * update its exit status and
     * ensure that its exit state gets reported to avoid hanging
     * don't forget to check if the process was signaled.
     */
    if (!PRTE_FLAG_TEST(proc, PRTE_PROC_FLAG_ALIVE)) {
        PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child %s was already dead exit code %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name),
                             proc->exit_code));
        if (WIFEXITED(proc->exit_code)) {
            proc->exit_code = WEXITSTATUS(proc->exit_code);
            if (0 != proc->exit_code) {
                state = PRTE_PROC_STATE_TERM_NON_ZERO;
            }
        } else {
            if (WIFSIGNALED(proc->exit_code)) {
                state = PRTE_PROC_STATE_ABORTED_BY_SIG;
                proc->exit_code = WTERMSIG(proc->exit_code) + 128;
            }
        }
        goto MOVEON;
    }

    /* mark that the waitpid fired */
    PRTE_FLAG_SET(proc, PRTE_PROC_FLAG_WAITPID);

    /* if the proc called "abort", then we just need to flag that it
     * came thru here */
    if (PRTE_FLAG_TEST(proc, PRTE_PROC_FLAG_ABORT)) {
        /* even though the process exited "normally", it happened
         * via an prte_abort call
         */
        PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child %s died by call to abort",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name)));
        state = PRTE_PROC_STATE_CALLED_ABORT;
        goto MOVEON;
    }

    /* get the jobdat for this child */
    if (NULL == (jobdat = prte_get_job_data_object(proc->name.nspace))) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        goto MOVEON;
    }

    /* if this child was ordered to die, then just pass that along
     * so we don't hang
     */
    if (PRTE_PROC_STATE_KILLED_BY_CMD == proc->state) {
        PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child %s was ordered to die",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name)));
        goto MOVEON;
    }

    /* determine the state of this process */
    if (WIFEXITED(proc->exit_code)) {

        /* set the exit status appropriately */
        proc->exit_code = WEXITSTATUS(proc->exit_code);

        PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child %s exit code %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name),
                             proc->exit_code));

        /* provide a default state */
        state = PRTE_PROC_STATE_WAITPID_FIRED;
        flag = prte_get_attribute(&jobdat->attributes, PRTE_JOB_ERROR_NONZERO_EXIT, NULL, PMIX_BOOL);

        /* check to see if a sync was required and if it was received */
        if (PRTE_FLAG_TEST(proc, PRTE_PROC_FLAG_REG)) {
            if (PRTE_FLAG_TEST(proc, PRTE_PROC_FLAG_HAS_DEREG) ||
                prte_allowed_exit_without_sync ||
                0 != proc->exit_code) {
                /* if we did recv a finalize sync, or one is not required,
                 * then declare it normally terminated
                 * unless it returned with a non-zero status indicating the code
                 * felt it was non-normal - in this latter case, we do not
                 * require that the proc deregister before terminating
                 */
                if (0 != proc->exit_code && flag) {
                    PMIX_OUTPUT_VERBOSE(
                        (5, prte_odls_base_framework.framework_output,
                         "%s odls:waitpid_fired child process %s terminated normally "
                         "but with a non-zero exit status - it "
                         "will be treated as an abnormal termination",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name)));
                    state = PRTE_PROC_STATE_TERM_NON_ZERO;
                } else {
                    /* indicate the waitpid fired */
                    state = PRTE_PROC_STATE_WAITPID_FIRED;
                }
            } else {
                /* we required a finalizing sync and didn't get it, so this
                 * is considered an abnormal termination and treated accordingly
                 */
                state = PRTE_PROC_STATE_TERM_WO_SYNC;
                PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                     "%s odls:waitpid_fired child process %s terminated normally "
                                     "but did not provide a required finalize sync - it "
                                     "will be treated as an abnormal termination",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     PRTE_NAME_PRINT(&proc->name)));
            }
        } else {
            /* has any child in this job already registered? */
            for (i = 0; i < prte_local_children->size; i++) {
                cptr = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i);
                if (NULL == cptr) {
                    continue;
                }
                if (!PMIX_CHECK_NSPACE(cptr->name.nspace, proc->name.nspace)) {
                    continue;
                }
                if (PRTE_FLAG_TEST(cptr, PRTE_PROC_FLAG_REG) && !prte_allowed_exit_without_sync) {
                    /* someone has registered, and we didn't before
                     * terminating - this is an abnormal termination unless
                     * the allowed_exit_without_sync flag is set
                     */
                    if (0 != proc->exit_code) {
                        state = PRTE_PROC_STATE_TERM_NON_ZERO;
                        PMIX_OUTPUT_VERBOSE(
                            (5, prte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child process %s terminated normally "
                             "but with a non-zero exit status - it "
                             "will be treated as an abnormal termination",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name)));
                    } else {
                        state = PRTE_PROC_STATE_TERM_WO_SYNC;
                        PMIX_OUTPUT_VERBOSE(
                            (5, prte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child process %s terminated normally "
                             "but did not provide a required init sync - it "
                             "will be treated as an abnormal termination",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name)));
                    }
                    goto MOVEON;
                }
            }
            /* if no child has registered, then it is possible that
             * none of them will. This is considered acceptable. Still
             * flag it as abnormal if the exit code was non-zero
             */
            if (0 != proc->exit_code && flag) {
                state = PRTE_PROC_STATE_TERM_NON_ZERO;
            } else {
                state = PRTE_PROC_STATE_WAITPID_FIRED;
            }
        }

        PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child process %s terminated %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name),
                             (0 == proc->exit_code) ? "normally" : "with non-zero status"));
    } else {
        /* the process was terminated with a signal! That's definitely
         * abnormal, so indicate that condition
         */
        state = PRTE_PROC_STATE_ABORTED_BY_SIG;
        /* If a process was killed by a signal, then make the
         * exit code of prun be "signo + 128" so that "prog"
         * and "prun prog" will both yield the same exit code.
         *
         * This is actually what the shell does for you when
         * a process dies by signal, so this makes prun treat
         * the termination code to exit status translation the
         * same way
         */
        PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child process %s terminated with signal %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name),
                             strsignal(WTERMSIG(proc->exit_code))));
        proc->exit_code = WTERMSIG(proc->exit_code) + 128;

        /* Do not decrement the number of local procs here. That is handled in the errmgr */
    }

MOVEON:
    /* cancel the wait as this proc has already terminated */
    prte_wait_cb_cancel(proc);
    PRTE_ACTIVATE_PROC_STATE(&proc->name, state);
    /* cleanup the tracker */
    PMIX_RELEASE(t2);
}

typedef struct {
    pmix_list_item_t super;
    prte_proc_t *child;
} prte_odls_quick_caddy_t;
static void qcdcon(prte_odls_quick_caddy_t *p)
{
    p->child = NULL;
}
static void qcddes(prte_odls_quick_caddy_t *p)
{
    if (NULL != p->child) {
        PMIX_RELEASE(p->child);
    }
}
PMIX_CLASS_INSTANCE(prte_odls_quick_caddy_t, pmix_list_item_t, qcdcon, qcddes);

int prte_odls_base_default_kill_local_procs(pmix_pointer_array_t *procs,
                                            prte_odls_base_kill_local_fn_t kill_local)
{
    prte_proc_t *child;
    pmix_list_t procs_killed;
    prte_proc_t *proc, proctmp;
    int i, j;
    pmix_pointer_array_t procarray, *procptr;
    bool do_cleanup;
    prte_odls_quick_caddy_t *cd;
    struct timespec tp = {0, 250000000};

    PMIX_CONSTRUCT(&procs_killed, pmix_list_t);

    /* if the pointer array is NULL, then just kill everything */
    if (NULL == procs) {
        PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                             "%s odls:kill_local_proc working on WILDCARD",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        PMIX_CONSTRUCT(&procarray, pmix_pointer_array_t);
        pmix_pointer_array_init(&procarray, 1, 1, 1);
        PMIX_CONSTRUCT(&proctmp, prte_proc_t);
        PMIX_LOAD_PROCID(&proctmp.name, NULL, PMIX_RANK_WILDCARD);
        pmix_pointer_array_add(&procarray, &proctmp);
        procptr = &procarray;
        do_cleanup = true;
    } else {
        PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                             "%s odls:kill_local_proc working on provided array",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        procptr = procs;
        do_cleanup = false;
    }

    /* cycle through the provided array of processes to kill */
    for (i = 0; i < procptr->size; i++) {
        if (NULL == (proc = (prte_proc_t *) pmix_pointer_array_get_item(procptr, i))) {
            continue;
        }
        for (j = 0; j < prte_local_children->size; j++) {
            child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, j);
            if (NULL == child) {
                continue;
            }

            PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                 "%s odls:kill_local_proc checking child process %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 PRTE_NAME_PRINT(&child->name)));

            /* do we have a child from the specified job? Because the
             *  job could be given as a WILDCARD value, we must
             *  check for that as well as for equality.
             */
            if (!PMIX_NSPACE_INVALID(proc->name.nspace) &&
                !PMIX_CHECK_NSPACE(proc->name.nspace, child->name.nspace)) {

                PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                     "%s odls:kill_local_proc child %s is not part of job %s",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     PRTE_NAME_PRINT(&child->name),
                                     PRTE_JOBID_PRINT(proc->name.nspace)));
                continue;
            }

            /* see if this is the specified proc - could be a WILDCARD again, so check
             * appropriately
             */
            if (PMIX_RANK_WILDCARD != proc->name.rank && proc->name.rank != child->name.rank) {

                PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                     "%s odls:kill_local_proc child %s is not covered by rank %s",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     PRTE_NAME_PRINT(&child->name),
                                     PRTE_VPID_PRINT(proc->name.rank)));
                continue;
            }

            /* is this process alive? if not, then nothing for us
             * to do to it
             */
            if (!PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_ALIVE) || 0 == child->pid) {

                PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                     "%s odls:kill_local_proc child %s is not alive",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     PRTE_NAME_PRINT(&child->name)));

                /* ensure, though, that the state is terminated so we don't lockup if
                 * the proc never started
                 */
                if (PRTE_PROC_STATE_UNDEF == child->state ||
                    PRTE_PROC_STATE_INIT == child->state ||
                    PRTE_PROC_STATE_RUNNING == child->state) {
                    /* we can't be sure what happened, but make sure we
                     * at least have a value that will let us eventually wakeup
                     */
                    child->state = PRTE_PROC_STATE_TERMINATED;
                    /* ensure we realize that the waitpid will never come, if
                     * it already hasn't
                     */
                    PRTE_FLAG_SET(child, PRTE_PROC_FLAG_WAITPID);
                    child->pid = 0;
                    goto CLEANUP;
                } else {
                    continue;
                }
            }

            /* ensure the stdin IOF channel for this child is closed. The other
             * channels will automatically close when the proc is killed
             */
            if (NULL != prte_iof.close) {
                prte_iof.close(&child->name, PRTE_IOF_STDIN);
            }

            /* cancel the waitpid callback as this induces unmanageable race
             * conditions when we are deliberately killing the process
             */
            prte_wait_cb_cancel(child);

            /* First send a SIGCONT in case the process is in stopped state.
               If it is in a stopped state and we do not first change it to
               running, then SIGTERM will not get delivered.  Ignore return
               value. */
            PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                 "%s SENDING SIGCONT TO %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 PRTE_NAME_PRINT(&child->name)));
            cd = PMIX_NEW(prte_odls_quick_caddy_t);
            PMIX_RETAIN(child);
            cd->child = child;
            pmix_list_append(&procs_killed, &cd->super);
            kill_local(child->pid, SIGCONT);
            continue;

        CLEANUP:
            /* check for everything complete - this will remove
             * the child object from our local list
             */
            if (!prte_finalizing && PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_IOF_COMPLETE)
                && PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_WAITPID)) {
                PRTE_ACTIVATE_PROC_STATE(&child->name, child->state);
            }
        }
    }

    /* if we are issuing signals, then we need to wait a little
     * and send the next in sequence */
    if (0 < pmix_list_get_size(&procs_killed)) {
        /* Wait a little. Do so in nanosleep() - can be interrupted by a
         * signal. Most likely SIGCHLD in this case */
        PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                             "%s Sleep %ld nsec",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                             (long)tp.tv_nsec));
        (void)nanosleep(&tp, NULL);
        /* issue a SIGTERM to all */
        PMIX_LIST_FOREACH(cd, &procs_killed, prte_odls_quick_caddy_t)
        {
            PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                 "%s SENDING SIGTERM TO %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 PRTE_NAME_PRINT(&cd->child->name)));
            kill_local(cd->child->pid, SIGTERM);
        }
        PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                             "%s Sleep %ld nsec",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                             (long)tp.tv_nsec));
        /* Wait a little. Do so in nanosleep() - can be interrupted by a
         * signal. Most likely SIGCHLD in this case */
        (void)nanosleep(&tp, NULL);

        /* issue a SIGKILL to all */
        PMIX_LIST_FOREACH(cd, &procs_killed, prte_odls_quick_caddy_t)
        {
            PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                                 "%s SENDING SIGKILL TO %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 PRTE_NAME_PRINT(&cd->child->name)));
            kill_local(cd->child->pid, SIGKILL);
            /* indicate the waitpid fired as this is effectively what
             * has happened
             */
            PRTE_FLAG_SET(cd->child, PRTE_PROC_FLAG_WAITPID);

            /* Since we are not going to wait for this process, make sure
             * we mark it as not-alive so that we don't wait for it
             * in orted_cmd
             */
            PRTE_FLAG_UNSET(cd->child, PRTE_PROC_FLAG_ALIVE);
            cd->child->pid = 0;

            /* mark the child as "killed" */
            if (cd->child->state < PRTE_PROC_STATE_TERMINATED) {
                cd->child->state = PRTE_PROC_STATE_KILLED_BY_CMD; /* we ordered it to die */
            }

            /* check for everything complete - this will remove
             * the child object from our local list
             */
            if (!prte_finalizing &&
                PRTE_FLAG_TEST(cd->child, PRTE_PROC_FLAG_IOF_COMPLETE) &&
                PRTE_FLAG_TEST(cd->child, PRTE_PROC_FLAG_WAITPID)) {
                PRTE_ACTIVATE_PROC_STATE(&cd->child->name, cd->child->state);
            }
        }
    }
    PMIX_LIST_DESTRUCT(&procs_killed);

    /* cleanup arrays, if required */
    if (do_cleanup) {
        PMIX_DESTRUCT(&procarray);
        PMIX_DESTRUCT(&proctmp);
    }

    return PRTE_SUCCESS;
}

int prte_odls_base_default_restart_proc(prte_proc_t *child,
                                        prte_odls_base_fork_local_proc_fn_t fork_local)
{
    int rc;
    prte_app_context_t *app;
    prte_job_t *jobdat;
    char basedir[MAXPATHLEN];
    char *wdir = NULL;
    prte_odls_spawn_caddy_t *cd;
    prte_event_base_t *evb;

    PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                         "%s odls:restart_proc for proc %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         PRTE_NAME_PRINT(&child->name)));

    /* establish our baseline working directory - we will be potentially
     * bouncing around as we execute this app, but we will always return
     * to this place as our default directory
     */
    if (NULL == getcwd(basedir, sizeof(basedir))) {
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    /* find this child's jobdat */
    if (NULL == (jobdat = prte_get_job_data_object(child->name.nspace))) {
        /* not found */
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }

    /* CHECK THE NUMBER OF TIMES THIS CHILD HAS BEEN RESTARTED
     * AGAINST MAX_RESTARTS */

    child->state = PRTE_PROC_STATE_FAILED_TO_START;
    child->exit_code = 0;
    PRTE_FLAG_UNSET(child, PRTE_PROC_FLAG_WAITPID);
    PRTE_FLAG_UNSET(child, PRTE_PROC_FLAG_IOF_COMPLETE);
    child->pid = 0;
    if (NULL != child->rml_uri) {
        free(child->rml_uri);
        child->rml_uri = NULL;
    }
    app = (prte_app_context_t *) pmix_pointer_array_get_item(jobdat->apps, child->app_idx);

    /* setup the path */
    if (PRTE_SUCCESS != (rc = setup_path(app, &wdir))) {
        PRTE_ERROR_LOG(rc);
        if (NULL != wdir) {
            free(wdir);
        }
        goto CLEANUP;
    }

    /* NEED TO UPDATE THE REINCARNATION NUMBER IN PMIX */

    /* dispatch this child to the next available launch thread */
    cd = PMIX_NEW(prte_odls_spawn_caddy_t);
    if (NULL != wdir) {
        cd->wdir = strdup(wdir);
        free(wdir);
    }
    cd->jdata = jobdat;
    cd->app = app;
    cd->child = child;
    cd->fork_local = fork_local;
    /* setup any IOF */
    cd->opts.usepty = PRTE_ENABLE_PTY_SUPPORT;

    /* do we want to setup stdin? */
    if (jobdat->stdin_target == PMIX_RANK_WILDCARD || child->name.rank == jobdat->stdin_target) {
        cd->opts.connect_stdin = true;
    } else {
        cd->opts.connect_stdin = false;
    }
    if (PRTE_SUCCESS != (rc = prte_iof_base_setup_prefork(&cd->opts))) {
        PRTE_ERROR_LOG(rc);
        child->exit_code = rc;
        PMIX_RELEASE(cd);
        PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_FAILED_TO_LAUNCH);
        goto CLEANUP;
    }
    if (PRTE_FLAG_TEST(jobdat, PRTE_JOB_FLAG_FORWARD_OUTPUT)) {
        /* connect endpoints IOF */
        rc = prte_iof_base_setup_parent(&child->name, &cd->opts);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
            PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_FAILED_TO_LAUNCH);
            goto CLEANUP;
        }
    }
    ++prte_odls_globals.next_base;
    if (prte_odls_globals.num_threads <= prte_odls_globals.next_base) {
        prte_odls_globals.next_base = 0;
    }
    evb = prte_odls_globals.ev_bases[prte_odls_globals.next_base];
    prte_wait_cb(child, prte_odls_base_default_wait_local_proc, NULL);

    PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output, "%s restarting app %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), app->app));

    prte_event_set(evb, &cd->ev, -1, PRTE_EV_WRITE, prte_odls_base_spawn_proc, cd);
    prte_event_active(&cd->ev, PRTE_EV_WRITE, 1);

CLEANUP:
    PMIX_OUTPUT_VERBOSE((5, prte_odls_base_framework.framework_output,
                         "%s odls:restart of proc %s %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         PRTE_NAME_PRINT(&child->name),
                         (PRTE_SUCCESS == rc) ? "succeeded" : "failed"));

    /* reset our working directory back to our default location - if we
     * don't do this, then we will be looking for relative paths starting
     * from the last wdir option specified by the user. Thus, we would
     * be requiring that the user keep track on the cmd line of where
     * each app was located relative to the prior app, instead of relative
     * to their current location
     */
    if (0 != chdir(basedir)) {
        PRTE_ERROR_LOG(PRTE_ERROR);
    }

    return rc;
}
