/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/types.h"

#include "opal_stdint.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include <string.h>
#include <pmi.h>
#include <pmi2.h>

#include "opal/mca/pmix/base/base.h"
#include "opal/mca/pmix/base/pmix_base_hash.h"
#include "pmix_cray.h"

static char cray_pmi_version[128];

static int cray_init(void);
static int cray_fini(void);
static int cray_initialized(void);
static int cray_abort(int flat, const char *msg,
                      opal_list_t *procs);
static int cray_spawn(opal_list_t *jobinfo, opal_list_t *apps, opal_jobid_t *jobid);
static int cray_spawn_nb(opal_list_t *jobinfo, opal_list_t *apps,
                         opal_pmix_spawn_cbfunc_t cbfunc,
                         void *cbdata);
static int cray_job_connect(opal_list_t *procs);
static int cray_job_disconnect(opal_list_t *procs);
static int cray_job_disconnect_nb(opal_list_t *procs,
                                  opal_pmix_op_cbfunc_t cbfunc,
                                  void *cbdata);
static int cray_resolve_peers(const char *nodename,
                              opal_jobid_t jobid,
                              opal_list_t *procs);
static int cray_resolve_nodes(opal_jobid_t jobid, char **nodelist);
static int cray_put(opal_pmix_scope_t scope, opal_value_t *kv);
static int cray_fence(opal_list_t *procs, int collect_data);
static int cray_fence_nb(opal_list_t *procs, int collect_data,
                         opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
static int cray_commit(void);
static int cray_get(const opal_process_name_t *id,
                    const char *key, opal_list_t *info,
                    opal_value_t **kv);
static int cray_get_nb(const opal_process_name_t *id, const char *key,
                       opal_list_t *info,
                       opal_pmix_value_cbfunc_t cbfunc, void *cbdata);
static int cray_publish(opal_list_t *info);
static int cray_publish_nb(opal_list_t *info,
                           opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
static int cray_lookup(opal_list_t *data, opal_list_t *info);
static int cray_lookup_nb(char **keys, opal_list_t *info,
                          opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata);
static int cray_unpublish(char **keys, opal_list_t *info);
static int cray_unpublish_nb(char **keys, opal_list_t *info,
                            opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
static const char *cray_get_version(void);
static int cray_store_local(const opal_process_name_t *proc,
                          opal_value_t *val);
static const char *cray_get_nspace(opal_jobid_t jobid);
static void cray_register_jobid(opal_jobid_t jobid, const char *nspace);

#if 0
static bool cray_get_attr(const char *attr, opal_value_t **kv);
#endif

const opal_pmix_base_module_t opal_pmix_cray_module = {
    .init = cray_init,
    .finalize = cray_fini,
    .initialized = cray_initialized,
    .abort = cray_abort,
    .commit = cray_commit,
    .fence = cray_fence,
    .fence_nb = cray_fence_nb,
    .put = cray_put,
    .get = cray_get,
    .get_nb = cray_get_nb,
    .publish = cray_publish,
    .publish_nb = cray_publish_nb,
    .lookup = cray_lookup,
    .lookup_nb = cray_lookup_nb,
    .unpublish = cray_unpublish,
    .unpublish_nb = cray_unpublish_nb,
    .spawn = cray_spawn,
    .spawn_nb = cray_spawn_nb,
    .connect = cray_job_connect,
    .disconnect = cray_job_disconnect,
    .disconnect_nb = cray_job_disconnect_nb,
    .resolve_peers = cray_resolve_peers,
    .resolve_nodes = cray_resolve_nodes,
    .get_version = cray_get_version,
    .register_errhandler = opal_pmix_base_register_handler,
    .deregister_errhandler = opal_pmix_base_deregister_handler,
    .store_local = cray_store_local,
    .get_nspace = cray_get_nspace,
    .register_jobid = cray_register_jobid
};

// usage accounting
static int pmix_init_count = 0;

// PMI constant values:
static int pmix_kvslen_max = 0;
static int pmix_keylen_max = 0;
static int pmix_vallen_max = 0;
static int pmix_vallen_threshold = INT_MAX;

// Job environment description
static int pmix_size = 0;
static int pmix_rank = 0;
static int pmix_lrank = 0;
static int pmix_nrank = 0;
static int pmix_nlranks = 0;
static int pmix_appnum = 0;
static int pmix_usize = 0;
static char *pmix_kvs_name = NULL;
static int *pmix_lranks = NULL;
static opal_process_name_t pmix_pname;
static uint32_t pmix_jobid = -1;

static char* pmix_error(int pmix_err);
#define OPAL_PMI_ERROR(pmi_err, pmi_func)                       \
    do {                                                        \
        opal_output(0, "%s [%s:%d:%s]: %s\n",                   \
                    pmi_func, __FILE__, __LINE__, __func__,     \
                    pmix_error(pmi_err));                       \
    } while(0);

static int cray_init(void)
{
    int i, spawned, size, rank, appnum, my_node;
    int rc, ret = OPAL_ERROR;
    char *pmapping = NULL;
    char buf[PMI2_MAX_ATTRVALUE];
    int found;
    int major, minor, revision;
    uint32_t jobfam;
    opal_value_t kv;
    opal_process_name_t ldr;
    char nmtmp[64];
    char *str, **localranks = NULL;

    ++pmix_init_count;

    /* if we can't startup PMI, we can't be used */
    if ( PMI2_Initialized () ) {
        opal_output_verbose(10, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray: pmi already initialized",
                        OPAL_NAME_PRINT(pmix_pname));
        return OPAL_SUCCESS;
    }
    size = -1;
    rank = -1;
    appnum = -1;
    if (PMI_SUCCESS != (rc = PMI2_Init(&spawned, &size, &rank, &appnum))) {
        opal_show_help("help-pmix-base.txt", "pmix2-init-failed", true, rc);
        return OPAL_ERROR;
    }
    if( size < 0 || rank < 0 ){
        opal_show_help("help-pmix-base.txt", "pmix2-init-returned-bad-values", true);
        goto err_exit;
    }

    pmix_size = size;
    pmix_rank = rank;
    pmix_appnum = appnum;

    pmix_vallen_max = PMI2_MAX_VALLEN;
    pmix_kvslen_max = PMI2_MAX_VALLEN; // FIX ME: What to put here for versatility?
    pmix_keylen_max = PMI2_MAX_KEYLEN;
    pmix_vallen_threshold = PMI2_MAX_VALLEN * 3;
    pmix_vallen_threshold >>= 2;

    /*
     * get the version info
     */

    if (PMI_SUCCESS != PMI_Get_version_info(&major,&minor,&revision)) {
        return OPAL_ERROR;
    }

    snprintf(cray_pmi_version, sizeof(cray_pmi_version),
             "%d.%d.%d", major, minor, revision);

    pmix_kvs_name = (char*)malloc(pmix_kvslen_max);
    if( pmix_kvs_name == NULL ){
         PMI2_Finalize();
         ret = OPAL_ERR_OUT_OF_RESOURCE;
         goto err_exit;
    }

    rc = PMI2_Job_GetId(pmix_kvs_name, pmix_kvslen_max);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI2_Job_GetId");
        goto err_exit;
    }

    rc = sscanf(pmix_kvs_name,"kvs_%u",&jobfam);
    if (rc != 1) {
        opal_output_verbose(10, opal_pmix_base_framework.framework_output,
                           "%s pmix:cray: pmix_kvs_name %s",
                            OPAL_NAME_PRINT(pmix_pname), pmix_kvs_name);
        rc = OPAL_ERROR;
        goto err_exit;
    }

    pmix_jobid = jobfam << 16;

    /* store our name in the opal_proc_t so that
     * debug messages will make sense - an upper
     * layer will eventually overwrite it, but that
     * won't do any harm */
    pmix_pname.jobid = pmix_jobid;
    pmix_pname.vpid = pmix_rank;
    opal_proc_set_name(&pmix_pname);
    opal_output_verbose(10, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray: assigned tmp name %d %d pmix_kvs_name %s",
                        OPAL_NAME_PRINT(pmix_pname),pmix_pname.jobid,pmix_pname.vpid,pmix_kvs_name);

    pmapping = (char*)malloc(PMI2_MAX_VALLEN);
    if( pmapping == NULL ){
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    rc = PMI2_Info_GetJobAttr("PMI_process_mapping", pmapping, PMI2_MAX_VALLEN, &found);
    if( !found || PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc,"PMI2_Info_GetJobAttr");
        return OPAL_ERROR;
    }

    pmix_lranks = pmix_cray_parse_pmap(pmapping, pmix_rank, &my_node, &pmix_nlranks);
    if (NULL == pmix_lranks) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    free(pmapping);

    // setup hash table
    opal_pmix_base_hash_init();

    /* save the job size */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_JOB_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = pmix_size;
    if (OPAL_SUCCESS != (rc = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    /* save the appnum */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_APPNUM);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = pmix_appnum;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    rc = PMI2_Info_GetJobAttr("universeSize", buf, 16, &found);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_universe_size");
        goto err_exit;
    }

    pmix_usize = atoi(buf);

    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_UNIV_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = pmix_usize;
    if (OPAL_SUCCESS != (rc = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_JOBID);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = pmix_jobid;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    /* save the local size */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_LOCAL_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = pmix_nlranks;
    if (OPAL_SUCCESS != (rc = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    ldr.vpid = pmix_lranks[0];
    ldr.jobid = pmix_pname.jobid;

    /* find ourselves and build up a string for local peer info */
    memset(nmtmp, 0, 64);
    for (i=0; i < pmix_nlranks; i++) {
        ret = snprintf(nmtmp, 64, "%d", pmix_lranks[i]);
        opal_argv_append_nosize(&localranks, nmtmp);
        if (pmix_rank == pmix_lranks[i]) {
            pmix_lrank = i;
            pmix_nrank = i;
        }
    }

    str = opal_argv_join(localranks, ',');
    opal_argv_free(localranks);

    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_LOCAL_PEERS);
    kv.type = OPAL_STRING;
    kv.data.string = str;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    /* save the local leader */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_LOCALLDR);
    kv.type = OPAL_UINT64;
    kv.data.uint64 = *(uint64_t*)&ldr;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }

    /* save our local rank */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_LOCAL_RANK);
    kv.type = OPAL_UINT16;
    kv.data.uint16 = pmix_lrank;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }

    /* and our node rank */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_NODE_RANK);
    kv.type = OPAL_UINT16;
    kv.data.uint16 = pmix_nrank;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    return OPAL_SUCCESS;
err_exit:
    PMI2_Finalize();
    return ret;
}

static int cray_fini(void) {

    if (0 == pmix_init_count) {
        return OPAL_SUCCESS;
    }

    if (0 == --pmix_init_count) {
        PMI2_Finalize();
    }

    if (NULL != pmix_kvs_name) {
        free(pmix_kvs_name);
        pmix_kvs_name = NULL;
    }

    if (NULL != pmix_lranks) {
        free(pmix_lranks);
        pmix_lranks = NULL;
    }

    return OPAL_SUCCESS;
}

static int cray_initialized(void)
{
    if (0 < pmix_init_count) {
        return 1;
    }
    return 0;
}

static int cray_abort(int flag, const char *msg,
                      opal_list_t *procs)
{
    PMI2_Abort(flag, msg);
    return OPAL_SUCCESS;
}

static int cray_spawn(opal_list_t *jobinfo, opal_list_t *apps, opal_jobid_t *jobid)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int cray_spawn_nb(opal_list_t *jobinfo, opal_list_t *apps,
                         opal_pmix_spawn_cbfunc_t cbfunc,
                         void *cbdata)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int cray_job_connect(opal_list_t *procs)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int cray_job_disconnect(opal_list_t *procs)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int cray_job_disconnect_nb(opal_list_t *procs,
                                  opal_pmix_op_cbfunc_t cbfunc,
                                  void *cbdata)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int cray_resolve_peers(const char *nodename,
                              opal_jobid_t jobid,
                              opal_list_t *procs)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int cray_resolve_nodes(opal_jobid_t jobid, char **nodelist)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int cray_put(opal_pmix_scope_t scope,
                  opal_value_t *kv)
{
    int rc;

    opal_output_verbose(10, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray cray_put key %s scope %d\n",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kv->key, scope);

    if (!pmix_init_count) {
        return OPAL_ERROR;
    }

    /*
     * for now just always just global cache
     */

    if (NULL == mca_pmix_cray_component.cache_global) {
        mca_pmix_cray_component.cache_global = OBJ_NEW(opal_buffer_t);
    }

    opal_output_verbose(20, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray put global data for key %s type %d",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kv->key, kv->type);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(mca_pmix_cray_component.cache_global, &kv, 1, OPAL_VALUE))) {
        OPAL_PMI_ERROR(rc,"pmix:cray opal_dss.pack returned error");
        OPAL_ERROR_LOG(rc);
    }

    return rc;
}

static int cray_commit(void)
{
    return OPAL_SUCCESS;
}

static int cray_fence(opal_list_t *procs, int collect_data)
{
    int rc, cnt;
    int32_t i;
    int *all_lens = NULL;
    opal_value_t *kp, kvn;
    opal_buffer_t *send_buffer = NULL;
    opal_buffer_t *buf = NULL;
    void *sbuf_ptr;
    char *cptr, *rcv_buff = NULL;
    opal_process_name_t id;
    typedef struct {
        uint32_t pmix_rank;
        opal_process_name_t name;
        int32_t nbytes;
    } bytes_and_rank_t;
    int32_t rcv_nbytes_tot;
    bytes_and_rank_t s_bytes_and_rank;
    bytes_and_rank_t *r_bytes_and_ranks = NULL;
    opal_hwloc_locality_t locality;
    opal_list_t vals;
    char *cpuset = NULL;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray executing fence cache_global %p cache_local %p",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        (void *)mca_pmix_cray_component.cache_global,
                        (void *)mca_pmix_cray_component.cache_local);


    /*
     * "unload" the cache_local/cache_global buffers, first copy
     * it so we can continue to use the local buffers if further
     * calls to put can be made
     */

    send_buffer = OBJ_NEW(opal_buffer_t);
    if (NULL == send_buffer) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    opal_dss.copy_payload(send_buffer, mca_pmix_cray_component.cache_global);
    opal_dss.unload(send_buffer, &sbuf_ptr, &s_bytes_and_rank.nbytes);
    s_bytes_and_rank.pmix_rank = pmix_rank;
    s_bytes_and_rank.name = OPAL_PROC_MY_NAME;

    r_bytes_and_ranks = (bytes_and_rank_t *)malloc(pmix_size * sizeof(bytes_and_rank_t));
    if (NULL == r_bytes_and_ranks) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto fn_exit;
    }

    /*
     * gather up all the buffer sizes and rank order.
     * doing this step below since the cray pmi PMI_Allgather doesn't deliver
     * the gathered data necessarily in PMI rank order, although the order stays
     * the same for the duration of a job - assuming no node failures.
     */

    if (PMI_SUCCESS != (rc = PMI_Allgather(&s_bytes_and_rank,r_bytes_and_ranks,sizeof(bytes_and_rank_t)))) {
        OPAL_PMI_ERROR(rc,"PMI_Allgather");
        rc = OPAL_ERR_COMM_FAILURE;
        goto fn_exit;
    }


    for (rcv_nbytes_tot=0,i=0; i < pmix_size; i++) {
        rcv_nbytes_tot += r_bytes_and_ranks[i].nbytes;
    }

    opal_output_verbose(20, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray total number of bytes to receive %d",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), rcv_nbytes_tot);

    rcv_buff = (char *) malloc(rcv_nbytes_tot * sizeof(char));
    if (NULL == rcv_buff) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto fn_exit;
    }

    all_lens = (int *)malloc(sizeof(int) * pmix_size);
    if (NULL == all_lens) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto fn_exit;
    }
    for (i=0; i< pmix_size; i++) {
        all_lens[r_bytes_and_ranks[i].pmix_rank] = r_bytes_and_ranks[i].nbytes;
    }

    if (PMI_SUCCESS != (rc = PMI_Allgatherv(sbuf_ptr,s_bytes_and_rank.nbytes,rcv_buff,all_lens))) {
        OPAL_PMI_ERROR(rc,"PMI_Allgatherv");
        rc = OPAL_ERR_COMM_FAILURE;
        goto fn_exit;
    }

    OBJ_RELEASE(send_buffer);
    send_buffer  = NULL;

    buf = OBJ_NEW(opal_buffer_t);
    if (buf == NULL) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto fn_exit;
    }

    for (cptr = rcv_buff, i=0; i < pmix_size; i++) {

        id = r_bytes_and_ranks[i].name;

        buf->base_ptr = NULL;  /* TODO: ugh */
        if (OPAL_SUCCESS != (rc = opal_dss.load(buf, (void *)cptr, r_bytes_and_ranks[i].nbytes))) {
            OPAL_PMI_ERROR(rc,"pmix:cray opal_dss.load failed");
            goto fn_exit;
        }

        /* unpack and stuff in to the dstore */

        cnt = 1;
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(buf, &kp, &cnt, OPAL_VALUE))) {
            opal_output_verbose(20, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray unpacked kp with key %s type(%d) for id  %s",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kp->key, kp->type, OPAL_NAME_PRINT(id));
            if (OPAL_SUCCESS != (rc = opal_pmix_base_store(&id, kp))) {
                OPAL_ERROR_LOG(rc);
                goto fn_exit;
            }
             OBJ_RELEASE(kp);
             cnt = 1;
        }

        cptr += r_bytes_and_ranks[i].nbytes;

    }

    buf->base_ptr = NULL;  /* TODO: ugh */
    OBJ_RELEASE(buf);

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray kvs_fence complete",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* fetch my cpuset */
    OBJ_CONSTRUCT(&vals, opal_list_t);
    if (OPAL_SUCCESS == (rc = opal_pmix_base_fetch(&pmix_pname,
                                                   OPAL_PMIX_CPUSET, &vals))) {
        kp = (opal_value_t*)opal_list_get_first(&vals);
        cpuset = strdup(kp->data.string);
    } else {
        cpuset = NULL;
    }
    OPAL_LIST_DESTRUCT(&vals);

    /* Get the modex data from each local process and set the
     * localities to avoid having the MPI layer fetch data
     * for every process in the job.
     *
     *  we only need to set locality for each local rank as "not found"
     * equates to "non-local" 
     */

    for (i=0; i < pmix_nlranks; i++) {
        id.vpid = pmix_lranks[i];
        id.jobid = pmix_jobid;
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s checking out if %s is local to me",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                OPAL_NAME_PRINT(id));
        /* fetch cpuset for this vpid */
        OBJ_CONSTRUCT(&vals, opal_list_t);
        if (OPAL_SUCCESS != (rc = opal_pmix_base_fetch(&id,
                                                    OPAL_PMIX_CPUSET, &vals))) {
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s cpuset for local proc %s not found",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                OPAL_NAME_PRINT(id));
            OPAL_LIST_DESTRUCT(&vals);
            /* even though the cpuset wasn't found, we at least know it is
             * on the same node with us */
            locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
        } else {
            kp = (opal_value_t*)opal_list_get_first(&vals);
            if (NULL == kp->data.string) {
                /* if we share a node, but we don't know anything more, then
                 * mark us as on the node as this is all we know
                 */
                locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
            } else {
                /* determine relative location on our node */
                locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                                 cpuset,
                                                                 kp->data.string);
            }
            OPAL_LIST_DESTRUCT(&vals);
        }
        OPAL_OUTPUT_VERBOSE((1, opal_pmix_base_framework.framework_output,
                             "%s pmix:cray proc %s locality %s",
                             OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                             OPAL_NAME_PRINT(id),
                             opal_hwloc_base_print_locality(locality)));

        OBJ_CONSTRUCT(&kvn, opal_value_t);
        kvn.key = strdup(OPAL_PMIX_LOCALITY);
        kvn.type = OPAL_UINT16;
        kvn.data.uint16 = locality;
        opal_pmix_base_store(&id, &kvn);
        OBJ_DESTRUCT(&kvn);
    }

fn_exit:
    if (NULL != cpuset) {
        free(cpuset);
    }
    if (all_lens != NULL) {
        free(all_lens);
    }
    if (rcv_buff != NULL) {
        free(rcv_buff);
    }
    if (r_bytes_and_ranks != NULL) {
        free(r_bytes_and_ranks);
    }
    return rc;
}

static int cray_fence_nb(opal_list_t *procs, int collect_data,
                         opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int cray_get(const opal_process_name_t *id, const char *key, opal_list_t *info, opal_value_t **kv)
{
    int rc;
    opal_list_t vals;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray getting value for proc %s key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        OPAL_NAME_PRINT(*id), key);

    OBJ_CONSTRUCT(&vals, opal_list_t);
    rc = opal_pmix_base_fetch(id, key, &vals);
    if (OPAL_SUCCESS == rc) {
        *kv = (opal_value_t*)opal_list_remove_first(&vals);
        return OPAL_SUCCESS;
    } else {
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                "%s pmix:cray fetch from dstore failed: %d",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), rc);
    }
    OPAL_LIST_DESTRUCT(&vals);

    return rc;

}

static int cray_get_nb(const opal_process_name_t *id, const char *key,
                       opal_list_t *info, opal_pmix_value_cbfunc_t cbfunc, void *cbdata)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int cray_publish(opal_list_t *info)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int cray_publish_nb(opal_list_t *info,
                           opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int cray_lookup(opal_list_t *data, opal_list_t *info)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int cray_lookup_nb(char **keys, opal_list_t *info,
                          opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int cray_unpublish(char **keys, opal_list_t *info)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int cray_unpublish_nb(char **keys, opal_list_t *info,
                            opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static const char *cray_get_version(void)
{
    return cray_pmi_version;
}

static int cray_store_local(const opal_process_name_t *proc,
                          opal_value_t *val)
{
    opal_pmix_base_store(proc, val);

    return OPAL_SUCCESS;
}

static const char *cray_get_nspace(opal_jobid_t jobid)
{
    return "N/A";
}

static void cray_register_jobid(opal_jobid_t jobid, const char *nspace)
{
    return;
}

static char* pmix_error(int pmix_err)
{
    char * err_msg;

    switch(pmix_err) {
        case PMI_FAIL: err_msg = "Operation failed"; break;
        case PMI_ERR_INIT: err_msg = "PMI is not initialized"; break;
        case PMI_ERR_NOMEM: err_msg = "Input buffer not large enough"; break;
        case PMI_ERR_INVALID_ARG: err_msg = "Invalid argument"; break;
        case PMI_ERR_INVALID_KEY: err_msg = "Invalid key argument"; break;
        case PMI_ERR_INVALID_KEY_LENGTH: err_msg = "Invalid key length argument"; break;
        case PMI_ERR_INVALID_VAL: err_msg = "Invalid value argument"; break;
        case PMI_ERR_INVALID_VAL_LENGTH: err_msg = "Invalid value length argument"; break;
        case PMI_ERR_INVALID_LENGTH: err_msg = "Invalid length argument"; break;
        case PMI_ERR_INVALID_NUM_ARGS: err_msg = "Invalid number of arguments"; break;
        case PMI_ERR_INVALID_ARGS: err_msg = "Invalid args argument"; break;
        case PMI_ERR_INVALID_NUM_PARSED: err_msg = "Invalid num_parsed length argument"; break;
        case PMI_ERR_INVALID_KEYVALP: err_msg = "Invalid keyvalp argument"; break;
        case PMI_ERR_INVALID_SIZE: err_msg = "Invalid size argument"; break;
#if defined(PMI_ERR_INVALID_KVS)
	/* pmi.h calls this a valid return code but mpich doesn't define it (slurm does). */
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
#endif
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
