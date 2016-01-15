/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
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
#include "opal/util/show_help.h"

#include <string.h>
#include <pmi.h>

#include "opal/mca/pmix/base/base.h"
#include "opal/mca/pmix/base/pmix_base_fns.h"
#include "opal/mca/pmix/base/pmix_base_hash.h"
#include "pmix_s1.h"

static int s1_init(void);
static int s1_fini(void);
static int s1_initialized(void);
static int s1_abort(int flag, const char msg[],
                    opal_list_t *procs);
static int s1_commit(void);
static int s1_fence(opal_list_t *procs, int collect_data);
static int s1_put(opal_pmix_scope_t scope,
                  opal_value_t *kv);
static int s1_get(const opal_process_name_t *id,
                  const char *key, opal_list_t *info,
                  opal_value_t **kv);
static int s1_publish(opal_list_t *info);
static int s1_lookup(opal_list_t *data, opal_list_t *info);
static int s1_unpublish(char **keys, opal_list_t *info);
static int s1_spawn(opal_list_t *jobinfo, opal_list_t *apps, opal_jobid_t *jobid);
static int s1_job_connect(opal_list_t *procs);
static int s1_job_disconnect(opal_list_t *procs);
static int s1_store_local(const opal_process_name_t *proc,
                          opal_value_t *val);
static const char *s1_get_nspace(opal_jobid_t jobid);
static void s1_register_jobid(opal_jobid_t jobid, const char *nspace);

const opal_pmix_base_module_t opal_pmix_s1_module = {
    .init = s1_init,
    .finalize = s1_fini,
    .initialized = s1_initialized,
    .abort = s1_abort,
    .commit = s1_commit,
    .fence = s1_fence,
    .put = s1_put,
    .get = s1_get,
    .publish = s1_publish,
    .lookup = s1_lookup,
    .unpublish = s1_unpublish,
    .spawn = s1_spawn,
    .connect = s1_job_connect,
    .disconnect = s1_job_disconnect,
    .register_errhandler = opal_pmix_base_register_handler,
    .deregister_errhandler = opal_pmix_base_deregister_handler,
    .store_local = s1_store_local,
    .get_nspace = s1_get_nspace,
    .register_jobid = s1_register_jobid
};

// usage accounting
static int pmix_init_count = 0;

// PMI constant values:
static int pmix_kvslen_max = 0;
static int pmix_keylen_max = 0;
static int pmix_vallen_max = 0;
static int pmix_vallen_threshold = INT_MAX;

// Job environment description
static char *pmix_kvs_name = NULL;
static bool s1_committed = false;
static char* pmix_packed_data = NULL;
static int pmix_packed_data_offset = 0;
static char* pmix_packed_encoded_data = NULL;
static int pmix_packed_encoded_data_offset = 0;
static int pmix_pack_key = 0;
static opal_process_name_t s1_pname;
static int *lranks = NULL, nlranks;
static bool got_modex_data = false;

static char* pmix_error(int pmix_err);
#define OPAL_PMI_ERROR(pmi_err, pmi_func)                       \
    do {                                                        \
        opal_output(0, "%s [%s:%d:%s]: %s\n",                   \
                    pmi_func, __FILE__, __LINE__, __func__,     \
                    pmix_error(pmi_err));                        \
    } while(0);

static int kvs_get(const char key[], char value [], int maxvalue)
{
    int rc;
    rc = PMI_KVS_Get(pmix_kvs_name, key, value, maxvalue);
    if( PMI_SUCCESS != rc ){
        /* silently return an error - might be okay */
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int kvs_put(const char key[], const char value[])
{
    int rc;
    rc = PMI_KVS_Put(pmix_kvs_name, key, value);
    if( PMI_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
        return OPAL_ERROR;
    }
    return rc;
}

static int s1_init(void)
{
    PMI_BOOL initialized;
    int spawned;
    int rc, ret = OPAL_ERROR;
    int i, rank, lrank, nrank;
    char *pmix_id, tmp[64];
    opal_value_t kv;
    char *str;
    uint32_t ui32;
    opal_process_name_t ldr;
    char **localranks=NULL;

    if (PMI_SUCCESS != (rc = PMI_Initialized(&initialized))) {
        OPAL_PMI_ERROR(rc, "PMI_Initialized");
        return OPAL_ERROR;
    }

    if (PMI_TRUE != initialized && PMI_SUCCESS != (rc = PMI_Init(&spawned))) {
        OPAL_PMI_ERROR(rc, "PMI_Init");
        return OPAL_ERROR;
    }

    // setup hash table
    opal_pmix_base_hash_init();

    // Initialize space demands
    rc = PMI_KVS_Get_value_length_max(&pmix_vallen_max);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_value_length_max");
        goto err_exit;
    }
    pmix_vallen_threshold = pmix_vallen_max * 3;
    pmix_vallen_threshold >>= 2;

    rc = PMI_KVS_Get_name_length_max(&pmix_kvslen_max);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_name_length_max");
        goto err_exit;
    }

    rc = PMI_KVS_Get_key_length_max(&pmix_keylen_max);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_key_length_max");
        goto err_exit;
    }

    // Initialize job environment information
    pmix_id = (char*)malloc(pmix_vallen_max);
    if (pmix_id == NULL) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }
    /* Get domain id */
    if (PMI_SUCCESS != (rc = PMI_Get_kvs_domain_id(pmix_id, pmix_vallen_max))) {
        free(pmix_id);
        goto err_exit;
    }

    /* get our rank */
    ret = PMI_Get_rank(&rank);
    if( PMI_SUCCESS != ret ) {
        OPAL_PMI_ERROR(ret, "PMI_Get_rank");
        free(pmix_id);
        goto err_exit;
    }

    /* Slurm PMI provides the job id as an integer followed
     * by a '.', followed by essentially a stepid. The first integer
     * defines an overall job number. The second integer is the number of
     * individual jobs we have run within that allocation. */
    s1_pname.jobid = strtoul(pmix_id, &str, 10);
    s1_pname.jobid = (s1_pname.jobid << 16) & 0xffff0000;
    if (NULL != str) {
        ui32 = strtoul(str, NULL, 10);
        s1_pname.jobid |= (ui32 & 0x0000ffff);
    }
    ldr.jobid = s1_pname.jobid;
    s1_pname.vpid = rank;
    /* store our name in the opal_proc_t so that
     * debug messages will make sense - an upper
     * layer will eventually overwrite it, but that
     * won't do any harm */
    opal_proc_set_name(&s1_pname);
    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1: assigned tmp name",
                        OPAL_NAME_PRINT(s1_pname));

    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_JOBID);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = s1_pname.jobid;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    /* save it */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_RANK);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = rank;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    pmix_kvs_name = (char*)malloc(pmix_kvslen_max);
    if (pmix_kvs_name == NULL) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }

    rc = PMI_KVS_Get_my_name(pmix_kvs_name, pmix_kvslen_max);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_my_name");
        goto err_exit;
    }

    /* get our local proc info to find our local rank */
    if (PMI_SUCCESS != (rc = PMI_Get_clique_size(&nlranks))) {
        OPAL_PMI_ERROR(rc, "PMI_Get_clique_size");
        return rc;
    }
    /* save the local size */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_LOCAL_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = nlranks;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);
    lrank = 0;
    nrank = 0;
    ldr.vpid = rank;
    if (0 < nlranks) {
        /* now get the specific ranks */
        lranks = (int*)calloc(nlranks, sizeof(int));
        if (NULL == lranks) {
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        if (PMI_SUCCESS != (rc = PMI_Get_clique_ranks(lranks, nlranks))) {
            OPAL_PMI_ERROR(rc, "PMI_Get_clique_ranks");
            free(lranks);
            return rc;
        }
        /* note the local ldr */
        ldr.vpid = lranks[0];
        /* save this */
        memset(tmp, 0, 64);
        for (i=0; i < nlranks; i++) {
            (void)snprintf(tmp, 64, "%d", lranks[i]);
            opal_argv_append_nosize(&localranks, tmp);
            if (rank == lranks[i]) {
                lrank = i;
                nrank = i;
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
    }

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
    OBJ_DESTRUCT(&kv);
    /* save our local rank */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_LOCAL_RANK);
    kv.type = OPAL_UINT16;
    kv.data.uint16 = lrank;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);
    /* and our node rank */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_NODE_RANK);
    kv.type = OPAL_UINT16;
    kv.data.uint16 = nrank;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    /* get universe size */
    ret = PMI_Get_universe_size(&i);
    if (PMI_SUCCESS != ret) {
        OPAL_PMI_ERROR(ret, "PMI_Get_universe_size");
        goto err_exit;
    }
    /* push this into the dstore for subsequent fetches */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_UNIV_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = i;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    /* get job size */
    ret = PMI_Get_size(&i);
    if (PMI_SUCCESS != ret) {
        OPAL_PMI_ERROR(ret, "PMI_Get_size");
        goto err_exit;
    }
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_JOB_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = i;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    /* get appnum */
    ret = PMI_Get_appnum(&i);
    if (PMI_SUCCESS != ret) {
        OPAL_PMI_ERROR(ret, "PMI_Get_appnum");
        goto err_exit;
    }
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_APPNUM);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = i;
    if (OPAL_SUCCESS != (ret = opal_pmix_base_store(&OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

   /* increment the init count */
    ++pmix_init_count;

    return OPAL_SUCCESS;

 err_exit:
    PMI_Finalize();
    return ret;
}

static int s1_fini(void) {
    if (0 == pmix_init_count) {
        return OPAL_SUCCESS;
    }

    if (0 == --pmix_init_count) {
        PMI_Finalize ();
    }

    // teardown hash table
    opal_pmix_base_hash_finalize();

    return OPAL_SUCCESS;
}

static int s1_initialized(void)
{
    if (0 < pmix_init_count) {
        return 1;
    }
    return 0;
}

static int s1_abort(int flag, const char msg[],
                    opal_list_t *procs)
{
    PMI_Abort(flag, msg);
    return OPAL_SUCCESS;
}

static int s1_spawn(opal_list_t *jobinfo, opal_list_t *apps, opal_jobid_t *jobid)
{
    /*
    int rc;
    size_t preput_vector_size;
    const int info_keyval_sizes[1];
    info_keyval_sizes[0] = (int)opal_list_get_size(info_keyval_vector);
    //FIXME what's the size of array of lists?
    preput_vector_size = opal_list_get_size(preput_keyval_vector);
    rc = PMI_Spawn_multiple(count, cmds, argcs, argvs, maxprocs, info_keyval_sizes, info_keyval_vector, (int)preput_vector_size, preput_keyval_vector);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Spawn_multiple");
        return OPAL_ERROR;
    }*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s1_put(opal_pmix_scope_t scope,
                  opal_value_t *kv)
{
    int rc;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 put for key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kv->key);

    if (OPAL_SUCCESS != (rc = opal_pmix_base_store_encoded (kv->key, (void*)&kv->data, kv->type, &pmix_packed_data, &pmix_packed_data_offset))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    if (pmix_packed_data_offset == 0) {
        /* nothing to write */
        return OPAL_SUCCESS;
    }

    if (((pmix_packed_data_offset/3)*4) + pmix_packed_encoded_data_offset < pmix_vallen_max) {
        /* this meta-key is still being filled,
         * nothing to put yet
         */
        return OPAL_SUCCESS;
    }

    rc = opal_pmix_base_partial_commit_packed (&pmix_packed_data, &pmix_packed_data_offset,
                                               &pmix_packed_encoded_data, &pmix_packed_encoded_data_offset,
                                               pmix_vallen_max, &pmix_pack_key, kvs_put);

    s1_committed = false;
    return rc;
}

static int s1_commit(void)
{
    int rc;

    /* check if there is partially filled meta key and put them */
    opal_pmix_base_commit_packed (&pmix_packed_data, &pmix_packed_data_offset,
                                  &pmix_packed_encoded_data, &pmix_packed_encoded_data_offset,
                                  pmix_vallen_max, &pmix_pack_key, kvs_put);

    if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmix_kvs_name))) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Commit");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s1_fence(opal_list_t *procs, int collect_data)
{
    int rc;
    int32_t i;
    opal_value_t *kp, kvn;
    opal_hwloc_locality_t locality;
    opal_process_name_t s1_pname;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 called fence",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* use the PMI barrier function */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        OPAL_PMI_ERROR(rc, "PMI_Barrier");
        return OPAL_ERROR;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 barrier complete",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* get the modex data from each local process and set the
     * localities to avoid having the MPI layer fetch data
     * for every process in the job */
    s1_pname.jobid = OPAL_PROC_MY_NAME.jobid;
    if (!got_modex_data) {
        got_modex_data = true;
        /* we only need to set locality for each local rank as "not found"
         * equates to "non-local" */
        for (i=0; i < nlranks; i++) {
            s1_pname.vpid = lranks[i];
            rc = opal_pmix_base_cache_keys_locally(&s1_pname, OPAL_PMIX_CPUSET,
                                                   &kp, pmix_kvs_name, pmix_vallen_max, kvs_get);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
            if (NULL == kp || NULL == kp->data.string) {
                /* if we share a node, but we don't know anything more, then
                 * mark us as on the node as this is all we know
                 */
                locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
            } else {
                /* determine relative location on our node */
                locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                                 opal_process_info.cpuset,
                                                                 kp->data.string);
            }
            if (NULL != kp) {
                OBJ_RELEASE(kp);
            }
            OPAL_OUTPUT_VERBOSE((1, opal_pmix_base_framework.framework_output,
                                 "%s pmix:s1 proc %s locality %s",
                                 OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                 OPAL_NAME_PRINT(s1_pname),
                                 opal_hwloc_base_print_locality(locality)));

            OBJ_CONSTRUCT(&kvn, opal_value_t);
            kvn.key = strdup(OPAL_PMIX_LOCALITY);
            kvn.type = OPAL_UINT16;
            kvn.data.uint16 = locality;
            opal_pmix_base_store(&s1_pname, &kvn);
            OBJ_DESTRUCT(&kvn);
        }
    }

    return OPAL_SUCCESS;
}

static int s1_get(const opal_process_name_t *id,
                  const char *key, opal_list_t *info,
                  opal_value_t **kv)
{
    int rc;
    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 called get for key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), key);

    rc = opal_pmix_base_cache_keys_locally(id, key, kv, pmix_kvs_name, pmix_vallen_max, kvs_get);
     opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 got key %s",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), key);

   return rc;
}

static int s1_publish(opal_list_t *info)
{
    // SLURM PMIv1 doesn't implement this function

    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_lookup(opal_list_t *data, opal_list_t *info)
{
    // Allocate mem for port here? Otherwise we won't get success!
    // SLURM PMIv1 doesn't implement this function

    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_unpublish(char **keys, opal_list_t *info)
{
    // SLURM PMIv1 doesn't implement this function

    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_job_connect(opal_list_t *procs)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_job_disconnect(opal_list_t *procs)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_store_local(const opal_process_name_t *proc,
                          opal_value_t *val)
{
    opal_pmix_base_store(proc, val);

    return OPAL_SUCCESS;
}

static const char *s1_get_nspace(opal_jobid_t jobid)
{
    return "N/A";
}
static void s1_register_jobid(opal_jobid_t jobid, const char *nspace)
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
	/* pmix.h calls this a valid return code but mpich doesn't define it (slurm does). */
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
#endif
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
