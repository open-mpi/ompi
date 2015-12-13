/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>

#include <pmix.h>
#include <pmi.h>

#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include PMIX_EVENT_HEADER

#define ANL_MAPPING "PMI_process_mapping"

#include "src/buffer_ops/buffer_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"

#define PMI_MAX_ID_LEN       PMIX_MAX_NSLEN  /* Maximim size of PMI process group ID */
#define PMI_MAX_KEY_LEN      PMIX_MAX_KEYLEN /* Maximum size of a PMI key */
#define PMI_MAX_KVSNAME_LEN  PMIX_MAX_NSLEN  /* Maximum size of KVS name */
#define PMI_MAX_VAL_LEN      4096            /* Maximum size of a PMI value */

#define PMI_CHECK() \
	do {                     \
        if (!pmi_init) {     \
            return PMI_FAIL; \
        }                    \
    } while (0)

/* local functions */
static pmix_status_t convert_int(int *value, pmix_value_t *kv);
static int convert_err(pmix_status_t rc);
static pmix_proc_t myproc;
static bool data_commited = false;
static int pmi_init = 0;

int PMI_Init(int *spawned)
{
    pmix_value_t *val;
    pmix_status_t rc;

    if (PMIX_SUCCESS != PMIx_Init(&myproc)) {
        return PMI_ERR_INIT;
    }

    if (NULL != spawned) {
        /* get the spawned flag */
        if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_SPAWNED, NULL, 0, &val)) {
            rc = convert_int(spawned, val);
            PMIX_VALUE_RELEASE(val);
            if (PMIX_SUCCESS != rc) {
                return convert_err(rc);
            }
        } else {
            /* if not found, default to not spawned */
            *spawned = 0;
        }
    }
    pmi_init = 1;

    return PMI_SUCCESS;
}

int PMI_Initialized(PMI_BOOL *initialized)
{
    if (NULL == initialized) {
        return PMI_ERR_INVALID_ARG;
    }

    *initialized = (PMIx_Initialized() ? PMI_TRUE : PMI_FALSE);

    return PMI_SUCCESS;
}

int PMI_Finalize(void)
{
    pmix_status_t rc = PMIX_SUCCESS;

    PMI_CHECK();

    pmi_init = 0;
    rc = PMIx_Finalize();
    return convert_err(rc);
}

int PMI_Abort(int flag, const char msg[])
{
    pmix_status_t rc = PMIX_SUCCESS;

    PMI_CHECK();

    rc = PMIx_Abort(flag, msg, NULL, 0);
    return convert_err(rc);
}

/* KVS_Put - we default to PMIX_GLOBAL scope and ignore the
 * provided kvsname as we only put into our own nspace */
int PMI_KVS_Put(const char kvsname[], const char key[], const char value[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t val;

    PMI_CHECK();

    if ((kvsname == NULL) || (strlen(kvsname) > PMI_MAX_KVSNAME_LEN)) {
        return PMI_ERR_INVALID_KVS;
    }
    if ((key == NULL) || (strlen(key) >PMI_MAX_KEY_LEN)) {
        return PMI_ERR_INVALID_KEY;
    }
    if ((value == NULL) || (strlen(value) > PMI_MAX_VAL_LEN)) {
        return PMI_ERR_INVALID_VAL;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
            "PMI_KVS_Put: KVS=%s, key=%s value=%s", kvsname, key, value);

    val.type = PMIX_STRING;
    val.data.string = (char*)value;
    rc = PMIx_Put(PMIX_GLOBAL, key, &val);
    return convert_err(rc);
}

/* KVS_Commit */
int PMI_KVS_Commit(const char kvsname[])
{
    pmix_status_t rc = PMIX_SUCCESS;

    PMI_CHECK();

    if ((kvsname == NULL) || (strlen(kvsname) > PMI_MAX_KVSNAME_LEN)) {
        return PMI_ERR_INVALID_KVS;
    }

    pmix_output_verbose(2, pmix_globals.debug_output, "PMI_KVS_Commit: KVS=%s",
            kvsname);

    rc = PMIx_Commit();
    /* PMIx permits only one data commit! */
    data_commited = true;
    return convert_err(rc);
}

int PMI_KVS_Get( const char kvsname[], const char key[], char value[], int length)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t *val;
    uint32_t i;
    static pmix_proc_t proc;
    uint32_t procnum;
    proc = myproc;

    PMI_CHECK();

    if ((kvsname == NULL) || (strlen(kvsname) > PMI_MAX_KVSNAME_LEN)) {
        return PMI_ERR_INVALID_KVS;
    }
    if ((key == NULL) || (strlen(key) >PMI_MAX_KEY_LEN)) {
        return PMI_ERR_INVALID_KEY;
    }
    if (value == NULL) {
        return PMI_ERR_INVALID_VAL;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
            "PMI_KVS_Get: KVS=%s, key=%s value=%s", kvsname, key, value);

    /* PMI-1 expects resource manager to set
     * process mapping in ANL notation. */
    if (!strcmp(key, ANL_MAPPING)) {
        /* we are looking in the job-data. If there is nothing there
         * we don't want to look in rank's data, thus set rank to widcard */
        proc.rank = PMIX_RANK_WILDCARD;
        if (PMIX_SUCCESS == PMIx_Get(&proc, PMIX_ANL_MAP, NULL, 0, &val) &&
               (NULL != val) && (PMIX_STRING == val->type)) {
            strncpy(value, val->data.string, length);
            PMIX_VALUE_FREE(val, 1);
            return PMI_SUCCESS;
        } else {
            /* artpol:
             * Some RM's (i.e. SLURM) already have ANL precomputed. The export it
             * through PMIX_ANL_MAP variable.
             * If we haven't found it we want to have our own packing functionality
             * since it's common.
             * Somebody else has to write it since I've already done that for
             * GPL'ed SLURM :) */
            return PMI_FAIL;
        }
    }

    /* We don't know what process keeps this data. So it looks like we need to
     * check each process.
     * TODO: Is there any beter way?
     * WARNING: this may lead to the VERY long HANG's if we ask for the unknown key
     * before we've done Commit on all nodes. We need a workaround for that.
     *
     * SOLUTION: perhaps rovide "OK if nothing" info flag to tell PMIx that
     * the key supposed to already be there and if nothing there - gave up with
     * an error and don't try to use direct modex.
     */

    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                "pmi1: executing put for KVS %s, key %s value %s", kvsname, key,
                value);
        return convert_err(rc);
    }
    procnum = val->data.uint32;
    PMIX_VALUE_FREE(val, 1);

    for (i = 0; i < procnum; i++) {
        proc.rank = i;
        if (PMIX_SUCCESS == PMIx_Get(&proc, key, NULL, 0, &val) && (NULL != val)
                && (PMIX_STRING == val->type)) {
            strncpy(value, val->data.string, length);
            PMIX_VALUE_FREE(val, 1);
            return PMI_SUCCESS;
        }
        PMIX_VALUE_FREE(val, 1);
    }
    return PMI_FAIL;
}

/* Barrier only applies to our own nspace, and we want all
 * data to be collected upon completion */
int PMI_Barrier(void)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_info_t buf;
    int ninfo = 0;
    pmix_info_t *info = NULL;

    PMI_CHECK();

    if (data_commited) {
        bool val = 1;
        info = &buf;
        PMIX_INFO_CONSTRUCT(info);
        PMIX_INFO_LOAD(info, PMIX_COLLECT_DATA, &val, PMIX_BOOL);
        ninfo = 1;
    }
    rc = PMIx_Fence(NULL, 0, info, ninfo);

    if (NULL != info) {
        PMIX_INFO_DESTRUCT(info);
    }
    return rc;
}

int PMI_Get_size(int *size)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t *val;

    PMI_CHECK();

    if (NULL == size) {
        return PMI_ERR_INVALID_ARG;
    }

    if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_JOB_SIZE, NULL, 0, &val)) {
        rc = convert_int(size, val);
        PMIX_VALUE_RELEASE(val);
        return convert_err(rc);
    }

    return PMI_FAIL;
}

int PMI_Get_rank(int *rk)
{
    PMI_CHECK();

    if (NULL == rk) {
        return PMI_ERR_INVALID_ARG;
    }

    *rk = myproc.rank;
    return PMI_SUCCESS;
}

int PMI_Get_universe_size(int *size)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t *val;

    PMI_CHECK();

    if (NULL == size) {
        return PMI_ERR_INVALID_ARG;
    }

    if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_UNIV_SIZE, NULL, 0, &val)) {
        rc = convert_int(size, val);
        PMIX_VALUE_RELEASE(val);
        return convert_err(rc);
    }
    return PMI_FAIL;
}

int PMI_Get_appnum(int *appnum)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t *val;

    PMI_CHECK();

    if (NULL != appnum &&
        PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_APPNUM, NULL, 0, &val)) {
        rc = convert_int(appnum, val);
        PMIX_VALUE_RELEASE(val);
        return convert_err(rc);
    }

    return PMI_FAIL;
}

int PMI_Publish_name(const char service_name[], const char port[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_info_t info;

    PMI_CHECK();

    if (NULL == service_name || NULL == port) {
        return PMI_ERR_INVALID_ARG;
    }

    /* pass the service/port */
    (void) strncpy(info.key, service_name, PMIX_MAX_KEYLEN);
    info.value.type = PMIX_STRING;
    info.value.data.string = (char*) port;

    /* publish the info - PMI-1 doesn't support
     * any scope other than inside our own nspace */
    rc = PMIx_Publish(&info, 1);

    return convert_err(rc);
}

int PMI_Unpublish_name(const char service_name[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    char *keys[2];

    PMI_CHECK();

    if (NULL == service_name) {
        return PMI_ERR_INVALID_ARG;
    }

    /* pass the service */
    keys[0] = (char*) service_name;
    keys[1] = NULL;

    rc = PMIx_Unpublish(keys, NULL, 0);
    return convert_err(rc);
}

int PMI_Lookup_name(const char service_name[], char port[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_pdata_t pdata;

    PMI_CHECK();

    if (NULL == service_name || NULL == port) {
        return PMI_ERR_INVALID_ARG;
    }

    PMIX_PDATA_CONSTRUCT(&pdata);

    /* pass the service */
    (void) strncpy(pdata.key, service_name, PMIX_MAX_KEYLEN);

    /* PMI-1 doesn't want the nspace back */
    if (PMIX_SUCCESS != (rc = PMIx_Lookup(&pdata, 1, NULL, 0))) {
        return convert_err(rc);
    }

    /* should have received a string back */
    if (PMIX_STRING != pdata.value.type || NULL == pdata.value.data.string) {
        return convert_err(PMIX_ERR_NOT_FOUND);
    }

    /* return the port - sadly, this API doesn't tell us
     * the size of the port array, and so there is a
     * potential we could overrun it. As this feature
     * isn't widely supported in PMI-1, try being
     * conservative */
    (void) strncpy(port, pdata.value.data.string, PMIX_MAX_KEYLEN);
    PMIX_PDATA_DESTRUCT(&pdata);

    return PMIX_SUCCESS;
}

int PMI_Get_id(char id_str[], int length)
{
    /* we already obtained our nspace during PMI_Init,
     * so all we have to do here is return it */

    PMI_CHECK();

    /* bozo check */
    if (NULL == id_str) {
        return PMI_ERR_INVALID_ARGS;
    }
    if (length < PMI_MAX_ID_LEN) {
        return PMI_ERR_INVALID_LENGTH;
    }

    (void) strncpy(id_str, myproc.nspace, length);
    return PMI_SUCCESS;
}

int PMI_Get_kvs_domain_id(char id_str[], int length)
{
    PMI_CHECK();

    /* same as PMI_Get_id */
    return PMI_Get_id(id_str, length);
}

int PMI_Get_id_length_max(int *length)
{
    PMI_CHECK();

    if (NULL == length) {
        return PMI_ERR_INVALID_VAL_LENGTH;
    }

    *length = PMI_MAX_ID_LEN;
    return PMI_SUCCESS;
}

int PMI_Get_clique_size(int *size)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t *val;

    PMI_CHECK();

    if (NULL == size) {
        return PMI_ERR_INVALID_ARGS;
    }

    if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_LOCAL_SIZE, NULL, 0, &val)) {
        rc = convert_int(size, val);
        PMIX_VALUE_RELEASE(val);
        return convert_err(rc);
    }

    return PMI_FAIL;
}

int PMI_Get_clique_ranks(int ranks[], int length)
{
    pmix_value_t *val;
    char **rks;
    int i;

    PMI_CHECK();

    if (NULL == ranks) {
        return PMI_ERR_INVALID_ARGS;
    }

    if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_LOCAL_PEERS, NULL, 0, &val)) {
        /* kv will contain a string of comma-separated
         * ranks on my node */
        rks = pmix_argv_split(val->data.string, ',');
        for (i = 0; NULL != rks[i] && i < length; i++) {
            ranks[i] = strtol(rks[i], NULL, 10);
        }
        pmix_argv_free(rks);
        PMIX_VALUE_RELEASE(val);
        return PMI_SUCCESS;
    }
    return PMI_FAIL;
}

int PMI_KVS_Get_my_name(char kvsname[], int length)
{
    PMI_CHECK();

    /* same as PMI_Get_id */
    return PMI_Get_id(kvsname, length);
}

int PMI_KVS_Get_name_length_max(int *length)
{
    PMI_CHECK();

    if (NULL == length) {
        return PMI_ERR_INVALID_ARG;
    }

    *length = PMI_MAX_KVSNAME_LEN;
    return PMI_SUCCESS;
}

int PMI_KVS_Get_key_length_max(int *length)
{
    PMI_CHECK();

    if (NULL == length) {
        return PMI_ERR_INVALID_ARG;
    }

    *length = PMI_MAX_KEY_LEN;
    return PMI_SUCCESS;
}

int PMI_KVS_Get_value_length_max(int *length)
{
    PMI_CHECK();

    if (NULL == length) {
        return PMI_ERR_INVALID_ARG;
    }

    /* don't give them an enormous size of some implementations
     * immediately malloc a data block for their use */
    *length = PMI_MAX_VAL_LEN;
    return PMI_SUCCESS;
}

/* nobody supports this call, which is why it was
 * dropped for PMI-2 */
int PMI_KVS_Create(char kvsname[], int length)
{
    return PMI_FAIL;
}

/* nobody supports this call, which is why it was
 * dropped for PMI-2 */
int PMI_KVS_Destroy(const char kvsname[])
{
    return PMI_FAIL;
}

/* nobody supports this call, which is why it was
 * dropped for PMI-2 */
int PMI_KVS_Iter_first(const char kvsname[], char key[], int key_len, char val[], int val_len)
{
    return PMI_FAIL;
}

/* nobody supports this call, which is why it was
 * dropped for PMI-2 */
int PMI_KVS_Iter_next(const char kvsname[], char key[], int key_len, char val[], int val_len)
{
    return PMI_FAIL;
}

int PMI_Spawn_multiple(int count,
                       const char * cmds[],
                       const char ** argvs[],
                       const int maxprocs[],
                       const int info_keyval_sizesp[],
                       const PMI_keyval_t * info_keyval_vectors[],
                       int preput_keyval_size,
                       const PMI_keyval_t preput_keyval_vector[],
                       int errors[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_app_t *apps;
    int i, k;
    size_t j;
    char *evar;

    PMI_CHECK();

    if (NULL == cmds) {
        return PMI_ERR_INVALID_ARG;
    }

    /* setup the apps */
    PMIX_APP_CREATE(apps, count);
    for (i = 0; i < count; i++) {
        apps[i].cmd = strdup(cmds[i]);
        apps[i].maxprocs = maxprocs[i];
        apps[i].argv = pmix_argv_copy((char**) argvs[i]);
        apps[i].argc = pmix_argv_count(apps[i].argv);
        apps[i].ninfo = info_keyval_sizesp[i];
        if (0 < apps[i].ninfo) {
            apps[i].info = (pmix_info_t*)malloc(apps[i].ninfo * sizeof(pmix_info_t));
            /* copy the info objects */
            for (j = 0; j < apps[i].ninfo; j++) {
                (void)strncpy(apps[i].info[j].key, info_keyval_vectors[i][j].key, PMIX_MAX_KEYLEN);
                apps[i].info[j].value.type = PMIX_STRING;
                apps[i].info[j].value.data.string = strdup(info_keyval_vectors[i][j].val);
            }
        }
        /* push the preput values into the apps environ */
        for (k = 0; k < preput_keyval_size; k++) {
            (void)asprintf(&evar, "%s=%s", preput_keyval_vector[k].key, preput_keyval_vector[k].val);
            pmix_argv_append_nosize(&apps[i].env, evar);
            free(evar);
        }
    }

    rc = PMIx_Spawn(NULL, 0, apps, count, NULL);
    /* tear down the apps array */
    for (i = 0; i < count; i++) {
        PMIX_APP_DESTRUCT(&apps[i]);
    }
    free(apps);
    if (NULL != errors) {
        for (i = 0; i < count; i++) {
            errors[i] = convert_err(rc);
        }
    }
    return convert_err(rc);
}

/* nobody supports this call, which is why it was
 * dropped for PMI-2 */
int PMI_Parse_option(int num_args, char *args[], int *num_parsed, PMI_keyval_t **keyvalp, int *size)
{
    return PMI_FAIL;
}

/* nobody supports this call, which is why it was
 * dropped for PMI-2 */
int PMI_Args_to_keyval(int *argcp, char *((*argvp)[]), PMI_keyval_t **keyvalp, int *size)
{
    return PMI_FAIL;
}

/* nobody supports this call, which is why it was
 * dropped for PMI-2 */
int PMI_Free_keyvals(PMI_keyval_t keyvalp[], int size)
{
    return PMI_FAIL;
}

/* nobody supports this call, which is why it was
 * dropped for PMI-2 */
int PMI_Get_options(char *str, int *length)
{
    return PMI_FAIL;
}

/***   UTILITY FUNCTIONS   ***/
/* internal function */
static pmix_status_t convert_int(int *value, pmix_value_t *kv)
{
    switch (kv->type) {
    case PMIX_INT:
        *value = kv->data.integer;
        break;
    case PMIX_INT8:
        *value = kv->data.int8;
        break;
    case PMIX_INT16:
        *value = kv->data.int16;
        break;
    case PMIX_INT32:
        *value = kv->data.int32;
        break;
    case PMIX_INT64:
        *value = kv->data.int64;
        break;
    case PMIX_UINT:
        *value = kv->data.uint;
        break;
    case PMIX_UINT8:
        *value = kv->data.uint8;
        break;
    case PMIX_UINT16:
        *value = kv->data.uint16;
        break;
    case PMIX_UINT32:
        *value = kv->data.uint32;
        break;
    case PMIX_UINT64:
        *value = kv->data.uint64;
        break;
    case PMIX_BYTE:
        *value = kv->data.byte;
        break;
    case PMIX_SIZE:
        *value = kv->data.size;
        break;
    case PMIX_BOOL:
        *value = kv->data.flag;
        break;
    default:
        /* not an integer type */
        return PMIX_ERR_BAD_PARAM;
    }
    return PMIX_SUCCESS;
}

static int convert_err(pmix_status_t rc)
{
    switch (rc) {
    case PMIX_ERR_INVALID_SIZE:
        return PMI_ERR_INVALID_SIZE;

    case PMIX_ERR_INVALID_KEYVALP:
        return PMI_ERR_INVALID_KEYVALP;

    case PMIX_ERR_INVALID_NUM_PARSED:
        return PMI_ERR_INVALID_NUM_PARSED;

    case PMIX_ERR_INVALID_ARGS:
        return PMI_ERR_INVALID_ARGS;

    case PMIX_ERR_INVALID_NUM_ARGS:
        return PMI_ERR_INVALID_NUM_ARGS;

    case PMIX_ERR_INVALID_LENGTH:
        return PMI_ERR_INVALID_LENGTH;

    case PMIX_ERR_INVALID_VAL_LENGTH:
        return PMI_ERR_INVALID_VAL_LENGTH;

    case PMIX_ERR_INVALID_VAL:
        return PMI_ERR_INVALID_VAL;

    case PMIX_ERR_INVALID_KEY_LENGTH:
        return PMI_ERR_INVALID_KEY_LENGTH;

    case PMIX_ERR_INVALID_KEY:
        return PMI_ERR_INVALID_KEY;

    case PMIX_ERR_INVALID_ARG:
        return PMI_ERR_INVALID_ARG;

    case PMIX_ERR_NOMEM:
        return PMI_ERR_NOMEM;

    case PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER:
    case PMIX_ERR_COMM_FAILURE:
    case PMIX_ERR_NOT_IMPLEMENTED:
    case PMIX_ERR_NOT_SUPPORTED:
    case PMIX_ERR_NOT_FOUND:
    case PMIX_ERR_SERVER_NOT_AVAIL:
    case PMIX_ERR_INVALID_NAMESPACE:
    case PMIX_ERR_DATA_VALUE_NOT_FOUND:
    case PMIX_ERR_OUT_OF_RESOURCE:
    case PMIX_ERR_RESOURCE_BUSY:
    case PMIX_ERR_BAD_PARAM:
    case PMIX_ERR_IN_ERRNO:
    case PMIX_ERR_UNREACH:
    case PMIX_ERR_TIMEOUT:
    case PMIX_ERR_NO_PERMISSIONS:
    case PMIX_ERR_PACK_MISMATCH:
    case PMIX_ERR_PACK_FAILURE:
    case PMIX_ERR_UNPACK_FAILURE:
    case PMIX_ERR_UNPACK_INADEQUATE_SPACE:
    case PMIX_ERR_TYPE_MISMATCH:
    case PMIX_ERR_PROC_ENTRY_NOT_FOUND:
    case PMIX_ERR_UNKNOWN_DATA_TYPE:
    case PMIX_ERR_WOULD_BLOCK:
    case PMIX_EXISTS:
    case PMIX_ERROR:
        return PMI_FAIL;

    case PMIX_ERR_INIT:
        return PMI_ERR_INIT;

    case PMIX_SUCCESS:
        return PMI_SUCCESS;
    default:
        return PMI_FAIL;
    }
}
