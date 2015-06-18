/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

#include "src/buffer_ops/buffer_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"

/* local functions */
static pmix_status_t convert_int(int *value, pmix_value_t *kv);
static int convert_err(pmix_status_t rc);
static pmix_proc_t myproc;

int PMI_Init( int *spawned )
{
    pmix_value_t *kv;
    pmix_status_t rc;

    if (PMIX_SUCCESS != PMIx_Init(&myproc)) {
        return PMI_ERR_INIT;
    }

    if (NULL == spawned) {
        return PMI_SUCCESS;
    }

    /* get the spawned flag - this will likely pull
     * down all attributes assigned to the job, thus
     * making all subsequent "get" operations purely
     * local */
    if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_SPAWNED, NULL, 0, &kv)) {
        rc = convert_int(spawned, kv);
        PMIX_VALUE_RELEASE(kv);
        return convert_err(rc);
    }
    /* if it wasn't found, then default to "not spawned" */
    *spawned = 0;
    return PMI_SUCCESS;
}

int PMI_Initialized(PMI_BOOL *initialized)
{
    *initialized = (PMI_BOOL)PMIx_Initialized();
    return PMI_SUCCESS;
}

int PMI_Finalize(void)
{
    return PMIx_Finalize();
}

int PMI_Abort(int flag, const char msg[])
{
    pmix_status_t rc;

    rc = PMIx_Abort(flag, msg, NULL, 0);
    return convert_err(rc);
}

/* KVS_Put - we default to PMIX_GLOBAL scope and ignore the
 * provided kvsname as we only put into our own nspace */
int PMI_KVS_Put(const char kvsname[], const char key[], const char value[])
{
    pmix_status_t rc;
    pmix_value_t val;

    val.type = PMIX_STRING;
    val.data.string = (char*)value;
    rc = PMIx_Put(PMIX_GLOBAL, key, &val);
    return convert_err(rc);
}

/* KVS_Commit */
int PMI_KVS_Commit(const char kvsname[])
{
    pmix_status_t rc;

    rc = PMIx_Commit();
    return convert_err(rc);
}

/* Barrier only applies to our own nspace, and we want all
 * data to be collected upon completion */
int PMI_Barrier(void)
{
    return PMIx_Fence(NULL, 0, NULL, 0);
}

int PMI_Get_size(int *size)
{
    pmix_value_t *kv;
    pmix_status_t rc;

    if (NULL == size) {
        return PMI_FAIL;
    }

    if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_JOB_SIZE, NULL, 0, &kv)) {
        rc = convert_int(size, kv);
        PMIX_VALUE_RELEASE(kv);
        return convert_err(rc);
    }

    return PMI_FAIL;
}

int PMI_Get_rank(int *rk)
{
    if (NULL == rk) {
        return PMI_FAIL;
    }

    *rk = pmix_globals.myid.rank;
    return PMI_SUCCESS;
}

int PMI_Get_universe_size(int *size)
{
    pmix_value_t *kv;
    pmix_status_t rc;

    if (NULL == size) {
        return PMI_FAIL;
    }

    if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_UNIV_SIZE, NULL, 0, &kv)) {
        rc = convert_int(size, kv);
        PMIX_VALUE_RELEASE(kv);
        return convert_err(rc);
    }
    return PMI_FAIL;
}

int PMI_Get_appnum(int *appnum)
{
    pmix_value_t *kv;
    pmix_status_t rc;

    if (NULL != appnum &&
        PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_APPNUM, NULL, 0, &kv)) {
        rc = convert_int(appnum, kv);
        PMIX_VALUE_RELEASE(kv);
        return convert_err(rc);
    }

    return PMI_FAIL;
}

int PMI_Publish_name(const char service_name[], const char port[])
{
    pmix_status_t rc;
    pmix_info_t info;

    if (NULL == service_name || NULL == port) {
        return convert_err(PMIX_ERR_BAD_PARAM);
    }
    /* pass the service/port */
    (void)strncpy(info.key, service_name, PMIX_MAX_KEYLEN);
    info.value.type = PMIX_STRING;
    info.value.data.string = (char*)port;

    /* publish the info - PMI-1 doesn't support
     * any scope other than inside our own nspace */
    rc = PMIx_Publish(PMIX_NAMESPACE, PMIX_PERSIST_APP, &info, 1);

    return convert_err(rc);
}

int PMI_Unpublish_name(const char service_name[])
{
    pmix_status_t rc;
    char *keys[2];

    /* pass the service */
    keys[0] = (char*)service_name;
    keys[1] = NULL;

    rc = PMIx_Unpublish(PMIX_NAMESPACE, keys);
    return convert_err(rc);
}

int PMI_Lookup_name(const char service_name[], char port[])
{
    pmix_status_t rc;
    pmix_pdata_t pdata;

    PMIX_PDATA_CONSTRUCT(&pdata);

    /* pass the service */
    (void)strncpy(pdata.key, service_name, PMIX_MAX_KEYLEN);

    /* PMI-1 doesn't want the nspace back */
    if (PMIX_SUCCESS != (rc = PMIx_Lookup(PMIX_NAMESPACE, NULL, 0, &pdata, 1))) {
        return convert_err(rc);
    }

    /* should have received a string back */
    if (PMIX_STRING != pdata.value.type ||
        NULL == pdata.value.data.string) {
        return convert_err(PMIX_ERR_NOT_FOUND);
    }

    /* return the port */
    (void)strncpy(port, pdata.value.data.string, PMIX_MAX_VALLEN);
    PMIX_PDATA_DESTRUCT(&pdata);

    return PMIX_SUCCESS;
}

int PMI_Get_id(char id_str[], int length)
{
    /* we already obtained our nspace during PMI_Init,
     * so all we have to do here is return it */

    /* bozo check */
    if (NULL == id_str) {
        return PMI_ERR_INVALID_ARGS;
    }
    (void)strncpy(id_str, pmix_globals.myid.nspace, length);
    return PMI_SUCCESS;
}

int PMI_Get_kvs_domain_id(char id_str[], int length)
{
    /* same as PMI_Get_id */
    return PMI_Get_id(id_str, length);
}

int PMI_Get_id_length_max(int *length)
{
    if (NULL == length) {
        return PMI_ERR_INVALID_VAL_LENGTH;
    }
    *length = PMIX_MAX_VALLEN;
    return PMI_SUCCESS;
}

int PMI_Get_clique_size(int *size)
{
    pmix_value_t *kv;
    pmix_status_t rc;

    if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_LOCAL_SIZE, NULL, 0, &kv)) {
        rc = convert_int(size, kv);
        PMIX_VALUE_RELEASE(kv);
        return convert_err(rc);
    }

    return PMI_FAIL;
}

int PMI_Get_clique_ranks(int ranks[], int length)
{
    pmix_value_t *kv;
    char **rks;
    int i;

    if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_LOCAL_PEERS, NULL, 0, &kv)) {
        /* kv will contain a string of comma-separated
         * ranks on my node */
        rks = pmix_argv_split(kv->data.string, ',');
        for (i=0; NULL != rks[i] && i < length; i++) {
            ranks[i] = strtol(rks[i], NULL, 10);
        }
        pmix_argv_free(rks);
        PMIX_VALUE_RELEASE(kv);
        return PMI_SUCCESS;
    }
    return PMI_FAIL;
}

int PMI_KVS_Get_my_name(char kvsname[], int length)
{
    /* same as PMI_Get_id */
    return PMI_Get_id(kvsname, length);
}

int PMI_KVS_Get_name_length_max(int *length)
{
   if (NULL == length) {
        return PMI_ERR_INVALID_VAL_LENGTH;
    }
    *length = PMIX_MAX_NSLEN;
    return PMI_SUCCESS;
}

int PMI_KVS_Get_key_length_max(int *length)
{
    if (NULL == length) {
        return PMI_ERR_INVALID_VAL_LENGTH;
    }
    *length = PMIX_MAX_KEYLEN;
    return PMI_SUCCESS;
}

int PMI_KVS_Get_value_length_max(int *length)
{
    if (NULL == length) {
        return PMI_ERR_INVALID_VAL_LENGTH;
    }
    *length = PMIX_MAX_VALLEN;
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
    pmix_app_t *apps;
    int i, k;
    pmix_status_t rc;
    size_t j;
    char *evar;

    /* setup the apps */
    PMIX_APP_CREATE(apps, count);
    for (i=0; i < count; i++) {
        apps[i].cmd = strdup(cmds[i]);
        apps[i].maxprocs = maxprocs[i];
        apps[i].argv = pmix_argv_copy((char**)argvs[i]);
        apps[i].argc = pmix_argv_count(apps[i].argv);
        apps[i].ninfo = info_keyval_sizesp[i];
        if (0 < apps[i].ninfo) {
            apps[i].info = (pmix_info_t*)malloc(apps[i].ninfo * sizeof(pmix_info_t));
            /* copy the info objects */
            for (j=0; j < apps[i].ninfo; j++) {
                (void)strncpy(apps[i].info[j].key, info_keyval_vectors[i][j].key, PMIX_MAX_KEYLEN);
                apps[i].info[j].value.type = PMIX_STRING;
                apps[i].info[j].value.data.string = strdup(info_keyval_vectors[i][j].val);
            }
        }
        /* push the preput values into the apps environ */
        for (k=0; k < preput_keyval_size; k++) {
            (void)asprintf(&evar, "%s=%s", preput_keyval_vector[k].key, preput_keyval_vector[k].val);
            pmix_argv_append_nosize(&apps[i].env, evar);
            free(evar);
        }
    }

    rc = PMIx_Spawn(NULL, 0, apps, count, NULL);
    /* tear down the apps array */
    for (i=0; i < count; i++) {
        PMIX_APP_DESTRUCT(&apps[i]);
    }
    free(apps);
    if (NULL != errors) {
        for (i=0; i < count; i++) {
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
    switch(kv->type) {
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
    default:
        /* not an integer type */
        return PMIX_ERR_BAD_PARAM;
    }
    return PMIX_SUCCESS;
}

static int convert_err(pmix_status_t rc)
{
    switch(rc) {
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
