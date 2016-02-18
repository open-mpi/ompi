/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
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
#include <pmi2.h>

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


#define PMI2_CHECK() \
    do {                     \
        if (!pmi2_init) {     \
            return PMI2_FAIL; \
        }                    \
    } while (0)

/* local functions */
static pmix_status_t convert_int(int *value, pmix_value_t *kv);
static int convert_err(pmix_status_t rc);
static pmix_proc_t myproc;
static int pmi2_init = 0;

int PMI2_Init(int *spawned, int *size, int *rank, int *appnum)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t *val;
    pmix_proc_t proc;
    pmix_info_t info[1];
    bool  val_optinal = 1;

    if (PMIX_SUCCESS != PMIx_Init(&myproc)) {
        return PMI2_ERR_INIT;
    }

    /* get the rank */
    *rank = myproc.rank;

    /* getting internal key requires special rank value */
    memcpy(&proc, &myproc, sizeof(myproc));
    proc.rank = PMIX_RANK_UNDEF;

    /* set controlling parameters
     * PMIX_OPTIONAL - expect that these keys should be available on startup
     */
    PMIX_INFO_CONSTRUCT(&info[0]);
    PMIX_INFO_LOAD(&info[0], PMIX_OPTIONAL, &val_optinal, PMIX_BOOL);

    if (NULL != size) {
        /* get the universe size - this will likely pull
         * down all attributes assigned to the job, thus
         * making all subsequent "get" operations purely
         * local */
        if (PMIX_SUCCESS == PMIx_Get(&proc, PMIX_UNIV_SIZE, info, 1, &val)) {
            rc = convert_int(size, val);
            PMIX_VALUE_RELEASE(val);
            if (PMIX_SUCCESS != rc) {
                goto error;
            }
        } else {
            /* cannot continue without this info */
            rc = PMIX_ERR_INIT;
            goto error;
        }
    }

    if (NULL != spawned) {
        /* get the spawned flag */
        if (PMIX_SUCCESS == PMIx_Get(&proc, PMIX_SPAWNED, info, 1, &val)) {
            rc = convert_int(spawned, val);
            PMIX_VALUE_RELEASE(val);
            if (PMIX_SUCCESS != rc) {
                goto error;
            }
        } else {
            /* if not found, default to not spawned */
            *spawned = 0;
        }
    }

    if (NULL != appnum) {
        /* get our appnum */
        if (PMIX_SUCCESS == PMIx_Get(&proc, PMIX_APPNUM, info, 1, &val)) {
            rc = convert_int(appnum, val);
            PMIX_VALUE_RELEASE(val);
            if (PMIX_SUCCESS != rc) {
                goto error;
            }
        } else {
            /* if not found, default to 0 */
            *appnum = 0;
        }
    }
    pmi2_init = 1;

    rc = PMIX_SUCCESS;

error:
    PMIX_INFO_DESTRUCT(&info[0]);

    return convert_err(rc);
}

int PMI2_Initialized(void)
{
    int initialized;
    initialized = (int)PMIx_Initialized();
    return initialized;
}

int PMI2_Finalize(void)
{
    pmix_status_t rc = PMIX_SUCCESS;

    PMI2_CHECK();

    pmi2_init = 0;
    rc = PMIx_Finalize();
    return convert_err(rc);
}

int PMI2_Abort(int flag, const char msg[])
{
    pmix_status_t rc = PMIX_SUCCESS;

    PMI2_CHECK();

    rc = PMIx_Abort(flag, msg, NULL, 0);
    return convert_err(rc);
}

/* KVS_Put - we default to PMIX_GLOBAL scope */
int PMI2_KVS_Put(const char key[], const char value[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t val;

    PMI2_CHECK();

    if ((NULL == key) || (NULL == value)) {
        return PMI2_ERR_INVALID_ARG;
    }

    pmix_output_verbose(3, pmix_globals.debug_output,
            "PMI2_KVS_Put: key=%s value=%s", key, value);

    val.type = PMIX_STRING;
    val.data.string = (char*)value;
    rc = PMIx_Put(PMIX_GLOBAL, key, &val);
    return convert_err(rc);
}

/* KVS_Fence */
int PMI2_KVS_Fence(void)
{
    pmix_status_t rc = PMIX_SUCCESS;

    PMI2_CHECK();

    pmix_output_verbose(3, pmix_globals.debug_output, "PMI2_KVS_Fence");

    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        return convert_err(rc);
    }

    /* we want all data to be collected upon completion */
    {
        pmix_info_t info[1];
        bool  val_data = 1;

        /* set controlling parameters
         * PMIX_COLLECT_DATA - meet legacy PMI2 requirement
         */
        PMIX_INFO_CONSTRUCT(&info[0]);
        PMIX_INFO_LOAD(&info[0], PMIX_COLLECT_DATA, &val_data, PMIX_BOOL);

        rc = PMIx_Fence(NULL, 0, &info[0], 1);
        PMIX_INFO_DESTRUCT(&info[0]);
    }

    return convert_err(rc);
}

/* the jobid is equated to the nspace in PMIx, and the
 * src_pmi_id equates to the rank. If jobid=NULL, then PMIx
 * will use the local nspace, which matches the PMI2 spec.
 * The only type of value supported by PMI2 is a string, so
 * the return of anything else is an error */
int PMI2_KVS_Get(const char *jobid, int src_pmi_id,
                 const char key[], char value [],
                 int maxvalue, int *vallen)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t *val;
    pmix_proc_t proc;

    PMI2_CHECK();

    /* set default */
    *vallen = 0;

    if ((NULL == key) || (NULL == value)) {
        return PMI2_ERR_INVALID_ARG;
    }

    pmix_output_verbose(3, pmix_globals.debug_output,
            "PMI2_KVS_Get: key=%s jobid=%s src_pmi_id=%d", key, (jobid ? jobid : "null"), src_pmi_id);

    (void)strncpy(proc.nspace, (jobid ? jobid : myproc.nspace), PMIX_MAX_NSLEN);
    if (src_pmi_id == PMI2_ID_NULL) {
        /* the rank is UNDEF */
        proc.rank = PMIX_RANK_UNDEF;
    } else {
        proc.rank = src_pmi_id;
    }

    rc = PMIx_Get(&proc, key, NULL, 0, &val);
    if (PMIX_SUCCESS == rc && NULL != val) {
        if (PMIX_STRING != val->type) {
            rc = PMIX_ERROR;
        } else if (NULL != val->data.string) {
            (void)strncpy(value, val->data.string, maxvalue);
            *vallen = strlen(val->data.string);
        }
        PMIX_VALUE_RELEASE(val);
    }

    return convert_err(rc);
}

int PMI2_Info_GetNodeAttr(const char name[], char value[], int valuelen, int *found, int waitfor)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t *val;
    pmix_info_t info[1];
    bool  val_optinal = 1;

    PMI2_CHECK();

    if ((NULL == name) || (NULL == value) || (NULL == found)) {
        return PMI2_ERR_INVALID_ARG;
    }

    /* set controlling parameters
     * PMIX_OPTIONAL - expect that these keys should be available on startup
     */
    PMIX_INFO_CONSTRUCT(&info[0]);
    PMIX_INFO_LOAD(&info[0], PMIX_OPTIONAL, &val_optinal, PMIX_BOOL);

    *found = 0;
    rc = PMIx_Get(&myproc, name, info, 1, &val);
    if (PMIX_SUCCESS == rc && NULL != val) {
        if (PMIX_STRING != val->type) {
            rc = PMIX_ERROR;
        } else if (NULL != val->data.string) {
            (void)strncpy(value, val->data.string, valuelen);
            *found = 1;
        }
        PMIX_VALUE_RELEASE(val);
    } else if (PMIX_ERR_NOT_FOUND == rc) {
        rc = PMIX_SUCCESS;
    }

    PMIX_INFO_DESTRUCT(&info[0]);

    return convert_err(rc);
}

/* push info at the PMIX_LOCAL scope */
int PMI2_Info_PutNodeAttr(const char name[], const char value[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t val;

    PMI2_CHECK();

    if ((NULL == name) || (NULL == value)) {
        return PMI2_ERR_INVALID_ARG;
    }

    val.type = PMIX_STRING;
    val.data.string = (char*)value;
    rc = PMIx_Put(PMIX_LOCAL, name, &val);
    return convert_err(rc);
}

int PMI2_Info_GetJobAttr(const char name[], char value[], int valuelen, int *found)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_value_t *val;
    pmix_proc_t proc;
    pmix_info_t info[1];
    bool  val_optinal = 1;

    PMI2_CHECK();

    if ((NULL == name) || (NULL == value) || (NULL == found)) {
        return PMI2_ERR_INVALID_ARG;
    }

    /* getting internal key requires special rank value */
    memcpy(&proc, &myproc, sizeof(myproc));
    proc.rank = PMIX_RANK_UNDEF;

    /* set controlling parameters
     * PMIX_OPTIONAL - expect that these keys should be available on startup
     */
    PMIX_INFO_CONSTRUCT(&info[0]);
    PMIX_INFO_LOAD(&info[0], PMIX_OPTIONAL, &val_optinal, PMIX_BOOL);

    *found = 0;
    rc = PMIx_Get(&proc, name, info, 1, &val);
    if (PMIX_SUCCESS == rc && NULL != val) {
        if (PMIX_STRING != val->type) {
            rc = PMIX_ERROR;
        } else if (NULL != val->data.string) {
            (void)strncpy(value, val->data.string, valuelen);
            *found = 1;
        }
        PMIX_VALUE_RELEASE(val);
    } else if (PMIX_ERR_NOT_FOUND == rc) {
        rc = PMIX_SUCCESS;
    }

    PMIX_INFO_DESTRUCT(&info[0]);

    return convert_err(rc);
}

int PMI2_Info_GetJobAttrIntArray(const char name[], int array[], int arraylen, int *outlen, int *found)
{
    return PMI2_FAIL;
}

int PMI2_Nameserv_publish(const char service_name[], const PMI_keyval_t *info_ptr, const char port[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    int nvals;
    pmix_info_t info[2];

    PMI2_CHECK();

    if (NULL == service_name || NULL == port) {
        return PMI2_ERR_INVALID_ARG;
    }

    /* pass the service/port */
    (void)strncpy(info[0].key, service_name, PMIX_MAX_KEYLEN);
    info[0].value.type = PMIX_STRING;
    info[0].value.data.string = (char*)port;
    nvals = 1;

    /* if provided, add any other value */
    if (NULL != info_ptr) {
        (void)strncpy(info[1].key, info_ptr->key, PMIX_MAX_KEYLEN);
        info[1].value.type = PMIX_STRING;
        info[1].value.data.string = (char*)info_ptr->val;
        nvals = 2;
    }
    /* publish the info - PMI-2 doesn't support
     * any scope other than inside our own nspace */
    rc = PMIx_Publish(info, nvals);

    return convert_err(rc);
}

int PMI2_Nameserv_unpublish(const char service_name[],
                           const PMI_keyval_t *info_ptr)
{
    pmix_status_t rc = PMIX_SUCCESS;
    char *keys[3];

    PMI2_CHECK();

    if (NULL == service_name || NULL == info_ptr) {
        return PMI2_ERR_INVALID_ARG;
    }

    /* pass the service */
    keys[0] = (char*)service_name;
    keys[1] = NULL;
    keys[2] = NULL;

    /* if provided, add any other value */
    if (NULL != info_ptr) {
        keys[1] = info_ptr->key;
    }

    rc = PMIx_Unpublish(keys, NULL, 0);
    return convert_err(rc);
}

int PMI2_Nameserv_lookup(const char service_name[], const PMI_keyval_t *info_ptr,
                         char port[], int portLen)
{
    pmix_status_t rc = PMIX_SUCCESS;
    int nvals;
    pmix_pdata_t pdata[2];

    PMI2_CHECK();

    if (NULL == service_name || NULL == info_ptr || NULL == port) {
        return PMI2_ERR_INVALID_ARG;
    }

    PMIX_PDATA_CONSTRUCT(&pdata[0]);
    PMIX_PDATA_CONSTRUCT(&pdata[1]);

    /* pass the service */
    (void)strncpy(pdata[0].key, service_name, PMIX_MAX_KEYLEN);
    nvals = 1;

    /* if provided, add any other value */
    if (NULL != info_ptr) {
        (void)strncpy(pdata[1].key, info_ptr->key, PMIX_MAX_KEYLEN);
        pdata[1].value.type = PMIX_STRING;
        pdata[1].value.data.string = info_ptr->val;
        nvals = 2;
    }

    /* lookup the info */
    if (PMIX_SUCCESS != (rc = PMIx_Lookup(pdata, nvals, NULL, 0))) {
        PMIX_PDATA_DESTRUCT(&pdata[0]);
        PMIX_PDATA_DESTRUCT(&pdata[1]);
        return convert_err(rc);
    }

    /* should have received a string back */
    if (PMIX_STRING != pdata[0].value.type ||
        NULL == pdata[0].value.data.string) {
        PMIX_PDATA_DESTRUCT(&pdata[0]);
        PMIX_PDATA_DESTRUCT(&pdata[1]);
        return PMI2_FAIL;
    }

    /* return the port */
    (void)strncpy(port, pdata[0].value.data.string, portLen);
    PMIX_PDATA_DESTRUCT(&pdata[0]);

    if (NULL != info_ptr) {
    }
    PMIX_PDATA_DESTRUCT(&pdata[1]);

    return PMI2_SUCCESS;
}

int PMI2_Job_GetId(char jobid[], int jobid_size)
{
    /* we already obtained our nspace during pmi2_init,
     * so all we have to do here is return it */

    PMI2_CHECK();

    /* bozo check */
    if (NULL == jobid) {
        return PMI2_ERR_INVALID_ARGS;
    }
    (void)strncpy(jobid, myproc.nspace, jobid_size);
    return PMI2_SUCCESS;
}

int PMI2_Job_GetRank(int *rank)
{
    PMI2_CHECK();

    if (NULL == rank) {
        return PMI2_ERR_INVALID_ARGS;
    }
    *rank = myproc.rank;
    return PMI2_SUCCESS;
}

int PMI2_Info_GetSize(int *size)
{
    pmix_status_t rc = PMIX_ERROR;
    pmix_value_t *val;
    pmix_info_t info[1];
    bool  val_optinal = 1;

    PMI2_CHECK();

    if (NULL == size) {
        return PMI2_ERR_INVALID_ARGS;
    }

    /* set controlling parameters
     * PMIX_OPTIONAL - expect that these keys should be available on startup
     */
    PMIX_INFO_CONSTRUCT(&info[0]);
    PMIX_INFO_LOAD(&info[0], PMIX_OPTIONAL, &val_optinal, PMIX_BOOL);

    if (PMIX_SUCCESS == PMIx_Get(&myproc, PMIX_LOCAL_SIZE, info, 1, &val)) {
        rc = convert_int(size, val);
        PMIX_VALUE_RELEASE(val);
    }

    PMIX_INFO_DESTRUCT(&info[0]);

    return convert_err(rc);
}

int PMI2_Job_Connect(const char jobid[], PMI2_Connect_comm_t *conn)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_proc_t proc;

    PMI2_CHECK();

    if (NULL == conn) {
        return PMI2_ERR_INVALID_ARGS;
    }

    (void)strncpy(proc.nspace, (jobid ? jobid : myproc.nspace), sizeof(myproc.nspace));
    proc.rank = PMIX_RANK_WILDCARD;
    rc = PMIx_Connect(&proc, 1, NULL, 0);
    return convert_err(rc);
}

int PMI2_Job_Disconnect(const char jobid[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_proc_t proc;

    PMI2_CHECK();

    (void)strncpy(proc.nspace, (jobid ? jobid : myproc.nspace), sizeof(myproc.nspace));
    proc.rank = PMIX_RANK_WILDCARD;
    rc = PMIx_Disconnect(&proc, 1, NULL, 0);
    return convert_err(rc);
}

int PMI2_Job_Spawn(int count, const char * cmds[],
                   int argcs[], const char ** argvs[],
                   const int maxprocs[],
                   const int info_keyval_sizes[],
                   const PMI_keyval_t *info_keyval_vectors[],
                   int preput_keyval_size,
                   const PMI_keyval_t *preput_keyval_vector[],
                   char jobId[], int jobIdSize,
                   int errors[])
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_app_t *apps;
    int i, k;
    size_t j;
    char *evar;

    PMI2_CHECK();

    if (NULL == cmds) {
        return PMI2_ERR_INVALID_ARGS;
    }

    /* setup the apps */
    PMIX_APP_CREATE(apps, count);
    for (i=0; i < count; i++) {
        apps[i].cmd = strdup(cmds[i]);
        apps[i].maxprocs = maxprocs[i];
        apps[i].argv = pmix_argv_copy((char**)argvs[i]);
        apps[i].argc = pmix_argv_count(apps[i].argv);
        apps[i].ninfo = info_keyval_sizes[i];
        apps[i].info = (pmix_info_t*)malloc(apps[i].ninfo * sizeof(pmix_info_t));
        /* copy the info objects */
        for (j=0; j < apps[i].ninfo; j++) {
            (void)strncpy(apps[i].info[j].key, info_keyval_vectors[i][j].key, PMIX_MAX_KEYLEN);
            apps[i].info[j].value.type = PMIX_STRING;
            apps[i].info[j].value.data.string = strdup(info_keyval_vectors[i][j].val);
        }
        /* push the preput values into the apps environ */
        for (k=0; k < preput_keyval_size; k++) {
            (void)asprintf(&evar, "%s=%s", preput_keyval_vector[j]->key, preput_keyval_vector[j]->val);
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
    switch(rc) {
    case PMIX_ERR_INVALID_SIZE:
        return PMI2_ERR_INVALID_SIZE;

    case PMIX_ERR_INVALID_KEYVALP:
        return PMI2_ERR_INVALID_KEYVALP;

    case PMIX_ERR_INVALID_NUM_PARSED:
        return PMI2_ERR_INVALID_NUM_PARSED;

    case PMIX_ERR_INVALID_ARGS:
        return PMI2_ERR_INVALID_ARGS;

    case PMIX_ERR_INVALID_NUM_ARGS:
        return PMI2_ERR_INVALID_NUM_ARGS;

    case PMIX_ERR_INVALID_LENGTH:
        return PMI2_ERR_INVALID_LENGTH;

    case PMIX_ERR_INVALID_VAL_LENGTH:
        return PMI2_ERR_INVALID_VAL_LENGTH;

    case PMIX_ERR_INVALID_VAL:
        return PMI2_ERR_INVALID_VAL;

    case PMIX_ERR_INVALID_KEY_LENGTH:
        return PMI2_ERR_INVALID_KEY_LENGTH;

    case PMIX_ERR_INVALID_KEY:
        return PMI2_ERR_INVALID_KEY;

    case PMIX_ERR_INVALID_ARG:
        return PMI2_ERR_INVALID_ARG;

    case PMIX_ERR_NOMEM:
        return PMI2_ERR_NOMEM;

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
        return PMI2_FAIL;

    case PMIX_ERR_INIT:
        return PMI2_ERR_INIT;

    case PMIX_SUCCESS:
        return PMI2_SUCCESS;
    default:
        return PMI2_FAIL;
    }
}
