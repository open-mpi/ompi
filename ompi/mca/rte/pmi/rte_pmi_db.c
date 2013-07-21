/*
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <stdio.h>
#include <pmi.h>
#if WANT_PMI2_SUPPORT
#include <pmi2.h>
#endif

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/dss/dss.h"
#include "opal/threads/tsd.h"
#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"
#include "ompi/constants.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/mca/rte/base/base.h"

#include "rte_pmi.h"
#include "rte_pmi_internal.h"

#define OMPI_PMI_PAD  10

/* Local variables */
static char *pmi_kvs_name = NULL;
static int pmi_vallen_max = -1;
static int pmi_keylen_max = -1;
static opal_pointer_array_t local_data;

/* local data storage */
typedef struct {
    opal_object_t super;
    char *nodename;
    ompi_node_rank_t node_rank;
} local_data_t;

static void ld_con(local_data_t *ptr)
{
    ptr->nodename = NULL;
}
static void ld_des(local_data_t *ptr)
{
    if (NULL != ptr->nodename) {
        free(ptr->nodename);
    }
}
OBJ_CLASS_INSTANCE(local_data_t,
                   opal_object_t,
                   ld_con, ld_des);

/* Because Cray uses PMI2 extensions for some, but not all,
 * PMI functions, we define a set of wrappers for those
 * common functions we will use
 */
static int kvs_put(const char *key, const char *value)
{
#if WANT_PMI2_SUPPORT
    return PMI2_KVS_Put(key, value);
#else
    return PMI_KVS_Put(pmi_kvs_name, key, value);
#endif
}

static int kvs_get(const char *key, char *value, int valuelen)
{
#if WANT_PMI2_SUPPORT
    int len;

    return PMI2_KVS_Get(pmi_kvs_name, PMI2_ID_NULL, key, value, valuelen, &len);
#else
    return PMI_KVS_Get(pmi_kvs_name, key, value, valuelen);
#endif
}


static int setup_pmi(void)
{
    int max_length, rc;

#if WANT_PMI2_SUPPORT
    pmi_vallen_max = PMI2_MAX_VALLEN;
#else
    rc = PMI_KVS_Get_value_length_max(&pmi_vallen_max);
    if (PMI_SUCCESS != rc) {
        return OMPI_ERROR;
    }
#endif

#if WANT_PMI2_SUPPORT
    /* TODO -- is this ok */
    max_length = 1024;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_name_length_max(&max_length))) {
        return OMPI_ERROR;
    }
#endif
    pmi_kvs_name = (char*)malloc(max_length);
    if (NULL == pmi_kvs_name) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

#if WANT_PMI2_SUPPORT
    rc = PMI2_Job_GetId(pmi_kvs_name, max_length);
#else
    rc = PMI_KVS_Get_my_name(pmi_kvs_name,max_length);
#endif
    if (PMI_SUCCESS != rc) {
        return OMPI_ERROR;
    }

#if WANT_PMI2_SUPPORT
    pmi_keylen_max = PMI2_MAX_KEYLEN;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_key_length_max(&pmi_keylen_max))) {
        return OMPI_ERROR;
    }
#endif

    return OMPI_SUCCESS;
}


static char* setup_key(const ompi_process_name_t *name, const char *key)
{
    char *pmi_kvs_key;

    if (pmi_keylen_max <= asprintf(&pmi_kvs_key, "%s-%s",
                                   OMPI_NAME_PRINT(name), key)) {
        free(pmi_kvs_key);
        return NULL;
    }

    return pmi_kvs_key;
}

static inline unsigned char pmi_base64_encsym (unsigned char value) {
    assert (value < 64);

    if (value < 26) {
	return 'A' + value;
    } else if (value < 52) {
	return 'a' + (value - 26);
    } else if (value < 62) {
	return '0' + (value - 52);
    }

    return (62 == value) ? '+' : '/';
}

static inline unsigned char pmi_base64_decsym (unsigned char value) {
    if ('+' == value) {
	return 62;
    } else if ('/' == value) {
	return 63;
    } else if (' ' == value) {
	return 64;
    } else if (value <= '9') {
	return (value - '0') + 52;
    } else if (value <= 'Z') {
	return (value - 'A');
    } else if (value <= 'z') {
	return (value - 'a') + 26;
    }

    return 64;
}

static inline void pmi_base64_encode_block (unsigned char in[3], unsigned char out[4], int len) {
    out[0] = pmi_base64_encsym (in[0] >> 2);
    out[1] = pmi_base64_encsym (((in[0] & 0x03) << 4) | ((in[1] & 0xf0) >> 4));
    /* Cray PMI doesn't allow = in PMI attributes so pad with spaces */
    out[2] = 1 < len ? pmi_base64_encsym(((in[1] & 0x0f) << 2) | ((in[2] & 0xc0) >> 6)) : ' ';
    out[3] = 2 < len ? pmi_base64_encsym(in[2] & 0x3f) : ' ';
}

static inline int pmi_base64_decode_block (unsigned char in[4], unsigned char out[3]) {
    char in_dec[4];

    in_dec[0] = pmi_base64_decsym (in[0]);
    in_dec[1] = pmi_base64_decsym (in[1]);
    in_dec[2] = pmi_base64_decsym (in[2]);
    in_dec[3] = pmi_base64_decsym (in[3]);

    out[0] = in_dec[0] << 2 | in_dec[1] >> 4;
    if (64 == in_dec[2]) {
	return 1;
    }

    out[1] = in_dec[1] << 4 | in_dec[2] >> 2;
    if (64 == in_dec[3]) {
	return 2;
    }

    out[2] = ((in_dec[2] << 6) & 0xc0) | in_dec[3];
    return 3;
}


static int pmi_encode(char *outdata, const void *val, size_t vallen) {
    unsigned char *tmp = (unsigned char*)outdata;
    size_t i;

    /* check for size */
    if ((size_t)pmi_vallen_max < (2 + vallen * 4) / 3 + 1) {
        return OMPI_ERR_BAD_PARAM;
    }

    for (i = 0 ; i < vallen ; i += 3, tmp += 4) {
        pmi_base64_encode_block((unsigned char *) val + i, tmp, vallen - i);
    }

    tmp[0] = (unsigned char)'\0';

    return OMPI_SUCCESS;
}


static uint8_t* pmi_decode (char *data, size_t *retlen) {
    size_t input_len = strlen (data) / 4;
    unsigned char *ret, *val;
    int out_len;
    size_t i;

    ret = calloc (1, 3 * input_len + 1);
    if (NULL == ret) {
        return ret;
    }

    val = (unsigned char *) data;
    for (i = 0, out_len = 0 ; i < input_len ; i++, val += 4) {
	out_len += pmi_base64_decode_block(val, ret + 3 * i);
    }

    ret[out_len] = '\0';
    *retlen = out_len;
    return ret;
}


static char* fetch_string(const char *key)
{
    char *tmp_val, *ptr, *tmpkey;
    int i, nsections;
    char *data;

    /* create our sandbox */
    tmp_val = (char*)malloc(pmi_vallen_max * sizeof(char));

    /* the first section of the string has the original key, so fetch it */
    if (PMI_SUCCESS != kvs_get(key, tmp_val, pmi_vallen_max)) {
        OMPI_ERROR_LOG(OMPI_ERR_NOT_FOUND);
        free(tmp_val);
        return NULL;
    }

    opal_output_verbose(5, ompi_rte_base_framework.framework_output,
                "%s db:pmi:fetch_string: received key %s DATA %s",
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                key, tmp_val);

    /* the data in this section was prepended with the number of sections
     * required to hold the entire string - get it
     */
    ptr = strchr(tmp_val, ':');
    *ptr = '\0';
    nsections = strtol(tmp_val, NULL, 10);
    /* save the actual data */
    ptr++;
    data = strdup(ptr);

    /* get any remaining sections */
    for (i=1; i < nsections; i++) {
        /* create the key */
        asprintf(&tmpkey, "%s:%d", key, i);
        /* fetch it */
        if (PMI_SUCCESS != kvs_get(tmpkey, tmp_val, pmi_vallen_max)) {
            OMPI_ERROR_LOG(OMPI_ERR_NOT_FOUND);
            free(tmp_val);
            free(tmpkey);
            free(data);
            return NULL;
        }
        opal_output_verbose(5, ompi_rte_base_framework.framework_output,
                    "%s db:pmi:fetch_string: received key %s DATA %s",
                    OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                    tmpkey, tmp_val);

        /* add it to our data */
        asprintf(&ptr, "%s%s", data, tmp_val);
        free(data);
        data = ptr;
        /* cleanup */
        free(tmpkey);
    }

    /* cleanup */
    free(tmp_val);

    return data;
}

static local_data_t* fetch_rtedat(const ompi_process_name_t *proc)
{
    local_data_t *pdat;
    char *pmikey, **fields;
    char *tmp_val;

    /* see if we already fetched the data for this proc */
    if (NULL != (pdat = (local_data_t*)opal_pointer_array_get_item(&local_data, proc->vpid))) {
        return pdat;
    }

    /* nope - go get it and break it down */
    if (NULL == (pmikey = setup_key(proc, OMPI_DB_RTE_INFO))) {
	OMPI_ERROR_LOG(OMPI_ERR_BAD_PARAM);
	return NULL;
    }
    if (NULL == (tmp_val = fetch_string(pmikey))) {
        OMPI_ERROR_LOG(OMPI_ERR_NOT_FOUND);
        return NULL;
    }

    /* split on commas */
    fields = opal_argv_split(tmp_val, ',');
    free(tmp_val);
    /* sanity check */
    if (2 != opal_argv_count(fields)) {
        OMPI_ERROR_LOG(OMPI_ERR_BAD_PARAM);
        return NULL;
    }

    /* setup the data object */
    pdat = OBJ_NEW(local_data_t);
    /* next is the hostname */
    pdat->nodename = strdup(fields[0]);
    /* node rank */
    pdat->node_rank = strtoul(fields[1], NULL, 10);
    /* insert into the right place */
    opal_pointer_array_set_item(&local_data, proc->vpid, pdat);
    /* cleanup */
    opal_argv_free(fields);
    return pdat;
}


int
ompi_rte_pmi_db_init(void)
{
    int rc;

    if (OMPI_SUCCESS != (rc = setup_pmi())) {
        OMPI_ERROR_LOG(rc);
    }

    OBJ_CONSTRUCT(&local_data, opal_pointer_array_t);
    opal_pointer_array_init(&local_data, 1, INT_MAX, 2);

    return rc;
}


void
ompi_rte_pmi_db_fini(void)
{
    int i;
    local_data_t *pdat;

    if (NULL != pmi_kvs_name) {
        free(pmi_kvs_name);
        pmi_kvs_name = NULL;
    }

    for (i=0; i < local_data.size; i++) {
        if (NULL != (pdat = (local_data_t*)opal_pointer_array_get_item(&local_data, i))) {
            OBJ_RELEASE(pdat);
        }
    }
    OBJ_DESTRUCT(&local_data);
}


int
ompi_rte_db_store(const ompi_process_name_t *proc,
                  const char *key,
                  const void *data,
                  opal_data_type_t type)
{
    int i, rc;
    char *pmidata, *str;
    int64_t i64;
    uint64_t ui64;
    opal_byte_object_t *bo;
    char *pmikey, *tmpkey, *tmp, sav;
    char **strdata=NULL;

    opal_output_verbose(5, ompi_rte_base_framework.framework_output,
                "%s db:pmi:store: storing key %s[%s] for proc %s",
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                key, opal_dss.lookup_data_type(type), OMPI_NAME_PRINT(proc));

    if (NULL == (pmikey = setup_key(OMPI_PROC_MY_NAME, key))) {
	OMPI_ERROR_LOG(OMPI_ERR_BAD_PARAM);
	return OMPI_ERR_BAD_PARAM;
    }

    switch (type) {
    case OPAL_STRING:
        str = (char*)data;
        while (pmi_vallen_max < (int)(OMPI_PMI_PAD + strlen(str))) {
            /* the string is too long, so we need to break it into
             * multiple sections
             */
            tmp = str + pmi_vallen_max - OMPI_PMI_PAD;
            sav = *tmp;
            *tmp = '\0';
            opal_argv_append_nosize(&strdata, str);
            *tmp = sav;
            str = tmp;
        }
        /* put whatever remains on the stack */
        opal_argv_append_nosize(&strdata, str);
        /* the first value we put uses the original key, but
         * the data is prepended with the number of sections
         * required to hold the entire string
         */
        asprintf(&pmidata, "%d:%s", opal_argv_count(strdata), strdata[0]);
        opal_output_verbose(5, ompi_rte_base_framework.framework_output,
                    "%s db:pmi:store: storing key %s data %s",
                    OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                    pmikey, pmidata);

        if (PMI_SUCCESS != (rc = kvs_put(pmikey, pmidata))) {
            free(pmidata);
            free(pmikey);
            opal_argv_free(strdata);
            return OMPI_ERROR;
        }
        free(pmidata);
        /* for each remaining segment, augment the key with the index */
        for (i=1; NULL != strdata[i]; i++) {
            asprintf(&tmpkey, "%s:%d", pmikey, i);
            opal_output_verbose(5, ompi_rte_base_framework.framework_output,
                        "%s db:pmi:store: storing key %s data %s",
                        OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                        pmikey, strdata[i]);

            if (PMI_SUCCESS != (rc = kvs_put(tmpkey, strdata[i]))) {
                free(pmikey);
                opal_argv_free(strdata);
                return OMPI_ERROR;
            }
            free(tmpkey);
        }
        free(pmikey);
        opal_argv_free(strdata);
        return OMPI_SUCCESS;

    case OPAL_INT:
        i64 = (int64_t)(*((int*)data));
        asprintf(&pmidata, "%ld", (long)i64);
        break;
        
    case OPAL_INT32:
        i64 = (int64_t)(*((int32_t*)data));
        asprintf(&pmidata, "%ld", (long)i64);
        break;
        
    case OPAL_INT64:
        i64 = (int64_t)(*((int*)data));
        asprintf(&pmidata, "%ld", (long)i64);
        break;

    case OPAL_UINT64:
        ui64 = *((uint64_t*)data);
        asprintf(&pmidata, "%lu", (unsigned long)ui64);
        break;
    
    case OPAL_UINT32:
        ui64 = (uint64_t)(*((uint32_t*)data));
        asprintf(&pmidata, "%lu", (unsigned long)ui64);
        break;
       
    case OPAL_UINT16:
        ui64 = (uint64_t)(*((uint16_t*)data));
        asprintf(&pmidata, "%lu", (unsigned long)ui64);
        break;
    
    case OPAL_BYTE_OBJECT:
        bo = (opal_byte_object_t*)data;
        pmidata = (char*)malloc(pmi_vallen_max*sizeof(char));
        if (OMPI_SUCCESS != (rc = pmi_encode(pmidata, bo->bytes, bo->size))) {
            OMPI_ERROR_LOG(rc);
            free(pmidata);
            return rc;
        }
        break;

    default:
        OMPI_ERROR_LOG(OMPI_ERR_NOT_SUPPORTED);
        return OMPI_ERR_NOT_SUPPORTED;
    }

    opal_output_verbose(5, ompi_rte_base_framework.framework_output,
                "%s PUTTING KEY %s DATA %s",
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                pmikey, pmidata);

    rc = kvs_put(pmikey, pmidata);
    if (PMI_SUCCESS != rc) {
	return OMPI_ERROR;
    }
    free(pmidata);
    free(pmikey);
    return OMPI_SUCCESS;
}


int
ompi_rte_db_fetch(const ompi_process_name_t *proc,
                  const char *key,
                  void **data,
                  opal_data_type_t type)
{
    local_data_t *pdat;
    opal_byte_object_t *boptr;
    uint16_t ui16;
    uint32_t ui32;
    int ival;
    unsigned int uival;
    char *pmikey;
    char tmp_val[1024];
    opal_hwloc_locality_t locality;
    size_t sval;

    opal_output_verbose(5, ompi_rte_base_framework.framework_output,
                "%s db:pmi:fetch: searching for key %s[%s] on proc %s",
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                (NULL == key) ? "NULL" : key,
                opal_dss.lookup_data_type(type),
                OMPI_NAME_PRINT(proc));

    /* if the key is NULL, that is an error */
    if (NULL == key) {
        OMPI_ERROR_LOG(OMPI_ERR_BAD_PARAM);
        return OMPI_ERR_BAD_PARAM;
    }

    /* a few keys are consolidated to reduce the number of entries being
     * pushed to PMI. This is an unfortunate requirement when running at
     * scale on a Cray as the default max number of keys is set too low.
     * See the corresponding entry in orte/mca/grpcomm/pmi where the
     * consolidation occurs.
     */
    if (0 == strcmp(key, OMPI_DB_HOSTNAME)) {
        if (NULL == (pdat = fetch_rtedat(proc))) {
            return OMPI_ERR_NOT_FOUND;
        }
        *data = strdup(pdat->nodename);
        return OMPI_SUCCESS;
    } else if (0 == strcmp(key, OMPI_DB_NODERANK)) {
        if (NULL == (pdat = fetch_rtedat(proc))) {
            return OMPI_ERR_NOT_FOUND;
        }
        memcpy(*data, &pdat->node_rank, sizeof(ompi_node_rank_t));
        return OMPI_SUCCESS;
    }

    /* if it is the locality key, then compute that value as it
     * isn't something that gets pushed to PMI
     */
    if (0 == strcmp(key, OMPI_DB_LOCALITY)) {
        if (proc->jobid == OMPI_PROC_MY_NAME->jobid &&
            proc->vpid == OMPI_PROC_MY_NAME->vpid) {
            /* if this is for myself, then set locality to all */
            locality = OPAL_PROC_ALL_LOCAL;
        } else {
            if (NULL == (pdat = fetch_rtedat(proc))) {
                return OMPI_ERR_NOT_FOUND;
            }
            if (0 != strcmp(pdat->nodename, ompi_process_info.nodename)) {
		/* this is on a different node, then mark as non-local */
		locality = OPAL_PROC_NON_LOCAL;
	    } else {
                /* BWB: FiX ME: Might want to say more than on node... */
		locality = OPAL_PROC_ON_NODE;
            }
        }
        memcpy(*data, &locality, sizeof(opal_hwloc_locality_t));

        opal_output_verbose(5, ompi_rte_base_framework.framework_output,
                    "%s db:pmi:fetch: done searching for key %s[%s] on proc %s",
                    OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                    (NULL == key) ? "NULL" : key,
                    opal_dss.lookup_data_type(type),
                    OMPI_NAME_PRINT(proc));
        return OMPI_SUCCESS;
    }

    /* setup the key */
    if (NULL == (pmikey = setup_key(proc, key))) {
	OMPI_ERROR_LOG(OMPI_ERR_BAD_PARAM);
	return OMPI_ERR_BAD_PARAM;
    }

    /* if it isn't an RTE key, then check to see if they are looking for a string */
    if (OPAL_STRING == type) {
        /* might have been passed in multiple sections */
        *data = fetch_string(pmikey);
        free(pmikey);
        return OMPI_SUCCESS;
    }

    /* otherwise, retrieve the pmi keyval */
    if (NULL == (pmikey = setup_key(proc, key))) {
	OMPI_ERROR_LOG(OMPI_ERR_BAD_PARAM);
	return OMPI_ERR_BAD_PARAM;
    }
    if (PMI_SUCCESS != kvs_get(pmikey, tmp_val, pmi_vallen_max)) {
        OMPI_ERROR_LOG(OMPI_ERR_NOT_FOUND);
        free(pmikey);
        return OMPI_ERR_NOT_FOUND;
    }
    free(pmikey);

    /* return the value according to the provided type */
    switch (type) {
    case OPAL_UINT32:
        ui32 = (uint32_t)strtoul(tmp_val, NULL, 10);
        memcpy(*data, &ui32, sizeof(uint32_t));
        break;
    case OPAL_UINT16:
        ui16 = (uint16_t)strtoul(tmp_val, NULL, 10);
        memcpy(*data, &ui16, sizeof(uint16_t));
        break;
    case OPAL_INT:
        ival = (int)strtol(tmp_val, NULL, 10);
        memcpy(*data, &ival, sizeof(int));
        break;
    case OPAL_UINT:
        uival = (unsigned int)strtoul(tmp_val, NULL, 10);
        memcpy(*data, &uival, sizeof(unsigned int));
        break;
    case OPAL_BYTE_OBJECT:
        boptr = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        boptr->bytes = (uint8_t*)pmi_decode(tmp_val, &sval);
        boptr->size = sval;
        *data = boptr;
        break;
    default:
        OMPI_ERROR_LOG(OMPI_ERR_NOT_SUPPORTED);
        return OMPI_ERR_NOT_SUPPORTED;
    }

    opal_output_verbose(5, ompi_rte_base_framework.framework_output,
                        "%s db:pmi:fetch: done searching for key %s[%s] on proc %s",
                        OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                        (NULL == key) ? "NULL" : key,
                        opal_dss.lookup_data_type(type),
                        OMPI_NAME_PRINT(proc));

    return OMPI_SUCCESS;
}


int
ompi_rte_db_fetch_pointer(const ompi_process_name_t *proc,
                          const char *key,
                          void **data,
                          opal_data_type_t type)
{
    local_data_t *pdat;

    opal_output_verbose(5, ompi_rte_base_framework.framework_output,
                        "%s db:pmi:fetch_pointer: searching for key %s on proc %s",
                        OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                        (NULL == key) ? "NULL" : key, OMPI_NAME_PRINT(proc));

    /* if the key is NULL, that is an error */
    if (NULL == key) {
        OMPI_ERROR_LOG(OMPI_ERR_BAD_PARAM);
        return OMPI_ERR_BAD_PARAM;
    }

    /* we only support hostname for now */
    if (0 != strcmp(key, OMPI_DB_HOSTNAME)) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    if (NULL == (pdat = fetch_rtedat(proc))) {
        return OMPI_ERR_NOT_FOUND;
    }

    *data = pdat->nodename;
    return OMPI_SUCCESS;
}
