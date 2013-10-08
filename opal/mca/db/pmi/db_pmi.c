/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "opal_config.h"
#include "opal/constants.h"

#include <time.h>
#include <string.h>
#include <pmi.h>
#if WANT_PMI2_SUPPORT
#include <pmi2.h>
#endif

#include <regex.h>

#include "opal_stdint.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss_types.h"
#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "opal/mca/common/pmi/common_pmi.h"
#include "opal/mca/db/base/base.h"
#include "db_pmi.h"


#define OPAL_PMI_PAD  10

static int init(void);
static void finalize(void);
static int store(const opal_identifier_t *id,
                 opal_scope_t scope,
                 const char *key, const void *object,
                 opal_data_type_t type);
static int store_pointer(const opal_identifier_t *proc,
                         opal_value_t *kv);
static void commit(const opal_identifier_t *proc);
static int fetch(const opal_identifier_t *proc,
                 const char *key, void **data, opal_data_type_t type);
static int fetch_pointer(const opal_identifier_t *proc,
                         const char *key,
                         void **data, opal_data_type_t type);
static int fetch_multiple(const opal_identifier_t *proc,
                          opal_scope_t scope,
                          const char *key,
                          opal_list_t *kvs);
static int remove_data(const opal_identifier_t *proc, const char *key);

opal_db_base_module_t opal_db_pmi_module = {
    init,
    finalize,
    opal_db_base_set_id,
    store,
    store_pointer,
    commit,
    fetch,
    fetch_pointer,
    fetch_multiple,
    remove_data,
    NULL
};

static char *pmi_encode(const void *val, size_t vallen);
static uint8_t* pmi_decode(const char *data, size_t *retlen);
static int setup_pmi(void);
static char* setup_key(opal_identifier_t name, const char *key);

/* Local variables */
static char *pmi_kvs_name = NULL;
static int pmi_vallen_max = -1;
static int pmi_keylen_max = -1;

static char *pmi_packed_data = NULL;
static int pmi_pack_key = 0;
static int pmi_packed_data_off = 0;

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

static int init(void)
{
    int rc;

    rc = setup_pmi();
    /* don't error log this return status as it
     * could just mean we don't have PMI setup
     * for this job
     */
 
    return rc;
}

static void finalize(void)
{
    if (NULL != pmi_kvs_name) {
        free(pmi_kvs_name);
        pmi_kvs_name = NULL;
    }

}

static int pmi_commit_packed (const opal_identifier_t *uid) {
    char *pmikey = NULL, *tmp;
    opal_identifier_t proc;
    char tmp_key[32], save;
    char *encoded_data;
    int rc, left;

    if (pmi_packed_data_off == 0) {
	/* nothing to write */
	return OPAL_SUCCESS;
    }

    /* to protect alignment, copy the data across */
    memcpy(&proc, uid, sizeof(opal_identifier_t));

    if (NULL == (encoded_data = pmi_encode(pmi_packed_data, pmi_packed_data_off))) {
	OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
	return OPAL_ERR_OUT_OF_RESOURCE;
    }

    for (left = strlen (encoded_data), tmp = encoded_data ; left ; ) {
	size_t value_size = pmi_vallen_max > left ? left : pmi_vallen_max - 1;

	sprintf (tmp_key, "key%d", pmi_pack_key);
        
	if (NULL == (pmikey = setup_key(proc, tmp_key))) {
	    OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
	    rc = OPAL_ERR_BAD_PARAM;
	    break;
	}

	/* only write value_size bytes */
	save = tmp[value_size];
	tmp[value_size] = '\0';

	rc = kvs_put(pmikey, tmp);
	free (pmikey);
	if (PMI_SUCCESS != rc) {
	    OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
	    rc = OPAL_ERROR;
	    break;
	}

	tmp[value_size] = save;
	tmp += value_size;
	left -= value_size;

	pmi_pack_key ++;

	rc = OPAL_SUCCESS;
    }

    if (encoded_data) {
	free (encoded_data);
    }

    pmi_packed_data_off = 0;
    free (pmi_packed_data);
    pmi_packed_data = NULL;

    return rc;
}

static int pmi_store_encoded(const opal_identifier_t *uid, const char *key, const void *data, opal_data_type_t type)
{
    opal_byte_object_t *bo;
    size_t data_len = 0;
    size_t needed;

    switch (type) {
    case OPAL_STRING:
	data_len = data ? strlen (data) + 1 : 0;
	break;
    case OPAL_INT:
    case OPAL_UINT:
	data_len = sizeof (int);
	break;
    case OPAL_INT16:
    case OPAL_UINT16:
	data_len = sizeof (int16_t);
	break;
    case OPAL_INT32:
    case OPAL_UINT32:
	data_len = sizeof (int32_t);
	break;
    case OPAL_INT64:
    case OPAL_UINT64:
	data_len = sizeof (int64_t);
	break;
    case OPAL_BYTE_OBJECT:
	bo = (opal_byte_object_t *) data;
	data = bo->bytes;
	data_len = bo->size;
    }

    needed = 10 + data_len + strlen (key);

    if (NULL == pmi_packed_data) {
	pmi_packed_data = calloc (needed, 1);
    } else {
	/* grow the region */
	pmi_packed_data = realloc (pmi_packed_data, pmi_packed_data_off + needed);
    }

    /* special length meaning NULL */
    if (NULL == data) {
        data_len = 0xffff;
    }

    /* serialize the opal datatype */
    pmi_packed_data_off += sprintf (pmi_packed_data + pmi_packed_data_off,
				    "%s%c%02x%c%04x%c", key, '\0', type, '\0',
                                    (int) data_len, '\0');
    if (NULL != data) {
        memmove (pmi_packed_data + pmi_packed_data_off, data, data_len);
        pmi_packed_data_off += data_len;
    }

    return OPAL_SUCCESS;
}

static int pmi_get_packed (const opal_identifier_t *uid, char **packed_data, size_t *len)
{
    char *tmp_encoded = NULL, *pmikey, *pmi_tmp;
    int remote_key, size;
    size_t bytes_read;
    opal_identifier_t proc;
    int rc;

    /* set default */
    *packed_data = NULL;
    *len = 0;

    /* to protect alignment, copy the data across */
    memcpy(&proc, uid, sizeof(opal_identifier_t));

    pmi_tmp = calloc (pmi_vallen_max, 1);
    if (NULL == pmi_tmp) {
	return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* read all of the packed data from this proc */
    for (remote_key = 0, bytes_read = 0 ; ; ++remote_key) {
        char tmp_key[32];

        sprintf (tmp_key, "key%d", remote_key);

        if (NULL == (pmikey = setup_key(proc, tmp_key))) {
	    rc = OPAL_ERR_OUT_OF_RESOURCE;
            OPAL_ERROR_LOG(rc);
            return rc;
        }

        OPAL_OUTPUT_VERBOSE((10, opal_db_base_framework.framework_output,
                             "GETTING KEY %s", pmikey));

        rc = kvs_get(pmikey, pmi_tmp, pmi_vallen_max);
	free (pmikey);
        if (PMI_SUCCESS != rc) {
	    break;
        }

	size = strlen (pmi_tmp);

	if (NULL == tmp_encoded) {
	    tmp_encoded = malloc (size + 1);
	} else {
	    tmp_encoded = realloc (tmp_encoded, bytes_read + size + 1);
	}

	strcpy (tmp_encoded + bytes_read, pmi_tmp);
	bytes_read += size;

        /* is the string terminator present? */
        if ('-' == tmp_encoded[bytes_read-1]) {
            break;
        }
    }

    free (pmi_tmp);

    OPAL_OUTPUT_VERBOSE((10, opal_db_base_framework.framework_output,
                         "Read data %s\n",
                         (NULL == tmp_encoded) ? "NULL" : tmp_encoded));

    if (NULL != tmp_encoded) {
        *packed_data = (char *) pmi_decode (tmp_encoded, len);
        free (tmp_encoded);
        if (NULL == *packed_data) {
	    return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    return OPAL_SUCCESS;
}

static void cache_keys_locally(const opal_identifier_t *uid)
{
    char *tmp, *tmp2, *tmp3, *tmp_val;
    opal_data_type_t stored_type;
    size_t len, offset;
    int rc, size;

    OPAL_OUTPUT_VERBOSE((1, opal_db_base_framework.framework_output,
                         "db:pmi:fetch get all keys for proc %" PRIu64 " in KVS %s",
			 *uid, pmi_kvs_name));

    rc = pmi_get_packed (uid, &tmp_val, &len);
    if (OPAL_SUCCESS != rc) {
        return;
    }

    /* search for this key in the decoded data */
    for (offset = 0 ; offset < len && '\0' != tmp_val[offset] ; ) {
        /* type */
	tmp = tmp_val + offset + strlen (tmp_val + offset) + 1;
        /* size */
	tmp2 = tmp + strlen (tmp) + 1;
        /* data */
        tmp3 = tmp2 + strlen (tmp2) + 1;

        stored_type = (opal_data_type_t) strtol (tmp, NULL, 16);
        size = strtol (tmp2, NULL, 16);

        /* cache value locally so we don't have to look it up via pmi again */
        if (OPAL_BYTE_OBJECT == stored_type) {
            opal_byte_object_t bo;
            if (size == 0xffff) {
                bo.bytes = NULL;
                bo.size = 0;
            } else {
                bo.bytes = (uint8_t*)tmp3;
                bo.size = size;
            }
            opal_db.store (uid, OPAL_SCOPE_GLOBAL, tmp_val + offset, &bo, stored_type);
        } else if (size < 0xffff) {
            opal_db.store (uid, OPAL_SCOPE_GLOBAL, tmp_val + offset, tmp3, stored_type);
        } else {
            opal_db.store (uid, OPAL_SCOPE_GLOBAL, tmp_val + offset, NULL, stored_type);
        }

        /* keep going and cache everything locally */
        offset = (size_t) (tmp3 - tmp_val) + size;
    }

    free (tmp_val);
}

static int store(const opal_identifier_t *uid,
                 opal_scope_t scope,
                 const char *key, const void *data, opal_data_type_t type)
{
    opal_identifier_t proc;
    int rc;

    /* to protect alignment, copy the data across */
    memcpy(&proc, uid, sizeof(opal_identifier_t));

    /* we never push other proc's data, or INTERNAL data */
    if (OPAL_SCOPE_INTERNAL & scope ||
        proc != opal_db_base.my_id) {
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:pmi:store: storing key %s[%s] for proc %" PRIu64 "",
                         key, opal_dss.lookup_data_type(type), proc));

    if (OPAL_SUCCESS != (rc = pmi_store_encoded (uid, key, data, type))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    /* we want our internal data to be stored internally
     * as well since some of the upper layer components
     * want to retrieve it
     */
    return OPAL_ERR_TAKE_NEXT_OPTION;
}

static int store_pointer(const opal_identifier_t *uid,
                         opal_value_t *kv)
{
    int rc;
    opal_identifier_t proc;

    /* to protect alignment, copy the data across */
    memcpy(&proc, uid, sizeof(opal_identifier_t));

    /* we never push other proc's data, or INTERNAL data */
    if (OPAL_SCOPE_INTERNAL & kv->scope ||
        proc != opal_db_base.my_id) {
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

    /* just push this to PMI */
    if (OPAL_SUCCESS != (rc = store(uid, kv->scope, kv->key, (void*)&kv->data, kv->type))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    /* we want our internal data to be stored internally
     * as well since some of the upper layer components
     * want to retrieve it
     */
    return OPAL_ERR_TAKE_NEXT_OPTION;
}

static void commit(const opal_identifier_t *proc)
{
    /* commit the packed data to PMI */
    pmi_commit_packed (proc);
    
#if WANT_PMI2_SUPPORT
    PMI2_KVS_Fence();
#else
    {
        int rc;
        
        if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmi_kvs_name))) {
            OPAL_PMI_ERROR(rc, "PMI_KVS_Commit");
            return;
        }
        /* Barrier here to ensure all other procs have committed */
        PMI_Barrier();
    }
#endif
}

static int fetch(const opal_identifier_t *uid,
                 const char *key, void **data,
                 opal_data_type_t type)
{
    opal_identifier_t proc;

    /* to protect alignment, copy the data across */
    memcpy(&proc, uid, sizeof(opal_identifier_t));

    /* if it is my own id, the data isn't here */
    if (proc == opal_db_base.my_id) {
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

    cache_keys_locally(&proc);
    /* all keys will be available internally now */
    return OPAL_ERR_TAKE_NEXT_OPTION;
}

static int fetch_pointer(const opal_identifier_t *uid,
                         const char *key,
                         void **data, opal_data_type_t type)
{
    opal_identifier_t proc;

    /* to protect alignment, copy the data across */
    memcpy(&proc, uid, sizeof(opal_identifier_t));

    /* if it is my own id, the data isn't here */
    if (proc == opal_db_base.my_id) {
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }
    cache_keys_locally(&proc);
    /* all keys will be available internally now */
    return OPAL_ERR_TAKE_NEXT_OPTION;
}

static int fetch_multiple(const opal_identifier_t *uid,
                          opal_scope_t scope,
                          const char *key,
                          opal_list_t *kvs)
{
    opal_identifier_t proc;

    /* to protect alignment, copy the data across */
    memcpy(&proc, uid, sizeof(opal_identifier_t));

    /* if it is my own id, the data isn't here */
    if (proc == opal_db_base.my_id) {
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

     OPAL_OUTPUT_VERBOSE((1, opal_db_base_framework.framework_output,
                         "db:pmi:fetch_multiple get key %s for proc %" PRIu64 " in KVS %s",
                          (NULL == key) ? "NULL" : key, proc, pmi_kvs_name));

    cache_keys_locally(&proc);
    /* all keys will be available internally now */
    return OPAL_ERR_TAKE_NEXT_OPTION;
}

static int remove_data(const opal_identifier_t *proc, const char *key)
{
    /* nothing to do here */
    return OPAL_SUCCESS;
}

static int setup_pmi(void)
{
    int max_length, rc;

#if WANT_PMI2_SUPPORT
    pmi_vallen_max = PMI2_MAX_VALLEN;
    max_length = PMI2_MAX_VALLEN;
#else
    rc = PMI_KVS_Get_value_length_max(&pmi_vallen_max);
    if (PMI_SUCCESS != rc) {
        OPAL_OUTPUT_VERBOSE((1, opal_db_base_framework.framework_output,
                             "db:pmi:pmi_setup failed %s with error %s",
                             "PMI_Get_value_length_max",
                             opal_errmgr_base_pmi_error(rc)));
        return OPAL_ERROR;
    }

    if (PMI_SUCCESS != (rc = PMI_KVS_Get_name_length_max(&max_length))) {
        OPAL_OUTPUT_VERBOSE((1, opal_db_base_framework.framework_output,
                             "db:pmi:pmi_setup failed %s with error %s",
                             "PMI_KVS_Get_name_length_max",
                             opal_errmgr_base_pmi_error(rc)));
        return OPAL_ERROR;
    }
#endif
    pmi_kvs_name = (char*)malloc(max_length);
    if (NULL == pmi_kvs_name) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

#if WANT_PMI2_SUPPORT
    rc = PMI2_Job_GetId(pmi_kvs_name, max_length);
#else
    rc = PMI_KVS_Get_my_name(pmi_kvs_name,max_length);
#endif
    if (PMI_SUCCESS != rc) {
        OPAL_OUTPUT_VERBOSE((1, opal_db_base_framework.framework_output,
                             "db:pmi:pmi_setup failed %s with error %s on maxlength %d",
                             "PMI_KVS_Get_my_name",
                             opal_errmgr_base_pmi_error(rc), max_length));
        return OPAL_ERROR;
    }

#if WANT_PMI2_SUPPORT
    pmi_keylen_max = PMI2_MAX_KEYLEN;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_key_length_max(&pmi_keylen_max))) {
        OPAL_OUTPUT_VERBOSE((1, opal_db_base_framework.framework_output,
                             "db:pmi:pmi_setup failed %s with error %s",
                             "PMI_KVS_Get_key_length_max",
                             opal_errmgr_base_pmi_error(rc)));
        return OPAL_ERROR;
    }
#endif

    return OPAL_SUCCESS;
}

static char* setup_key(opal_identifier_t name, const char *key)
{
    char *pmi_kvs_key;

    if (pmi_keylen_max <= asprintf(&pmi_kvs_key, "%" PRIu64 "-%s",
                                   name, key)) {
        free(pmi_kvs_key);
        return NULL;
    }

    return pmi_kvs_key;
}

/* base64 encoding with illegal (to Cray PMI) characters removed ('=' is replaced by ' ') */
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

static inline void pmi_base64_encode_block (const unsigned char in[3], char out[4], int len) {
    out[0] = pmi_base64_encsym (in[0] >> 2);
    out[1] = pmi_base64_encsym (((in[0] & 0x03) << 4) | ((in[1] & 0xf0) >> 4));
    /* Cray PMI doesn't allow = in PMI attributes so pad with spaces */
    out[2] = 1 < len ? pmi_base64_encsym(((in[1] & 0x0f) << 2) | ((in[2] & 0xc0) >> 6)) : ' ';
    out[3] = 2 < len ? pmi_base64_encsym(in[2] & 0x3f) : ' ';
}

static inline int pmi_base64_decode_block (const char in[4], unsigned char out[3]) {
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


/* PMI only supports strings. For now, do a simple base64. */
static char *pmi_encode(const void *val, size_t vallen) {
    char *outdata, *tmp;
    size_t i;

    outdata = calloc (((2 + vallen) * 4) / 3 + 2, 1);
    if (NULL == outdata) {
	return NULL;
    }

    for (i = 0, tmp = outdata ; i < vallen ; i += 3, tmp += 4) {
        pmi_base64_encode_block((unsigned char *) val + i, tmp, vallen - i);
    }

    /* mark the end of the pmi string */
    tmp[0] = (unsigned char)'-';
    tmp[1] = (unsigned char)'\0';

    return outdata;
}

static uint8_t *pmi_decode (const char *data, size_t *retlen) {
    size_t input_len = (strlen (data) - 1) / 4;
    unsigned char *ret;
    int out_len;
    size_t i;

    /* default */
    *retlen = 0;

    ret = calloc (1, 3 * input_len + 1);
    if (NULL == ret) {
        return ret;
    }

    for (i = 0, out_len = 0 ; i < input_len ; i++, data += 4) {
	out_len += pmi_base64_decode_block(data, ret + 3 * i);
    }

    ret[out_len] = '\0';
    *retlen = out_len;
    return ret;
}
