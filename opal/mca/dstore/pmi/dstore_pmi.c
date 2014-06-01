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

#include "opal/mca/common/pmi/common_pmi.h"

#include <regex.h>

#include "opal_stdint.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss_types.h"
#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "opal/mca/dstore/base/base.h"
#include "dstore_pmi.h"


#define OPAL_PMI_PAD  10

static void finalize(struct opal_dstore_base_module_t *imod);
static int store(struct opal_dstore_base_module_t *imod,
                 const opal_identifier_t *proc,
                 opal_value_t *kv);
static void commit(struct opal_dstore_base_module_t *mod,
                   const opal_identifier_t *id);
static int fetch(struct opal_dstore_base_module_t *imod,
                 const opal_identifier_t *proc,
                 const char *key,
                 opal_list_t *kvs);
static int remove_data(struct opal_dstore_base_module_t *imod,
                       const opal_identifier_t *proc, const char *key);

mca_dstore_pmi_module_t opal_dstore_pmi_module = {
    {
        NULL,
        finalize,
        store,
        commit,
        fetch,
        remove_data
    }
};


static char *pmi_encode(const void *val, size_t vallen);
static uint8_t* pmi_decode(const char *data, size_t *retlen);
static char* setup_key(mca_dstore_pmi_module_t *mod,
                       opal_identifier_t name, const char *key);

/* Local variables */

/* Because Cray uses PMI2 extensions for some, but not all,
 * PMI functions, we define a set of wrappers for those
 * common functions we will use
 */
static inline int kvs_put(mca_dstore_pmi_module_t *mod,
                   const char *key, const char *value)
{
    return mca_common_pmi_put(mod->pmi_kvs_name, key, value);
}

static inline int kvs_get(mca_dstore_pmi_module_t *mod,
                   const char *key, char *value, int valuelen)
{
    return mca_common_pmi_get(mod->pmi_kvs_name, key, value, valuelen);
}

static void finalize(struct opal_dstore_base_module_t *imod)
{
    mca_dstore_pmi_module_t *mod;
    opal_dstore_proc_data_t *proc_data;
    uint64_t key;
    char *node;

    mod = (mca_dstore_pmi_module_t*)imod;

    if (NULL != mod->pmi_kvs_name) {
        free(mod->pmi_kvs_name);
        mod->pmi_kvs_name = NULL;
    }

    /* to assist in getting a clean valgrind, cycle thru the hash table
     * and release all data stored in it
     */
    if (OPAL_SUCCESS == opal_hash_table_get_first_key_uint64(&mod->hash_data, &key,
                                                             (void**)&proc_data,
                                                             (void**)&node)) {
        if (NULL != proc_data) {
            OBJ_RELEASE(proc_data);
        }
        while (OPAL_SUCCESS == opal_hash_table_get_next_key_uint64(&mod->hash_data, &key,
                                                                   (void**)&proc_data,
                                                                   node, (void**)&node)) {
            if (NULL != proc_data) {
                OBJ_RELEASE(proc_data);
            }
        }
    }
    OBJ_DESTRUCT(&mod->hash_data);

}

static int pmi_commit_packed(mca_dstore_pmi_module_t *mod,
                             opal_identifier_t proc) {
    char *pmikey = NULL, *tmp;
    char tmp_key[32], save;
    char *encoded_data;
    int rc, left;

    if (mod->pmi_packed_data_off == 0) {
	/* nothing to write */
	return OPAL_SUCCESS;
    }

    if (NULL == (encoded_data = pmi_encode(mod->pmi_packed_data, mod->pmi_packed_data_off))) {
	OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
	return OPAL_ERR_OUT_OF_RESOURCE;
    }

    for (left = strlen (encoded_data), tmp = encoded_data ; left ; ) {
	size_t value_size = mod->pmi_vallen_max > left ? left : mod->pmi_vallen_max - 1;

	sprintf (tmp_key, "key%d", mod->pmi_pack_key);
        
	if (NULL == (pmikey = setup_key(mod, proc, tmp_key))) {
	    OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
	    rc = OPAL_ERR_BAD_PARAM;
	    break;
	}

	/* only write value_size bytes */
	save = tmp[value_size];
	tmp[value_size] = '\0';

	rc = kvs_put(mod, pmikey, tmp);
	free(pmikey);
    if (OPAL_SUCCESS != rc) {
	    break;
	}

	tmp[value_size] = save;
	tmp += value_size;
	left -= value_size;

	mod->pmi_pack_key ++;

	rc = OPAL_SUCCESS;
    }

    if (encoded_data) {
	free(encoded_data);
    }

    mod->pmi_packed_data_off = 0;
    free(mod->pmi_packed_data);
    mod->pmi_packed_data = NULL;

    return rc;
}

static int pmi_store_encoded(mca_dstore_pmi_module_t *mod,
                             const char *key, const void *data,
                             opal_data_type_t type)
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

    if (NULL == mod->pmi_packed_data) {
	mod->pmi_packed_data = calloc (needed, 1);
    } else {
	/* grow the region */
	mod->pmi_packed_data = realloc (mod->pmi_packed_data, mod->pmi_packed_data_off + needed);
    }

    /* special length meaning NULL */
    if (NULL == data) {
        data_len = 0xffff;
    }

    /* serialize the opal datatype */
    mod->pmi_packed_data_off += sprintf (mod->pmi_packed_data + mod->pmi_packed_data_off,
                                         "%s%c%02x%c%04x%c", key, '\0', type, '\0',
                                         (int) data_len, '\0');
    if (NULL != data) {
        memmove (mod->pmi_packed_data + mod->pmi_packed_data_off, data, data_len);
        mod->pmi_packed_data_off += data_len;
    }

    return OPAL_SUCCESS;
}

static int pmi_get_packed(mca_dstore_pmi_module_t *mod,
                          opal_identifier_t proc,
                          char **packed_data, size_t *len)
{
    char *tmp_encoded = NULL, *pmikey, *pmi_tmp;
    int remote_key, size;
    size_t bytes_read;
    int rc;

    /* set default */
    *packed_data = NULL;
    *len = 0;

    pmi_tmp = calloc (mod->pmi_vallen_max, 1);
    if (NULL == pmi_tmp) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* read all of the packed data from this proc */
    for (remote_key = 0, bytes_read = 0 ; ; ++remote_key) {
        char tmp_key[32];

        sprintf (tmp_key, "key%d", remote_key);

        if (NULL == (pmikey = setup_key(mod, proc, tmp_key))) {
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            OPAL_ERROR_LOG(rc);
            return rc;
        }

        OPAL_OUTPUT_VERBOSE((10, opal_dstore_base_framework.framework_output,
                             "GETTING KEY %s", pmikey));

        rc = kvs_get(mod, pmikey, pmi_tmp, mod->pmi_vallen_max);
        free (pmikey);
        if (OPAL_SUCCESS != rc) {
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

    OPAL_OUTPUT_VERBOSE((10, opal_dstore_base_framework.framework_output,
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

static void cache_keys_locally(mca_dstore_pmi_module_t *mod,
                               opal_identifier_t id,
                               opal_dstore_proc_data_t *proc_data)
{
    char *tmp, *tmp2, *tmp3, *tmp_val;
    opal_data_type_t stored_type;
    size_t len, offset;
    int rc, size;
    opal_value_t *kv;

    OPAL_OUTPUT_VERBOSE((1, opal_dstore_base_framework.framework_output,
                         "dstore:pmi:fetch get all keys for proc %" PRIu64 " in KVS %s",
			 id, mod->pmi_kvs_name));

    rc = pmi_get_packed(mod, id, &tmp_val, &len);
    if (OPAL_SUCCESS != rc) {
        return;
    }

    /* search for each key in the decoded data */
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
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(tmp_val + offset);
        kv->type = stored_type;
        opal_list_append(&proc_data->data, &kv->super);
        switch (stored_type) {
        case OPAL_BYTE:
            kv->data.byte = *tmp3;
            break;
        case OPAL_STRING:
            if (NULL != tmp3) {
                kv->data.string = strdup(tmp3);
            } else {
                kv->data.string = NULL;
            }
            break;
        case OPAL_PID:
            kv->data.pid = strtoul(tmp3, NULL, 10);
            break;
        case OPAL_INT:
            kv->data.integer = strtol(tmp3, NULL, 10);
            break;
        case OPAL_INT8:
            kv->data.int8 = strtol(tmp3, NULL, 10);
            break;
        case OPAL_INT16:
            kv->data.int16 = strtol(tmp3, NULL, 10);
            break;
        case OPAL_INT32:
            kv->data.int32 = strtol(tmp3, NULL, 10);
            break;
        case OPAL_INT64:
            kv->data.int64 = strtol(tmp3, NULL, 10);
            break;
        case OPAL_UINT:
            kv->data.uint = strtoul(tmp3, NULL, 10);
            break;
        case OPAL_UINT8:
            kv->data.uint8 = strtoul(tmp3, NULL, 10);
            break;
        case OPAL_UINT16:
            kv->data.uint16 = strtoul(tmp3, NULL, 10);
            break;
        case OPAL_UINT32:
            kv->data.uint32 = strtoul(tmp3, NULL, 10);
            break;
        case OPAL_UINT64:
            kv->data.uint64 = strtoul(tmp3, NULL, 10);
            break;
        case OPAL_BYTE_OBJECT:
            if (size == 0xffff) {
                kv->data.bo.bytes = NULL;
                kv->data.bo.size = 0;
            } else {
                kv->data.bo.bytes = malloc(size);
                memcpy(kv->data.bo.bytes, tmp3, size);
                kv->data.bo.size = size;
            }
            break;
        default:
            opal_output(0, "UNSUPPORTED TYPE %d", stored_type);
            return;
        }

        /* keep going and cache everything locally */
        offset = (size_t) (tmp3 - tmp_val) + size;
    }
    proc_data->loaded = true;

    free (tmp_val);
}

static int store(struct opal_dstore_base_module_t *imod,
                 const opal_identifier_t *uid,
                 opal_value_t *val)
{
    int rc;
    opal_value_t *kv;
    opal_dstore_proc_data_t *proc_data;
    opal_identifier_t id;
    mca_dstore_pmi_module_t *mod;

    mod = (mca_dstore_pmi_module_t*)imod;


    /* to protect alignment, copy the data across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                         "dstore:pmi:store: storing %s for proc %" PRIu64 "",
                         val->key, id));

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = opal_dstore_base_lookup_proc(&mod->hash_data, id))) {
        /* unrecoverable error */
        OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                             "dstore:pmi:store: storing data for proc %" PRIu64 " unrecoverably failed",
                             id));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    if (proc_data->loaded) {
        return OPAL_SUCCESS;
    }

    /* add it to our PMI payload */
    if (OPAL_SUCCESS != (rc = pmi_store_encoded(mod, val->key, (void*)&val->data, val->type))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }
    /* retain a local copy */
    kv = opal_dstore_base_lookup_keyval(proc_data, val->key);
    OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                         "dstore:hash:store: %s key %s[%s] for proc %" PRIu64 "",
                         (NULL == kv ? "storing" : "updating"),
                         val->key, opal_dss.lookup_data_type(val->type), id));
    
    if (NULL != kv) {
        opal_list_remove_item(&proc_data->data, &kv->super);
        OBJ_RELEASE(kv);
    }
    /* create the copy */
    if (OPAL_SUCCESS != (rc = opal_dss.copy((void**)&kv, val, OPAL_VALUE))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }
    opal_list_append(&proc_data->data, &kv->super);

    return OPAL_SUCCESS;
}

static void commit(struct opal_dstore_base_module_t *imod,
                   const opal_identifier_t *uid)
{
    mca_dstore_pmi_module_t *mod;
    opal_identifier_t id;

    mod = (mca_dstore_pmi_module_t*)imod;
    /* to protect alignment, copy the identifier across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    /* commit the packed data to PMI */
    pmi_commit_packed(mod, id);
    
    int rc = mca_common_pmi_commit(mod->pmi_kvs_name);
    if( OPAL_SUCCESS != rc ){
        // TODO: What we do here? failure exit?

    }
}

static int fetch(struct opal_dstore_base_module_t *imod,
                 const opal_identifier_t *uid,
                 const char *key, opal_list_t *kvs)
{
    opal_dstore_proc_data_t *proc_data;
    mca_dstore_pmi_module_t *mod;
    opal_identifier_t id;
    int rc;
    opal_value_t *kv, *knew;

    mod = (mca_dstore_pmi_module_t*)imod;

    /* to protect alignment, copy the identifier across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    /* get the hash entry for this proc */
    if (NULL == (proc_data = opal_dstore_base_lookup_proc(&mod->hash_data, id))) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (!proc_data->loaded) {
        /* new proc - go get its data */
        cache_keys_locally(mod, id, proc_data);
    }

    /* all keys will be available internally now. so
     * retrieve the data from our hash table
     */

    /* if the key is NULL, that we want everything */
    if (NULL == key) {
        OPAL_LIST_FOREACH(kv, &proc_data->data, opal_value_t) {
            /* copy the value */
            if (OPAL_SUCCESS != (rc = opal_dss.copy((void**)&knew, kv, OPAL_VALUE))) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
            /* add it to the output list */
            opal_list_append(kvs, &knew->super);
        }
        return OPAL_SUCCESS;
    }

    /* find the value */
    if (NULL == (kv = opal_dstore_base_lookup_keyval(proc_data, key))) {
        OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                             "dstore_pmi:fetch key %s for proc %" PRIu64 " not found",
                             (NULL == key) ? "NULL" : key, id));
        return OPAL_ERR_NOT_FOUND;
    }

    /* create the copy */
    if (OPAL_SUCCESS != (rc = opal_dss.copy((void**)&knew, kv, OPAL_VALUE))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }
    /* add it to the output list */
    opal_list_append(kvs, &knew->super);

    return OPAL_SUCCESS;
}

static int remove_data(struct opal_dstore_base_module_t *imod,
                       const opal_identifier_t *uid, const char *key)
{
    opal_value_t *kv;
    opal_identifier_t id;
    mca_dstore_pmi_module_t *mod;
    opal_dstore_proc_data_t *proc_data;

    mod = (mca_dstore_pmi_module_t*)imod;

    /* to protect alignment, copy the identifier across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    /* lookup the specified proc */
    if (NULL == (proc_data = opal_dstore_base_lookup_proc(&mod->hash_data, id))) {
        /* no data for this proc */
        return OPAL_SUCCESS;
    }

    /* if key is NULL, remove all data for this proc */
    if (NULL == key) {
        while (NULL != (kv = (opal_value_t *) opal_list_remove_first(&proc_data->data))) {
            OBJ_RELEASE(kv);
        }
        /* remove the proc_data object itself from the jtable */
        opal_hash_table_remove_value_uint64(&mod->hash_data, id);
        /* cleanup */
        OBJ_RELEASE(proc_data);
        return OPAL_SUCCESS;
    }

    /* remove this item */
    OPAL_LIST_FOREACH(kv, &proc_data->data, opal_value_t) {
        if (0 == strcmp(key, kv->key)) {
            opal_list_remove_item(&proc_data->data, &kv->super);
            OBJ_RELEASE(kv);
            break;
        }
    }

    return OPAL_SUCCESS;
}

static char* setup_key(mca_dstore_pmi_module_t *mod,
                       opal_identifier_t name, const char *key)
{
    char *pmi_kvs_key;

    if (mod->pmi_keylen_max <= asprintf(&pmi_kvs_key, "%" PRIu64 "-%s",
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
