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
#if WANT_CRAY_PMI2_EXT
#include <pmi2.h>
#endif

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
static int store(opal_identifier_t id,
                 opal_db_locality_t locality,
                 const char *key, const void *object,
                 opal_data_type_t type);
static int store_pointer(opal_identifier_t proc,
                         opal_db_locality_t locality,
                         opal_value_t *kv);
static int fetch(opal_identifier_t proc,
                 const char *key, void **data, opal_data_type_t type);
static int fetch_pointer(opal_identifier_t proc,
                         const char *key,
                         void **data, opal_data_type_t type);
static int fetch_multiple(opal_identifier_t proc,
                          const char *key,
                          opal_list_t *kvs);
static int remove_data(opal_identifier_t proc, const char *key);

opal_db_base_module_t opal_db_pmi_module = {
    init,
    finalize,
    store,
    store_pointer,
    fetch,
    fetch_pointer,
    fetch_multiple,
    remove_data,
    NULL
};

static int pmi_encode(char *outdata, const void *val, size_t vallen);
static uint8_t* pmi_decode(char *data, size_t *retlen);
static int setup_pmi(void);
static char* setup_key(opal_identifier_t name, const char *key);

/* Local variables */
static char *pmi_kvs_name = NULL;
static int pmi_vallen_max = -1;
static int pmi_keylen_max = -1;

/* Because Cray uses PMI2 extensions for some, but not all,
 * PMI functions, we define a set of wrappers for those
 * common functions we will use
 */
static int kvs_put(const char *key, const char *value)
{
#if WANT_CRAY_PMI2_EXT
    return PMI2_KVS_Put(key, value);
#else
    return PMI_KVS_Put(pmi_kvs_name, key, value);
#endif
}

static int kvs_get(const char *key, char *value, int valuelen)
{
#if WANT_CRAY_PMI2_EXT
    int len;

    return PMI2_KVS_Get(pmi_kvs_name, PMI2_ID_NULL, key, value, valuelen, &len);
#else
    return PMI_KVS_Get(pmi_kvs_name, key, value, valuelen);
#endif
}

#if WANT_CRAY_PMI2_EXT
static char escape_char = '$';
static char *illegal = "/;=";
static char *sub = "012";
#endif

static int init(void)
{
    int rc;

    if (OPAL_SUCCESS != (rc = setup_pmi())) {
        OPAL_ERROR_LOG(rc);
    }

    return rc;
}

static void finalize(void)
{
    if (NULL != pmi_kvs_name) {
        free(pmi_kvs_name);
        pmi_kvs_name = NULL;
    }

}

static int store(opal_identifier_t proc,
                 opal_db_locality_t locality,
                 const char *key, const void *data, opal_data_type_t type)
{
    int i, rc;
    char *pmidata, *str, *localdata;
    int64_t i64;
    uint64_t ui64;
    opal_byte_object_t *bo;
    char *pmikey, *tmpkey, *tmp, sav;
    char **strdata=NULL;

    /* pass internal stores down to someone else */
    if (OPAL_DB_INTERNAL == locality) {
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:pmi:store: storing key %s[%s] for proc %" PRIu64 "",
                         key, opal_dss.lookup_data_type(type), proc));

    if (NULL == (pmikey = setup_key(proc, key))) {
	OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
	return OPAL_ERR_BAD_PARAM;
    }

    switch (type) {
    case OPAL_STRING:
#if WANT_CRAY_PMI2_EXT
        {
            /* the blasted Cray PMI implementation marked a number of common
             * ASCII characters as "illegal", so if we are on one of those
             * machines, then we have to replace those characters with something
             * else
             */
            size_t n, k;
            bool subbed;
            char *ptr;

            str = (char*)data;
            /* first, count how many characters need to be replaced - since Cray
             * is the source of the trouble, we only make this slow for them!
             */
            ptr = str;
            i=0;
            for (n=0; n < strlen(illegal); n++) {
                while (NULL != (tmp = strchr(ptr, illegal[n]))) {
                    i++;
                    ptr = tmp;
                    ptr++;
                }
            }
            /* stretch the string */
            ptr = (char*)malloc(sizeof(char) * (1 + strlen(str) + 2*i));
            /* now construct it */
            k=0;
            for (n=0; n < strlen(str); n++) {
                subbed = false;
                for (i=0; i < (int)strlen(illegal); i++) {
                    if (str[n] == illegal[i]) {
                        /* escape the character */
                        ptr[k++] = escape_char;
                        ptr[k++] = sub[i];
                        subbed = true;
                        break;
                    }
                }
                if (!subbed) {
                    ptr[k++] = str[i];
                }
            }
            /* pass the result */
            localdata = ptr;
        }
#else
        localdata = strdup((char*)data);
#endif
        str = localdata;
        while (pmi_vallen_max < (int)(OPAL_PMI_PAD + strlen(str))) {
            /* the string is too long, so we need to break it into
             * multiple sections
             */
            tmp = str + pmi_vallen_max - OPAL_PMI_PAD;
            sav = *tmp;
            *tmp = '\0';
            opal_argv_append_nosize(&strdata, str);
            *tmp = sav;
            str = tmp;
        }
        /* put whatever remains on the stack */
        opal_argv_append_nosize(&strdata, str);
        /* cleanup */
        free(localdata);
        /* the first value we put uses the original key, but
         * the data is prepended with the number of sections
         * required to hold the entire string
         */
        asprintf(&pmidata, "%d:%s", opal_argv_count(strdata), strdata[0]);
        OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                             "db:pmi:store: storing key %s data %s",
                             pmikey, pmidata));

        if (PMI_SUCCESS != (rc = kvs_put(pmikey, pmidata))) {
            OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
            free(pmidata);
            free(pmikey);
            opal_argv_free(strdata);
            return OPAL_ERROR;
        }
        free(pmidata);
        /* for each remaining segment, augment the key with the index */
        for (i=1; NULL != strdata[i]; i++) {
            asprintf(&tmpkey, "%s:%d", pmikey, i);
            OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                                 "db:pmi:store: storing key %s data %s",
                                 pmikey, strdata[i]));

            if (PMI_SUCCESS != (rc = kvs_put(tmpkey, strdata[i]))) {
                OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
                free(pmikey);
                opal_argv_free(strdata);
                return OPAL_ERROR;
            }
            free(tmpkey);
        }
        free(pmikey);
        opal_argv_free(strdata);
        return OPAL_SUCCESS;

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
        if (OPAL_SUCCESS != (rc = pmi_encode(pmidata, bo->bytes, bo->size))) {
            OPAL_ERROR_LOG(rc);
            free(pmidata);
            return rc;
        }
        break;

    default:
        OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
        return OPAL_ERR_NOT_SUPPORTED;
    }

    OPAL_OUTPUT_VERBOSE((10, opal_db_base_framework.framework_output,
                         "PUTTING KEY %s DATA %s",
                         pmikey, pmidata));

    rc = kvs_put(pmikey, pmidata);
    if (PMI_SUCCESS != rc) {
	OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
	return OPAL_ERROR;
    }
    free(pmidata);
    free(pmikey);
    return OPAL_SUCCESS;
}

static int store_pointer(opal_identifier_t proc,
                         opal_db_locality_t locality,
                         opal_value_t *kv)
{
    int rc;

    /* pass internal stores down to someone else */
    if (OPAL_DB_INTERNAL == locality) {
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:pmi:store: storing pointer of key %s for proc %" PRIu64 "",
                         kv->key, proc));

    /* just push this to PMI */
    if (OPAL_SUCCESS != (rc = store(proc, locality, kv->key, (void*)&kv->data, kv->type))) {
        OPAL_ERROR_LOG(rc);
    }
    return rc;
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
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        free(tmp_val);
        return NULL;
    }

    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:pmi:fetch_string: received key %s DATA %s",
                         key, tmp_val));

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
            OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
            free(tmp_val);
            free(tmpkey);
            free(data);
            return NULL;
        }
        OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                             "db:pmi:fetch_string: received key %s DATA %s",
                             tmpkey, tmp_val));

        /* add it to our data */
        asprintf(&ptr, "%s%s", data, tmp_val);
        free(data);
        data = ptr;
        /* cleanup */
        free(tmpkey);
    }

    /* cleanup */
    free(tmp_val);

#if WANT_CRAY_PMI2_EXT
        {
            /* the blasted Cray PMI implementation marked a number of common
             * ASCII characters as "illegal", so if we are on one of those
             * machines, then replaced those characters with something
             * else - now recover them
             */
            size_t n, k;
            char *tmp;
            char conv[2];

            /* first, count how many characters were replaced - since Cray
             * is the source of the trouble, we only make this slow for them!
             */
            ptr = data;
            i=0;
            while (NULL != (tmp = strchr(ptr, escape_char))) {
                i++;
                ptr = tmp;
                ptr++;
            }
            /* shrink the string */
            ptr = (char*)malloc(sizeof(char) * (1 + strlen(data) - i));
            /* now construct it */
            k=0;
            conv[1] = '\0';
            for (n=0; n < strlen(data); n++) {
                if (escape_char == data[n]) {
                    /* the next character tells us which character
                     * was subbed out
                     */
                    n++;
                    conv[0] = data[n];
                    i = strtol(conv, NULL, 10);
                    ptr[k++] = illegal[i];
                } else {
                    ptr[k++] = data[n];
                }
            }
            /* pass the result */
            free(data);
            data = ptr;
        }
#endif

    return data;
}

static int fetch(const opal_identifier_t proc,
                 const char *key, void **data, opal_data_type_t type)
{
    opal_byte_object_t *boptr;
    uint16_t ui16;
    uint32_t ui32;
    int ival;
    unsigned int uival;
    char *pmikey;
    char tmp_val[1024];
    size_t sval;

    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:pmi:fetch: searching for key %s[%s] on proc %" PRIu64 "",
                         (NULL == key) ? "NULL" : key,
                         opal_dss.lookup_data_type(type), proc));

    /* if the key is NULL, that is an error */
    if (NULL == key) {
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        return OPAL_ERR_BAD_PARAM;
    }

    /* setup the key */
    if (NULL == (pmikey = setup_key(proc, key))) {
	OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
	return OPAL_ERR_BAD_PARAM;
    }

    /* check to see if they are looking for a string */
    if (OPAL_STRING == type) {
        /* might have been passed in multiple sections */
        *data = fetch_string(pmikey);
        free(pmikey);
        return OPAL_SUCCESS;
    }

    /* otherwise, retrieve the pmi keyval */
    if (NULL == (pmikey = setup_key(proc, key))) {
	OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
	return OPAL_ERR_BAD_PARAM;
    }
    if (PMI_SUCCESS != kvs_get(pmikey, tmp_val, pmi_vallen_max)) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        free(pmikey);
        return OPAL_ERR_NOT_FOUND;
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
        sval = 0;
        boptr = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        boptr->bytes = (uint8_t*)pmi_decode(tmp_val, &sval);
        boptr->size = sval;
        *data = boptr;
        break;
    default:
        OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
        return OPAL_ERR_NOT_SUPPORTED;
    }

    return OPAL_SUCCESS;
}

/* the only current use for fetch_pointer is to retrieve the
 * hostname for the process - so don't worry about other uses
 * here just yet
 */
static int fetch_pointer(opal_identifier_t proc,
                         const char *key,
                         void **data, opal_data_type_t type)
{
    /* has to be provided from local storage */
    return OPAL_ERR_TAKE_NEXT_OPTION;
}

static int fetch_multiple(opal_identifier_t proc,
                          const char *key,
                          opal_list_t *kvs)
{

    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:pmi:fetch_multiple: searching for key %s on proc %" PRIu64 "",
                         (NULL == key) ? "NULL" : key, proc));

    return OPAL_ERR_NOT_SUPPORTED;
}

static int remove_data(opal_identifier_t proc, const char *key)
{
    /* nothing to do here */
    return OPAL_SUCCESS;
}

static int setup_pmi(void)
{
    int max_length, rc;

#if WANT_CRAY_PMI2_EXT
    pmi_vallen_max = PMI2_MAX_VALLEN;
#else
    rc = PMI_KVS_Get_value_length_max(&pmi_vallen_max);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_Get_value_length_max");
        return OPAL_ERROR;
    }
#endif

#if WANT_CRAY_PMI2_EXT
    /* TODO -- is this ok */
    max_length = 1024;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_name_length_max(&max_length))) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_name_length_max");
        return OPAL_ERROR;
    }
#endif
    pmi_kvs_name = (char*)malloc(max_length);
    if (NULL == pmi_kvs_name) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

#if WANT_CRAY_PMI2_EXT
    rc = PMI2_Job_GetId(pmi_kvs_name, max_length);
#else
    rc = PMI_KVS_Get_my_name(pmi_kvs_name,max_length);
#endif
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_my_name");
        return OPAL_ERROR;
    }

#if WANT_CRAY_PMI2_EXT
    pmi_keylen_max = PMI2_MAX_KEYLEN;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_key_length_max(&pmi_keylen_max))) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_key_length_max");
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


/* PMI only supports strings. For now, do a simple base16 
 * encoding. Should do something smarter, both with the 
 * algorith used and its implementation. */
static int pmi_encode(char *outdata, const void *val, size_t vallen) {
    unsigned char *tmp = (unsigned char*)outdata;
    size_t i;

    /* check for size */
    if ((size_t)pmi_vallen_max < (2 + vallen * 4) / 3 + 1) {
        return OPAL_ERR_BAD_PARAM;
    }

    for (i = 0 ; i < vallen ; i += 3, tmp += 4) {
        pmi_base64_encode_block((unsigned char *) val + i, tmp, vallen - i);
    }

    tmp[0] = (unsigned char)'\0';

    return OPAL_SUCCESS;
}

static uint8_t* pmi_decode (char *data, size_t *retlen) {
    size_t input_len = strlen (data) / 4;
    unsigned char *ret, *val;
    int out_len;
    size_t i;

    /* default */
    *retlen = 0;

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
