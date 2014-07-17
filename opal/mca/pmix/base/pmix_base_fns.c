/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "opal_config.h"
#include "opal/constants.h"


#include <regex.h>

#include <time.h>
#include <string.h>

#include "opal_stdint.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/mca/pmix/base/pmix_base_fns.h"

#define OPAL_PMI_PAD  10

//static char *pmi_packed_data = NULL;
//static int pmi_pack_key = 0;
//static int pmi_packed_data_off = 0;

int pmi_store_encoded(const char *key, const void *data,
        opal_data_type_t type, char** buffer, int* length)
{
    opal_byte_object_t *bo;
    size_t data_len = 0;
    size_t needed;

    char* pmi_packed_data = *buffer;
    int pmi_packed_data_off = *length;

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

    *length = pmi_packed_data_off;
    return OPAL_SUCCESS;
}

char* setup_key(opal_identifier_t* name, const char *key, int pmix_keylen_max)
{
    char *pmi_kvs_key;

    if (pmix_keylen_max <= asprintf(&pmi_kvs_key, "%" PRIu64 "-%s",
                                        *name, key)) {
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
char *pmi_encode(const void *val, size_t vallen) 
{
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

uint8_t *pmi_decode (const char *data, size_t *retlen) 
{
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
