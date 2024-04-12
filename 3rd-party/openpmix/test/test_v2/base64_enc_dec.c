/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2020-2021 Triad National Security, LLC. All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

/* This code was taken from Open MPI project, file
   opal/mca/pmix/base/pmix_base_fns.c
*/

#include "test_common.h"
#include <assert.h>
#include <string.h>

/* base64 encoding with illegal characters removed ('=' is replaced by ' ') */
static inline unsigned char pmixt_base64_encsym(unsigned char value)
{
    if (value >= 64) {
        TEST_ERROR(("Unknown value passed to encoding function, exiting."));
        exit(1);
    }

    if (value < 26) {
        return 'A' + value;
    } else if (value < 52) {
        return 'a' + (value - 26);
    } else if (value < 62) {
        return '0' + (value - 52);
    }

    return (62 == value) ? '+' : '/';
}

static inline unsigned char pmixt_base64_decsym(unsigned char value)
{
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

static inline void pmixt_base64_encode_block(const unsigned char in[3], char out[4], int len)
{
    out[0] = pmixt_base64_encsym(in[0] >> 2);
    // len is the length of in[] - we need to make sure we don't reference uninitialized data, hence
    // the conditionals
    out[1] = 1 < len ? pmixt_base64_encsym(((in[0] & 0x03) << 4) | ((in[1] & 0xf0) >> 4))
                     : pmixt_base64_encsym((in[0] & 0x03) << 4);

    out[2] = 1 < len ? pmixt_base64_encsym((in[1] & 0x0f) << 2) : ' ';
    out[2] = 2 < len ? pmixt_base64_encsym(((in[1] & 0x0f) << 2) | ((in[2] & 0xc0) >> 6)) : out[2];
    out[3] = 2 < len ? pmixt_base64_encsym(in[2] & 0x3f) : ' ';
}

static inline int pmixt_base64_decode_block(const char in[4], unsigned char out[3])
{
    char in_dec[4];

    in_dec[0] = pmixt_base64_decsym(in[0]);
    in_dec[1] = pmixt_base64_decsym(in[1]);
    in_dec[2] = pmixt_base64_decsym(in[2]);
    in_dec[3] = pmixt_base64_decsym(in[3]);

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

char *pmixt_encode(const void *val, size_t vallen)
{
    char *outdata, *tmp;
    size_t i, outlen;

    outlen = ((2 + vallen) * 4) / 3 + 2;
    outdata = calloc(outlen, 1);
    if (NULL == outdata) {
        return NULL;
    }

    for (i = 0, tmp = outdata; i < vallen; i += 3, tmp += 4) {
        pmixt_base64_encode_block((unsigned char *) val + i, tmp, vallen - i);
    }

    tmp[0] = (unsigned char) '\0';
    // TEST_VERBOSE(("vallen = %ld, outlen = %ld", vallen, outlen));
    return outdata;
}

ssize_t pmixt_decode(const char *data, void *decdata, size_t buffsz)
{
    size_t input_len = strlen(data) / 4;
    int out_len;
    size_t i;

    for (i = 0, out_len = 0; i < input_len; i++, data += 4) {
        // check against size of buffer to make sure we don't overrun
        if (buffsz - out_len >= 1) {
            out_len += pmixt_base64_decode_block(data, ((unsigned char *)(decdata) + (3 * i)) );
        } else {
            assert((buffsz - out_len) >= 1);
            exit(1);
        }
    }

    return out_len;
}
