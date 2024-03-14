/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2022      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*********************************************************************
* Filename:   sha256.c
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Performs known-answer tests on the corresponding SHA1
              implementation. These tests do not encompass the full
              range of available test vectors, however, if the tests
              pass it is very, very likely that the code is correct
              and was compiled properly. This code also serves as
              example usage of the functions.
*********************************************************************/

/*************************** HEADER FILES ***************************/
#include <stdio.h>
#include <memory.h>
#include <string.h>
#include "support.h"
#include "opal/util/sha256.h"

/*********************** FUNCTION DEFINITIONS ***********************/
static int sha256_test(void)
{
    BYTE text1[] = {"abc"};
    BYTE text2[] = {"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"};
    BYTE text3[] = {"aaaaaaaaaa"};
    BYTE hash1[OPAL_SHA256_BLOCK_SIZE] = {0xba,0x78,0x16,0xbf,0x8f,0x01,0xcf,0xea,0x41,0x41,0x40,0xde,0x5d,0xae,0x22,0x23,
                                     0xb0,0x03,0x61,0xa3,0x96,0x17,0x7a,0x9c,0xb4,0x10,0xff,0x61,0xf2,0x00,0x15,0xad};
    BYTE hash2[OPAL_SHA256_BLOCK_SIZE] = {0x24,0x8d,0x6a,0x61,0xd2,0x06,0x38,0xb8,0xe5,0xc0,0x26,0x93,0x0c,0x3e,0x60,0x39,
                                     0xa3,0x3c,0xe4,0x59,0x64,0xff,0x21,0x67,0xf6,0xec,0xed,0xd4,0x19,0xdb,0x06,0xc1};
    BYTE hash3[OPAL_SHA256_BLOCK_SIZE] = {0xcd,0xc7,0x6e,0x5c,0x99,0x14,0xfb,0x92,0x81,0xa1,0xc7,0xe2,0x84,0xd7,0x3e,0x67,
                                     0xf1,0x80,0x9a,0x48,0xa4,0x97,0x20,0x0e,0x04,0x6d,0x39,0xcc,0xc7,0x11,0x2c,0xd0};
    BYTE buf[OPAL_SHA256_BLOCK_SIZE];
    opal_sha256_ctx ctx;
    int idx;
    int pass = 1;

    opal_sha256_init(&ctx);
    opal_sha256_update(&ctx, text1, strlen((const char *)text1));
    opal_sha256_final(&ctx, buf);
    pass = pass && !memcmp(hash1, buf, OPAL_SHA256_BLOCK_SIZE);

    opal_sha256_init(&ctx);
    opal_sha256_update(&ctx, text2, strlen((const char *)text2));
    opal_sha256_final(&ctx, buf);
    pass = pass && !memcmp(hash2, buf, OPAL_SHA256_BLOCK_SIZE);

    opal_sha256_init(&ctx);
    for (idx = 0; idx < 100000; ++idx)
       opal_sha256_update(&ctx, text3, strlen((const char *)text3));
    opal_sha256_final(&ctx, buf);
    pass = pass && !memcmp(hash3, buf, OPAL_SHA256_BLOCK_SIZE);

    return pass;
}

int main(void)
{
    test_init("sha256 test");

    if (sha256_test()) {
        test_success();
    } else {
        test_failure("sh256 test failed");
    }

    return test_finalize();
}
