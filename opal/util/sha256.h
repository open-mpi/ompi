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
* Filename:   sha256.h
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Defines the API for the corresponding SHA1 implementation.
* Notes:      see https://github.com/B-Con/crypto-algorithms
*********************************************************************/

#ifndef OPAL_SHA256_H
#define OPAL_SHA256_H

/*************************** HEADER FILES ***************************/
#include <stddef.h>

/****************************** MACROS ******************************/
#define OPAL_SHA256_BLOCK_SIZE 32            // SHA256 outputs a 32 byte digest

/**************************** DATA TYPES ****************************/
typedef unsigned char BYTE;             // 8-bit byte
typedef unsigned int  WORD;             // 32-bit word, change to "long" for 16-bit machines

typedef struct {
    BYTE data[64];
    WORD datalen;
    unsigned long long bitlen;
    WORD state[8];
} opal_sha256_ctx;

/*********************** FUNCTION DECLARATIONS **********************/
void opal_sha256_init(opal_sha256_ctx *ctx);
void opal_sha256_update(opal_sha256_ctx *ctx, const BYTE data[], size_t len);
void opal_sha256_final(opal_sha256_ctx *ctx, BYTE hash[]);

#endif   // OPAL_SHA256_H
