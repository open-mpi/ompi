/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2014-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <stdio.h>
#include <limits.h>

#include "pmix_common.h"
#include "src/class/pmix_bitmap.h"

/* The number of bits in the underlying type of the bitmap field
 * in the pmix_bitmap_t struct
 */
#define SIZE_OF_BASE_TYPE  64

static void pmix_bitmap_construct(pmix_bitmap_t *bm);
static void pmix_bitmap_destruct(pmix_bitmap_t *bm);

PMIX_CLASS_INSTANCE(pmix_bitmap_t, pmix_object_t,
                    pmix_bitmap_construct, pmix_bitmap_destruct);


static void
pmix_bitmap_construct(pmix_bitmap_t *bm)
{
    bm->bitmap = NULL;
    bm->array_size = 0;
    bm->max_size = INT_MAX;
}


static void
pmix_bitmap_destruct(pmix_bitmap_t *bm)
{
    if (NULL != bm->bitmap) {
        free(bm->bitmap);
        bm->bitmap = NULL;
    }
}


int pmix_bitmap_set_max_size (pmix_bitmap_t *bm, int max_size)
{
    if (NULL == bm) {
        return PMIX_ERR_BAD_PARAM;
    }

    /*
     * Only if the caller wants to set the maximum size,
     * we set it (in numbers of bits!), otherwise it is
     * set to INT_MAX in the constructor.
     */
    bm->max_size = (int)(((size_t)max_size + SIZE_OF_BASE_TYPE - 1) / SIZE_OF_BASE_TYPE);

    return PMIX_SUCCESS;
}


int
pmix_bitmap_init(pmix_bitmap_t *bm, int size)
{
    /*
     * Only if the caller set the maximum size before initializing,
     * we test here (in numbers of bits!)
     * By default, the max size is INT_MAX, set in the constructor.
     */
    if ((size <= 0) || (NULL == bm) || (size > bm->max_size)) {
        return PMIX_ERR_BAD_PARAM;
    }

    bm->array_size = (int)(((size_t)size + SIZE_OF_BASE_TYPE - 1) / SIZE_OF_BASE_TYPE);
    if( NULL != bm->bitmap ) {
        free(bm->bitmap);
        if(bm->max_size < bm->array_size)
            bm->max_size = bm->array_size;
    }
    bm->bitmap = (uint64_t*) malloc(bm->array_size * sizeof(uint64_t));
    if (NULL == bm->bitmap) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    pmix_bitmap_clear_all_bits(bm);
    return PMIX_SUCCESS;
}


int
pmix_bitmap_set_bit(pmix_bitmap_t *bm, int bit)
{
    int index, offset, new_size;

    if ((bit < 0) || (NULL == bm) || (bit > bm->max_size)) {
        return PMIX_ERR_BAD_PARAM;
    }

    index = bit / SIZE_OF_BASE_TYPE;
    offset = bit % SIZE_OF_BASE_TYPE;

    if (index >= bm->array_size) {

        /* We need to allocate more space for the bitmap, since we are
         out of range. We don't throw any error here, because this is
         valid and we simply expand the bitmap */

        new_size = index + 1;
        if( new_size > bm->max_size )
            new_size = bm->max_size;

        /* New size is just a multiple of the original size to fit in
         the index. */
        bm->bitmap = (uint64_t*)realloc(bm->bitmap, new_size*sizeof(uint64_t));
        if (NULL == bm->bitmap) {
            return PMIX_ERR_OUT_OF_RESOURCE;
        }

        /* zero out the new elements */
        memset(&bm->bitmap[bm->array_size], 0, (new_size - bm->array_size) * sizeof(uint64_t));

        /* Update the array_size */
        bm->array_size = new_size;
    }

    /* Now set the bit */
    bm->bitmap[index] |= (1UL << offset);

    return PMIX_SUCCESS;
}


int
pmix_bitmap_clear_bit(pmix_bitmap_t *bm, int bit)
{
    int index, offset;

    if ((bit < 0) || NULL == bm || (bit >= (bm->array_size * SIZE_OF_BASE_TYPE))) {
        return PMIX_ERR_BAD_PARAM;
    }

    index = bit / SIZE_OF_BASE_TYPE;
    offset = bit % SIZE_OF_BASE_TYPE;

    bm->bitmap[index] &= ~(1UL << offset);
    return PMIX_SUCCESS;
}


bool
pmix_bitmap_is_set_bit(pmix_bitmap_t *bm, int bit)
{
    int index, offset;

    if ((bit < 0) || NULL == bm || (bit >= (bm->array_size * SIZE_OF_BASE_TYPE))) {
        return false;
    }

    index = bit / SIZE_OF_BASE_TYPE;
    offset = bit % SIZE_OF_BASE_TYPE;

    if (0 != (bm->bitmap[index] & (1UL << offset))) {
        return true;
    }

    return false;
}


int
pmix_bitmap_clear_all_bits(pmix_bitmap_t *bm)
{
    if (NULL == bm) {
        return PMIX_ERR_BAD_PARAM;
    }

    memset(bm->bitmap, 0, bm->array_size * sizeof(uint64_t));
    return PMIX_SUCCESS;
}


int
pmix_bitmap_set_all_bits(pmix_bitmap_t *bm)
{
    if (NULL == bm) {
        return PMIX_ERR_BAD_PARAM;
    }

    memset(bm->bitmap, 0xff, bm->array_size * sizeof(uint64_t));

    return PMIX_SUCCESS;
}


int
pmix_bitmap_find_and_set_first_unset_bit(pmix_bitmap_t *bm, int *position)
{
    int i = 0;
    uint64_t temp, all_ones = 0xffffffffffffffffUL;

    if (NULL == bm) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* Neglect all which don't have an unset bit */
    *position = 0;
    while((i < bm->array_size) && (bm->bitmap[i] == all_ones)) {
        ++i;
    }

    if (i == bm->array_size) {
        /* increase the bitmap size then */
        *position = bm->array_size * SIZE_OF_BASE_TYPE;
        return pmix_bitmap_set_bit(bm, *position);
    }

    /* This one has an unset bit, find its bit number */

    temp = bm->bitmap[i];
    bm->bitmap[i] |= (bm->bitmap[i] + 1); /* Set the first zero bit */
    temp ^= bm->bitmap[i];  /* Compute the change: the first unset bit in the original number */
    while( !(temp & 0x1) ) {
        ++(*position);
        temp >>= 1;
    }

    (*position) += i * SIZE_OF_BASE_TYPE;
    return PMIX_SUCCESS;
}

int pmix_bitmap_bitwise_and_inplace(pmix_bitmap_t *dest, pmix_bitmap_t *right)
{
    int i;

    /*
     * Sanity check
     */
    if( NULL == dest || NULL == right ) {
        return PMIX_ERR_BAD_PARAM;
    }
    if( dest->array_size != right->array_size ) {
        return PMIX_ERR_BAD_PARAM;
    }

    /*
     * Bitwise AND
     */
    for(i = 0; i < dest->array_size; ++i) {
        dest->bitmap[i] &= right->bitmap[i];
    }

    return PMIX_SUCCESS;
}

int pmix_bitmap_bitwise_or_inplace(pmix_bitmap_t *dest, pmix_bitmap_t *right)
{
    int i;

    /*
     * Sanity check
     */
    if( NULL == dest || NULL == right ) {
        return PMIX_ERR_BAD_PARAM;
    }
    if( dest->array_size != right->array_size ) {
        return PMIX_ERR_BAD_PARAM;
    }

    /*
     * Bitwise OR
     */
    for(i = 0; i < dest->array_size; ++i) {
        dest->bitmap[i] |= right->bitmap[i];
    }

    return PMIX_SUCCESS;
}

int pmix_bitmap_bitwise_xor_inplace(pmix_bitmap_t *dest, pmix_bitmap_t *right)
{
    int i;

    /*
     * Sanity check
     */
    if( NULL == dest || NULL == right ) {
        return PMIX_ERR_BAD_PARAM;
    }
    if( dest->array_size != right->array_size ) {
        return PMIX_ERR_BAD_PARAM;
    }

    /*
     * Bitwise XOR
     */
    for(i = 0; i < dest->array_size; ++i) {
        dest->bitmap[i] ^= right->bitmap[i];
    }

    return PMIX_SUCCESS;
}

bool pmix_bitmap_are_different(pmix_bitmap_t *left, pmix_bitmap_t *right)
{
    int i;

    /*
     * Sanity check
     */
    if( NULL == left || NULL == right ) {
        return PMIX_ERR_BAD_PARAM;
    }

    if( pmix_bitmap_size(left) != pmix_bitmap_size(right) ) {
        return true;
    }

    /*
     * Direct comparison
     */
    for(i = 0; i < left->array_size; ++i) {
        if( left->bitmap[i] != right->bitmap[i] ) {
            return true;
        }
    }

    return false;
}

char * pmix_bitmap_get_string(pmix_bitmap_t *bitmap)
{
    int i;
    char *bitmap_str = NULL;

    if( NULL == bitmap) {
        return NULL;
    }

    bitmap_str = malloc(bitmap->array_size * SIZE_OF_BASE_TYPE + 1);
    if (NULL == bitmap_str) {
        return NULL;
    }
    bitmap_str[bitmap->array_size * SIZE_OF_BASE_TYPE] = '\0';

    for( i = 0; i < (bitmap->array_size * SIZE_OF_BASE_TYPE); ++i) {
        if( pmix_bitmap_is_set_bit(bitmap, i) ) {
            bitmap_str[i] = 'X';
        } else {
            bitmap_str[i] = '_';
        }
    }

    return bitmap_str;
}

int pmix_bitmap_num_unset_bits(pmix_bitmap_t *bm, int len)
{
    return (len - pmix_bitmap_num_set_bits(bm, len));
}

int pmix_bitmap_num_set_bits(pmix_bitmap_t *bm, int len)
{
    int i, cnt = 0;
    uint64_t val;

#if PMIX_ENABLE_DEBUG
    if ((len < 0) || NULL == bm || (len >= (bm->array_size * SIZE_OF_BASE_TYPE))) {
        return 0;
    }
#endif

    for(i = 0; i < len; ++i) {
        if( 0 == (val = bm->bitmap[i]) ) continue;
        /*  Peter Wegner in CACM 3 (1960), 322. This method goes through as many
         *  iterations as there are set bits. */
        for( ; val; cnt++ ) {
            val &= val - 1;  /* clear the least significant bit set */
        }
    }

    return cnt;
}

bool pmix_bitmap_is_clear(pmix_bitmap_t *bm)
{
    int i;

    for (i = 0; i < bm->array_size; ++i) {
        if (0 != bm->bitmap[i]) {
            return false;
        }
    }
    return true;
}
