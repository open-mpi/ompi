/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include <stdio.h>

#include "orte/orte_constants.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/class/orte_bitmap.h"


#define SIZE_OF_CHAR (sizeof(char) * 8)
#define DEFAULT_BITMAP_SIZE    64

static void orte_bitmap_construct(orte_bitmap_t *bm);
static void orte_bitmap_destruct(orte_bitmap_t *bm);

OBJ_CLASS_INSTANCE(orte_bitmap_t, opal_object_t,
                   orte_bitmap_construct, orte_bitmap_destruct);


static void
orte_bitmap_construct(orte_bitmap_t *bm)
{
    orte_std_cntr_t size;

    size = DEFAULT_BITMAP_SIZE / SIZE_OF_CHAR;

    bm->array_size = size + ((size % SIZE_OF_CHAR == 0) ? 0 : 1);
    bm->bitmap = (unsigned char *) malloc(bm->array_size);
    bm->legal_numbits = SIZE_OF_CHAR*bm->array_size;
    memset(bm->bitmap, 0, bm->array_size);
}


static void
orte_bitmap_destruct(orte_bitmap_t *bm)
{
    if (NULL != bm->bitmap) {
        free(bm->bitmap);
    }
}


int
orte_bitmap_resize(orte_bitmap_t *bm, orte_std_cntr_t bit)
{
    orte_std_cntr_t index, new_size, i;

    index = bit / SIZE_OF_CHAR;
    index += (bit % SIZE_OF_CHAR == 0) ? 0 : 1;

    if (index >= bm->array_size) {

            /* We need to allocate more space for the bitmap, since we are
               out of range. We dont throw any error here, because this is
               valid and we simply expand the bitmap */

            new_size = (index / bm->array_size + 1 ) * bm->array_size;

            /* New size is just a multiple of the original size to fit in
               the index. */

            bm->bitmap = (unsigned char *) realloc(bm->bitmap, new_size);
            if (NULL == bm->bitmap) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            /* zero out the new elements */
            for (i = bm->array_size; i < new_size; ++i) {
                bm->bitmap[i] = 0;
            }

            /* Update the array_size */
            bm->array_size = new_size;
            bm->legal_numbits = new_size*SIZE_OF_CHAR;
    }

    return ORTE_SUCCESS;
}

int
orte_bitmap_set_bit(orte_bitmap_t *bm, orte_std_cntr_t bit)
{
    orte_std_cntr_t index, offset;
    int rc;

    if (NULL == bm) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* make sure the bitmap covers the requested bit */
    if (ORTE_SUCCESS != (rc = orte_bitmap_resize(bm, bit))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    index = bit / SIZE_OF_CHAR;
    offset = bit % SIZE_OF_CHAR;

    /* Now set the bit */
    bm->bitmap[index] |= (1 << offset);

    return ORTE_SUCCESS;
}


int
orte_bitmap_clear_bit(orte_bitmap_t *bm, orte_std_cntr_t bit)
{
    orte_std_cntr_t index, offset;
    int rc;

    if (NULL == bm) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* make sure the bitmap covers the requested bit */
    if (ORTE_SUCCESS != (rc = orte_bitmap_resize(bm, bit))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    index = bit / SIZE_OF_CHAR;
    offset = bit % SIZE_OF_CHAR;

    /* now clear the bit */
    bm->bitmap[index] &= ~(1 << offset);
    return ORTE_SUCCESS;
}


int
orte_bitmap_is_set_bit(orte_bitmap_t *bm, orte_std_cntr_t bit)
{
    orte_std_cntr_t index, offset;

    if ((bit > bm->legal_numbits - 1) || (NULL == bm)) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    index = bit / SIZE_OF_CHAR;
    offset = bit % SIZE_OF_CHAR;

    if (index >= bm->array_size) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    if (0 != (bm->bitmap[index] & (1 << offset))) {
        return (int) true;
    }

    return (int) false;
}


int
orte_bitmap_clear_all_bits(orte_bitmap_t *bm)
{
    if (NULL == bm) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    memset(bm->bitmap, 0, bm->array_size);
    return ORTE_SUCCESS;
}


int
orte_bitmap_set_all_bits(orte_bitmap_t *bm)
{
    orte_std_cntr_t i;

    if (NULL == bm) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    for (i = 0; i < bm->array_size; ++i) {
        bm->bitmap[i] = ~((char) 0);
    }
    return ORTE_SUCCESS;
}


int
orte_bitmap_find_and_set_first_unset_bit(orte_bitmap_t *bm, orte_std_cntr_t *position)
{
    orte_std_cntr_t i = 0;
    unsigned char temp;
    unsigned char all_ones = 0xff;

    if (NULL == bm || NULL == position) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* Neglect all which don't have an unset bit */
    *position = 0;
    while((i < bm->array_size) && (bm->bitmap[i] == all_ones)) {
        ++i;
    }

    if (i == bm->array_size) {
        /* increase the bitmap size then */
        *position = bm->array_size * SIZE_OF_CHAR;
        return orte_bitmap_set_bit(bm, *position);
    }

    /* This one has an unset bit, find its bit number */

    temp = bm->bitmap[i];
    while (temp & 0x1) {
        ++(*position);
        temp >>= 1;
    }

    /* Now set the bit number */
    bm->bitmap[i] |= (bm->bitmap[i] + 1);

    (*position) += i * SIZE_OF_CHAR;
    return ORTE_SUCCESS;
}
