/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 *
 */

/** @file
 *
 * See ompi_bitmap.h for an explanation of why there is a split
 * between OMPI and ORTE for this generic class.
 *
 *  A bitmap implementation. The bits start off with 0, so this bitmap
 *  has bits numbered as bit 0, bit 1, bit 2 and so on. This bitmap
 *  has auto-expansion capabilities, that is once the size is set
 *  during init, it can be automatically expanded by setting the bit
 *  beyond the current size. But note, this is allowed just when the
 *  bit is set -- so the valid functions are set_bit and
 *  find_and_set_bit. Other functions like clear, if passed a bit
 *  outside the initialized range will result in an error.
 *
 */

#ifndef ORTE_BITMAP_H
#define ORTE_BITMAP_H

#include "orte_config.h"
#include "orte/orte_types.h"

#include <string.h>

#include "opal/types.h"
#include "opal/class/opal_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct orte_bitmap_t {
    opal_object_t super; /**< Subclass of opal_object_t */
    unsigned char *bitmap; /**< The actual bitmap array of characters */
    orte_std_cntr_t array_size;  /**< The actual array size that maintains the bitmap */
    orte_std_cntr_t legal_numbits; /**< The number of bits which are legal (the
                actual bitmap may contain more bits, since
                it needs to be rounded to the nearest
                char  */
};

typedef struct orte_bitmap_t orte_bitmap_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_bitmap_t);


/**
 * Sizes the bitmap to ensure it has at least the specified number of
 * bits in it. If it already does, then nothing happens. However, if the
 * bitmap is too small, then it is resized to accommodate the bit, with
 * all bits in the new space "cleared"
 *
 * @param  bitmap The input bitmap (IN)
 * @param  bit   The bit that must be "covered" by the bitmap (IN)
 * @return ORTE error code or success
 *
 */
ORTE_DECLSPEC int orte_bitmap_resize(orte_bitmap_t *bm, orte_std_cntr_t bit);


/**
 * Set a bit of the bitmap. If the bit asked for is beyond the current
 * size of the bitmap, then the bitmap is extended to accomodate the
 * bit
 *
 * @param  bitmap The input bitmap (IN)
 * @param  bit    The bit which is to be set (IN)
 * @return OMPI error code or success
 *
 */
ORTE_DECLSPEC int orte_bitmap_set_bit(orte_bitmap_t *bm, orte_std_cntr_t bit);


/**
 * Clear/unset a bit of the bitmap. If the bit is beyond the current
 * size of the bitmap, an error is returned
 *
 * @param  bitmap The input bitmap (IN)
 * @param  bit    The bit which is to be cleared (IN)
 * @return ORTE error code if the bit is out of range, else success
 *
 */
ORTE_DECLSPEC int orte_bitmap_clear_bit(orte_bitmap_t *bm, orte_std_cntr_t bit);


/**
  * Find out if a bit is set in the bitmap
  *
  * @param  bitmap  The input bitmap (IN)
  * @param  bit     The bit which is to be checked (IN)
  * @return ORTE error code if the bit is out of range
  *         1 if the bit is set
  *         0 if the bit is not set
  *
  */
ORTE_DECLSPEC int orte_bitmap_is_set_bit(orte_bitmap_t *bm, orte_std_cntr_t bit);


/**
 * Find the first clear bit in the bitmap and set it
 *
 * @param  bitmap     The input bitmap (IN)
 * @param  position   Position of the first clear bit (OUT)

 * @return err        ORTE_SUCCESS on success
 */
ORTE_DECLSPEC int orte_bitmap_find_and_set_first_unset_bit(orte_bitmap_t *bm,
                                                           orte_std_cntr_t *position);


/**
 * Clear all bits in the bitmap
 *
 * @param bitmap The input bitmap (IN)
 * @return ORTE error code if bm is NULL
 *
 */
ORTE_DECLSPEC int orte_bitmap_clear_all_bits(orte_bitmap_t *bm);


/**
 * Set all bits in the bitmap
 * @param bitmap The input bitmap (IN)
 * @return ORTE error code if bm is NULL
 *
 */
ORTE_DECLSPEC int orte_bitmap_set_all_bits(orte_bitmap_t *bm);


/**
 * Gives the current size (number of bits) in the bitmap. This is the
 * legal (accessible) number of bits
 *
 * @param bitmap The input bitmap (IN)
 * @return ORTE error code if bm is NULL
 *
 */
static inline int orte_bitmap_size(orte_bitmap_t *bm)
{
    return (NULL == bm) ? 0 : (int)bm->legal_numbits;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
