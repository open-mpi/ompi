/*
 * $HEADER$
 *
 */

/** @file
 *
 *  A bitmap implementation. The bits start off with 0, so this
 *  bitmap has bits numbered as bit 0, bit 1, bit 2 and so on
 */

#ifndef LAM_BITMAP_H
#define LAM_BITMAP_H

#include <string.h>

#include "lam_config.h"
#include "include/types.h"
#include "lfc/lam_object.h"

/* VPS: Just to compile right now, has to move later on */
#define LAM_ERR_SYSRESOURCE -1

extern lam_class_t lam_bitmap_t_class;

struct lam_bitmap_t {
    lam_object_t super; /**< Subclass of lam_object_t */
    char *bitmap;       /**< The actual bitmap array of characters */
    size_t size;        /**< The number of bits in the bitmap */
    size_t array_size;  /**< The actual array size that maintains the bitmap */
};

typedef struct lam_bitmap_t lam_bitmap_t;

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * Initializes the bitmap and sets its size. This must be called
 * before the bitmap can be actually used
 *
 * @param  bitmap The input bitmap (IN)
 * @param  size   The initial size of the bitmap in terms of bits (IN)
 * @return LAM error code or success
 *
 */
int lam_bitmap_init (lam_bitmap_t *bm, size_t size);


/**
 * Set a bit of the bitmap. If the bit asked for is beyond the current
 * size of the bitmap, then the bitmap is extended to accomodate the
 * bit
 *
 * @param  bitmap The input bitmap (IN)
 * @param  bit    The bit which is to be set (IN)
 * @return LAM error code or success
 *
 */
int lam_bitmap_set_bit(lam_bitmap_t *bm, int bit); 


/**
 * Clear/unset a bit of the bitmap. If the bit is beyond the current
 * size of the bitmap, an error is returned
 *
 * @param  bitmap The input bitmap (IN)
 * @param  bit    The bit which is to be cleared (IN)
 * @return LAM error code if the bit is out of range, else success
 *
 */
int lam_bitmap_clear_bit(lam_bitmap_t *bm, int bit);


/**
  * Find out if a bit is set in the bitmap
  *
  * @param  bitmap  The input bitmap (IN)
  * @param  bit     The bit which is to be checked (IN)
  * @return LAM error code if the bit is out of range
  *         1 if the bit is set
  *         0 if the bit is not set
  *
  */
int lam_bitmap_is_set_bit(lam_bitmap_t *bm, int bit);


/**
 * Find the first clear bit in the bitmap and set it
 *
 * @param  bitmap     The input bitmap (IN)
 * @return bit number The bit number of the first unset bit
 */
int lam_bitmap_find_and_set_first_unset_bit(lam_bitmap_t *bm); 


/**
 * Clear all bits in the bitmap
 *
 * @param bitmap The input bitmap (IN)
 * 
 */
void lam_bitmap_clear_all_bits(lam_bitmap_t *bm);


/**
 * Set all bits in the bitmap
 * @param bitmap The input bitmap (IN)
 *
 */
void lam_bitmap_set_all_bits(lam_bitmap_t *bm);


/**
 * Gives the current size (number of bits) in the bitmap
 *
 * @param bitmap The input bitmap (IN)
 *
 */
size_t lam_bitmap_size (lam_bitmap_t *bm);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
    
#endif
