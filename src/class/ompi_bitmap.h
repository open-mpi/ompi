/*
 * $HEADER$
 *
 */

/** @file
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
 * Since these bitmaps are only used to track fortran handles (which
 * MPI defines to be int's), it is assumed that we can never have more
 * than OMPI_FORTRAN_HANDLE_MAX (which is min(INT_MAX, fortran
 * INTEGER max)).
 */

#ifndef OMPI_BITMAP_H
#define OMPI_BITMAP_H

#include "ompi_config.h"

#include <string.h>

#include "include/types.h"
#include "class/ompi_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct ompi_bitmap_t {
    ompi_object_t super; /**< Subclass of ompi_object_t */
    unsigned char *bitmap; /**< The actual bitmap array of characters */
    int array_size;  /**< The actual array size that maintains the bitmap */
    int legal_numbits; /**< The number of bits which are legal (the
			    actual bitmap may contain more bits, since
			    it needs to be rounded to the nearest
			    char  */
};

typedef struct ompi_bitmap_t ompi_bitmap_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_bitmap_t);

/**
 * Initializes the bitmap and sets its size. This must be called
 * before the bitmap can be actually used
 *
 * @param  bitmap The input bitmap (IN)
 * @param  size   The initial size of the bitmap in terms of bits (IN)
 * @return OMPI error code or success
 *
 */
OMPI_DECLSPEC int ompi_bitmap_init (ompi_bitmap_t *bm, int size);


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
OMPI_DECLSPEC int ompi_bitmap_set_bit(ompi_bitmap_t *bm, int bit); 


/**
 * Clear/unset a bit of the bitmap. If the bit is beyond the current
 * size of the bitmap, an error is returned
 *
 * @param  bitmap The input bitmap (IN)
 * @param  bit    The bit which is to be cleared (IN)
 * @return OMPI error code if the bit is out of range, else success
 *
 */
OMPI_DECLSPEC int ompi_bitmap_clear_bit(ompi_bitmap_t *bm, int bit);


/**
  * Find out if a bit is set in the bitmap
  *
  * @param  bitmap  The input bitmap (IN)
  * @param  bit     The bit which is to be checked (IN)
  * @return OMPI error code if the bit is out of range
  *         1 if the bit is set
  *         0 if the bit is not set
  *
  */
OMPI_DECLSPEC int ompi_bitmap_is_set_bit(ompi_bitmap_t *bm, int bit);


/**
 * Find the first clear bit in the bitmap and set it
 *
 * @param  bitmap     The input bitmap (IN)
 * @param  position   Position of the first clear bit (OUT)

 * @return err        OMPI_SUCCESS on success
 */
OMPI_DECLSPEC int ompi_bitmap_find_and_set_first_unset_bit(ompi_bitmap_t *bm, 
                                                           int *position); 


/**
 * Clear all bits in the bitmap
 *
 * @param bitmap The input bitmap (IN)
 * @return OMPI error code if bm is NULL
 * 
 */
OMPI_DECLSPEC int ompi_bitmap_clear_all_bits(ompi_bitmap_t *bm);


/**
 * Set all bits in the bitmap
 * @param bitmap The input bitmap (IN)
 * @return OMPI error code if bm is NULL
 *
 */
OMPI_DECLSPEC int ompi_bitmap_set_all_bits(ompi_bitmap_t *bm);


/**
 * Gives the current size (number of bits) in the bitmap. This is the
 * legal (accessible) number of bits
 *
 * @param bitmap The input bitmap (IN)
 * @return OMPI error code if bm is NULL
 *
 */
static inline int ompi_bitmap_size(ompi_bitmap_t *bm)
{
    return (NULL == bm) ? 0 : bm->legal_numbits;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
    
#endif
