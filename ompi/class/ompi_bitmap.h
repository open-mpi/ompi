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
 *
 * --------------------------------------------------------------------
 *
 * There are several classes that have forked between their OMPI and
 * ORTE implementations.  As of this writing:
 *
 * - ompi_bitmap and orte_bitmap
 * - ompi_pointer_array and orte_pointer_array
 * - opal_value_array and orte_value_array
 *
 * Short version: 
 *
 * They were split to accomodate a few differences between
 * requirements (e.g., "size" parameters being int vs. size_t).  It
 * would be nice to re-merge them someday; there's a few technical
 * issues that would need to be solved, but nothing impossible.  But
 * there's no pressing *need* to re-merge these, so they have fallen
 * somewhat low on the priority list of things to do.
 *
 * Longer version:
 *
 * Although these are generic functionality classes, the ORTE versions
 * split from the OMPI (soon to be OPAL) versions because of
 * restrictions imposed by the OMPI versions.  Specifically, the OMPI
 * versions specifically limit size arguments to "int" (in multiple
 * different ways, e.g.: types of "size" parameters to functions,
 * maximum allowable values of size parameters, etc.).  The ORTE
 * functions need most size parameters to be of type size_t, not int.
 * This is the most fundamental difference.  In C++, we could have
 * templated these functions, but we unfortunately can't easily do
 * that in C (in hindsight, perhaps some preprocessor macros might
 * have been sufficient, but...).
 *
 * Another, more subtle, reason why these were split because of the
 * scary word "FORTRAN" that appears in some of the upper value limit
 * checks in the OMPI versions.  That is, we always check for max size
 * against OMPI_FORTRAN_HANDLE_MAX.  This value is simply min(INT_MAX,
 * max value of Fortran INTEGER) -- it's the minimum of the maximum
 * values of integers in C and Fortran.  Usually, it's the same value
 * (2^32), but the macro is there to ensure that even if it's
 * different, we end up with a value that can be represented in both
 * languages.
 *
 * This is because the primary purpose of these classes is to serve as
 * an interface to the Fortran language bindings -- fortran handles
 * may be directly represented as indices into arrays or bitmaps.
 * Hence, the size has to be representable in both C and Fortran.
 *
 * Regardless, we need to check for *some* max value for the size of
 * these entites.  Perhaps the name "FORTRAN" in the macro is
 * scary/misleading -- it can certainly be changed in the future if it
 * would be more clear.  So it's ok to have a max -- but perhaps
 * changing the name would make it more palatable to both layers
 * (remebering that the max value is still going to be enormous --
 * usually 2^32; even if the size is of type size_t, you're going to
 * run out of memory long before you have 2^32 entries).  Or perhaps
 * the max value can be parameterized to depend on whether the size
 * type is int or size_t -- I'm sure something can be worked out.
 *
 * As mentioned above, if these really are the only two differences
 * (int vs. size_t and the max sentinel value) -- and I'm pretty sure
 * that they are -- then these classes can be re-merged someday,
 * resulting in less code to maintain.  This would be good for
 * long-term maintenance.  However, it's kinda low on the priority
 * list.  So it hasn't been done [yet].  If someone wants to do this,
 * please feel free!  :-)
 *
 * Given the fact that we just had yet another round of discussions
 * about the splitting / re-merging of these classes (26 May 2005), it
 * was decided to put this big comment in the hopes of:
 *
 * - someday motivating someone to re-merge the classes
 * - prevent yet-another round of these discussions by documenting the
 *   issues once and for all :-)
 */

#ifndef OMPI_BITMAP_H
#define OMPI_BITMAP_H

#include "ompi_config.h"

#include <string.h>

#include "ompi/types.h"
#include "opal/class/opal_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct ompi_bitmap_t {
    opal_object_t super; /**< Subclass of opal_object_t */
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
