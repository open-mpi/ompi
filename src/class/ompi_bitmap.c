/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "class/ompi_bitmap.h"


#define SIZE_OF_CHAR (sizeof(char) * 8)

static void ompi_bitmap_construct(ompi_bitmap_t *bm);
static void ompi_bitmap_destruct(ompi_bitmap_t *bm);

OBJ_CLASS_INSTANCE(ompi_bitmap_t, ompi_object_t, 
                   ompi_bitmap_construct, ompi_bitmap_destruct);


static void 
ompi_bitmap_construct(ompi_bitmap_t *bm) 
{
    bm->legal_numbits = 0;
    bm->array_size = 0;
    bm->bitmap = NULL;
}


static void
ompi_bitmap_destruct(ompi_bitmap_t *bm)
{
    if (NULL != bm->bitmap) {
        free(bm->bitmap);
    }
}


int
ompi_bitmap_init(ompi_bitmap_t *bm, size_t size)
{
    size_t actual_size;

    if (((int)size <= 0) || (NULL == bm)) {
	return OMPI_ERR_BAD_PARAM;
    }

    bm->legal_numbits = size;
    actual_size = size / SIZE_OF_CHAR;
  
    actual_size += (size % SIZE_OF_CHAR == 0) ? 0 : 1;
    bm->bitmap = (unsigned char *) malloc(actual_size);
    if (NULL == bm->bitmap) {
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    bm->array_size = actual_size;
    ompi_bitmap_clear_all_bits(bm);
    return OMPI_SUCCESS;
}
  

int
ompi_bitmap_set_bit(ompi_bitmap_t *bm, size_t bit)
{
    size_t index, offset, new_size, i;

    if ((bit < 0) || (NULL == bm)) {
	return OMPI_ERR_BAD_PARAM;
    }

    index = bit / SIZE_OF_CHAR; 
    offset = bit % SIZE_OF_CHAR;

    if (index >= bm->array_size) {
	/* We need to allocate more space for the bitmap, since we are
	   out of range. We dont throw any error here, because this is
	   valid and we simply expand the bitmap */

	new_size = (index / bm->array_size + 1 ) * bm->array_size;
	/* New size is just a multiple of the original size to fit in
	   the index */

	bm->bitmap = (unsigned char *) realloc(bm->bitmap, new_size);
	if (NULL == bm->bitmap) {
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}

	/* zero out the new elements */
	for (i = bm->array_size; i < new_size; ++i) {
	    bm->bitmap[i] = 0;
	}

	/* Update the array_size */
	bm->array_size = new_size;
	bm->legal_numbits = bit + 1;
    }
    
    /* Now set the bit */
    bm->bitmap[index] |= (1 << offset);

    return OMPI_SUCCESS;
}


int
ompi_bitmap_clear_bit(ompi_bitmap_t *bm, size_t bit)
{
    size_t index, offset;

    if ((bit < 0) || (bit > bm->legal_numbits - 1) || (NULL == bm)) {
	return OMPI_ERR_BAD_PARAM;
    }

    index = bit / SIZE_OF_CHAR; 
    offset = bit % SIZE_OF_CHAR;
  
    if (index >= bm->array_size) {
	return OMPI_ERR_BAD_PARAM;
    }

    bm->bitmap[index] &= ~(1 << offset);
    return OMPI_SUCCESS;
}


int
ompi_bitmap_is_set_bit(ompi_bitmap_t *bm, size_t bit)
{
    size_t index, offset;
  
    if ((bit < 0) || (bit > bm->legal_numbits - 1) || (NULL == bm)) {
	return OMPI_ERR_BAD_PARAM;
    }

    index = bit / SIZE_OF_CHAR; 
    offset = bit % SIZE_OF_CHAR;
  
    if (index >= bm->array_size) {
	return OMPI_ERR_BAD_PARAM;
    }
  
    if (0 != (bm->bitmap[index] & (1 << offset))) {
	return (int) true;
    }

    return (int) false;
}


int
ompi_bitmap_clear_all_bits(ompi_bitmap_t *bm)
{
    if (NULL == bm) {
	return OMPI_ERR_BAD_PARAM;
    }

    memset(bm->bitmap, 0, bm->array_size);
    return OMPI_SUCCESS;
}


int
ompi_bitmap_set_all_bits(ompi_bitmap_t *bm)
{
    size_t i;
    
    if (NULL == bm) {
	return OMPI_ERR_BAD_PARAM;
    }
     
    for (i = 0; i < bm->array_size; ++i) {
	bm->bitmap[i] = ~((char) 0);
    }
    return OMPI_SUCCESS;
}


int
ompi_bitmap_find_and_set_first_unset_bit(ompi_bitmap_t *bm, size_t *position)
{
    size_t i = 0;
    unsigned char temp;
    unsigned char all_ones = 0xff;

    if (NULL == bm) {
	return OMPI_ERR_BAD_PARAM;
    }

    /* Neglect all which dont have an unset bit */
    *position = 0;
    while((i < bm->array_size) && (bm->bitmap[i] == all_ones)) {
	++i;
    }

    if (i == bm->array_size) {
	/* increase the bitmap size then */
	*position = bm->array_size * SIZE_OF_CHAR;
	ompi_bitmap_set_bit(bm, *position);
	return OMPI_SUCCESS;
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
    return OMPI_SUCCESS;
}
