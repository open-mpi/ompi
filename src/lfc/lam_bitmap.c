/*
 * $HEADER$
 */

#include "lfc/lam_bitmap.h"

#define SIZE_OF_CHAR (sizeof(char) * 8)

static void lam_bitmap_construct(lam_bitmap_t *bm);
static void lam_bitmap_destruct(lam_bitmap_t *bm);

lam_class_t lam_bitmap_t_class = {
    "lam_bitmap_t",
    OBJ_CLASS(lam_object_t),
    (lam_construct_t)lam_bitmap_construct,
    (lam_construct_t)lam_bitmap_destruct
};


static void 
lam_bitmap_construct(lam_bitmap_t *bm) 
{
    bm->legal_numbits = 0;
    bm->array_size = 0;
    bm->bitmap = NULL;
}


static void
lam_bitmap_destruct(lam_bitmap_t *bm)
{
}


int
lam_bitmap_init(lam_bitmap_t *bm, size_t size)
{
    
    size_t actual_size;

    if (((int)size <= 0) || (NULL == bm))
	return LAM_ERR_ARG;

    bm->legal_numbits = size;
    actual_size = size / SIZE_OF_CHAR;
  
    actual_size += (size % SIZE_OF_CHAR == 0) ? 0 : 1;
    bm->bitmap = (unsigned char *) malloc(actual_size);
    if (NULL == bm->bitmap)
	return LAM_ERR_SYSRESOURCE;

    bm->array_size = actual_size;
    lam_bitmap_clear_all_bits(bm);
    return 0;
}
  

int
lam_bitmap_set_bit(lam_bitmap_t *bm, int bit)
{
    size_t index, offset, new_size;
    int i;

    if ((bit < 0) || (NULL == bm))
	return LAM_ERR_ARG;

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
	    return LAM_ERR_SYSRESOURCE;
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

    return 0;
}


int
lam_bitmap_clear_bit(lam_bitmap_t *bm, int bit)
{
    size_t index, offset;

    if ((bit < 0) || (bit > bm->legal_numbits - 1) || (NULL == bm))
	return LAM_ERR_ARG;

    index = bit / SIZE_OF_CHAR; 
    offset = bit % SIZE_OF_CHAR;
  
    if (index >= bm->array_size) {
	return LAM_INVALID_BIT;
    }

    bm->bitmap[index] &= ~(1 << offset);
    return 0;
}


int
lam_bitmap_is_set_bit(lam_bitmap_t *bm, int bit)
{
    size_t index, offset;
  
    if ((bit < 0) || (bit > bm->legal_numbits - 1) || (NULL == bm))
	return LAM_ERR_ARG;


    index = bit / SIZE_OF_CHAR; 
    offset = bit % SIZE_OF_CHAR;
  
    if (index >= bm->array_size) {
	return LAM_INVALID_BIT;
    }
  
    if (0 != (bm->bitmap[index] & (1 << offset)))
	return 1;

    return 0;
}


int
lam_bitmap_clear_all_bits(lam_bitmap_t *bm)
{
    if (NULL == bm)
	return LAM_ERR_ARG;

    memset(bm->bitmap, 0, bm->array_size);
    return 0;
}


int
lam_bitmap_set_all_bits(lam_bitmap_t *bm)
{
    int i;
    
    if (NULL == bm)
	return LAM_ERR_ARG;
     
    for (i = 0; i < bm->array_size; ++i) {
	bm->bitmap[i] = ~((char) 0);
    }
    return 0;
}

#include <stdio.h>
int
lam_bitmap_find_and_set_first_unset_bit(lam_bitmap_t *bm)
{
    size_t i = 0;
    int position = 0;
    unsigned char temp;
    unsigned char all_ones = 0xff;

    if (NULL == bm)
	return LAM_ERR_ARG;

    /* Neglect all which dont have an unset bit */
    while((i < bm->array_size) && (bm->bitmap[i] == all_ones)) {
	++i;
    }

    if (i == bm->array_size) {
	/* increase the bitmap size then */
	position = bm->array_size * SIZE_OF_CHAR;
	lam_bitmap_set_bit(bm, position);
	return position;
    }

    /* This one has an unset bit, find its bit number */

    temp = bm->bitmap[i];
    while (temp & 0x1) {
	++position;
	temp >>= 1;
    }
      
    /* Now set the bit number */
    bm->bitmap[i] |= (bm->bitmap[i] + 1);

    position += i * SIZE_OF_CHAR;
    return position;
}


size_t
lam_bitmap_size(lam_bitmap_t *bm)
{
    if (NULL == bm)
	return LAM_ERR_ARG;
    
    return bm->legal_numbits;
}
