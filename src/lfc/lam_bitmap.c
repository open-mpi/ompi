/*
 * $HEADER$
 */

#include "lfc/lam_bitmap.h"

#define SIZE_OF_CHAR (sizeof(char) * 8)
#define LAM_INVALID_BIT -1

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
    bm->size = 0;
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
    size_t actual_size = size / SIZE_OF_CHAR;
  
    actual_size += (size % SIZE_OF_CHAR == 0) ? 0 : 1;
    bm->size = actual_size * SIZE_OF_CHAR;
    bm->bitmap = (char *) realloc(bm->bitmap, actual_size);
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

    index = bit / SIZE_OF_CHAR - 1; 
    offset = bit % SIZE_OF_CHAR -1;

    if (index >= bm->array_size) {
	/* We need to allocate more space for the bitmap, since we are
	   out of range. We dont throw any error here, because this is
	   valid and we simply expand the bitmap */

	new_size = index / bm->array_size * bm->array_size;
	/* New size is just a multiple of the original size to fit in
	   the index */

	bm->bitmap = (char *) realloc(bm->bitmap, new_size);
	if (NULL == bm->bitmap) {
	    return LAM_ERR_SYSRESOURCE;
	}

	/* zero out the new elements */
	for (i = bm->array_size; i < new_size; ++i) {
	    bm->bitmap[i] = 0;
	}
    }
    bm->bitmap[index] |= (1 << offset);
    return 0;
}


int
lam_bitmap_clear_bit(lam_bitmap_t *bm, int bit)
{
    size_t index, offset;
  
    index = bit / SIZE_OF_CHAR - 1; 
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
  
    index = bit / SIZE_OF_CHAR - 1; 
    offset = bit % SIZE_OF_CHAR -1;
  
    if (index >= bm->array_size) {
	return LAM_INVALID_BIT;
    }
  
    if ((0 != bm->bitmap[index]) & (1 << offset))
	return 1;

    return 0;
}


void
lam_bitmap_clear_all_bits(lam_bitmap_t *bm)
{
    memset(bm->bitmap, 0, bm->array_size);
}


void
lam_bitmap_set_all_bits(lam_bitmap_t *bm)
{
    int i;
    for (i = 0; i < bm->array_size; ++i) {
	bm->bitmap[i] = ~((char) 0);
    }
}


int
lam_bitmap_find_and_set_first_unset_bit(lam_bitmap_t *bm)
{
    size_t i;
    int position = 0;
    char temp;
    for (i = 0; i < bm->array_size; ++i) {
    
	/* Neglect all which dont have an unset bit */
	while (bm->bitmap[i] == (char)~0)
	    continue;
    
	/* This one has an unset bit, find its bit number */
	temp = bm->bitmap[i];
	while (temp & 0x1) {
	    ++position;
	    temp >>= 1;
	}
      
	/* Now set the bit number */
	bm->bitmap[i] |= (bm->bitmap[i] + 1);
    }
    return 0;
}


size_t
lam_bitmap_size(lam_bitmap_t *bm)
{
    return bm->size;
}
