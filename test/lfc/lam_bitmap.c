/* 
 * Testcase for bitmap
 */

#include <stdio.h>
#include "lfc/lam_bitmap.h"

#define BSIZE 26
#define SIZE_OF_CHAR (sizeof(char) * 8)
#define LAM_INVALID_BIT -1
#define ERR_CODE -2

#define PRINT_VALID_ERR \
    fprintf(stderr, "================================ \n"); \
    fprintf(stderr, "This is suppossed to throw error \n"); \
    fprintf(stderr, "================================ \n")


static void test_bitmap_set(lam_bitmap_t *bm);
static void test_bitmap_clear(lam_bitmap_t *bm);
static void test_bitmap_is_set(lam_bitmap_t *bm);
static void test_bitmap_clear_all(lam_bitmap_t *bm);
static void test_bitmap_set_all(lam_bitmap_t *bm);
static void test_bitmap_find_and_set(lam_bitmap_t *bm);
static void test_bitmap_find_size(lam_bitmap_t *bm);


static int set_bit(lam_bitmap_t *bm, int bit);
static int clear_bit(lam_bitmap_t *bm, int bit);
static int is_set_bit(lam_bitmap_t *bm, int bit);
static int clear_all(lam_bitmap_t *bm);
static int set_all(lam_bitmap_t *bm);
static int find_and_set(lam_bitmap_t *bm, int bit);
static int find_size(lam_bitmap_t *bm);

static void print_bitmap(lam_bitmap_t *bm);


int main(int argc, char *argv[])
{
    lam_bitmap_t bm;
    int err;

    /* Initialize bitmap  */

    PRINT_VALID_ERR;
    err = lam_bitmap_init(NULL, 2);
    if (err == LAM_ERR_ARG)
	fprintf(stderr, "ERROR: Initialization of bitmap failed\n\n");

    PRINT_VALID_ERR;
    err = lam_bitmap_init(&bm, -1);
    if (err == LAM_ERR_ARG)
	fprintf(stderr, "ERROR: Initialization of bitmap failed \n\n");

    err = lam_bitmap_init(&bm, BSIZE);
    if (0 > err) {
	fprintf(stderr, "Error in bitmap create -- aborting \n");
	exit(-1);
    }

    fprintf(stderr, "\nTesting bitmap set... \n");
    test_bitmap_set(&bm);

    fprintf(stderr, "\nTesting bitmap clear ... \n");
    test_bitmap_clear(&bm);

    fprintf(stderr, "\nTesting bitmap is_set ... \n");
    test_bitmap_is_set(&bm);

    fprintf(stderr, "\nTesting bitmap clear_all... \n");
    test_bitmap_clear_all(&bm);

    fprintf(stderr, "\nTesting bitmap set_all... \n");
    test_bitmap_set_all(&bm);

    fprintf(stderr, "\nTesting bitmap find_and_set... \n");
    test_bitmap_find_and_set(&bm);

    fprintf(stderr, "\nTesting bitmap find_size... \n");
    test_bitmap_find_size(&bm);

    fprintf(stderr, "\n~~~~~~     Testing complete     ~~~~~~ \n\n");
    return 0;
}



void test_bitmap_set(lam_bitmap_t *bm) {

    /* start of bitmap and boundaries */
    set_bit(bm, 0);
    set_bit(bm, 1);
    set_bit(bm, 7);
    set_bit(bm, 8);
    /* middle of bitmap  */
    set_bit(bm, 24);

    /* end of bitmap initial size */
    set_bit(bm, 31);
    set_bit(bm, 32);
    
    /* beyond bitmap -- this is valid */
    set_bit(bm, 44);
    set_bit(bm, 82);

    /* invalid bit */
    PRINT_VALID_ERR;
    set_bit(bm, -1);
}


void test_bitmap_clear(lam_bitmap_t *bm) {

    /* Valid set bits  */
    clear_bit(bm, 29);
    clear_bit(bm, 31);
    clear_bit(bm, 33);
    clear_bit(bm, 32);
    clear_bit(bm, 0);
    
    /* invalid bit */
    PRINT_VALID_ERR;
    clear_bit(bm, -1);
    PRINT_VALID_ERR;
    clear_bit(bm, 142);

}


void test_bitmap_is_set(lam_bitmap_t *bm)
{
    /* First set some bits */
    test_bitmap_set(bm);
    is_set_bit(bm, 0);
    is_set_bit(bm, 1);
    is_set_bit(bm, 31);
    is_set_bit(bm, 32);
    is_set_bit(bm, 1122);
    is_set_bit(bm, -33);
    is_set_bit(bm, -1);
}


void test_bitmap_find_and_set(lam_bitmap_t *bm) 
{
    int bsize;

    lam_bitmap_clear_all_bits(bm);
    find_and_set(bm, 0);
    find_and_set(bm, 1);
    find_and_set(bm, 2);
    find_and_set(bm, 3);

    lam_bitmap_set_bit(bm, 5);
    find_and_set(bm, 4);
    
    lam_bitmap_set_bit(bm, 6);
    lam_bitmap_set_bit(bm, 7);

    /* Setting beyond a char boundary */
    find_and_set(bm, 8);
    lam_bitmap_set_bit(bm, 9);
    find_and_set(bm, 10);

    /* Setting beyond the current size of bitmap  */
    lam_bitmap_set_all_bits(bm);
    bsize = bm->array_size * SIZE_OF_CHAR;
    find_and_set(bm, bsize);
}

void test_bitmap_clear_all(lam_bitmap_t *bm)
{
    clear_all(bm);
}


void test_bitmap_set_all(lam_bitmap_t *bm)
{
    set_all(bm);
}

void test_bitmap_find_size(lam_bitmap_t *bm)
{
    find_size(bm);
}


int set_bit(lam_bitmap_t *bm, int bit)
{
    int err = lam_bitmap_set_bit(bm, bit);
    if (err != 0 
	|| !(bm->bitmap[bit/SIZE_OF_CHAR] & (1 << bit % SIZE_OF_CHAR))) {
	    fprintf(stderr, "ERROR: set_bit for bit = %d\n\n", bit);
	    return ERR_CODE;
	}
    return 0;
}


int clear_bit(lam_bitmap_t *bm, int bit)
{
    int err = lam_bitmap_clear_bit(bm, bit);
    if ((err != 0)
	|| (bm->bitmap[bit/SIZE_OF_CHAR] & (1 << bit % SIZE_OF_CHAR))) {
	fprintf(stderr, "ERROR: clear_bit for bit = %d \n\n", bit);
	return ERR_CODE;
    }

    return 0;
}


int is_set_bit(lam_bitmap_t *bm, int bit) 
{
    int result = lam_bitmap_is_set_bit(bm, bit);
    if (((1 == result) 
	&& !(bm->bitmap[bit/SIZE_OF_CHAR] & (1 << bit % SIZE_OF_CHAR)))
	|| (result < 0)
	|| ((0 == result) 
	    &&(bm->bitmap[bit/SIZE_OF_CHAR] & (1 << bit % SIZE_OF_CHAR)))) {
	fprintf(stderr, "ERROR: is_set_bit for bit = %d \n\n",bit);
	return ERR_CODE;
    }
	
    return 0;
}


int find_and_set(lam_bitmap_t *bm, int bit) 
{
    /* bit here is the bit that should be found and set, in the top
       level stub, this function will be called in sequence to test */

    int pos = lam_bitmap_find_and_set_first_unset_bit(bm);

    if (pos != bit) {
	fprintf(stderr, "ERROR: find_and_set: expected to find_and_set %d\n\n",
		bit);
	return ERR_CODE;
    }

    return 0;
}


int clear_all(lam_bitmap_t *bm) 
{
    int i, err;
    err = lam_bitmap_clear_all_bits(bm);
    for (i = 0; i < bm->array_size; ++i)
	if (bm->bitmap[i] != 0) {
	    fprintf(stderr, "ERROR: clear_all for bitmap arry entry %d\n\n",
		    i);
	    return ERR_CODE;
	}
    return 0;
}
	    

int set_all(lam_bitmap_t *bm)
{
   int i, err;
   err = lam_bitmap_set_all_bits(bm);
   for (i = 0; i < bm->array_size; ++i)
       if (bm->bitmap[i] != 0xff) {
	   fprintf(stderr, "ERROR: set_all for bitmap arry entry %d\n\n", i);
	   return ERR_CODE;
       }
   return 0;
}

	
int find_size(lam_bitmap_t *bm)
{
    if (lam_bitmap_size(bm) != bm->legal_numbits) {
	fprintf(stderr, "ERROR: find_size: expected %d reported %d\n\n",
		bm->array_size, lam_bitmap_size(bm));
	return ERR_CODE;
    }
    return 0;
}


void print_bitmap(lam_bitmap_t *bm) 
{
    /* Accessing the fields within the structure, since its not an
       opaque structure  */

    int i;
    for (i = 0; i < bm->array_size; ++i) {
	fprintf(stderr, "---\n bitmap[%d] = %x \n---\n\n", i, 
		(bm->bitmap[i] & 0xff));
    }
    fprintf(stderr, "========================= \n");
    return;
}
