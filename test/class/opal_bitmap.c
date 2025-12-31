/*
 * Testcase for bitmap
 */

#include "opal_config.h"

#include <stdio.h>
#include "support.h"

#include "opal/class/opal_bitmap.h"
#include "opal/constants.h"

#define BSIZE 26
#define OPAL_INVALID_BIT -1
#define ERR_CODE -2

#define PRINT_VALID_ERR \
    fprintf(error_out, "================================ \n"); \
    fprintf(error_out, "This is supposed to throw error  \n"); \
    fprintf(error_out, "================================ \n")

static void test_bitmap_set(opal_bitmap_t *bm);
static void test_bitmap_clear(opal_bitmap_t *bm);
static void test_bitmap_is_set(opal_bitmap_t *bm);
static void test_bitmap_clear_all(opal_bitmap_t *bm);
static void test_bitmap_set_all(opal_bitmap_t *bm);
static void test_bitmap_find_and_set(opal_bitmap_t *bm);
static void test_bitmap_num_set_bits(opal_bitmap_t *bm);


static int set_bit(opal_bitmap_t *bm, int bit);
static int clear_bit(opal_bitmap_t *bm, int bit);
static int is_set_bit(opal_bitmap_t *bm, int bit);
static int clear_all(opal_bitmap_t *bm);
static int set_all(opal_bitmap_t *bm);
static int find_and_set(opal_bitmap_t *bm, int bit);

#define WANT_PRINT_BITMAP 0
#if WANT_PRINT_BITMAP
static void print_bitmap(opal_bitmap_t *bm);
#endif

static FILE *error_out=NULL;

int main(int argc, char *argv[])
{
    /* Local variables */
    opal_bitmap_t bm;
    int err;

    /* Perform overall test initialization */
    test_init("opal_bitmap_t");

#ifdef STANDALONE
    error_out = stderr;
#else
    error_out = fopen( "./opal_bitmap_test_out.txt", "w" );
    if( error_out == NULL ) error_out = stderr;
#endif

    /* Initialize bitmap  */
    OBJ_CONSTRUCT(&bm, opal_bitmap_t);
    PRINT_VALID_ERR;
    err = opal_bitmap_init(NULL, 2);
    if (err == OPAL_ERR_BAD_PARAM)
	fprintf(error_out, "ERROR: Initialization of bitmap failed\n\n");

    PRINT_VALID_ERR;
    err = opal_bitmap_init(&bm, -1);
    if (err == OPAL_ERR_BAD_PARAM)
	fprintf(error_out, "ERROR: Initialization of bitmap failed \n\n");

    err = opal_bitmap_init(&bm, BSIZE);
    if (0 > err) {
	fprintf(error_out, "Error in bitmap create -- aborting \n");
	exit(-1);
    }

    fprintf(error_out, "\nTesting bitmap set... \n");
    test_bitmap_set(&bm);

    fprintf(error_out, "\nTesting bitmap clear ... \n");
    test_bitmap_clear(&bm);

    fprintf(error_out, "\nTesting bitmap is_set ... \n");
    test_bitmap_is_set(&bm);

    fprintf(error_out, "\nTesting bitmap clear_all... \n");
    test_bitmap_clear_all(&bm);

    fprintf(error_out, "\nTesting bitmap set_all... \n");
    test_bitmap_set_all(&bm);

    fprintf(error_out, "\nTesting bitmap find_and_set... \n");
    test_bitmap_find_and_set(&bm);

    fprintf(error_out, "\nTesting bitmap num_set_bits... \n");
    test_bitmap_num_set_bits(&bm);

    fprintf(error_out, "\n~~~~~~     Testing complete     ~~~~~~ \n\n");

    test_finalize();
#ifndef STANDALONE
    fclose(error_out);
#endif

    return 0;
}



void test_bitmap_set(opal_bitmap_t *bm) {
    int result=0;

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
    result = set_bit(bm, -1);
    TEST_AND_REPORT(result, ERR_CODE,"opal_bitmap_set_bit");
}


void test_bitmap_clear(opal_bitmap_t *bm) {
    int result=0;

    /* Valid set bits  */
    clear_bit(bm, 29);
    clear_bit(bm, 31);
    clear_bit(bm, 33);
    clear_bit(bm, 32);
    clear_bit(bm, 0);

    /* invalid bit */
    PRINT_VALID_ERR;
    result = clear_bit(bm, -1);
    TEST_AND_REPORT(result, ERR_CODE,"opal_bitmap_clear_bit");
    PRINT_VALID_ERR;
    result = clear_bit(bm, 142);
    TEST_AND_REPORT(result, ERR_CODE,"opal_bitmap_clear_bit");
}


void test_bitmap_is_set(opal_bitmap_t *bm)
{
    int result=0;

    /* First set some bits */
    test_bitmap_set(bm);
    is_set_bit(bm, 0);
    is_set_bit(bm, 1);
    is_set_bit(bm, 31);
    is_set_bit(bm, 32);

    result = is_set_bit(bm, 1122);
    TEST_AND_REPORT(result,0,"opal_bitmap_is_set_bit");
    is_set_bit(bm, -33);
    TEST_AND_REPORT(result,0,"opal_bitmap_is_set_bit");
    is_set_bit(bm, -1);
    TEST_AND_REPORT(result,0,"opal_bitmap_is_set_bit");
}


void test_bitmap_find_and_set(opal_bitmap_t *bm)
{
    int bsize;
    int result=0;

    opal_bitmap_clear_all_bits(bm);
    result = find_and_set(bm, 0);
    TEST_AND_REPORT(result, 0, "opal_bitmap_find_and_set_first_unset_bit");
    result = find_and_set(bm, 1);
    TEST_AND_REPORT(result, 0, "opal_bitmap_find_and_set_first_unset_bit");
    result = find_and_set(bm, 2);
    TEST_AND_REPORT(result, 0, "opal_bitmap_find_and_set_first_unset_bit");
    result = find_and_set(bm, 3);
    TEST_AND_REPORT(result, 0, "opal_bitmap_find_and_set_first_unset_bit");

    result = opal_bitmap_set_bit(bm, 5);
    result = find_and_set(bm, 4);
    TEST_AND_REPORT(result, 0, "opal_bitmap_find_and_set_first_unset_bit");

    result = opal_bitmap_set_bit(bm, 6);
    result = opal_bitmap_set_bit(bm, 7);

    /* Setting beyond a char boundary */
    result = find_and_set(bm, 8);
    TEST_AND_REPORT(result, 0, "opal_bitmap_find_and_set_first_unset_bit");
    opal_bitmap_set_bit(bm, 9);
    result = find_and_set(bm, 10);
    TEST_AND_REPORT(result, 0, "opal_bitmap_find_and_set_first_unset_bit");

    /* Setting beyond the current size of bitmap  */
    opal_bitmap_set_all_bits(bm);
    bsize = opal_bitmap_size(bm);
    result = find_and_set(bm, bsize);
    TEST_AND_REPORT(result, 0, "opal_bitmap_find_and_set_first_unset_bit");
}

void test_bitmap_clear_all(opal_bitmap_t *bm)
{
    int result = clear_all(bm);
    TEST_AND_REPORT(result, 0, " error in opal_bitmap_clear_all_bits");
}


void test_bitmap_set_all(opal_bitmap_t *bm)
{
    int result = set_all(bm);
    TEST_AND_REPORT(result, 0, " error in opal_bitmap_set_ala_bitsl");
}

void test_bitmap_num_set_bits(opal_bitmap_t *bm)
{
    int result, expected;

    /* Test 1: Clear all bits and count - should be 0 */
    opal_bitmap_clear_all_bits(bm);
    result = opal_bitmap_num_set_bits(bm, 64);
    expected = 0;
    TEST_AND_REPORT(result, expected, "opal_bitmap_num_set_bits: cleared bitmap");

    /* Test 2: Set specific bits and count within first 64 bits */
    opal_bitmap_set_bit(bm, 0);
    opal_bitmap_set_bit(bm, 1);
    opal_bitmap_set_bit(bm, 5);
    opal_bitmap_set_bit(bm, 63);
    result = opal_bitmap_num_set_bits(bm, 64);
    expected = 4;
    TEST_AND_REPORT(result, expected, "opal_bitmap_num_set_bits: 4 bits in first 64");

    /* Test 3: Count partial element (len not a multiple of 64) */
    /* Count only first 10 bits - should be 3 (bits 0, 1, 5) */
    result = opal_bitmap_num_set_bits(bm, 10);
    expected = 3;
    TEST_AND_REPORT(result, expected, "opal_bitmap_num_set_bits: partial element (10 bits)");

    /* Test 4: Count up to bit 63 - should include bit 63 */
    result = opal_bitmap_num_set_bits(bm, 64);
    expected = 4;
    TEST_AND_REPORT(result, expected, "opal_bitmap_num_set_bits: up to bit 63");

    /* Test 5: Set bits across multiple array elements */
    opal_bitmap_set_bit(bm, 64);
    opal_bitmap_set_bit(bm, 65);
    opal_bitmap_set_bit(bm, 100);
    opal_bitmap_set_bit(bm, 127);

    /* Count across 128 bits (2 full elements) */
    result = opal_bitmap_num_set_bits(bm, 128);
    expected = 8; /* 4 from first element + 4 from second element */
    TEST_AND_REPORT(result, expected, "opal_bitmap_num_set_bits: across 2 elements (128 bits)");

    /* Test 6: Count partial second element (130 bits = 2 elements + 2 bits) */
    result = opal_bitmap_num_set_bits(bm, 130);
    expected = 8; /* Should still be 8, as bits 128-129 are not set */
    TEST_AND_REPORT(result, expected,
                    "opal_bitmap_num_set_bits: partial second element (130 bits)");

    /* Test 7: Verify len is treated as bits, not array indices
     * Set bit 200 and count up to 201 bits (not 201 array elements) */
    opal_bitmap_clear_all_bits(bm);
    opal_bitmap_set_bit(bm, 200);
    result = opal_bitmap_num_set_bits(bm, 201);
    expected = 1;
    TEST_AND_REPORT(result, expected,
                    "opal_bitmap_num_set_bits: len treated as bits (201 bits)");

    /* Test 8: Count up to 200 should not include bit 200 */
    result = opal_bitmap_num_set_bits(bm, 200);
    expected = 0;
    TEST_AND_REPORT(result, expected,
                    "opal_bitmap_num_set_bits: len boundary not included (200 bits)");

    /* Test 9: Set all bits in first element and count partial */
    opal_bitmap_clear_all_bits(bm);
    for (int i = 0; i < 64; ++i) {
        opal_bitmap_set_bit(bm, i);
    }
    /* Count only first 50 bits - should be 50 */
    result = opal_bitmap_num_set_bits(bm, 50);
    expected = 50;
    TEST_AND_REPORT(result, expected,
                    "opal_bitmap_num_set_bits: partial full element (50 of 64 bits)");

    /* Test 10: Edge case - count exactly one full element */
    result = opal_bitmap_num_set_bits(bm, 64);
    expected = 64;
    TEST_AND_REPORT(result, expected,
                    "opal_bitmap_num_set_bits: exactly one element (64 bits)");

    /* Test 11: Test opal_bitmap_num_unset_bits consistency */
    opal_bitmap_clear_all_bits(bm);
    opal_bitmap_set_bit(bm, 5);
    opal_bitmap_set_bit(bm, 10);
    opal_bitmap_set_bit(bm, 15);
    result = opal_bitmap_num_set_bits(bm, 64);
    expected = 3;
    TEST_AND_REPORT(result, expected, "opal_bitmap_num_set_bits: 3 set bits in 64");

    result = opal_bitmap_num_unset_bits(bm, 64);
    expected = 61; /* 64 - 3 = 61 */
    TEST_AND_REPORT(result, expected, "opal_bitmap_num_unset_bits: 61 unset bits in 64");
}

int set_bit(opal_bitmap_t *bm, int bit)
{
    int err = opal_bitmap_set_bit(bm, bit);
    if (err != 0 || !opal_bitmap_is_set_bit(bm, bit)) {
	    fprintf(error_out, "ERROR: set_bit for bit = %d\n\n", bit);
	    return ERR_CODE;
	}
    return 0;
}


int clear_bit(opal_bitmap_t *bm, int bit)
{
    int err = opal_bitmap_clear_bit(bm, bit);
    if ((err != 0) || opal_bitmap_is_set_bit(bm, bit)) {
	fprintf(error_out, "ERROR: clear_bit for bit = %d \n\n", bit);
	return ERR_CODE;
    }

    return 0;
}


int is_set_bit(opal_bitmap_t *bm, int bit)
{
    bool result = opal_bitmap_is_set_bit(bm, bit);

    if (result) {
        if (bit < 0) {
            fprintf(error_out, "ERROR: is_set_bit for bit = %d \n\n",bit);
            return ERR_CODE;
        }
        return 0;
    }

    if (!result) {
        if (0 <= bit && bit <= bm->array_size && !opal_bitmap_is_set_bit(bm, bit)) {
            fprintf(error_out, "ERROR: is_set_bit for bit = %d \n\n",bit);
            return ERR_CODE;
        }
        return 0;
    }

    return 0;
}


int find_and_set(opal_bitmap_t *bm, int bit)
{
    int ret, pos;
    /* bit here is the bit that should be found and set, in the top
       level stub, this function will be called in sequence to test */

    ret = opal_bitmap_find_and_set_first_unset_bit(bm, &pos);
    if (ret != OPAL_SUCCESS) return ret;

    if (pos != bit) {
	fprintf(error_out, "ERROR: find_and_set: expected to find_and_set %d\n\n",
		bit);
	return ERR_CODE;
    }

    return 0;
}


int clear_all(opal_bitmap_t *bm)
{
    int i;
    if (OPAL_SUCCESS != opal_bitmap_clear_all_bits(bm)) {
        return ERR_CODE;
    }
    for (i = 0; i < bm->array_size; ++i)
	if (bm->bitmap[i] != 0) {
	    fprintf(error_out, "ERROR: clear_all for bitmap array entry %d\n\n",
		    i);
	    return ERR_CODE;
	}
    return 0;
}


int set_all(opal_bitmap_t *bm)
{
   int i;
   if (OPAL_SUCCESS != opal_bitmap_set_all_bits(bm)) {
       return ERR_CODE;
   }
   for (i = 0; i < bm->array_size; ++i)
       if (bm->bitmap[i] != 0xffffffffffffffffUL) {
	   fprintf(error_out, "ERROR: set_all for bitmap array entry %d\n\n", i);
	   return ERR_CODE;
       }
   return 0;
}


#if WANT_PRINT_BITMAP
void print_bitmap(opal_bitmap_t *bm)
{
    /* Accessing the fields within the structure, since its not an
       opaque structure  */

    int i;
    for (i = 0; i < bm->array_size; ++i) {
	fprintf(error_out, "---\n bitmap[%d] = %x \n---\n\n", i,
		(bm->bitmap[i] & 0xff));
    }
    fprintf(error_out, "========================= \n");
    return;
}
#endif
