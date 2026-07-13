/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for opal_bitmap_t.  Exercises the full public API with
 * self-checking assertions (via the support harness).  Note: the
 * library is compiled with -DNDEBUG, so assert() is a no-op here --
 * all verification must go through test_verify()/test_failure().
 */

#include "opal_config.h"

#include <stdio.h>

#include "support.h"

#include "opal/class/opal_bitmap.h"
#include "opal/constants.h"

#define BSIZE 26

/* Number of bits per underlying array element (mirrors the value used
   internally by opal_bitmap.c). */
#define BITS_PER_ELEMENT 64

static void test_init_errors(void);
static void test_set_and_is_set(void);
static void test_clear(void);
static void test_clear_and_set_all(void);
static void test_find_and_set(void);
static void test_num_bits(void);
static void test_bitwise_ops(void);
static void test_are_different(void);
static void test_get_string(void);
static void test_copy(void);
static void test_max_size(void);
static void test_size_accessor(void);

int main(int argc, char *argv[])
{
    test_init("opal_bitmap_t");

    test_init_errors();
    test_set_and_is_set();
    test_clear();
    test_clear_and_set_all();
    test_find_and_set();
    test_num_bits();
    test_bitwise_ops();
    test_are_different();
    test_get_string();
    test_copy();
    test_max_size();
    test_size_accessor();

    /* test_finalize() returns non-zero if any check failed, which
       becomes this program's exit status for "make check". */
    return test_finalize();
}

static void test_init_errors(void)
{
    opal_bitmap_t bm;
    OBJ_CONSTRUCT(&bm, opal_bitmap_t);

    /* NULL bitmap is rejected */
    test_verify("init(NULL) rejected", OPAL_ERR_BAD_PARAM == opal_bitmap_init(NULL, 2));
    /* non-positive sizes are rejected */
    test_verify("init(size=-1) rejected", OPAL_ERR_BAD_PARAM == opal_bitmap_init(&bm, -1));
    test_verify("init(size=0) rejected", OPAL_ERR_BAD_PARAM == opal_bitmap_init(&bm, 0));
    /* valid size succeeds and rounds up to a whole element */
    test_verify("init(BSIZE) ok", OPAL_SUCCESS == opal_bitmap_init(&bm, BSIZE));
    test_verify("init rounds up to one element", BITS_PER_ELEMENT == opal_bitmap_size(&bm));
    /* a freshly initialized bitmap is entirely clear */
    test_verify("fresh bitmap is clear", opal_bitmap_is_clear(&bm));

    OBJ_DESTRUCT(&bm);
}

static void test_set_and_is_set(void)
{
    opal_bitmap_t bm;
    OBJ_CONSTRUCT(&bm, opal_bitmap_t);
    opal_bitmap_init(&bm, BSIZE);

    /* Set bits at element boundaries and beyond the initial size (which
       must auto-expand the bitmap). */
    int bits[] = {0, 1, 7, 8, 24, 31, 32, 44, 63, 64, 82};
    int nbits = (int) (sizeof(bits) / sizeof(bits[0]));
    for (int i = 0; i < nbits; ++i) {
        test_verify("set_bit ok", OPAL_SUCCESS == opal_bitmap_set_bit(&bm, bits[i]));
    }
    for (int i = 0; i < nbits; ++i) {
        test_verify("set bit reads back set", opal_bitmap_is_set_bit(&bm, bits[i]));
    }

    /* Setting bit 82 must have grown the bitmap past 82 bits. */
    test_verify("set_bit auto-expanded", opal_bitmap_size(&bm) > 82);

    /* Bits we never set must read as unset. */
    test_verify("unset bit 2 reads clear", !opal_bitmap_is_set_bit(&bm, 2));
    test_verify("unset bit 30 reads clear", !opal_bitmap_is_set_bit(&bm, 30));

    /* Out-of-range / negative indices: set is rejected, is_set is false. */
    test_verify("set_bit(-1) rejected", OPAL_ERR_BAD_PARAM == opal_bitmap_set_bit(&bm, -1));
    test_verify("is_set_bit(-1) false", !opal_bitmap_is_set_bit(&bm, -1));
    test_verify("is_set_bit(huge) false", !opal_bitmap_is_set_bit(&bm, 1000000));

    OBJ_DESTRUCT(&bm);
}

static void test_clear(void)
{
    opal_bitmap_t bm;
    OBJ_CONSTRUCT(&bm, opal_bitmap_t);
    opal_bitmap_init(&bm, BSIZE);

    opal_bitmap_set_bit(&bm, 5);
    test_verify("bit 5 set", opal_bitmap_is_set_bit(&bm, 5));
    test_verify("clear_bit ok", OPAL_SUCCESS == opal_bitmap_clear_bit(&bm, 5));
    test_verify("bit 5 now clear", !opal_bitmap_is_set_bit(&bm, 5));

    /* Negative and out-of-range bits are rejected (clear does not grow). */
    test_verify("clear_bit(-1) rejected", OPAL_ERR_BAD_PARAM == opal_bitmap_clear_bit(&bm, -1));
    test_verify("clear_bit(out of range) rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_clear_bit(&bm, opal_bitmap_size(&bm)));

    OBJ_DESTRUCT(&bm);
}

static void test_clear_and_set_all(void)
{
    opal_bitmap_t bm;
    OBJ_CONSTRUCT(&bm, opal_bitmap_t);
    opal_bitmap_init(&bm, 128);
    int size = opal_bitmap_size(&bm);

    test_verify("set_all ok", OPAL_SUCCESS == opal_bitmap_set_all_bits(&bm));
    test_verify("set_all -> not clear", !opal_bitmap_is_clear(&bm));
    test_verify("set_all -> all bits set", size == opal_bitmap_num_set_bits(&bm, size));
    test_verify("set_all -> no unset bits", 0 == opal_bitmap_num_unset_bits(&bm, size));
    test_verify("set_all -> first bit set", opal_bitmap_is_set_bit(&bm, 0));
    test_verify("set_all -> last bit set", opal_bitmap_is_set_bit(&bm, size - 1));

    test_verify("clear_all ok", OPAL_SUCCESS == opal_bitmap_clear_all_bits(&bm));
    test_verify("clear_all -> is_clear", opal_bitmap_is_clear(&bm));
    test_verify("clear_all -> zero set bits", 0 == opal_bitmap_num_set_bits(&bm, size));

    /* NULL handling */
    test_verify("set_all(NULL) rejected", OPAL_ERR_BAD_PARAM == opal_bitmap_set_all_bits(NULL));
    test_verify("clear_all(NULL) rejected", OPAL_ERR_BAD_PARAM == opal_bitmap_clear_all_bits(NULL));

    OBJ_DESTRUCT(&bm);
}

static void test_find_and_set(void)
{
    opal_bitmap_t bm;
    int pos;
    OBJ_CONSTRUCT(&bm, opal_bitmap_t);
    opal_bitmap_init(&bm, BSIZE);
    opal_bitmap_clear_all_bits(&bm);

    /* Repeatedly find-and-set the first unset bit: should yield 0,1,2,3 */
    for (int expected = 0; expected < 4; ++expected) {
        test_verify("find_and_set ok",
                    OPAL_SUCCESS == opal_bitmap_find_and_set_first_unset_bit(&bm, &pos));
        test_verify("find_and_set returns next free bit", pos == expected);
    }

    /* Pre-set bit 5; the next find should skip it and return 4, then 6. */
    opal_bitmap_set_bit(&bm, 5);
    opal_bitmap_find_and_set_first_unset_bit(&bm, &pos);
    test_verify("find_and_set returns 4", 4 == pos);
    opal_bitmap_find_and_set_first_unset_bit(&bm, &pos);
    test_verify("find_and_set skips pre-set bit 5", 6 == pos);

    /* When an element is completely full, find_and_set must expand and
       return the first bit of the new element. */
    opal_bitmap_set_all_bits(&bm);
    int old_size = opal_bitmap_size(&bm);
    test_verify("find_and_set on full bitmap ok",
                OPAL_SUCCESS == opal_bitmap_find_and_set_first_unset_bit(&bm, &pos));
    test_verify("find_and_set expands at boundary", pos == old_size);

    test_verify("find_and_set(NULL) rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_find_and_set_first_unset_bit(NULL, &pos));

    OBJ_DESTRUCT(&bm);
}

static void test_num_bits(void)
{
    opal_bitmap_t bm;
    OBJ_CONSTRUCT(&bm, opal_bitmap_t);
    opal_bitmap_init(&bm, 256);

    opal_bitmap_clear_all_bits(&bm);
    test_verify("num_set on cleared == 0", 0 == opal_bitmap_num_set_bits(&bm, 64));

    opal_bitmap_set_bit(&bm, 0);
    opal_bitmap_set_bit(&bm, 1);
    opal_bitmap_set_bit(&bm, 5);
    opal_bitmap_set_bit(&bm, 63);
    test_verify("num_set in first 64 == 4", 4 == opal_bitmap_num_set_bits(&bm, 64));
    /* Partial element: only the first 10 bits -> bits 0,1,5 -> 3 */
    test_verify("num_set partial element == 3", 3 == opal_bitmap_num_set_bits(&bm, 10));

    /* Spread bits across multiple elements. */
    opal_bitmap_set_bit(&bm, 64);
    opal_bitmap_set_bit(&bm, 100);
    opal_bitmap_set_bit(&bm, 200);
    test_verify("num_set across 256 == 7", 7 == opal_bitmap_num_set_bits(&bm, 256));
    test_verify("num_unset across 256 == 249", 249 == opal_bitmap_num_unset_bits(&bm, 256));

    /* len is measured in bits, not elements: counting up to 200 excludes
       bit 200 itself. */
    test_verify("num_set up to 200 excludes bit 200", 6 == opal_bitmap_num_set_bits(&bm, 200));
    test_verify("num_set up to 201 includes bit 200", 7 == opal_bitmap_num_set_bits(&bm, 201));

    OBJ_DESTRUCT(&bm);
}

/* Initialize a bitmap to a given size with a caller-supplied 64-bit
   pattern in its first element. */
static void fill_pattern(opal_bitmap_t *bm, int size, uint64_t pattern)
{
    opal_bitmap_init(bm, size);
    opal_bitmap_clear_all_bits(bm);
    for (int b = 0; b < 64; ++b) {
        if (pattern & (1UL << b)) {
            opal_bitmap_set_bit(bm, b);
        }
    }
}

static void test_bitwise_ops(void)
{
    opal_bitmap_t a, b;
    OBJ_CONSTRUCT(&a, opal_bitmap_t);
    OBJ_CONSTRUCT(&b, opal_bitmap_t);

    /* AND */
    fill_pattern(&a, 64, 0xF0F0UL);
    fill_pattern(&b, 64, 0xFF00UL);
    test_verify("and ok", OPAL_SUCCESS == opal_bitmap_bitwise_and_inplace(&a, &b));
    test_verify("and result bit 12 set", opal_bitmap_is_set_bit(&a, 12)); /* 0xF000 */
    test_verify("and result bit 4 clear", !opal_bitmap_is_set_bit(&a, 4));

    /* OR */
    fill_pattern(&a, 64, 0x0F0FUL);
    fill_pattern(&b, 64, 0xF0F0UL);
    test_verify("or ok", OPAL_SUCCESS == opal_bitmap_bitwise_or_inplace(&a, &b));
    test_verify("or result == 0xFFFF (16 bits)", 16 == opal_bitmap_num_set_bits(&a, 16));

    /* XOR */
    fill_pattern(&a, 64, 0xFF00UL);
    fill_pattern(&b, 64, 0x0FF0UL);
    test_verify("xor ok", OPAL_SUCCESS == opal_bitmap_bitwise_xor_inplace(&a, &b));
    test_verify("xor result bit 4 set", opal_bitmap_is_set_bit(&a, 4));   /* 0xF0F0 */
    test_verify("xor result bit 8 clear", !opal_bitmap_is_set_bit(&a, 8));

    /* Error paths: NULL operands and mismatched sizes. */
    test_verify("and(NULL) rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_bitwise_and_inplace(&a, NULL));
    test_verify("or(NULL) rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_bitwise_or_inplace(NULL, &b));
    test_verify("xor(NULL) rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_bitwise_xor_inplace(&a, NULL));

    opal_bitmap_t big;
    OBJ_CONSTRUCT(&big, opal_bitmap_t);
    opal_bitmap_init(&big, 256);
    test_verify("and size mismatch rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_bitwise_and_inplace(&a, &big));
    OBJ_DESTRUCT(&big);

    OBJ_DESTRUCT(&a);
    OBJ_DESTRUCT(&b);
}

static void test_are_different(void)
{
    opal_bitmap_t a, b;
    OBJ_CONSTRUCT(&a, opal_bitmap_t);
    OBJ_CONSTRUCT(&b, opal_bitmap_t);

    fill_pattern(&a, 64, 0xABCDUL);
    fill_pattern(&b, 64, 0xABCDUL);
    test_verify("equal bitmaps not different", !opal_bitmap_are_different(&a, &b));

    opal_bitmap_set_bit(&b, 40);
    test_verify("differing content is different", opal_bitmap_are_different(&a, &b));

    opal_bitmap_t big;
    OBJ_CONSTRUCT(&big, opal_bitmap_t);
    opal_bitmap_init(&big, 256);
    test_verify("differing size is different", opal_bitmap_are_different(&a, &big));
    OBJ_DESTRUCT(&big);

    OBJ_DESTRUCT(&a);
    OBJ_DESTRUCT(&b);
}

static void test_get_string(void)
{
    opal_bitmap_t bm;
    OBJ_CONSTRUCT(&bm, opal_bitmap_t);
    opal_bitmap_init(&bm, 64);
    opal_bitmap_clear_all_bits(&bm);
    opal_bitmap_set_bit(&bm, 0);
    opal_bitmap_set_bit(&bm, 3);

    char *s = opal_bitmap_get_string(&bm);
    test_verify("get_string non-NULL", NULL != s);
    if (NULL != s) {
        test_verify("get_string length == size", (int) strlen(s) == opal_bitmap_size(&bm));
        test_verify("get_string marks set bit 0", 'X' == s[0]);
        test_verify("get_string marks unset bit 1", '_' == s[1]);
        test_verify("get_string marks set bit 3", 'X' == s[3]);
        free(s);
    }
    test_verify("get_string(NULL) == NULL", NULL == opal_bitmap_get_string(NULL));

    OBJ_DESTRUCT(&bm);
}

static void test_copy(void)
{
    opal_bitmap_t src, dst;
    OBJ_CONSTRUCT(&src, opal_bitmap_t);
    OBJ_CONSTRUCT(&dst, opal_bitmap_t);

    opal_bitmap_init(&src, 128);
    opal_bitmap_clear_all_bits(&src);
    opal_bitmap_set_bit(&src, 1);
    opal_bitmap_set_bit(&src, 70);

    /* Copy into a fresh (smaller/empty) destination -- exercises the
       (re)allocation path. */
    opal_bitmap_copy(&dst, &src);
    test_verify("copy into empty dst matches src", !opal_bitmap_are_different(&dst, &src));
    test_verify("copy preserved bit 1", opal_bitmap_is_set_bit(&dst, 1));
    test_verify("copy preserved bit 70", opal_bitmap_is_set_bit(&dst, 70));

    /* Copy again into an already-large-enough destination -- exercises the
       no-realloc path. */
    opal_bitmap_clear_all_bits(&src);
    opal_bitmap_set_bit(&src, 5);
    opal_bitmap_copy(&dst, &src);
    test_verify("second copy matches src", !opal_bitmap_are_different(&dst, &src));
    test_verify("second copy preserved bit 5", opal_bitmap_is_set_bit(&dst, 5));
    test_verify("second copy cleared old bit 70", !opal_bitmap_is_set_bit(&dst, 70));

    OBJ_DESTRUCT(&src);
    OBJ_DESTRUCT(&dst);
}

static void test_max_size(void)
{
    opal_bitmap_t bm;
    opal_bitmap_t cap65;
    OBJ_CONSTRUCT(&bm, opal_bitmap_t);

    test_verify("set_max_size(NULL) rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_set_max_size(NULL, 64));

    /* Cap the bitmap at 64 bits, then ask for more -> rejected. */
    test_verify("set_max_size ok", OPAL_SUCCESS == opal_bitmap_set_max_size(&bm, 64));
    test_verify("init beyond max rejected", OPAL_ERR_BAD_PARAM == opal_bitmap_init(&bm, 65));
    test_verify("init within max ok", OPAL_SUCCESS == opal_bitmap_init(&bm, 64));

    OBJ_DESTRUCT(&bm);

    /*
     * The cap is enforced in bits, not rounded up to a 64-bit word.  With
     * a non-word-aligned cap of 65 bits, a request for 128 bits -- which
     * rounds to the same 2-word allocation -- must still be rejected, and
     * bit indices at or beyond the cap must be refused.
     */
    OBJ_CONSTRUCT(&cap65, opal_bitmap_t);
    test_verify("set_max_size(65) ok", OPAL_SUCCESS == opal_bitmap_set_max_size(&cap65, 65));
    test_verify("init(128) beyond 65-bit cap rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_init(&cap65, 128));
    test_verify("init(66) beyond 65-bit cap rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_init(&cap65, 66));
    test_verify("init(65) at cap ok", OPAL_SUCCESS == opal_bitmap_init(&cap65, 65));
    test_verify("set_bit(64) within 65-bit cap ok",
                OPAL_SUCCESS == opal_bitmap_set_bit(&cap65, 64));
    test_verify("set_bit(65) at cap rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_set_bit(&cap65, 65));
    test_verify("set_bit(100) beyond cap rejected",
                OPAL_ERR_BAD_PARAM == opal_bitmap_set_bit(&cap65, 100));

    /*
     * find_and_set must honor the bit cap as well.  Fill every valid bit
     * [0, 65); a further find_and_set then has no room and must refuse,
     * rather than handing back an out-of-cap bit (e.g. bit 65) from the
     * unused tail of the last allocated word.  (Regression: the within-word
     * fast path used to set the bit without checking max_size.)
     */
    {
        int pos = -1;
        for (int b = 0; b < 65; b++) {
            (void) opal_bitmap_set_bit(&cap65, b);
        }
        test_verify("find_and_set on a full 65-bit bitmap is refused",
                    OPAL_SUCCESS != opal_bitmap_find_and_set_first_unset_bit(&cap65, &pos));
        test_verify("find_and_set did not set an out-of-cap bit",
                    !opal_bitmap_is_set_bit(&cap65, 65));
    }

    OBJ_DESTRUCT(&cap65);
}

static void test_size_accessor(void)
{
    opal_bitmap_t bm;
    OBJ_CONSTRUCT(&bm, opal_bitmap_t);

    test_verify("size(NULL) == 0", 0 == opal_bitmap_size(NULL));
    opal_bitmap_init(&bm, 100);
    /* 100 bits rounds up to 2 elements -> 128 bits. */
    test_verify("size rounds up to element multiple", 128 == opal_bitmap_size(&bm));

    OBJ_DESTRUCT(&bm);
}
