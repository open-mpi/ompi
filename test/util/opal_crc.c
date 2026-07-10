/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "support.h"
#include "opal/util/crc.h"

#include <string.h>
#include <stdint.h>
#include <stdlib.h>

/* -----------------------------------------------------------------------
 * Tests: opal_csum_partial / opal_csum (unsigned long variant)
 * ---------------------------------------------------------------------- */

static void test_csum_partial_determinism(void)
{
    /* Same input must produce the same output */
    static const unsigned char buf[16] = {
        0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
        0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10
    };
    unsigned long plong1 = 0, plong2 = 0;
    size_t plen1 = 0, plen2 = 0;

    unsigned long c1 = opal_csum_partial(buf, sizeof(buf), &plong1, &plen1);
    unsigned long c2 = opal_csum_partial(buf, sizeof(buf), &plong2, &plen2);

    test_verify("csum_partial: same input -> same result (c1==c2)", c1 == c2);
    test_verify("csum_partial: inline helper opal_csum matches",
                opal_csum(buf, sizeof(buf)) == c1);
}

static void test_csum_partial_different_inputs(void)
{
    static const unsigned char buf_a[8] = {0x01, 0x02, 0x03, 0x04,
                                           0x05, 0x06, 0x07, 0x08};
    static const unsigned char buf_b[8] = {0xff, 0xfe, 0xfd, 0xfc,
                                           0xfb, 0xfa, 0xf9, 0xf8};
    unsigned long plong = 0;
    size_t plen = 0;

    unsigned long ca = opal_csum_partial(buf_a, sizeof(buf_a), &plong, &plen);
    plong = 0;
    plen = 0;
    unsigned long cb = opal_csum_partial(buf_b, sizeof(buf_b), &plong, &plen);

    test_verify("csum_partial: different inputs give different checksums", ca != cb);
}

static void test_csum_partial_zero_buf(void)
{
    /* All-zero buffer: checksum must be 0 */
    static const unsigned char zeros[32] = {0};
    unsigned long plong = 0;
    size_t plen = 0;

    unsigned long c = opal_csum_partial(zeros, sizeof(zeros), &plong, &plen);
    test_verify("csum_partial: all-zeros buffer -> csum is 0", 0 == c);
}

static void test_csum_partial_nonaligned_length(void)
{
    /* Non-word-aligned length (5 bytes): two calls must match opal_csum */
    static const unsigned char buf[5] = {0xAA, 0xBB, 0xCC, 0xDD, 0xEE};
    unsigned long plong = 0;
    size_t plen = 0;

    unsigned long c_partial = opal_csum_partial(buf, 5, &plong, &plen);
    unsigned long c_inline = opal_csum(buf, 5);

    test_verify("csum_partial: 5-byte (non-aligned) matches opal_csum", c_partial == c_inline);
}

static void test_csum_partial_incremental(void)
{
    /*
     * Incremental: two partial calls over [A|B] must equal one call over [AB].
     * The partial accumulator state is threaded between calls.
     *
     * We split a 20-byte buffer at byte 7 (non-word-aligned boundary).
     */
    static const unsigned char full[20] = {
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,
        0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00,
        0x12, 0x34, 0x56, 0x78
    };
    const size_t split = 7;

    /* Single call over all 20 bytes */
    unsigned long plong0 = 0;
    size_t plen0 = 0;
    unsigned long c_full = opal_csum_partial(full, sizeof(full), &plong0, &plen0);

    /* Two incremental calls */
    unsigned long plong_inc = 0;
    size_t plen_inc = 0;
    unsigned long c1 = opal_csum_partial(full, split, &plong_inc, &plen_inc);
    unsigned long c2 = opal_csum_partial(full + split, sizeof(full) - split,
                                         &plong_inc, &plen_inc);
    unsigned long c_inc = c1 + c2;

    test_verify("csum_partial: incremental over [0..7) + [7..20) == single over [0..20)",
                c_inc == c_full);
}

/* -----------------------------------------------------------------------
 * Tests: opal_uicsum_partial / opal_uicsum (unsigned int variant)
 * ---------------------------------------------------------------------- */

static void test_uicsum_partial_determinism(void)
{
    static const unsigned char buf[16] = {
        0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
        0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10
    };
    unsigned int pint1 = 0, pint2 = 0;
    size_t plen1 = 0, plen2 = 0;

    unsigned int c1 = opal_uicsum_partial(buf, sizeof(buf), &pint1, &plen1);
    unsigned int c2 = opal_uicsum_partial(buf, sizeof(buf), &pint2, &plen2);

    test_verify("uicsum_partial: determinism (c1==c2)", c1 == c2);
    test_verify("uicsum_partial: inline opal_uicsum matches",
                opal_uicsum(buf, sizeof(buf)) == c1);
}

static void test_uicsum_partial_zero_buf(void)
{
    static const unsigned char zeros[32] = {0};
    unsigned int pint = 0;
    size_t plen = 0;

    unsigned int c = opal_uicsum_partial(zeros, sizeof(zeros), &pint, &plen);
    test_verify("uicsum_partial: all-zeros buffer -> csum is 0", 0u == c);
}

static void test_uicsum_partial_nonaligned(void)
{
    /* 3-byte buffer: non-int-aligned */
    static const unsigned char buf[3] = {0x01, 0x02, 0x03};
    unsigned int pint = 0;
    size_t plen = 0;

    unsigned int c_partial = opal_uicsum_partial(buf, 3, &pint, &plen);
    unsigned int c_inline  = opal_uicsum(buf, 3);

    test_verify("uicsum_partial: 3-byte non-aligned matches opal_uicsum",
                c_partial == c_inline);
}

static void test_uicsum_partial_incremental(void)
{
    /*
     * Split a 20-byte buffer at a 5-byte boundary (non-int-aligned).
     */
    static const unsigned char full[20] = {
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,
        0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00,
        0x12, 0x34, 0x56, 0x78
    };
    const size_t split = 5;

    unsigned int pint0 = 0;
    size_t plen0 = 0;
    unsigned int c_full = opal_uicsum_partial(full, sizeof(full), &pint0, &plen0);

    unsigned int pint_inc = 0;
    size_t plen_inc = 0;
    unsigned int c1 = opal_uicsum_partial(full, split, &pint_inc, &plen_inc);
    unsigned int c2 = opal_uicsum_partial(full + split, sizeof(full) - split,
                                          &pint_inc, &plen_inc);
    unsigned int c_inc = c1 + c2;

    test_verify("uicsum_partial: incremental [0..5)+[5..20) == single [0..20)",
                c_inc == c_full);
}

static void test_uicsum_partial_different_inputs(void)
{
    static const unsigned char a[8] = {0xAA, 0xBB, 0xCC, 0xDD,
                                       0x11, 0x22, 0x33, 0x44};
    static const unsigned char b[8] = {0x00, 0x01, 0x02, 0x03,
                                       0x04, 0x05, 0x06, 0x07};
    unsigned int pint = 0;
    size_t plen = 0;

    unsigned int ca = opal_uicsum_partial(a, sizeof(a), &pint, &plen);
    pint = 0;
    plen = 0;
    unsigned int cb = opal_uicsum_partial(b, sizeof(b), &pint, &plen);

    test_verify("uicsum_partial: different inputs -> different checksums", ca != cb);
}

/* -----------------------------------------------------------------------
 * Tests: opal_bcopy_csum_partial (unsigned long variant)
 * ---------------------------------------------------------------------- */

static void test_bcopy_csum_partial_copy(void)
{
    /* Verify that the bcopy variant actually copies src -> dst */
    static const unsigned char src[16] = {
        0xDE, 0xAD, 0xBE, 0xEF, 0xCA, 0xFE, 0xBA, 0xBE,
        0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF
    };
    unsigned char dst[16];
    unsigned long plong = 0;
    size_t plen = 0;

    memset(dst, 0, sizeof(dst));
    opal_bcopy_csum_partial(src, dst, sizeof(src), sizeof(src), &plong, &plen);

    test_verify("bcopy_csum_partial: destination contains copy of source",
                0 == memcmp(src, dst, sizeof(src)));
}

static void test_bcopy_csum_partial_matches_csum(void)
{
    /*
     * The checksum returned by bcopy_csum_partial must equal the one
     * returned by csum_partial over the same data.
     */
    static const unsigned char src[16] = {
        0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80,
        0x90, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xf0, 0x00
    };
    unsigned char dst[16];
    unsigned long plong_bc = 0, plong_cs = 0;
    size_t plen_bc = 0, plen_cs = 0;

    unsigned long c_bc = opal_bcopy_csum_partial(src, dst, sizeof(src), sizeof(src),
                                                 &plong_bc, &plen_bc);
    unsigned long c_cs = opal_csum_partial(src, sizeof(src), &plong_cs, &plen_cs);

    test_verify("bcopy_csum_partial: checksum matches csum_partial", c_bc == c_cs);
}

static void test_bcopy_csum_partial_nonaligned(void)
{
    /* 7-byte buffer: exercises the partial-word residue path */
    static const unsigned char src[7] = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07};
    unsigned char dst[7];
    unsigned long plong_bc = 0, plong_cs = 0;
    size_t plen_bc = 0, plen_cs = 0;

    memset(dst, 0, sizeof(dst));
    unsigned long c_bc = opal_bcopy_csum_partial(src, dst, sizeof(src), sizeof(src),
                                                 &plong_bc, &plen_bc);
    unsigned long c_cs = opal_csum_partial(src, sizeof(src), &plong_cs, &plen_cs);

    test_verify("bcopy_csum_partial 7-byte: dst is copy of src",
                0 == memcmp(src, dst, sizeof(src)));
    test_verify("bcopy_csum_partial 7-byte: checksum matches csum_partial", c_bc == c_cs);
}

/* -----------------------------------------------------------------------
 * Tests: opal_bcopy_uicsum_partial (unsigned int variant)
 * ---------------------------------------------------------------------- */

static void test_bcopy_uicsum_partial_copy(void)
{
    static const unsigned char src[12] = {
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,
        0x99, 0xAA, 0xBB, 0xCC
    };
    unsigned char dst[12];
    unsigned int pint = 0;
    size_t plen = 0;

    memset(dst, 0, sizeof(dst));
    opal_bcopy_uicsum_partial(src, dst, sizeof(src), sizeof(src), &pint, &plen);

    test_verify("bcopy_uicsum_partial: dst is copy of src",
                0 == memcmp(src, dst, sizeof(src)));
}

static void test_bcopy_uicsum_partial_matches_uicsum(void)
{
    static const unsigned char src[12] = {
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,
        0x99, 0xAA, 0xBB, 0xCC
    };
    unsigned char dst[12];
    unsigned int pint_bc = 0, pint_ui = 0;
    size_t plen_bc = 0, plen_ui = 0;

    unsigned int c_bc = opal_bcopy_uicsum_partial(src, dst, sizeof(src), sizeof(src),
                                                   &pint_bc, &plen_bc);
    unsigned int c_ui = opal_uicsum_partial(src, sizeof(src), &pint_ui, &plen_ui);

    test_verify("bcopy_uicsum_partial: checksum matches uicsum_partial", c_bc == c_ui);
}

static void test_bcopy_uicsum_partial_nonaligned(void)
{
    /* 5-byte: non-int-aligned */
    static const unsigned char src[5] = {0xAA, 0xBB, 0xCC, 0xDD, 0xEE};
    unsigned char dst[5];
    unsigned int pint_bc = 0, pint_ui = 0;
    size_t plen_bc = 0, plen_ui = 0;

    memset(dst, 0, sizeof(dst));
    unsigned int c_bc = opal_bcopy_uicsum_partial(src, dst, sizeof(src), sizeof(src),
                                                   &pint_bc, &plen_bc);
    unsigned int c_ui = opal_uicsum_partial(src, sizeof(src), &pint_ui, &plen_ui);

    test_verify("bcopy_uicsum_partial 5-byte: dst is copy of src",
                0 == memcmp(src, dst, sizeof(src)));
    test_verify("bcopy_uicsum_partial 5-byte: checksum matches uicsum_partial", c_bc == c_ui);
}

/* -----------------------------------------------------------------------
 * Tests: opal_uicrc_partial / opal_uicrc
 * ---------------------------------------------------------------------- */

static void test_uicrc_partial_table_init(void)
{
    /*
     * Calling opal_initialize_crc_table explicitly then opal_uicrc_partial
     * must produce the same result as calling opal_uicrc_partial cold (which
     * will auto-init the table).
     */
    opal_initialize_crc_table();
    static const unsigned char buf[8] = {0x01, 0x02, 0x03, 0x04,
                                         0x05, 0x06, 0x07, 0x08};
    unsigned int crc1 = opal_uicrc_partial(buf, sizeof(buf), CRC_INITIAL_REGISTER);
    unsigned int crc2 = opal_uicrc(buf, sizeof(buf));

    test_verify("uicrc: explicit table init matches inline opal_uicrc", crc1 == crc2);
}

static void test_uicrc_partial_determinism(void)
{
    static const unsigned char buf[16] = {
        0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
        0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70
    };

    unsigned int c1 = opal_uicrc(buf, sizeof(buf));
    unsigned int c2 = opal_uicrc(buf, sizeof(buf));

    test_verify("uicrc: determinism (c1==c2)", c1 == c2);
}

static void test_uicrc_partial_different_inputs(void)
{
    static const unsigned char a[4] = {0x01, 0x02, 0x03, 0x04};
    static const unsigned char b[4] = {0x05, 0x06, 0x07, 0x08};

    unsigned int ca = opal_uicrc(a, sizeof(a));
    unsigned int cb = opal_uicrc(b, sizeof(b));

    test_verify("uicrc: different inputs -> different CRCs", ca != cb);
}

static void test_uicrc_partial_nonaligned(void)
{
    /* 3-byte (non-int-aligned): exercises the unaligned source path */
    static const unsigned char buf[3] = {0xDE, 0xAD, 0xBE};

    unsigned int c1 = opal_uicrc(buf, 3);
    unsigned int c2 = opal_uicrc(buf, 3);

    test_verify("uicrc: 3-byte non-aligned, deterministic", c1 == c2);
}

static void test_uicrc_partial_incremental(void)
{
    /*
     * CRC chaining: crc(A || B) via two partial calls must equal
     * opal_uicrc over the concatenated buffer.
     *
     * opal_uicrc_partial(B, partial_crc) takes the running CRC as seed.
     */
    static const unsigned char full[16] = {
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,
        0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00
    };
    const size_t split = 7;   /* non-int-aligned split */

    unsigned int c_full = opal_uicrc(full, sizeof(full));

    /* chain: first call seeds with CRC_INITIAL_REGISTER */
    unsigned int crc_mid = opal_uicrc_partial(full, split, CRC_INITIAL_REGISTER);
    unsigned int c_chain = opal_uicrc_partial(full + split, sizeof(full) - split, crc_mid);

    test_verify("uicrc_partial: chained [0..7)+[7..16) == single [0..16)",
                c_chain == c_full);
}

static void test_uicrc_partial_all_zeros(void)
{
    /* All-zero buffer: CRC must be non-initial-register (CRC has diffusion) */
    static const unsigned char zeros[8] = {0};
    unsigned int crc = opal_uicrc(zeros, sizeof(zeros));

    /* CRC of zeros with standard polynomial is not CRC_INITIAL_REGISTER */
    test_verify("uicrc: CRC of all-zeros differs from initial register",
                CRC_INITIAL_REGISTER != crc);
}

static void test_uicrc_partial_large_aligned(void)
{
    /* 64-byte (word-aligned) buffer: exercises the fast int-aligned path */
    unsigned char buf[64];
    unsigned int i;
    for (i = 0; i < 64; i++) {
        buf[i] = (unsigned char) (i & 0xff);
    }

    unsigned int c1 = opal_uicrc(buf, sizeof(buf));
    unsigned int c2 = opal_uicrc(buf, sizeof(buf));

    test_verify("uicrc: 64-byte aligned buffer, deterministic", c1 == c2);
}

/* -----------------------------------------------------------------------
 * Tests: opal_bcopy_uicrc_partial
 * ---------------------------------------------------------------------- */

static void test_bcopy_uicrc_partial_copy(void)
{
    static const unsigned char src[16] = {
        0xDE, 0xAD, 0xBE, 0xEF, 0xCA, 0xFE, 0xBA, 0xBE,
        0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF
    };
    unsigned char dst[16];

    memset(dst, 0, sizeof(dst));
    opal_bcopy_uicrc_partial(src, dst, sizeof(src), sizeof(src), CRC_INITIAL_REGISTER);

    test_verify("bcopy_uicrc_partial: dst is copy of src",
                0 == memcmp(src, dst, sizeof(src)));
}

static void test_bcopy_uicrc_partial_matches_uicrc(void)
{
    static const unsigned char src[16] = {
        0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80,
        0x90, 0xa0, 0xb0, 0xc0, 0xd0, 0xe0, 0xf0, 0x00
    };
    unsigned char dst[16];

    unsigned int c_bc  = opal_bcopy_uicrc_partial(src, dst, sizeof(src), sizeof(src),
                                                   CRC_INITIAL_REGISTER);
    unsigned int c_crc = opal_uicrc(src, sizeof(src));

    test_verify("bcopy_uicrc_partial: checksum matches opal_uicrc", c_bc == c_crc);
}

static void test_bcopy_uicrc_partial_nonaligned(void)
{
    /* 5-byte: exercises the non-int-aligned copy path */
    static const unsigned char src[5] = {0x01, 0x02, 0x03, 0x04, 0x05};
    unsigned char dst[5];

    memset(dst, 0, sizeof(dst));
    unsigned int c_bc  = opal_bcopy_uicrc_partial(src, dst, sizeof(src), sizeof(src),
                                                   CRC_INITIAL_REGISTER);
    unsigned int c_crc = opal_uicrc(src, sizeof(src));

    test_verify("bcopy_uicrc_partial 5-byte: dst is copy of src",
                0 == memcmp(src, dst, sizeof(src)));
    test_verify("bcopy_uicrc_partial 5-byte: checksum matches opal_uicrc", c_bc == c_crc);
}

static void test_bcopy_uicrc_partial_inline_helper(void)
{
    /* opal_bcopy_uicrc is a static inline wrapping bcopy_uicrc_partial */
    static const unsigned char src[8] = {0xAA, 0xBB, 0xCC, 0xDD,
                                         0x11, 0x22, 0x33, 0x44};
    unsigned char dst1[8], dst2[8];

    memset(dst1, 0, sizeof(dst1));
    memset(dst2, 0, sizeof(dst2));

    unsigned int c1 = opal_bcopy_uicrc(src, dst1, sizeof(src), sizeof(src));
    unsigned int c2 = opal_uicrc(src, sizeof(src));

    test_verify("bcopy_uicrc (inline helper): checksum matches opal_uicrc", c1 == c2);
    test_verify("bcopy_uicrc (inline helper): dst is copy of src",
                0 == memcmp(src, dst1, sizeof(src)));
}

static void test_bcopy_uicrc_partial_crconly_residue(void)
{
    /*
     * Test crclen > copylen: the function must CRC crclen bytes total
     * while only copying copylen bytes from source.
     *
     * Use a large source buffer; copy only first half, but CRC the full buf.
     */
    static const unsigned char src[16] = {
        0xA1, 0xB2, 0xC3, 0xD4, 0xE5, 0xF6, 0x07, 0x18,
        0x29, 0x3A, 0x4B, 0x5C, 0x6D, 0x7E, 0x8F, 0x90
    };
    unsigned char dst[16];
    const size_t copylen = 8;
    const size_t crclen  = 16;

    memset(dst, 0, sizeof(dst));
    /* Copy 8 bytes, CRC all 16 */
    unsigned int c_partial = opal_bcopy_uicrc_partial(src, dst, copylen, crclen,
                                                       CRC_INITIAL_REGISTER);
    /* CRC of all 16 bytes via plain crc */
    unsigned int c_full    = opal_uicrc(src, crclen);

    test_verify("bcopy_uicrc_partial (crclen>copylen): only copylen bytes copied",
                0 == memcmp(src, dst, copylen));
    test_verify("bcopy_uicrc_partial (crclen>copylen): CRC covers full crclen",
                c_partial == c_full);
}

/* -----------------------------------------------------------------------
 * Tests: csum16 (inline in header -- test via the macro and direct call)
 * ---------------------------------------------------------------------- */

static void test_csum16_basic(void)
{
    /*
     * opal_csum16 is a static inline computing a 16-bit Internet-style
     * checksum (sum of 16-bit words, fold carry).
     * For an all-zero buffer the checksum is 0.
     */
    static const unsigned char zeros[8] = {0};
    uint16_t c = opal_csum16(zeros, sizeof(zeros));
    test_verify("csum16: all-zeros -> 0", 0u == c);
}

static void test_csum16_determinism(void)
{
    static const unsigned char buf[10] = {
        0x00, 0x01, 0x00, 0x02, 0x00, 0x03, 0x00, 0x04, 0x00, 0x05
    };
    uint16_t c1 = opal_csum16(buf, sizeof(buf));
    uint16_t c2 = opal_csum16(buf, sizeof(buf));
    test_verify("csum16: determinism (c1==c2)", c1 == c2);
}

static void test_csum16_different_inputs(void)
{
    static const unsigned char a[4] = {0x00, 0x01, 0x00, 0x02};
    static const unsigned char b[4] = {0x00, 0x03, 0x00, 0x04};
    uint16_t ca = opal_csum16(a, sizeof(a));
    uint16_t cb = opal_csum16(b, sizeof(b));
    test_verify("csum16: different inputs -> different checksums", ca != cb);
}

static void test_csum16_known_value(void)
{
    /*
     * opal_csum16 sums native-endian uint16_t words and returns the
     * carry-folded sum (not the one's complement -- see test_csum16_basic,
     * where an all-zero buffer checksums to 0).  A buffer holding a single
     * host-order uint16_t value therefore checksums to that exact value:
     * one word is read, and a value <= 0xFFFF produces no carry to fold.
     * Building the buffer from a uint16_t via memcpy makes the expected
     * result endianness-portable.
     */
    const uint16_t expected = 0x1234;
    unsigned char buf[sizeof(uint16_t)];
    uint16_t c;

    memcpy(buf, &expected, sizeof(expected));
    c = opal_csum16(buf, sizeof(buf));
    test_verify("csum16: single host-order uint16_t checksums to its own value",
                expected == c);
}

static void test_csum16_odd_length(void)
{
    /* Odd length exercises the "leftover byte" path */
    static const unsigned char buf[3] = {0x01, 0x02, 0x03};
    uint16_t c1 = opal_csum16(buf, sizeof(buf));
    uint16_t c2 = opal_csum16(buf, sizeof(buf));
    test_verify("csum16: odd-length (3) deterministic", c1 == c2);
}

/* -----------------------------------------------------------------------
 * Tests: OPAL_CSUM / OPAL_CSUM_PARTIAL macros
 * ---------------------------------------------------------------------- */

static void test_macro_csum(void)
{
    static const unsigned char buf[8] = {0xDE, 0xAD, 0xBE, 0xEF,
                                         0xCA, 0xFE, 0xBA, 0xBE};
    unsigned int pint = 0;
    size_t plen = 0;

    unsigned int c_macro = OPAL_CSUM(buf, sizeof(buf));
    unsigned int c_direct = opal_uicsum(buf, sizeof(buf));

    test_verify("OPAL_CSUM macro matches opal_uicsum", c_macro == c_direct);

    unsigned int c_partial_macro =
        OPAL_CSUM_PARTIAL(buf, sizeof(buf), &pint, &plen);
    pint = 0;
    plen = 0;
    unsigned int c_partial_direct =
        opal_uicsum_partial(buf, sizeof(buf), &pint, &plen);

    test_verify("OPAL_CSUM_PARTIAL macro matches opal_uicsum_partial",
                c_partial_macro == c_partial_direct);
}

/* -----------------------------------------------------------------------
 * main
 * ---------------------------------------------------------------------- */

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    test_init("opal_crc");

    /* opal_csum_partial / opal_csum */
    test_csum_partial_determinism();
    test_csum_partial_different_inputs();
    test_csum_partial_zero_buf();
    test_csum_partial_nonaligned_length();
    test_csum_partial_incremental();

    /* opal_uicsum_partial / opal_uicsum */
    test_uicsum_partial_determinism();
    test_uicsum_partial_zero_buf();
    test_uicsum_partial_nonaligned();
    test_uicsum_partial_incremental();
    test_uicsum_partial_different_inputs();

    /* opal_bcopy_csum_partial */
    test_bcopy_csum_partial_copy();
    test_bcopy_csum_partial_matches_csum();
    test_bcopy_csum_partial_nonaligned();

    /* opal_bcopy_uicsum_partial */
    test_bcopy_uicsum_partial_copy();
    test_bcopy_uicsum_partial_matches_uicsum();
    test_bcopy_uicsum_partial_nonaligned();

    /* opal_uicrc_partial / opal_uicrc (CRC32) */
    test_uicrc_partial_table_init();
    test_uicrc_partial_determinism();
    test_uicrc_partial_different_inputs();
    test_uicrc_partial_nonaligned();
    test_uicrc_partial_incremental();
    test_uicrc_partial_all_zeros();
    test_uicrc_partial_large_aligned();

    /* opal_bcopy_uicrc_partial */
    test_bcopy_uicrc_partial_copy();
    test_bcopy_uicrc_partial_matches_uicrc();
    test_bcopy_uicrc_partial_nonaligned();
    test_bcopy_uicrc_partial_inline_helper();
    test_bcopy_uicrc_partial_crconly_residue();

    /* opal_csum16 */
    test_csum16_basic();
    test_csum16_determinism();
    test_csum16_different_inputs();
    test_csum16_known_value();
    test_csum16_odd_length();

    /* macros */
    test_macro_csum();

    return test_finalize();
}
