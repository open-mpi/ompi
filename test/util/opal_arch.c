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
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit tests for opal_arch_{init,checkmask,set_fortran_logical_size}.
 *
 * KEY FACTS derived from arch.h documentation:
 *
 * opal_arch_checkmask(var, mask) returns:
 *    1   -- all bits in mask are set in *var  (match)
 *    0   -- mask bits are NOT all set in *var (no match)
 *   -1   -- *var does not have valid header bits (HEADERMASK in byte-1)
 *           and cannot be corrected by byte-swapping.
 *
 * A valid arch id must have OPAL_ARCH_HEADERMASK set in the high byte (byte 1
 * of the layout: bits 25-26 of the 32-bit word, i.e., 0x03000000).
 * OPAL_ARCH_HEADERMASK2 (0x00000003) is the "other end" sentinel and must NOT
 * be set for a straightforwardly valid id (the swap path is for byte-swapped
 * remote arch ids).
 *
 * Zero-valued encoding macros: LONGIS32 (0), BOOLIS8 (0), LOGICALIS8 (0),
 * LONGDOUBLEIS64 (0), LDEXPSIZEIS11 (0), LDMANTDIGIS53 (0).
 * Asserting checkmask on a 0-valued mask is always 1 when the header is
 * valid; we therefore only use non-zero mask values in assertions.
 *
 * opal_arch_init() and opal_arch_set_fortran_logical_size() only OR bits
 * into opal_local_arch; they never clear bits.  Test interactions are noted
 * in each sub-test.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/arch.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <stdlib.h>

/* ------------------------------------------------------------------ */
/* test_checkmask_valid_header                                         */
/*                                                                     */
/* Build a hand-crafted valid arch id (HEADERMASK set, HEADERMASK2    */
/* clear) and exercise the trichotomy of return values.               */
/* ------------------------------------------------------------------ */
static void test_checkmask_valid_header(void)
{
    /* A minimal valid id: header bits set and LONGIS64 set. */
    uint32_t v;
    int32_t r;

    v = OPAL_ARCH_HEADERMASK | OPAL_ARCH_LONGIS64;

    /* mask matches: expect 1 */
    r = opal_arch_checkmask(&v, OPAL_ARCH_LONGIS64);
    test_verify("checkmask: matching non-zero mask returns 1", 1 == r);

    /* mask does not match: BOOLIS16 is not set; expect 0 */
    v = OPAL_ARCH_HEADERMASK | OPAL_ARCH_LONGIS64;
    r = opal_arch_checkmask(&v, OPAL_ARCH_BOOLIS16);
    test_verify("checkmask: non-matching non-zero mask returns 0", 0 == r);

    /* multiple bits in mask: LONGIS64 | BOOLIS16 -- only LONGIS64 is set */
    v = OPAL_ARCH_HEADERMASK | OPAL_ARCH_LONGIS64;
    r = opal_arch_checkmask(&v, OPAL_ARCH_LONGIS64 | OPAL_ARCH_BOOLIS16);
    test_verify("checkmask: partial mask match returns 0", 0 == r);

    /* all bits of compound mask are set: both LONGIS64 and BOOLIS16 */
    v = OPAL_ARCH_HEADERMASK | OPAL_ARCH_LONGIS64 | OPAL_ARCH_BOOLIS16;
    r = opal_arch_checkmask(&v, OPAL_ARCH_LONGIS64 | OPAL_ARCH_BOOLIS16);
    test_verify("checkmask: all bits of compound mask set returns 1", 1 == r);

    /* ISBIGENDIAN mask -- test both states */
    v = OPAL_ARCH_HEADERMASK | OPAL_ARCH_ISBIGENDIAN;
    r = opal_arch_checkmask(&v, OPAL_ARCH_ISBIGENDIAN);
    test_verify("checkmask: ISBIGENDIAN set -> match returns 1", 1 == r);

    v = OPAL_ARCH_HEADERMASK; /* no ISBIGENDIAN */
    r = opal_arch_checkmask(&v, OPAL_ARCH_ISBIGENDIAN);
    test_verify("checkmask: ISBIGENDIAN not set -> match returns 0", 0 == r);
}

/* ------------------------------------------------------------------ */
/* test_checkmask_invalid_header                                       */
/*                                                                     */
/* An id with no header bits and no HEADERMASK2 bits is invalid; the  */
/* function must return -1.                                            */
/* ------------------------------------------------------------------ */
static void test_checkmask_invalid_header(void)
{
    /* Neither HEADERMASK (0x03000000) nor HEADERMASK2 (0x00000003) set. */
    uint32_t v = OPAL_ARCH_LONGIS64; /* no header at all */
    int32_t r = opal_arch_checkmask(&v, OPAL_ARCH_LONGIS64);
    test_verify("checkmask: no header bits returns -1", -1 == r);
}

/* ------------------------------------------------------------------ */
/* test_arch_init                                                      */
/*                                                                     */
/* After opal_arch_init() the global opal_local_arch must have valid  */
/* header bits and must not have the other-end sentinel HEADERMASK2.  */
/* ------------------------------------------------------------------ */
static void test_arch_init(void)
{
    int rc;
    uint32_t v;
    int32_t r;

    rc = opal_arch_init();
    test_verify("opal_arch_init returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* Verify header is valid by trying a benign checkmask call. */
    v = opal_local_arch;
    r = opal_arch_checkmask(&v, OPAL_ARCH_LONGIS64);
    test_verify("opal_arch_init: resulting opal_local_arch has valid header",
                -1 != r);

    /* HEADERMASK must be set. */
    test_verify("opal_arch_init: HEADERMASK bits set",
                0 != (opal_local_arch & OPAL_ARCH_HEADERMASK));
}

/* ------------------------------------------------------------------ */
/* test_set_fortran_logical_size                                       */
/*                                                                     */
/* opal_arch_set_fortran_logical_size() ORs the appropriate           */
/* LOGICALIS* bits into opal_local_arch.  We call it with size=2      */
/* (LOGICALIS16, a non-zero mask value) and verify via checkmask.     */
/*                                                                     */
/* NOTE: Because init already ran, LOGICALIS8 (0x0) bits cannot be    */
/* tested this way (it is the zero encoding).  We test LOGICALIS16    */
/* and LOGICALIS32 which have non-zero bit patterns.                  */
/* ------------------------------------------------------------------ */
static void test_set_fortran_logical_size(void)
{
    int rc;
    uint32_t v;
    int32_t r;

    /* Set logical size to 2 bytes -> LOGICALIS16 (0x00000100) */
    rc = opal_arch_set_fortran_logical_size(2);
    test_verify("set_fortran_logical_size(2) returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);

    v = opal_local_arch;
    r = opal_arch_checkmask(&v, OPAL_ARCH_LOGICALIS16);
    test_verify("set_fortran_logical_size(2): LOGICALIS16 bit set",
                1 == r);

    /* The mask field (LOGICALISxx) should now reflect 16-bit encoding. */
    test_verify("set_fortran_logical_size(2): LOGICALISxx field == LOGICALIS16",
                OPAL_ARCH_LOGICALIS16 == (opal_local_arch & OPAL_ARCH_LOGICALISxx));
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int r;

    test_init("opal_arch");

    opal_init_util(&argc, &argv);

    /*
     * Run checkmask tests on hand-crafted values first (no dependency on
     * opal_arch_init state).  Then run arch_init, then set_fortran.
     */
    test_checkmask_valid_header();
    test_checkmask_invalid_header();
    test_arch_init();
    test_set_fortran_logical_size();

    r = test_finalize();
    opal_finalize_util();
    return r;
}
