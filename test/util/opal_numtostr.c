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

#include "opal_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "support.h"
#include "opal/util/numtostr.h"
#include "opal/constants.h"

int main(int argc, char *argv[])
{
    char *result;

    test_init("opal_numtostr");

    /* --- opal_ltostr: zero --- */
    result = opal_ltostr(0L);
    test_verify("ltostr(0): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("ltostr(0): equals '0'", 0 == strcmp(result, "0"));
        free(result);
    }

    /* --- opal_ltostr: positive --- */
    result = opal_ltostr(42L);
    test_verify("ltostr(42): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("ltostr(42): equals '42'", 0 == strcmp(result, "42"));
        free(result);
    }

    /* --- opal_ltostr: negative --- */
    result = opal_ltostr(-1L);
    test_verify("ltostr(-1): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("ltostr(-1): equals '-1'", 0 == strcmp(result, "-1"));
        free(result);
    }

    /* --- opal_ltostr: negative large --- */
    result = opal_ltostr(-9999L);
    test_verify("ltostr(-9999): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("ltostr(-9999): equals '-9999'", 0 == strcmp(result, "-9999"));
        free(result);
    }

    /* --- opal_ltostr: large positive --- */
    result = opal_ltostr(1000000L);
    test_verify("ltostr(1000000): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("ltostr(1000000): equals '1000000'", 0 == strcmp(result, "1000000"));
        free(result);
    }

    /* --- opal_ltostr: value 10 (original test case) --- */
    result = opal_ltostr(10L);
    test_verify("ltostr(10): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("ltostr(10): equals '10'", 0 == strcmp(result, "10"));
        free(result);
    }

    /* --- opal_dtostr: zero ---
     * %f with no precision specifier always produces 6 decimal digits.
     */
    result = opal_dtostr(0.0);
    test_verify("dtostr(0.0): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("dtostr(0.0): equals '0.000000'", 0 == strcmp(result, "0.000000"));
        free(result);
    }

    /* --- opal_dtostr: positive --- */
    result = opal_dtostr(1.0);
    test_verify("dtostr(1.0): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("dtostr(1.0): equals '1.000000'", 0 == strcmp(result, "1.000000"));
        free(result);
    }

    /* --- opal_dtostr: positive fractional (original test case) --- */
    result = opal_dtostr(5.32);
    test_verify("dtostr(5.32): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("dtostr(5.32): equals '5.320000'", 0 == strcmp(result, "5.320000"));
        free(result);
    }

    /* --- opal_dtostr: negative --- */
    result = opal_dtostr(-3.14);
    test_verify("dtostr(-3.14): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("dtostr(-3.14): equals '-3.140000'", 0 == strcmp(result, "-3.140000"));
        free(result);
    }

    /* --- opal_dtostr: large value --- */
    result = opal_dtostr(1e6);
    test_verify("dtostr(1e6): not NULL", NULL != result);
    if (NULL != result) {
        test_verify("dtostr(1e6): equals '1000000.000000'",
                    0 == strcmp(result, "1000000.000000"));
        free(result);
    }

    return test_finalize();
}
