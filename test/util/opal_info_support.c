/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "opal_config.h"

#include <stdlib.h>
#include <string.h>

#include "support.h"
#include "opal/runtime/opal_info_support.h"

static void check_pmix_version(const char *input, const char *expected)
{
    char *result = opal_info_pmix_version_from_string(input);

    if (NULL == expected) {
        test_verify(input ? input : "(NULL)", NULL == result);
    } else {
        test_verify(input, NULL != result && 0 == strcmp(result, expected));
    }
    free(result);
}

int main(int argc, char *argv[])
{
    test_init("opal_info_support");

    // Release build of PMIx
    check_pmix_version("OpenPMIx 6.0.1 (PMIx Standard: 5.0, Stable ABI: 0.0, "
                       "Provisional ABI: 0.0)",
                       "6.0.1");
    // Greek versions
    check_pmix_version("OpenPMIx 7.0.0a1 (PMIx Standard: 5.0, Stable ABI: 0.0, "
                       "Provisional ABI: 0.0)",
                       "7.0.0a1");
    check_pmix_version("OpenPMIx 5.0.10rc1 (PMIx Standard: 4.2, Stable ABI: 0.0, "
                       "Provisional ABI: 0.0)",
                       "5.0.10rc1");
    // Build of PMIx from a git clone
    check_pmix_version("OpenPMIx 7.0.0a1, repo rev: v5.0.10rc1-1707-g2c52ad481 "
                       "(PMIx Standard: 5.0, Stable ABI: 0.0, Provisional ABI: 0.0)",
                       "7.0.0a1");
    // Just the version, with nothing after it
    check_pmix_version("OpenPMIx 6.0.1", "6.0.1");
    // Unknown formats are returned verbatim
    check_pmix_version("PMIx 4.2.0", "PMIx 4.2.0");
    check_pmix_version("OpenPMIx ", "OpenPMIx ");
    check_pmix_version("OpenPMIx  6.0.1", "OpenPMIx  6.0.1");
    check_pmix_version("", "");
    // NULL in, NULL out
    check_pmix_version(NULL, NULL);

    test_verify("value_or_not_available: value",
                0 == strcmp(opal_info_value_or_not_available("1.2.3"), "1.2.3"));
    test_verify("value_or_not_available: empty",
                0 == strcmp(opal_info_value_or_not_available(""), "not available"));
    test_verify("value_or_not_available: NULL",
                0 == strcmp(opal_info_value_or_not_available(NULL), "not available"));

    return test_finalize();
}
