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

#include <string.h>
#include <stdlib.h>

#include "support.h"
#include "opal/util/string_copy.h"
#include "opal/constants.h"

int main(int argc, char *argv[])
{
    char dest[64];
    const char *src;

    test_init("opal_string_copy");

    /* --- Normal: source shorter than dest_len --- */
    memset(dest, 0xFF, sizeof(dest));
    src = "hello";
    opal_string_copy(dest, src, sizeof(dest));
    test_verify("normal copy: content matches", 0 == strcmp(dest, "hello"));
    test_verify("normal copy: NUL terminated", '\0' == dest[5]);

    /* --- Exact fit: source length == dest_len - 1 (plus NUL = dest_len) --- */
    memset(dest, 0xFF, sizeof(dest));
    src = "abcde";          /* 5 chars + NUL = 6 bytes; dest_len=6 */
    opal_string_copy(dest, src, 6);
    test_verify("exact fit: content matches", 0 == strcmp(dest, "abcde"));
    test_verify("exact fit: NUL at position 5", '\0' == dest[5]);

    /* --- Truncation: source longer than dest_len --- */
    memset(dest, 0xFF, sizeof(dest));
    src = "abcdefghij";     /* 10 chars; dest_len=5 -> copy 4 chars + NUL */
    opal_string_copy(dest, src, 5);
    test_verify("truncation: dest[4] is NUL", '\0' == dest[4]);
    test_verify("truncation: dest[3] is 'd'", 'd' == dest[3]);
    test_verify("truncation: result is NUL-terminated string of 4 chars",
                0 == strcmp(dest, "abcd"));

    /* --- Truncation: dest_len == 1 -> result is empty string --- */
    memset(dest, 0xFF, sizeof(dest));
    src = "xyz";
    opal_string_copy(dest, src, 1);
    test_verify("len==1: dest[0] is NUL", '\0' == dest[0]);

    /* --- Empty source string --- */
    memset(dest, 0xFF, sizeof(dest));
    src = "";
    opal_string_copy(dest, src, sizeof(dest));
    test_verify("empty src: dest[0] is NUL", '\0' == dest[0]);
    test_verify("empty src: result is empty string", 0 == strcmp(dest, ""));

    /* --- Source length exactly dest_len - 1 (another boundary check) --- */
    memset(dest, 0xFF, sizeof(dest));
    src = "A";              /* 1 char + NUL; dest_len=2 */
    opal_string_copy(dest, src, 2);
    test_verify("len=2 fit: content is 'A'", 'A' == dest[0]);
    test_verify("len=2 fit: NUL at position 1", '\0' == dest[1]);

    /* (The dest_len == 0 case -- previously an out-of-bounds write, now
     * guarded in opal_string_copy() -- is exercised at the end of this
     * file: "Zero-length dst: must write nothing and not crash".) */

    /* --- Large but valid source (within 128K limit) --- */
    {
        char *big_src = (char *) malloc(1024);
        char *big_dest = (char *) malloc(512);
        if (NULL != big_src && NULL != big_dest) {
            int i;
            for (i = 0; i < 1023; ++i) {
                big_src[i] = 'x';
            }
            big_src[1023] = '\0';
            opal_string_copy(big_dest, big_src, 512);
            test_verify("large src truncated: dest[511] is NUL", '\0' == big_dest[511]);
            test_verify("large src truncated: first char is 'x'", 'x' == big_dest[0]);
            free(big_src);
            free(big_dest);
        }
    }

    /* --- Zero-length destination: must write nothing and not crash --- */
    {
        char z[4] = {'a', 'b', 'c', 'd'};
        opal_string_copy(z, "hello", 0);
        test_verify("dest_len==0 leaves buffer untouched", 'a' == z[0]);
    }

    return test_finalize();
}
