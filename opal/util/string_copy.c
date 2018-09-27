/*
 * Copyright (c) 2018 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/util/string_copy.h"


void opal_string_copy(char *dest, const char *src, size_t len)
{
    size_t i;
    char *new_dest = dest;

    for (i = 0; i < len; ++i, ++src, ++new_dest) {
        *new_dest = *src;
        if ('\0' == *src) {
            return;
        }
    }

    dest[i - 1] = '\0';
}
