/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"
#include <assert.h>


#if defined (MPL_HAVE_SYS_SYSINFO_H)
#include <sys/sysinfo.h>
#endif

#if defined (MPL_HAVE_UNISTD_H)
#include <unistd.h>
#endif

int MPL_get_nprocs(void)
{
#if defined (MPL_HAVE_GET_NPROCS)
    return get_nprocs();
#elif defined (MPL_HAVE_DECL__SC_NPROCESSORS_ONLN) && MPL_HAVE_DECL__SC_NPROCESSORS_ONLN
    int count = sysconf(_SC_NPROCESSORS_ONLN);
    return (count > 0) ? count : 1;
#else
    /* Neither MPL_HAVE_GET_NPROCS nor MPL_HAVE_DECL__SC_NPROCESSORS_ONLN are defined.
     * Should not reach here. */
    assert(0);
    return 1;
#endif
}

/* Simple hex encoding binary as hexadecimal string. For example,
 * a binary with 4 bytes, 0x12, 0x34, 0x56, 0x78, will be encoded
 * as ascii string "12345678". The encoded string will have string
 * length of exactly double the binary size plus a terminating "NUL".
 */

static int hex(unsigned char c)
{
    if (c >= '0' && c <= '9') {
        return c - '0';
    } else if (c >= 'a' && c <= 'f') {
        return 10 + c - 'a';
    } else if (c >= 'A' && c <= 'F') {
        return 10 + c - 'A';
    } else {
        assert(0);
        return -1;
    }
}

/* encodes src data into hex characters */
int MPL_hex_encode(int size, const void *src, char *dest)
{
    const char *srcp = src;

    for (int i = 0; i < size; i++) {
        snprintf(dest, 3, "%02X", (unsigned char) *srcp);
        srcp++;
        dest += 2;
    }

    return 0;
}

/* decodes hex encoded string into original src data */
int MPL_hex_decode(int size, const char *src, void *dest)
{
    int n = strlen(src);
    if (n != size * 2) {
        return 1;
    }

    char *destp = dest;

    for (int i = 0; i < size; i++) {
        if (hex(src[0]) < 0 || hex(src[1]) < 0) {
            return 1;
        }
        *destp = (char) (hex(src[0]) << 4) + hex(src[1]);
        src += 2;
        destp++;
    }

    return 0;
}
