/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009 Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * Read /proc/self/maps to find the path to the file containing
 * an address.
 */

#include "opal_config.h"
#include "opal_stdint.h"

#include "opal/constants.h"

#include <stdio.h>
#include <fcntl.h>
#include <string.h>

uintptr_t read_addr(FILE *);
int skip_fields(FILE *, int);
int read_path(FILE *);
int skip_line(FILE *);

const char *
opal_installdirs_autodetect_path(uintptr_t my_addr)
{
    FILE *f;
    uintptr_t lo_start, lo_end;

    f = fopen("/proc/self/maps", "r");
    if (NULL == f) {
        return NULL;
    }

    for (;;) {
        lo_start = read_addr(f);
        if (0 == lo_start) {
            fclose(f);
            return NULL;
        }
        lo_end = read_addr(f);
        if (0 == lo_end) {
            fclose(f);
            return NULL;
        }
        if (lo_start <= my_addr && lo_end > my_addr) {
            const char *path, *my_dir;
            int e = skip_fields(f, 4);
            if (OPAL_SUCCESS != e) {
                fclose(f);
                return e;
            }
            path = read_path(f);
            fclose(f);
            return path;
        } else {
            int e = skip_line(f);
            if (OPAL_SUCCESS != e) {
                fclose(f);
                return NULL;
            }
        }
    }
}

/*
 * The following routines don't use isspace et al, because we don't
 * want the locale setting to influence them.
 */

/*
 * Read a hex address from a file.  We don't know how many
 * digits there will be.  We only know that the result will
 * fit into a uintptr_t, and will not be zero.
 *
 * Return zero on error.
 */

static uintptr_t
read_addr(FILE *f)
{
    int c;
    uintptr_t n = 0;

    for (;;) {
        c = getc(f);
        if (EOF == c) {
            return 0;
        }
        if (' ' == c || '-' == c) {
            if (n > 0) {
                return n;
            } else {
                continue;
            }
        }
        if (c >= '0' && c <= '9') {
            n <<= 4;
            n += c - '0';
        } else if (c >= 'a' && c <= 'f') {
            n <<= 4;
            n += c - 'a' + 10;
        } else if (c >= 'A' && c <= 'F') {
            n <<= 4;
            n += c - 'A' + 10;
        } else {
            return n;
        }
    }
}

/*
 * Skip n fields of input, where fields are separated by white space.
 *
 * Return OPAL_SUCCESS on success.
 */

static int
skip_fields(FILE *f, int n)
{
    int c = getc(f);
    while (n-- > 0) {
        for (; c == ' ' || c == '\t'; c = getc(f));
        if (c == EOF || c == '\n') {
            return OPAL_ERR_NOT_AVAILABLE;
        }
        for(; c != ' ' && c != '\t' && c != '\n'; c = getc(f));
    }
    return OPAL_SUCCESS;
}

/*
 * Read a path, and return it.  Return NULL on error.
 */
 
static int
read_path(FILE *f)
{
    char *path = malloc(100);
    int n = 100;
    int i = 0;
    int c = getc(f);

    for (; (c == ' ' || c == '\t') && c != '\n' && c != EOF; c = getc(f));
    for (; c != '\n' && EOF != c; c = getc(f)) {
        if (i >= n) {
            n += 100;
            path = realloc(path, n);
        }
        if (NULL == path) {
            return NULL;
        }
        path[i++] = c;
    }
    if (i >= n) {
        n++;
        path = realloc(path, n);
    } else {
        path = realloc(path, i + 1);
    }
    if (NULL == path) {
        return NULL;
    }
    path[i] = '\0';
    return path;
}

/*
 * Skip to the next line of input.  Return OPAL_SUCCESS on success.
 */

static int
skip_line(FILE *f)
{
    int c = 0;
    while (c != '\n') {
        c = getc(f);
        if (EOF == c) {
            return OPAL_ERR_NOT_AVAILABLE;
        }
    }
    return OPAL_SUCCESS;
}
