/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"
#include <assert.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

/*@
  MPL_strdup - Duplicate a string

  Synopsis:
.vb
    char *MPL_strdup(const char *str)
.ve

Input Parameters:
. str - null-terminated string to duplicate

  Return value:
  A pointer to a copy of the string, including the terminating null.  A
  null pointer is returned on error, such as out-of-memory.

  Module:
  Utility
  @*/
#if !defined MPL_HAVE_STRDUP
char *MPL_strdup(const char *str)
{
    char *restrict p = (char *) malloc(strlen(str) + 1);
    const char *restrict in_p = str;
    char *save_p;

    save_p = p;
    if (p) {
        while (*in_p) {
            *p++ = *in_p++;
        }
        *p = 0;
    }
    return save_p;
}
#endif /* MPL_HAVE_STRDUP */

/*
 * MPL_strncpy - Copy at most n characters.  Stop once a null is reached.
 *
 * This is different from strncpy, which null pads so that exactly
 * n characters are copied.  The strncpy behavior is correct for many
 * applications because it guarantees that the string has no uninitialized
 * data.
 *
 * If n characters are copied without reaching a null, return an error.
 * Otherwise, return 0.
 *
 * Question: should we provide a way to request the length of the string,
 * since we know it?
 */
/*@ MPL_strncpy - Copy a string with a maximum length

Input Parameters:
+   instr - String to copy
-   maxlen - Maximum total length of 'outstr'

Output Parameters:
.   outstr - String to copy into

    Notes:
    This routine is the routine that you wish 'strncpy' was.  In copying
    'instr' to 'outstr', it stops when either the end of 'outstr' (the
    null character) is seen or the maximum length 'maxlen' is reached.
    Unlike 'strncpy', it does not add enough nulls to 'outstr' after
    copying 'instr' in order to move precisely 'maxlen' characters.
    Thus, this routine may be used anywhere 'strcpy' is used, without any
    performance cost related to large values of 'maxlen'.

    If there is insufficient space in the destination, the destination is
    still null-terminated, to avoid potential failures in routines that neglect
    to check the error code return from this routine.

  Module:
  Utility
  @*/
int MPL_strncpy(char *dest, const char *src, size_t n)
{
    char *restrict d_ptr = dest;
    const char *restrict s_ptr = src;
    register int i;

    if (n == 0)
        return 0;

    i = (int) n;
    while (*s_ptr && i-- > 0) {
        *d_ptr++ = *s_ptr++;
    }

    if (i > 0) {
        *d_ptr = 0;
        return 0;
    } else {
        /* Force a null at the end of the string (gives better safety
         * in case the user fails to check the error code) */
        dest[n - 1] = 0;
        /* We may want to force an error message here, at least in the
         * debugging version */
        /*printf("failure in copying %s with length %d\n", src, n); */
        return 1;
    }
}

/* replacement for strsep.  Conforms to the following description (from the OS X
 * 10.6 man page):
 *
 *   The strsep() function locates, in the string referenced by *stringp, the first occur-
 *   rence of any character in the string delim (or the terminating `\0' character) and
 *   replaces it with a `\0'.  The location of the next character after the delimiter
 *   character (or NULL, if the end of the string was reached) is stored in *stringp.  The
 *   original value of *stringp is returned.
 *
 *   An ``empty'' field (i.e., a character in the string delim occurs as the first charac-
 *   ter of *stringp) can be detected by comparing the location referenced by the returned
 *   pointer to `\0'.
 *
 *   If *stringp is initially NULL, strsep() returns NULL.
 */
char *MPL_strsep(char **stringp, const char *delim)
{
    int i, j;
    char *ret;

    if (!*stringp)
        return NULL;

    ret = *stringp;
    i = 0;
    while (1) {
        if (!ret[i]) {
            *stringp = NULL;
            return ret;
        }
        for (j = 0; delim[j] != '\0'; ++j) {
            if (ret[i] == delim[j]) {
                ret[i] = '\0';
                *stringp = &ret[i + 1];
                return ret;
            }
        }
        ++i;
    }
}


/* there's no standard portable way to convert error codes to human readable
 * strings.  The standard way to do that is via strerror() but if for some
 * resason we don't have it, then we'll merely output the error code seen */
#if !defined MPL_HAVE_STRERROR
char *MPL_strerror(int errnum)
{
#define STRERROR_SIZE 256
    static char msgbuf[STRERROR_SIZE];
    snprintf(msgbuf, STRERROR_SIZE, "errno = %d", errnum);
#undef STRERROR_SIZE
    return msgbuf;
}
#endif /* MPL_HAVE_STRERROR */

/*@ MPL_strnapp - Append to a string with a maximum length

Input Parameters:
+   instr - String to copy
-   maxlen - Maximum total length of 'outstr'

Output Parameters:
.   outstr - String to copy into

    Notes:
    This routine is similar to 'strncat' except that the 'maxlen' argument
    is the maximum total length of 'outstr', rather than the maximum
    number of characters to move from 'instr'.  Thus, this routine is
    easier to use when the declared size of 'instr' is known.

  Module:
  Utility
  @*/
int MPL_strnapp(char *dest, const char *src, size_t n)
{
    char *restrict d_ptr = dest;
    const char *restrict s_ptr = src;
    register int i;

    /* Get to the end of dest */
    i = (int) n;
    while (i-- > 0 && *d_ptr)
        d_ptr++;
    if (i <= 0)
        return 1;

    /* Append.  d_ptr points at first null and i is remaining space. */
    while (*s_ptr && i-- > 0) {
        *d_ptr++ = *s_ptr++;
    }

    /* We allow i >= (not just >) here because the first while decrements
     * i by one more than there are characters, leaving room for the null */
    if (i >= 0) {
        *d_ptr = 0;
        return 0;
    } else {
        /* Force the null at the end */
        *--d_ptr = 0;

        /* We may want to force an error message here, at least in the
         * debugging version */
        return 1;
    }
}

static unsigned int xorshift_rand(void)
{
    /* time returns long; keep the lower and most significant 32 bits */
    unsigned int val = time(NULL) & 0xffffffff;

    /* Marsaglia's xorshift random number generator */
    val ^= val << 13;
    val ^= val >> 17;
    val ^= val << 5;

    return val;
}

/*@ MPL_create_pathname - Generate a random pathname

Input Parameters:
+   dirname - String containing the path of the parent dir (current dir if NULL)
+   prefix - String containing the prefix of the generated name
-   is_dir - Boolean to tell if the path should be treated as a directory

Output Parameters:
.   dest_filename - String to copy the generated path name

    Notes:
    dest_filename should point to a preallocated buffer of PATH_MAX size.

  Module:
  Utility
  @*/
void MPL_create_pathname(char *dest_filename, const char *dirname,
                         const char *prefix, const int is_dir)
{
    /* Generate a random number which doesn't interfere with user application */
    const unsigned int rdm = xorshift_rand();
    const unsigned int pid = (unsigned int) getpid();

    if (dirname) {
        snprintf(dest_filename, PATH_MAX, "%s/%s.%u.%u%c", dirname, prefix,
                 rdm, pid, is_dir ? '/' : '\0');
    } else {
        snprintf(dest_filename, PATH_MAX, "%s.%u.%u%c", prefix, rdm, pid, is_dir ? '/' : '\0');
    }
}

/*@ MPL_stricmp - Case insensitive string comparison

Arguments:
s1, s2  - The strings to compare

Return:
  0 - s1 is equal to s2
  <0 - s1 is less than s2
  >0 - s1 is greater than s2

Module:
  Utility
@*/
int MPL_stricmp(const char *s1, const char *s2)
{
    while (*s1 && *s2) {
        if (toupper(*s1) < toupper(*s2)) {
            return -1;
        } else if (toupper(*s1) > toupper(*s2)) {
            return 1;
        }
        s1++;
        s2++;
    }
    if (*s1 == *s2) {
        /* both '\0' */
        return 0;
    } else if (*s2) {
        return -1;
    } else {
        return 1;
    }
}

/*@ MPL_strjoin - Join an array of strings with a separator

+ strs - Array of strings
. num  - Number of strings in the array
- sep  - Character separator

Return:
  Joined string.

Module:
  Utility
@*/
char *MPL_strjoin(char *strs[], int num, char sep)
{
    if (num <= 0) {
        return MPL_strdup("");
    }

    int len = 0;
    for (int i = 0; i < num; i++) {
        len += strlen(strs[i]);
    }

    char *outstr = MPL_malloc(len + num, MPL_MEM_OTHER);
    char *p = outstr;
    for (int i = 0; i < num; i++) {
        strcpy(p, strs[i]);
        if (i < num - 1) {
            p += strlen(strs[i]);
            *p++ = sep;
        }
    }

    return outstr;
}
