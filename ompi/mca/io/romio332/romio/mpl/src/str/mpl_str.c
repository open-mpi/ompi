/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
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

#if !defined MPL_HAVE_SNPRINTF
int MPL_snprintf(char *str, size_t size, mpl_const char *format, ...)
{
    int n;
    mpl_const char *p;
    char *out_str = str;
    va_list list;

    va_start(list, format);

    p = format;
    while (*p && size > 0) {
        char *nf;

        nf = strchr(p, '%');
        if (!nf) {
            /* No more format characters */
            while (size-- > 0 && *p) {
                *out_str++ = *p++;
            }
        } else {
            int nc;
            int width = -1;

            /* Copy until nf */
            while (p < nf && size-- > 0) {
                *out_str++ = *p++;
            }
            /* p now points at nf */
            /* Handle the format character */
            nc = nf[1];
            if (isdigit(nc)) {
                /* Get the field width */
                /* FIXME : Assumes ASCII */
                width = nc - '0';
                p = nf + 2;
                while (*p && isdigit(*p)) {
                    width = 10 * width + (*p++ - '0');
                }
                /* When there is no longer a digit, get the format
                 * character */
                nc = *p++;
            } else {
                /* Skip over the format string */
                p += 2;
            }

            switch (nc) {
                case '%':
                    *out_str++ = '%';
                    size--;
                    break;

                case 'd':
                    {
                        int val;
                        char tmp[20];
                        char *t = tmp;
                        /* Get the argument, of integer type */
                        val = va_arg(list, int);
                        sprintf(tmp, "%d", val);
                        if (width > 0) {
                            size_t tmplen = strlen(tmp);
                            /* If a width was specified, pad with spaces on the
                             * left (on the right if %-3d given; not implemented yet */
                            while (size-- > 0 && width-- > tmplen)
                                *out_str++ = ' ';
                        }
                        while (size-- > 0 && *t) {
                            *out_str++ = *t++;
                        }
                    }
                    break;

                case 'x':
                    {
                        int val;
                        char tmp[20];
                        char *t = tmp;
                        /* Get the argument, of integer type */
                        val = va_arg(list, int);
                        sprintf(tmp, "%x", val);
                        if (width > 0) {
                            size_t tmplen = strlen(tmp);
                            /* If a width was specified, pad with spaces on the
                             * left (on the right if %-3d given; not implemented yet */
                            while (size-- > 0 && width-- > tmplen)
                                *out_str++ = ' ';
                        }
                        while (size-- > 0 && *t) {
                            *out_str++ = *t++;
                        }
                    }
                    break;

                case 'p':
                    {
                        void *val;
                        char tmp[20];
                        char *t = tmp;
                        /* Get the argument, of pointer type */
                        val = va_arg(list, void *);
                        sprintf(tmp, "%p", val);
                        if (width > 0) {
                            size_t tmplen = strlen(tmp);
                            /* If a width was specified, pad with spaces on the
                             * left (on the right if %-3d given; not implemented yet */
                            while (size-- > 0 && width-- > tmplen)
                                *out_str++ = ' ';
                        }
                        while (size-- > 0 && *t) {
                            *out_str++ = *t++;
                        }
                    }
                    break;

                case 's':
                    {
                        char *s_arg;
                        /* Get the argument, of pointer to char type */
                        s_arg = va_arg(list, char *);
                        while (size-- > 0 && s_arg && *s_arg) {
                            *out_str++ = *s_arg++;
                        }
                    }
                    break;

                default:
                    /* Error, unknown case */
                    return -1;
                    break;
            }
        }
    }

    va_end(list);

    if (size-- > 0)
        *out_str++ = '\0';

    n = (int) (out_str - str);
    return n;
}
#endif /* MPL_HAVE_SNPRINTF */

/*@
  MPL_strdup - Duplicate a string

  Synopsis:
.vb
    char *MPL_strdup(mpl_const char *str)
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
char *MPL_strdup(mpl_const char *str)
{
    char *mpl_restrict p = (char *) malloc(strlen(str) + 1);
    mpl_const char *mpl_restrict in_p = str;
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
    char *mpl_restrict d_ptr = dest;
    const char *mpl_restrict s_ptr = src;
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
    char *mpl_restrict d_ptr = dest;
    const char *mpl_restrict s_ptr = src;
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
    const unsigned int random = xorshift_rand();
    const unsigned int pid = (unsigned int) getpid();

    if (dirname) {
        MPL_snprintf(dest_filename, PATH_MAX, "%s/%s.%u.%u%c", dirname, prefix,
                     random, pid, is_dir ? '/' : '\0');
    } else {
        MPL_snprintf(dest_filename, PATH_MAX, "%s.%u.%u%c", prefix,
                     random, pid, is_dir ? '/' : '\0');
    }
}
