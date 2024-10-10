/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"

/*
 * Below are the "safe" versions of the various string and printf
 * operations. They are directly taken from MPICH, with MPIU replaced by ADIOI.
 */

/*
 * ADIOI_Strncpy - Copy at most n character.  Stop once a null is reached.
 *
 * This is different from strncpy, which null pads so that exactly
 * n characters are copied.  The strncpy behavior is correct for many
 * applications because it guarantees that the string has no uninitialized
 * data.
 *
 * If n characters are copied without reaching a null, return an error.
 * Otherwise, return 0.
 *
 */
/*@ ADIOI_Strncpy - Copy a string with a maximum length

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

  Module:
  Utility
  @*/
int ADIOI_Strncpy(char *dest, const char *src, size_t n)
{
    char *restrict d_ptr = dest;
    const char *restrict s_ptr = src;
    register int i;

    i = (int) n;
    while (*s_ptr && i-- > 0) {
        *d_ptr++ = *s_ptr++;
    }

    if (i > 0) {
        *d_ptr = 0;
        return 0;
    } else
        /* We may want to force an error message here, at least in the
         * debugging version */
        return 1;
}

/*@
  ADIOI_Strdup - Duplicate a string

  Synopsis:
.vb
    char *ADIOI_Strdup(const char *str)
.ve

Input Parameters:
. str - null-terminated string to duplicate

  Return value:
  A pointer to a copy of the string, including the terminating null.  A
  null pointer is returned on error, such as out-of-memory.

  Notes:
  Like 'ADIOI_Malloc' and 'ADIOI_Free', this will often be implemented as a
  macro but may use 'ADIOI_trstrdup' to provide a tracing version.

  Module:
  Utility
  @*/
char *ADIOI_Strdup(const char *str)
{
    char *p = ADIOI_Malloc(strlen(str) + 1);
    char *in_p = (char *) str;
    char *save_p;

    save_p = p;
    if (p) {
        while (*in_p) {
            *p++ = *in_p++;
        }
        *p = '\0';
    }
    return save_p;
}
