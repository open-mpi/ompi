/*
 * $HEADER$
 *
 * This file is only here because some platforms have a broken strncpy
 * (e.g., Itanium with RedHat Advanced Server glibc).
 */

#include <lam_config.h>

#include <string.h>

#include "lam/util/strncpy.h"


/**
 * Provide a portable, working strncpy() for platforms that have
 * broken implementations.
 *
 * @param dest Destination string.
 * @param src Source string.
 * @param len Length of the string to copy.
 *
 * @return The value dest.
 *
 * This function is identical in behavior to strncpy(), but is not
 * optimized at all (we're not concerned with high-performance
 * strncpy!).  It is only here because some platforms have broken
 * implementations of strncpy() that cause segfaults (cough cough Red
 * Hat Advanced Server glibc cough cough).
 */
char *
lam_strncpy(char *dest, const char *src, size_t len)
{
  int i;
  int pad = 0;
  char *new_dest = dest;

  for (i = 0; i < len; ++i, ++src, ++new_dest) {
    if (pad != 0)
      *new_dest = '\0';
    else {
      *new_dest = *src;
      if ('\0' == *src)
        pad = 1;
    }
  }

  return dest;
}
