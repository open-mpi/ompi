/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <private/misc.h>

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>

int hwloc_snprintf(char *str, size_t size, const char *format, ...)
{
  int ret;
  va_list ap;
  static char bin;

  /* Some systems crash on str == NULL */
  if (!size) {
    str = &bin;
    size = 1;
  }

  va_start(ap, format);
  ret = vsnprintf(str, size, format, ap);
  va_end(ap);

  if (ret >= 0 && (size_t) ret != size-1)
    return ret;

  /* vsnprintf returned size-1 or -1. That could be a system which reports the
   * written data and not the actually required room. Try increasing buffer
   * size to get the latter. */

  do {
    size *= 2;
    str = malloc(size);
    if (NULL == str)
      return -1;
    va_start(ap, format);
    errno = 0;
    ret = vsnprintf(str, size, format, ap);
    va_end(ap);
    free(str);
  } while ((size_t) ret == size-1 || (ret < 0 && !errno));

  return ret;
}

int hwloc_namecoloncmp(const char *haystack, const char *needle, size_t n)
{
  size_t i = 0;
  while (*haystack && *haystack != ':') {
    int ha = *haystack++;
    int low_h = tolower(ha);
    int ne = *needle++;
    int low_n = tolower(ne);
    if (low_h != low_n)
      return 1;
    i++;
  }
  return i < n;
}
