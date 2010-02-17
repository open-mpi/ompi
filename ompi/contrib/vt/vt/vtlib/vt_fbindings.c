/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_fbindings.h"
#include <stdlib.h>
#include <string.h>

/*
 * creates a C string from an F77 string
 */
void vt_string_f2c(char* fstr, int len, char** cstr)
{
  char* end;
  int i;

  /* Leading and trailing blanks are discarded. */

  end = fstr + len - 1;

  for (i = 0; (i < len) && (' ' == *fstr); ++i, ++fstr) {
    continue;
  }

  if (i >= len) {
    len = 0;
  } else {
    for (; (end > fstr) && (' ' == *end); --end) {
      continue;
    }

    len = end - fstr + 1;
  }

  /* Allocate space for the C string. */

  if (NULL == (*cstr = malloc(len + 1)))
    return;

  /* Copy F77 string into C string and NULL terminate it. */

  if (len > 0) {
    strncpy(*cstr, fstr, len);
  }
  (*cstr)[len] = '\0';
}

/*
 * creates a F77 string from an C string
 */
void vt_string_c2f(char* cstr, char* fstr, int len)
{
  int i;

  strncpy(fstr, cstr, len);
  for (i = strlen(cstr); i < len; ++i) {
    fstr[i] = ' ';
  }
}
