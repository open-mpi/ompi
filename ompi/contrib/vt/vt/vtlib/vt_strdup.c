/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich GmbH, Federal
 * Republic of Germany
 *
 * See the file COPYRIGHT in the package base directory for details
 **/

#include "vt_strdup.h"

#include <stdlib.h>
#include <string.h>

char *vt_strdup(const char *s)
{
  char *c;

  if( s == NULL || ( c = (char*)malloc( strlen(s)+1 ) ) == NULL )
     return NULL;

  strcpy( c, s );

  return c;
}
