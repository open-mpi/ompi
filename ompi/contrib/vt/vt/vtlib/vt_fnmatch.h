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

#ifndef _VT_FNMATCH_H
#define _VT_FNMATCH_H

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#if defined(HAVE_FNMATCH_H) && HAVE_FNMATCH_H
#  include <fnmatch.h>
#  define vt_fnmatch fnmatch
#else /* HAVE_FNMATCH_H */
#  include <string.h>
#  define FNM_NOESCAPE (1 << 1) /* Backslashes don't quote special chars. */
#  define vt_fnmatch(_pattern, _string, __flags) \
      strcmp(_string, _pattern)
#endif /* HAVE_FNMATCH_H */

#endif /* _VT_FNMATCH_H */
