/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2014 Inria.  All rights reserved.
 * Copyright © 2009-2012 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/* The configuration file */

#ifndef HWLOC_CONFIG_H
#define HWLOC_CONFIG_H

#define __hwloc_restrict
#define __hwloc_inline __inline

#define __hwloc_attribute_unused
#define __hwloc_attribute_malloc
#define __hwloc_attribute_const
#define __hwloc_attribute_pure
#define __hwloc_attribute_deprecated
#define __hwloc_attribute_may_alias

/* Defined to 1 if you have the `windows.h' header. */
#define HWLOC_HAVE_WINDOWS_H 1
#define hwloc_pid_t HANDLE
#define hwloc_thread_t HANDLE

#include <windows.h>
#include <BaseTsd.h>
typedef DWORDLONG hwloc_uint64_t;
typedef SSIZE_T ssize_t;
#define snprintf _snprintf
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#define strdup _strdup
#define strtoull _strtoui64
#define strtoll _strtoi64
#define S_ISREG(m) ((m)&_S_IFREG)
#define S_ISDIR( m ) (((m) & S_IFMT) == S_IFDIR)
#define putenv _putenv

#if defined( _USRDLL ) /* dynamic linkage */
#if defined( DECLSPEC_EXPORTS )
#define HWLOC_DECLSPEC __declspec(dllexport)
#else
#define HWLOC_DECLSPEC __declspec(dllimport)
#endif
#else /* static linkage */
#define HWLOC_DECLSPEC
#endif

/* Whether we need to re-define all the hwloc public symbols or not */
#define HWLOC_SYM_TRANSFORM 0

/* The hwloc symbol prefix */
#define HWLOC_SYM_PREFIX hwloc_

/* The hwloc symbol prefix in all caps */
#define HWLOC_SYM_PREFIX_CAPS HWLOC_

#endif /* HWLOC_CONFIG_H */
