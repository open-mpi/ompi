/*
 * $HEADER$
 */

/*
 * ulm_os.h  - This file contains IRIX OS dependent definitions.
 */

#ifndef IRIX_ULM_OS
#define IRIX_ULM_OS

#include <strings.h>  /* for bcopy */

/* memory page size */

#define LOG2PAGESIZE 14
#define SMPPAGESIZE (1 << LOG2PAGESIZE)

#define RESTRICT_MACRO restrict
#define intwordaligned(a)   ( ( ((long long)(a)&3L) == 0L) ? 1 : 0 )

#define PAGESIZE 16384
#define CACHE_ALIGNMENT 128

/* memory copy function */
#define MEMCOPY_FUNC bcopy

#endif /* IRIX_ULM_OS */
