/*
 * $HEADER$
 */
#ifndef ULM_OS_H_HAS_BEEN_INCLUDED
#define ULM_OS_H_HAS_BEEN_INCLUDED

/*
 * ulm_os.h  - This file contains LINUX OS dependent definitions.
 */

#include <unistd.h>

#if defined(__linux__)

#define RESTRICT_MACRO __restrict__

#if defined(__i386)
#define PAGESIZE    4096
#define SMPPAGESIZE 4096
#define intwordaligned(a)   ( ( ((long)(a)&3L) == 0L) ? 1 : 0 )
#define MEMCOPY_FUNC bcopy
#define SMPFirstFragPayload 3496
#define SMPSecondFragPayload 8192
#define CACHE_ALIGNMENT 128

#else
#if defined(__ia64)
#define PAGESIZE 16384
#define SMPPAGESIZE 16384
#define CACHE_ALIGNMENT 128
#define intwordaligned(a)   ( ( ((long)(a)&3L) == 0L) ? 1 : 0 )

#else
#if defined(__alpha)
#define PAGESIZE 8192
#define SMPPAGESIZE 8192
#define CACHE_ALIGNMENT 128
#define intwordaligned(a)   ( ( ((long long)(a)&3L) == 0L) ? 1 : 0 )

#else
#if defined(__x86_64)
#define PAGESIZE    4096
#define SMPPAGESIZE 4096
#define intwordaligned(a)   ( ( ((long)(a)&3L) == 0L) ? 1 : 0 )
#define SMPFirstFragPayload 3496
#define SMPSecondFragPayload 8192
#define CACHE_ALIGNMENT 128
#else  /* !__x86_64 */
#error
#endif  /* __x86_64 */
#endif  /* __alpha */
#endif  /* __ia64 */
#endif  /* __i386 */

#else  /* ! __linux__ */
#error
#endif  /* __linux__ */

#endif  /* ULM_OS_H_HAS_BEEN_INCLUDED */
