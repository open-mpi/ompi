/*
 * $HEADER$
 */

/*
 * ulm_os.h  - This file contains LINUX OS dependent definitions.
 */

#include <unistd.h>

#ifdef __linux__

#define RESTRICT_MACRO __restrict__

#ifdef __i386
#define PAGESIZE    4096
#define SMPPAGESIZE 4096
#define intwordaligned(a)   ( ( ((long)(a)&3L) == 0L) ? 1 : 0 )
#define MEMCOPY_FUNC bcopy
#define SMPFirstFragPayload 3496
#define SMPSecondFragPayload 8192
#define CACHE_ALIGNMENT 128

#else
#ifdef __ia64
#define PAGESIZE 16384
#define SMPPAGESIZE 16384
#define CACHE_ALIGNMENT 128
#define intwordaligned(a)   ( ( ((long)(a)&3L) == 0L) ? 1 : 0 )

#else
#ifdef __alpha
#define PAGESIZE 8192
#define SMPPAGESIZE 8192
#define CACHE_ALIGNMENT 128
#define intwordaligned(a)   ( ( ((long long)(a)&3L) == 0L) ? 1 : 0 )

#else
#error
#endif
#endif
#endif

#else
#error
#endif
