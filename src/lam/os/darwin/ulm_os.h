/*
 * $HEADER$
 */

/*
 * ulm_os.h  - This file contains DARWIN OS dependent definitions.
 */

#include <unistd.h>

#ifdef __APPLE__

#define RESTRICT_MACRO __restrict__

#ifdef __ppc__
#define PAGESIZE    4096
#define SMPPAGESIZE 4096
#define intwordaligned(a)   ( ( ((long)(a)&3L) == 0L) ? 1 : 0 )
#define MEMCOPY_FUNC bcopy
#define SMPFirstFragPayload 3496
#define SMPSecondFragPayload 8192
#define CACHE_ALIGNMENT 256
// Reference: http://developer.apple.com/hardware/ve/caches.html gives cacheline as 32 bytes

#else
#error
#endif

#else
#error
#endif
