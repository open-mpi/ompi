/*
 * $HEADER$
 */

/*
 * ulm_os.h  - This file contains CYGWIN OS dependent definitions.
 */

#include <unistd.h>

#ifdef __CYGWIN__

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
#error
#endif

#else
#error
#endif
