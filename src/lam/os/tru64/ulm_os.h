/*
 * $HEADER$
 */

// ulm_os.h  - This file contains IRIX OS dependent definitions.

// memory page size

#define LOG2PAGESIZE 13
#define SMPPAGESIZE (1 << LOG2PAGESIZE)

#ifndef PAGESIZE
#define PAGESIZE 8192
#endif

#define CACHE_ALIGNMENT 64

// if cxx is invoked with -model ansi -accept restrict_keyword
#ifdef __MODEL_ANSI
#define RESTRICT_MACRO	__restrict
#else
#define RESTRICT_MACRO
#endif

#define intwordaligned(a)   ( ( ((long long)(a)&3L) == 0L) ? 1 : 0 )

#define MEMCOPY_FUNC bcopy
#define SMPSecondFragPayload 65536
#define SMPFirstFragPayload  3496

