/*
 * $HEADER$
 */

#ifndef OMPI_ATOMIC_H_INCLUDED
#define OMPI_ATOMIC_H_INCLUDED

#include "ompi_config.h"

/*
 * Atomic functions
 */

#if defined(__alpha)
#  if defined(__GNUC__)
#    include "os/linux/alpha/atomic.h"
#  else
#    include "os/tru64/atomic.h"
#  endif  /* __GNUC__ */
#endif  /* __alpha */

#if defined(__linux__) && defined(__i386)
#include "os/linux/i686/atomic.h"
#endif  /* defined(__linux__) && defined(__i386) */

#if defined(__CYGWIN__)
#include "os/cygwin/atomic.h"
#endif  /* defined(__CYGWIN__) */

#if defined(__ia64)
#include "os/linux/ia64/atomic.h"
#endif  /* defined(__ia64) */

#if defined(__x86_64)
#include "os/linux/x86_64/atomic.h"
#endif  /* defined(__x86_64) */

#if defined(__mips)
#include "os/irix/atomic.h"
#endif  /* defined(__mpis) */

#if defined(__APPLE__)
/* check if PowerPC 970 (G5) */
#if defined(__ppc_64__)
#include "os/darwin/ppc_64/atomic.h"
#else
#include "os/darwin/ppc_32/atomic.h"
#endif  /* defined(__ppc_64__) */

#endif  /* defined(__APPLE__) */

#ifndef mb
#define mb()
#endif

#ifndef rmb
#define rmb()
#endif

#ifndef wmb
#define wmb()
#endif

/*
 * macros
 */

#define ATOMIC_OMPI_LOCK_INIT(OMPI_LOCKPTR)  spinunlock(OMPI_LOCKPTR)
#define ATOMIC_OMPI_LOCK(OMPI_LOCKPTR)       spinlock(OMPI_LOCKPTR)
#define ATOMIC_OMPI_UNLOCK(OMPI_LOCKPTR)     spinunlock(OMPI_LOCKPTR)
#define ATOMIC_TRYOMPI_LOCK(OMPI_LOCKPTR)    spintrylock(OMPI_LOCKPTR)

#endif /* OMPI_ATOMIC_H_INCLUDED */
