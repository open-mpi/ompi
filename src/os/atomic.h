/*
 * $HEADER$
 */

#ifndef LAM_ATOMIC_H_INCLUDED
#define LAM_ATOMIC_H_INCLUDED

#include "lam_config.h"

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

#define ATOMIC_LOCK_INIT(LOCKPTR)  spinunlock(LOCKPTR)
#define ATOMIC_LOCK(LOCKPTR)       spinlock(LOCKPTR)
#define ATOMIC_UNLOCK(LOCKPTR)     spinunlock(LOCKPTR)
#define ATOMIC_TRYLOCK(LOCKPTR)    spintrylock(LOCKPTR)

#endif /* LAM_ATOMIC_H_INCLUDED */
