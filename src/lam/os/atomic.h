/*
 * $HEADER$
 */

#ifndef LAM_ATOMIC_H_INCLUDED
#define LAM_ATOMIC_H_INCLUDED

#include "lam_config.h"

/*
 * Atomic functions
 */

#ifdef __alpha
#  ifdef __GNUC__
#    include "lam/os/linux/alpha/atomic.h"
#  else
#    include "lam/os/tru64/atomic.h"
#  endif
#endif

#if defined (__linux__) && defined (__i386)
#include "lam/os/linux/i686/atomic.h"
#endif

#ifdef __CYGWIN__
#include "lam/os/cygwin/atomic.h"
#endif

#ifdef __ia64
#include "lam/os/linux/ia64/atomic.h"
#endif

#ifdef __mips
#include "lam/os/irix/atomic.h"
#endif

#ifdef __APPLE__
/* check if PowerPC 970 (G5) */
#ifdef __ppc_64__
#include "lam/os/darwin/ppc_64/atomic.h"
#else
#include "lam/os/darwin/ppc_32/atomic.h"
#endif

#endif      /* __APPLE__ */

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
