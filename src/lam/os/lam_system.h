/*
 * $HEADER$
 */

#ifndef LAM_SYSTEM_H
#define LAM_SYSTEM_H

/* conditionally include system dependent stuff here */

#if defined (__mips)

#include "lam/os/irix/ulm_os.h"

#elif defined (__linux__)

#include "lam/os/linux/ulm_os.h"

#elif defined (__osf__)

#include "lam/os/tru64/ulm_os.h"

#elif defined (__APPLE__)

#include "lam/os/darwin/ulm_os.h"

#elif defined (__CYGWIN__)

#include "lam/os/cygwin/ulm_os.h"

#else
# error
#endif


#endif  /* LAM_SYSTEM_H */
