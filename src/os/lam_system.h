/*
 * $HEADER$
 */

#ifndef OMPI_SYSTEM_H
#define OMPI_SYSTEM_H

/* conditionally include system dependent stuff here */

#if defined (__mips)

#include "os/irix/ulm_os.h"

#elif defined (__linux__)

#include "os/linux/ulm_os.h"

#elif defined (__osf__)

#include "os/tru64/ulm_os.h"

#elif defined (__APPLE__)

#include "os/darwin/ulm_os.h"

#elif defined (__CYGWIN__)

#include "os/cygwin/ulm_os.h"

#else
# error
#endif


#endif  /* OMPI_SYSTEM_H */
