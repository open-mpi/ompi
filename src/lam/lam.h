/*
 * $HEADER$
 */

#ifndef LAM_H
#define LAM_H

#include <sys/types.h>
#include <sys/socket.h>

#include "lam_config.h"
#include "lam/stdint.h"
#include "lam/types.h"
#include "lam/constants.h"

#if LAM_ENABLE_MEM_ZERO
#include <string.h>
#define LAM_MEM_ZERO(foo) memset(&(foo), 0, sizeof(foo))
#else
#define LAM_MEM_ZERO(foo)
#endif

#endif /* LAM_H */
