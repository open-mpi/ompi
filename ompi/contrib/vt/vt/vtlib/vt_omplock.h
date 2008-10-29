/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_OMPLOCK_H
#define _VT_OMPLOCK_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_defs.h"

EXTERN uint32_t vt_lock_init(void* lock);
EXTERN uint32_t vt_lock_id(void* lock);
EXTERN void     vt_lock_destroy(void* lock);
EXTERN void     vt_lock_close(void);

#endif










