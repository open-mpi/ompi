/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_USER_H
#define _VT_USER_H

#ifdef __VT_EXTERN_DECL
# error The macro __VT_EXTERN_DECL is used by VampirTrace internally and must not be defined by user code!
#endif

#ifdef __cplusplus
# define __VT_EXTERN_DECL extern "C" 
#else
# define __VT_EXTERN_DECL extern 
#endif

#include "vt_user_control.h"
#include "vt_user_comment.h"
#include "vt_user_count.h"
#include "vt_user_marker.h"
#include "vt_user_region.h"

#ifdef VTRACE_PTHREAD
# include "vt_wrap_pthread.h"
#endif /* VTRACE_PTHREAD */

#endif /* _VT_USER_H */
