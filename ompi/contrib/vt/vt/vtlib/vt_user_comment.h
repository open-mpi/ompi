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

#ifndef _VT_USER_COMMENT_H
#define _VT_USER_COMMENT_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#if (defined(VTRACE)) && !(defined(VTRACE_NO_COMMENT))

  EXTERN void VT_User_comment_def__(const char* comment);
  EXTERN void VT_User_comment__(const char* comment);

  #define VT_COMMENT_DEF(c) VT_User_comment_def__(c)
  #define VT_COMMENT(c) VT_User_comment__(c)

#else

  #define VT_COMMENT_DEF(c)
  #define VT_COMMENT(c)

#endif /* VTRACE */

#endif /* _VT_USER_COMMENT_H */
