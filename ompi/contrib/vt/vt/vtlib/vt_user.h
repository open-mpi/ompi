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

#ifndef _VT_USER_H
#define _VT_USER_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include <vt_user_count.h>
#include <vt_user_comment.h>

#ifdef VTRACE

  EXTERN void VT_User_start__(char* name, char* file, int lno);
  EXTERN void VT_User_end__(char* name);

  #ifdef __cplusplus
    class VT_Tracer {
    public:
      VT_Tracer(char* name, char* file, int lno) : n(name) {
        VT_User_start__(name, file, lno);
      }
      ~VT_Tracer() { VT_User_end__(n); }
    private:
      char *n;
    };
    #define VT_TRACER(n) VT_Tracer VT_Trc__(n, __FILE__, __LINE__)
  #endif

  #define VT_USER_START(n) VT_User_start__(n, __FILE__, __LINE__)
  #define VT_USER_END(n)   VT_User_end__(n)

#else

  #define VT_USER_START(n)
  #define VT_USER_END(n)
  #define VT_TRACER(n)

#endif /* VTRACE */

#endif /* _VT_USER_H */
