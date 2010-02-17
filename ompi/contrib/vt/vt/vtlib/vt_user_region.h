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
# error "vt_user_region.h should only be included from vt_user.h"
#endif /* _VT_USER_H */

#ifndef _VT_USER_REGION_H
#define _VT_USER_REGION_H

#if (defined(VTRACE)) && !(defined(VTRACE_NO_REGION))

  __VT_EXTERN_DECL void VT_User_start__(const char* name, const char* file, int lno);
  __VT_EXTERN_DECL void VT_User_end__(const char* name);

# ifdef __cplusplus
    class VT_Tracer {
    public:
      VT_Tracer(const char* name, const char* file, int lno) : n(name) {
        VT_User_start__(name, file, lno);
      }
      ~VT_Tracer() { VT_User_end__(n); }
    private:
      const char *n;
    };
#   define VT_TRACER(n) VT_Tracer VT_Trc__(n, __FILE__, __LINE__)
# endif /* __cplusplus */

# define VT_USER_START(n) VT_User_start__(n, __FILE__, __LINE__)
# define VT_USER_END(n)   VT_User_end__(n)

#else /* VTRACE && !VTRACE_NO_REGION */

# define VT_USER_START(n)
# define VT_USER_END(n)
# define VT_TRACER(n)

#endif /* VTRACE && !VTRACE_NO_REGION */

#endif /* _VT_USER_H */
