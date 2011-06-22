/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
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

__VT_EXTERN_DECL void VT_User_start__(const char* name, const char* file,
                                      int lno);
__VT_EXTERN_DECL void VT_User_end__(const char* name);
__VT_EXTERN_DECL unsigned int VT_User_def__(const char* name, const char* file,
                                            int lno);
__VT_EXTERN_DECL void VT_User_start_id__(unsigned int rid);
__VT_EXTERN_DECL void VT_User_end_id__(unsigned int rid);

#ifdef __cplusplus
  char vt_tracer_spec__(unsigned int);
  int vt_tracer_spec__(const char*);

  template<int> struct VT_Tracer {
    VT_Tracer(const char* r, const char* f, int l) __VT_NOINST_ATTR;
    ~VT_Tracer() __VT_NOINST_ATTR;
    const char* n;
  };
  template<> VT_Tracer<sizeof(int)>::VT_Tracer(const char* r, const char* f,
    int l) : n(r) { VT_User_start__(n, f, l); }
  template<> VT_Tracer<sizeof(int)>::~VT_Tracer() { VT_User_end__(n); }

  template<> struct VT_Tracer<1> {
    VT_Tracer(unsigned int r, const char* f = 0, int l = 0) __VT_NOINST_ATTR;
    ~VT_Tracer() __VT_NOINST_ATTR;
    unsigned int i;
  };
  VT_Tracer<1>::VT_Tracer(unsigned int r, const char* f, int l)
    : i(r) { VT_User_start_id__(i); }
  VT_Tracer<1>::~VT_Tracer() { VT_User_end_id__(i); }
#endif /* __cplusplus */

#if (defined(VTRACE)) && !(defined(VTRACE_NO_REGION))

# define VT_USER_START(n) VT_User_start__((n), __FILE__, __LINE__)
# define VT_USER_END(n) VT_User_end__((n))
# define VT_USER_DEF(n) VT_User_def__((n), 0, 0)
# define VT_USER_START_ID(i) VT_User_start_id__((i))
# define VT_USER_END_ID(i) VT_User_end_id__((i))
# ifdef __cplusplus
#   define VT_TRACER(r) VT_Tracer<sizeof(vt_tracer_spec__(r))> \
      vt_tracer__((r), __FILE__, __LINE__);
# endif /* __cplusplus */

#else /* VTRACE && !VTRACE_NO_REGION */

# define VT_USER_START(n)
# define VT_USER_END(n)
# define VT_USER_DEF(n) 0
# define VT_USER_START_ID(i)
# define VT_USER_END_ID(i)
# define VT_TRACER(r)

#endif /* VTRACE && !VTRACE_NO_REGION */

#endif /* _VT_USER_H */
