!**
!* VampirTrace
!* http://www.tu-dresden.de/zih/vampirtrace
!*
!* Copyright (c) 2005-2012, ZIH, TU Dresden, Federal Republic of Germany
!*
!* Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
!*                          Centre, Federal Republic of Germany
!*
!* See the file COPYING in the package base directory for details
!**

#if (defined(VTRACE)) && !(defined(VTRACE_NO_REGION))
# define VT_USER_START(n) call VT_User_start__((n), __FILE__, __LINE__)
# define VT_USER_END(n) call VT_User_end__((n))
# define VT_USER_DEF(n, i) call VT_User_def__((n), "", 0, i)
# define VT_USER_START_ID(i) call VT_User_start_id__((i))
# define VT_USER_END_ID(i) call VT_User_end_id__((i))
#else
# define VT_USER_START(n)
# define VT_USER_END(n)
# define VT_USER_DEF(n, i)
# define VT_USER_START_ID(i)
# define VT_USER_END_ID(i)
#endif
