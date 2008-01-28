!**
!* VampirTrace
!* http://www.tu-dresden.de/zih/vampirtrace
!*
!* Copyright (c) 2005-2007, ZIH, TU Dresden, Federal Republic of Germany
!*
!* Copyright (c) 1998-2005, Forschungszentrum Juelich GmbH, Federal
!* Republic of Germany
!*
!* See the file COPYRIGHT in the package base directory for details
!**

#include "vt_user_count.inc"
#include "vt_user_comment.inc"

#ifdef VTRACE
#  define VT_USER_START(n) call VT_User_start__(n, __FILE__, __LINE__)
#  define VT_USER_END(n)   call VT_User_end__(n)
#else
#  define VT_USER_START(n)
#  define VT_USER_END(n)
#endif
