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

#if (defined(VTRACE)) && !(defined(VTRACE_NO_COMMENT))
#  define VT_COMMENT_DEF(c) call VT_User_comment_def__(c)
#  define VT_COMMENT(c) call VT_User_comment__(c)
#else
#  define VT_COMMENT_DEF(c)
#  define VT_COMMENT(c)
#endif
