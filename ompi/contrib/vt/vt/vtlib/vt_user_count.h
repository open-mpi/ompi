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

#ifndef _VT_USER_COUNT_H
#define _VT_USER_COUNT_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#define VT_COUNT_DEFGROUP       -1

/* C/C++ types */
#define VT_COUNT_TYPE_SIGNED     1
#define VT_COUNT_TYPE_UNSIGNED   2
#define VT_COUNT_TYPE_FLOAT      3
#define VT_COUNT_TYPE_DOUBLE     4
/* Fortran types */
#define VT_COUNT_TYPE_INTEGER    11
#define VT_COUNT_TYPE_INTEGER8   12
#define VT_COUNT_TYPE_REAL       13

#if (defined(VTRACE)) && !(defined(VTRACE_NO_COUNT))

  EXTERN unsigned int VT_User_count_group_def__(char* gname);
  EXTERN unsigned int VT_User_count_def__(char* cname, char* cunit, int ctype,
                                          unsigned int gid);
  EXTERN void VT_User_count_signed_val__(unsigned int cid, long long val);
  EXTERN void VT_User_count_unsigned_val__(unsigned int cid, unsigned long long val);
  EXTERN void VT_User_count_float_val__(unsigned int cid, float val);
  EXTERN void VT_User_count_double_val__(unsigned int cid, double val);

  #define VT_COUNT_GROUP_DEF(n) VT_User_count_group_def__(n)
  #define VT_COUNT_DEF(n, u, t, gi) VT_User_count_def__(n, u, t, gi)
  #define VT_COUNT_SIGNED_VAL(i, v) VT_User_count_signed_val__(i, v)
  #define VT_COUNT_UNSIGNED_VAL(i, v) VT_User_count_unsigned_val__(i, v)
  #define VT_COUNT_FLOAT_VAL(i, v) VT_User_count_float_val__(i, v)
  #define VT_COUNT_DOUBLE_VAL(i, v) VT_User_count_double_val__(i, v)

#else

  #define VT_COUNT_GROUP_DEF(n) 0
  #define VT_COUNT_DEF(n, u, t, gi) 0
  #define VT_COUNT_SIGNED_VAL(i, v)
  #define VT_COUNT_UNSIGNED_VAL(i, v)
  #define VT_COUNT_FLOAT_VAL(i, v)
  #define VT_COUNT_DOUBLE_VAL(i, v)

#endif /* VTRACE */

#endif /* _VT_USER_COUNT_H */
