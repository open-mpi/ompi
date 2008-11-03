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

#ifndef _VT_MEMREG_H
#define _VT_MEMREG_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#define VT__MEM_MALLOC                             0
#define VT__MEM_REALLOC                            1
#define VT__MEM_FREE                               2
#define VT__MEM_REGID_NUM                          3

extern int     vt_mem_regid[VT__MEM_REGID_NUM];

EXTERN void    vt_mem_register(void);

#endif /* _VT_MEMREG_H */

