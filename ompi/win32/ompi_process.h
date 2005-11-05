/*
 *Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                        University Research and Technology
 *                        Corporation.  All rights reserved.
 *Copyright (c) 2004-2005 The University of Tennessee and The University
 *                        of Tennessee Research Foundation.  All rights
 *                        reserved.
 *Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                        University of Stuttgart.  All rights reserved.
 *Copyright (c) 2004-2005 The Regents of the University of California.
 *                        All rights reserved.
 *$COPYRIGHT$
 *
 *Additional copyrights may follow
 *
 *$HEADER$
 */

#ifndef OMPI_PROCESS_H
#define OMPI_PROCESS_H
#include "win32/ompi_declspec.h"
#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include<windows.h>
#undef WIN32_LEAN_AND_MEAN
#endif

#if defined(c_plusplus) || defined (__cplusplus)
extern "C"
{
#endif

OMPI_DECLSPEC pid_t waitpid (pid_t pid, int *status, int options) ;

OMPI_DECLSPEC int kill(pid_t pid, int sig) ;

#if defined(c_plusplus) || defined (__cplusplus)
}
#endif

#endif				/* OMPI_PROCESS_H */
