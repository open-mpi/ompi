/*
 $HEADER$
 */

#ifndef OMPI_VAR_ARGS_H
#define OMPI_VAR_ARGS_H

/* To include the right thing for va_list, va_start, va_end and friends */
/* Windows and __STDC__ and other c++ compilers require stdarg.h else varargs */

#if (defined(__STDC__) && __STDC__) || defined(__cplusplus) || defined(c_plusplus) || defined(WIN32)
#include<stdio.h>
#include<stdarg.h>
#else
#include<varargs.h>
#endif


#if (defined(__STDC__) && __STDC__) || defined(__cplusplus) || defined(c_plusplus) || defined(WIN32)
#define VA_START(list, v) va_start(list, v);
#else 
#define VA_START(list,v) va_start(list); 
#endif

#endif /* OMPI_VARARGS_H */
