#ifndef DEBUG_H_HAS_BEEN_INCLUDED
#define DEBUG_H_HAS_BEEN_INCLUDED

#include <stdio.h>

/* AIX requires this to be the first thing in the file.  */
#ifdef HAVE_ALLOCA
#  ifndef __GNUC__
#    ifdef _MSC_VER
#      include <malloc.h>
#      define alloca _alloca
#    else
#      ifdef HAVE_ALLOCA_H
#        include <alloca.h>
#      else
#        ifdef _AIX
#pragma alloca
#        else
#          ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#          endif
#        endif  /* _AIX */
#      endif  /* HAVE_ALLOCA_H */
#    endif  /* _MSC_VER */
#  else  /* Im a __GNUC__ happy compiler */
#include <stdlib.h>
#  endif  /* __GNUC__ */
#endif  /* HAVE_ALLOCA */

#if !defined(NDEBUG)
#if !defined(__FILE__)
#define __FILE__ "unsupported"
#endif  /* __FILE */
#if !defined(__LINE__)
#define __LINE__ -1
#endif  /* __LINE__ */
extern void* ftmpi_malloc( size_t, char*, int );
extern void* ftmpi_calloc( size_t, size_t, char*, int );
extern void* ftmpi_realloc( void*, size_t, char*, int );
extern void  ftmpi_free( void*, char*, int );
extern void ftmpi_display_memory_usage( void );

#define _MALLOC(size)           ftmpi_malloc( (size), __FILE__, __LINE__ )
#define _CALLOC(nb, size)       ftmpi_calloc( (nb), (size), __FILE__, __LINE__ )
#define _REALLOC(ptr, size)     ftmpi_realloc( (ptr), (size), __FILE__, __LINE__ )
#define _FREE(ptr)              ftmpi_free( (ptr), __FILE__, __LINE__ )
#define DUMP_ALLOCATED_MEMORY() ftmpi_display_memory_usage()

#else  /* !defined(NDEBUG) */
#include <stdlib.h>
#define _MALLOC(size)            malloc((size))
#define _CALLOC(nb, size)        calloc((nb), (size))
#define _REALLOC(ptr, size)      realloc( (ptr), (size) )
#define _FREE(ptr)               free((ptr))
#define DUMP_ALLOCATED_MEMORY()
#endif  /* NDEBUG */

#endif  /* DEBUG_H_HAS_BEEN_INCLUDED */
