#ifndef _TM_MALLOC_H_
#define _TM_MALLOC_H_

#include <stdlib.h>
#include "ompi_config.h"

OMPI_HIDDEN void *tm_malloc(size_t size, char *, int);
OMPI_HIDDEN void *tm_calloc(size_t count, size_t size, char *, int);
OMPI_HIDDEN void *tm_realloc(void *ptr, size_t size, char *, int);
OMPI_HIDDEN void tm_free(void *ptr);
OMPI_HIDDEN void tm_mem_check(void);

/* for debugging malloc */
/* #define __DEBUG_TM_MALLOC__ */
#undef __DEBUG_TM_MALLOC__
#ifdef __DEBUG_TM_MALLOC__
#define MALLOC(x) tm_malloc(x,__FILE__,__LINE__)
#define CALLOC(x,y) tm_calloc(x,y,__FILE__,__LINE__)
#define REALLOC(x,y) tm_realloc(x,y,__FILE__,__LINE__)
#define FREE   tm_free
#define MEM_CHECK tm_mem_check
#else
#define MALLOC    malloc
#define CALLOC    calloc
#define FREE      free
#define REALLOC   realloc
#define MEM_CHECK tm_mem_check
#endif


#endif
