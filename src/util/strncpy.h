/*
 * $HEADER$
 */

#ifndef OMPI_STRNCPY_H
#define OMPI_STRNCPY_H

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

/*
 * Use ompi_strncpy() instead of strncpy()
 */
#if defined(strncpy)
#undef strncpy
#endif
#define strncpy ompi_strncpy


char *ompi_strncpy(char *dest, const char *src, size_t len);

#endif /* OMPI_STRNCPY_H */
