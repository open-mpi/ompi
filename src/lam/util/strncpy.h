/*
 * $HEADER$
 */

#ifndef LAM_STRNCPY_H
#define LAM_STRNCPY_H

#include <sys/types.h>

/*
 * Use lam_strncpy() instead of strncpy()
 */
#define strncpy lam_strncpy


char *lam_strncpy(char *dest, const char *src, size_t len);

#endif /* LAM_STRNCPY_H */
