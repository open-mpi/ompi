/*
 * $HEADER$
 */

/*
 * Simple implementation of asprintf based on vsnprintf.
 *
 * Assumes that vsnprintf returns the required number of characters when
 * supplied with a null buffer (as specified in C99).
 */

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * Need to implement ompi_vsnprintf for platforms that don't have a
 * working vsnprintf.
 *
 * For now, assume a working vsnprintf...
 */

#define ompi_vsnprintf vsnprintf

int ompi_asprintf(char **ptr, const char *fmt, ...)
{
    int length;
    size_t size;
    va_list ap;

    /* find the required size */
    va_start(ap, fmt);
    length = ompi_vsnprintf(NULL, (size_t) 0, fmt, ap);
    size = (size_t) length + 1;
    va_end(ap);

    /* allocate a buffer */
    *ptr = (char *) malloc(size);
    if (*ptr == NULL) {
        errno = ENOMEM;
        return -1;
    }

    /* fill the buffer */
    va_start(ap, fmt);
    length = ompi_vsnprintf(*ptr, size, fmt, ap);
    va_end(ap);

    return length;
}
