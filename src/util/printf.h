/*
 * $HEADER$
 */

/** @file
 * 
 * Buffer safe printf functions for portability to archaic platforms.
 */

#ifndef OMPI_PRINTF_H
#define OMPI_PRINTF_H

#include "ompi_config.h"

#include <stdarg.h>
#include <stdlib.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * Writes to a string under the control of a format string
 * that specifies how subsequent arguments are converted for output.
 *
 * @param str   Output string buffer
 * @param size  Size of string buffer
 * @param fmt   Output format
 * @return      Length of output string
 *
 * At most size-1 characters are printed into the output string (the
 * size'th character then gets the terminating `\0'); if the return
 * value is greater than or equal to the size argument, the string was
 * too short and some of the printed characters were discarded.  The
 * output is always null-terminated.
 *
 * Returns the number of characters that would have been printed if
 * the size were unlimited (again, not including the final `\0').
 *
 * THIS IS A PORTABILITY FEATURE: USE snprintf() in CODE.
 */
OMPI_DECLSPEC int  ompi_snprintf(char *str, size_t size, const char *fmt, ...);


/**
 * Writes to a string under the control of a format string that
 * specifies how arguments accessed via the variable-length argument
 * facilities of stdarg(3) are converted for output.
 *
 * @param str   Output string buffer
 * @param size  Size of string buffer
 * @param fmt   Output format
 * @param ap    Variable argument list pointer
 * @return      Length of output string
 *
 * At most size-1 characters are printed into the output string (the
 * size'th character then gets the terminating `\0'); if the return
 * value is greater than or equal to the size argument, the string was
 * too short and some of the printed characters were discarded.  The
 * output is always null-terminated.
 *
 * Returns the number of characters that would have been printed if
 * the size were unlimited (again, not including the final `\0').
 *
 * THIS IS A PORTABILITY FEATURE: USE vsnprintf() in CODE.
 */
OMPI_DECLSPEC int  ompi_vsnprintf(char *str, size_t size, const char *fmt, va_list ap);

/**
 * Allocates and writes to a string under the control of a format
 * string that specifies how subsequent arguments are converted for
 * output.
 *
 * @param *ptr  Pointer to utput string buffer
 * @param fmt   Output format
 * @return      Length of output string
 *
 * Sets *ptr to be a pointer to a buffer sufficiently large to hold
 * the formatted string.  This pointer should be passed to free(3) to
 * release the allocated storage when it is no longer needed.  If
 * sufficient space cannot be allocated, asprintf() and vasprintf()
 * will return -1 and set ret to be a NULL pointer.
 *
 * Returns the number of characters printed.
 *
 * THIS IS A PORTABILITY FEATURE: USE asprintf() in CODE.
 */
OMPI_DECLSPEC int  ompi_asprintf(char **ptr, const char *fmt, ...);


/**
 * Allocates and writes to a string under the control of a format
 * string that specifies how arguments accessed via the
 * variable-length argument facilities of stdarg(3) are converted for
 * output.
 *
 * @param *ptr  Pointer to utput string buffer
 * @param fmt   Output format
 * @param ap    Variable argument list pointer
 * @return      Length of output string
 *
 * Sets *ptr to be a pointer to a buffer sufficiently large to hold
 * the formatted string.  This pointer should be passed to free(3) to
 * release the allocated storage when it is no longer needed.  If
 * sufficient space cannot be allocated, asprintf() and vasprintf()
 * will return -1 and set ret to be a NULL pointer.
 *
 * Returns the number of characters printed.
 *
 * THIS IS A PORTABILITY FEATURE: USE vasprintf() in CODE.
 */
OMPI_DECLSPEC int  ompi_vasprintf(char **ptr, const char *fmt, va_list ap);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_PRINTF_H */

