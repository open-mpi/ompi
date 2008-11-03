#ifndef _UTIL_H
#define _UTIL_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include <stdarg.h>
#include <stdlib.h>

EXTERN int vt_asprintf(char** ptr, const char* fmt, ...);
EXTERN int vt_snprintf(char* str, size_t size, const char* fmt, ...);
EXTERN int vt_vasprintf(char** ptr, const char* fmt, va_list ap);
EXTERN int vt_vsnprintf(char* str, size_t size, const char* fmt, va_list ap);
EXTERN char* vt_strdup(const char* s);

#endif /* _UTIL_H */
