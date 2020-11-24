/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_STR_H_INCLUDED
#define MPL_STR_H_INCLUDED

#include "mplconfig.h"

/* *INDENT-ON* */
#if defined(__cplusplus)
extern "C" {
#endif
/* *INDENT-OFF* */

#if defined MPL_NEEDS_SNPRINTF_DECL
extern int snprintf(char *, size_t, const char *, ...) ATTRIBUTE((format(printf,3,4)));
#endif

#if defined MPL_HAVE_SNPRINTF
#define MPL_snprintf snprintf
#else
int MPL_snprintf(char *, size_t, const char *, ...) ATTRIBUTE((format(printf,3,4)));
#endif /* MPL_HAVE_SNPRINTF */

int MPL_strncpy(char *dest, const char *src, size_t n);
char *MPL_strsep(char **stringp, const char *delim);

#if defined MPL_NEEDS_STRNCMP_DECL
extern int strncmp(const char *s1, const char *s2, size_t n);
#endif

#if defined MPL_HAVE_STRNCMP
#define MPL_strncmp strncmp
#else
#error "strncmp is required"
#endif /* MPL_HAVE_STRNCMP */

#if defined MPL_NEEDS_STRERROR_DECL
extern char *strerror(int errnum);
#endif
#if defined MPL_HAVE_STRERROR
#define MPL_strerror strerror
#else
char *MPL_strerror(int errnum);
#endif /* MPL_HAVE_STRERROR */

int MPL_strnapp(char *dest, const char *src, size_t n);
void MPL_create_pathname(char *dest_filename, const char *dirname,
                         const char *prefix, const int is_dir);

/* *INDENT-ON* */
#if defined(__cplusplus)
}
#endif
/* *INDENT-OFF* */

#endif /* MPL_STR_H_INCLUDED */
