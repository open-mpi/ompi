/*
 * $HEADER$
 */

#ifndef LAM_LOG_H
#define LAM_LOG_H

#include <stdio.h>


#define lam_dbg(x) \
do { \
    if (OPT_DBG) { \
        _lam_set_file_line( __FILE__, __LINE__) ; \
            _lam_dbg x ; \
    } \
} while (0)

#define lam_err(x) \
do { \
    _lam_set_file_line(__FILE__, __LINE__) ; \
        _lam_err x ; \
} while (0)

#define lam_warn(x) \
do { \
    _lam_set_file_line(__FILE__, __LINE__) ; \
        _lam_warn x ; \
} while (0)

#define lam_exit(x) \
do { \
    _lam_set_file_line(__FILE__, __LINE__) ; \
        _lam_exit x ; \
} while (0)

/* Error condition */
void _lam_err(const char* fmt, ...);

/* Warning condition */
void _lam_warn(const char* fmt, ...);

/* Debugging message */
void _lam_dbg(const char* fmt, ...);

/* Exit with error message */
void _lam_exit(int status, const char* fmt, ...);

/* Set file and line info */
void _lam_set_file_line(const char *file, int lineno);


#endif  /* LAM_LOG_H */


