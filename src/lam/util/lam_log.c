/*
 * $HEADER$
 */

#include "lam/util/lam_log.h"
#include <stdlib.h>
#include <stdarg.h>

#define FILE_LINE_MAX   255
static char file_line[FILE_LINE_MAX + 1] = "";

void _lam_log(FILE *fd, const char *fmt, va_list ap)
{
    /* Write to a file descriptor and the log file */
        
    if (fd != NULL) {
        fprintf(fd, file_line);
        vfprintf(fd, fmt, ap);
        fflush(fd);
    }
    
    file_line[0] = '\0';
}

void _lam_print(FILE * fd, const char *fmt, va_list ap)
{
    /* Write to a file descriptor (usually stdout or stderr) */
    
    if (fd != NULL) {
        fprintf(fd, file_line);
        vfprintf(fd, fmt, ap);
        fflush(fd);
    }
    file_line[0] = '\0';
}

void _lam_set_file_line(const char *name, int line)
{
    sprintf(file_line, "LAM/MPI:%s:%d: ", name, line);
}

void _lam_err(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    _lam_log(stderr, fmt, ap);
    va_end(ap);
}
void _lam_warn(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    _lam_log(stderr, fmt, ap);
    va_end(ap);
}

void _lam_dbg(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    _lam_log(stdout, fmt, ap);
    va_end(ap);
}

void _lam_exit(int status, const char* fmt, ...)
{
    va_list ap;
    
    if (fmt) {
        va_start(ap, fmt);
        if (status != 0) {
            _lam_log(stderr, fmt, ap);
        }
        else {
            _lam_print(stdout, fmt, ap);
        }
        va_end(ap);
    }
    
    exit(status);
}
