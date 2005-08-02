#include "btl_base_error.h"
#include <stdarg.h>

#if OMPI_ENABLE_DEBUG
int mca_btl_base_debug = 1;
#endif


int mca_btl_base_err(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stderr, fmt, list);
    va_end(list);
    return ret;
}


int mca_btl_base_out(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stdout, fmt, list);
    va_end(list);
    return ret;
}


