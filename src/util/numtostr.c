/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "util/numtostr.h"

#include <stdio.h>


char*
ltostr(long num)
{
    /* waste a little bit of space, but always have a big enough buffer */
    int buflen = sizeof(long) * 8;
    char *buf = NULL;
    int ret = 0;

    buf = malloc(sizeof(char) * buflen);
    if (NULL == buf) return NULL;
    
    ret = snprintf(buf, buflen, "%ld", num);
    if (ret < 0) {
        free(buf);
        return NULL;
    }
    
    return buf;
}


char*
dtostr(double num)
{
    /* waste a little bit of space, but always have a big enough buffer */
    int buflen = sizeof(long) * 8;
    char *buf = NULL;
    int ret = 0;
    
    buf = malloc(sizeof(char) * buflen);
    if (NULL == buf) return NULL;
    
    ret = snprintf(buf, buflen, "%f", num);
    if (ret < 0) {
        free(buf);
        return NULL;
    }
    
    return buf;
}
