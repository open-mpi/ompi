/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "util/numtostr.h"
#include "util/printf.h"
#include <stdio.h>
#include <stdlib.h>


char*
ompi_ltostr(long num)
{
    /* waste a little bit of space, but always have a big enough buffer */
    int buflen = sizeof(long) * 8;
    char *buf = NULL;
    int ret = 0;

    buf = (char*) malloc(sizeof(char) * buflen);
    if (NULL == buf) return NULL;
    
    ret = snprintf(buf, buflen, "%ld", num);
    if (ret < 0) {
        free(buf);
        return NULL;
    }
    
    return buf;
}


char*
ompi_dtostr(double num)
{
    /* waste a little bit of space, but always have a big enough buffer */
    int buflen = sizeof(long) * 8;
    char *buf = NULL;
    int ret = 0;
    
    buf = (char*) malloc(sizeof(char) * buflen);
    if (NULL == buf) return NULL;
    
    ret = snprintf(buf, buflen, "%f", num);
    if (ret < 0) {
        free(buf);
        return NULL;
    }
    
    return buf;
}
