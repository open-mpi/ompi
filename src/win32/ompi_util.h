/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
  
#ifndef OMPI_UTIL_H
#define OMPI_UTIL_H

#include <string.h>

static __inline int getpagesize(void) {
    SYSTEM_INFO sys_info;

    GetSystemInfo(&sys_info);
    return (int)sys_info.dwPageSize;
}


static __inline char *basename(char *path) {
    char *p = path;
    char *ret;

    if (path[strlen(path)-1] == '\\') {
        path[strlen(path)-1] = '\0';
    }
    
    while (*p != '\0') p++;
    while (*p != '\\') p--;
    ret = strdup(++p);
    
    return ret;
}

static __inline char *dirname(char *path) {

    /* remember, this is the windows version, so path is bound to contain
       the drive letter. Although, we are merely concerned with removing
       the last \ from the path offered. A new string should be allocated?? */
    char *dirname; 
    char *base;
	
	base = basename(path);
	dirname = strdup(path);

    strncpy(dirname, path, strlen(path)-strlen(base));
    dirname[strlen(path)-strlen(base)] = '\0';
    
    return dirname;
}

static __inline int strncasecmp (char *s1, char *s2, int n) {

    int ret;

    while (0 <= --n && (tolower(*s1) == tolower(*s2++))) {
        if ('\0' == tolower(*s1++)) {
            return 0;
         }
    }

    ret = (n < 0 ? 0 : tolower(*s1) - tolower(*--s2));

    return ret;
}

static __inline int strcasecmp(char *s1, char *s2) {

    return strncasecmp (s1, s2, (int)strlen(s1));
}

    
#endif
