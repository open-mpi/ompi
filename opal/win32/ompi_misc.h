/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_MISC_H
#define OMPI_MISC_H

#include <stdlib.h>

#define _SC_PAGESIZE 0
#define _SC_OPEN_MAX 1

/* currently, this is a memory leak */
static __inline char* getenv (const char *name)
{
    int ret;
    char *buffer;
    DWORD length = GetEnvironmentVariable( (LPCSTR)((void*)name), NULL, 0 );

    if( 0 == length ) return NULL;
    buffer = (char *)malloc(sizeof(char) * length);
    ret = GetEnvironmentVariable((LPCSTR)((void*)name), (LPSTR)((void*)buffer), length);
    return (ret > 0) ? buffer: NULL;
}


static __inline int setenv (const char *name, const char *value, int rewrite) {

    /* just push it back to the windows thingy */
    int ret = SetEnvironmentVariable ((LPCSTR)((void*)name), (LPCSTR)((void*)value));
    return (0 != ret)? 1: 0;
}

static __inline unsigned int sleep(unsigned int seconds) {

    /* microsoft sleep is in milliseconds. Note: interrupt beaviour has
      not yet been handled */
    Sleep(seconds * 100);
    return 0;
}

/* this function can currently ONLY return the page size. for it to 
   do the entire sysconf range it needs to be extended */
static __inline size_t sysconf(int option) {
    
    SYSTEM_INFO sys_info;

    /* hardcoded on windows ... The maximum limit seems to be 2048 but
     * it requires a call to _setmaxstdio.
     */
    if( _SC_OPEN_MAX == option )
        return 512;

    GetSystemInfo(&sys_info);
    if (_SC_PAGESIZE == option){
        return (size_t)sys_info.dwPageSize;
    }
    printf( "This functionality is not supported: line: %d\tfile: %s\n",
            __LINE__, __FILE__);
    abort();
    return 0;
}

#define F_GETFL 0
#define F_SETFL 1
#define O_NONBLOCK 0
/*
 * this function is currently defined only for setting the socket to be 
 * in the non-blocking mode. Else this function returns error not implemented.
 * This calls ioctlsocket in the winsock library
 */
static __inline int fcntl (int fildes, int cmd, ...) {
    int ret;
    int mode;

    switch (cmd) {
        case F_SETFL: mode = 1; ret = ioctlsocket ((SOCKET)fildes, FIONBIO, (u_long FAR*) &mode);
                      break;
        case F_GETFL: ret = 0;
                      break;
        default: printf("Option not supported: %d %s\n", __LINE__, __FILE__);
                      abort();
    };

    return ret;
}

#endif /* OMPI_MISC_H */
