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

#ifndef OMPI_MISC_H
#define OMPI_MISC_H

#define _SC_PAGESIZE 0

static __inline char* getenv (const char *name) {
    /* currently, this is a memory leak */
    int ret;
    char *buffer = (char *)malloc(sizeof(char) * 100);
    ret = GetEnvironmentVariable(name, buffer, 100);
    return (ret > 0) ? buffer: NULL;
}


static __inline int setenv (const char *name, const char *value, int rewrite) {

    /* just push it back to the windows thingy */
    int ret = SetEnvironmentVariable (name, value);
    return (0 != ret)? 1: 0;
}

static __inline unsigned int sleep(unsigned int seconds) {

    /* microsoft sleep is in milliseconds. Note: interrupt beaviour has
      not yet been handled */
    Sleep(seconds * 100);
    return 0;
}

/* this function can currently ONLY return the page size. for it to 
   do the entire sysconf range it needs to beb extended */
static __inline size_t sysconf(int option) {
    
    SYSTEM_INFO sys_info;

    GetSystemInfo(&sys_info);
    if (_SC_PAGESIZE == option){
        return (size_t)sys_info.dwPageSize;
    }
    else {
        printf("This functionality is not supported: line: %d\tfile: %s\n",
                                                                    __LINE__, __FILE__);
        abort();
    }
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
        case F_SETFL: ret = ioctlsocket ((SOCKET)fildes, FIONBIO, (u_long FAR*) &mode);
                      break;
        case F_GETFL: ret = 0;
                      break;
        default: printf("Option not supported: %d %s\n", __LINE__, __FILE__);
                      abort();
    };

    return ret;
}

#endif /* OMPI_MISC_H */
