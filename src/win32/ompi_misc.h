/*
$HEADER$
*/

#ifndef OMPI_MISC_H
#define OMPI_MISC_H

#define _SC_PAGESIZE 0

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
        abort();
    }
}

#endif /* OMPI_MISC_H */
