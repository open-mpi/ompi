/*
  $HEADER$
 */
  
#ifndef OMPI_UTIL_H
#define OMPI_UTIL_H

static __inline int getpagesize(void) {
    SYSTEM_INFO sys_info;

    GetSystemInfo(&sys_info);
    return (int)sys_info.dwPageSize;
}

static __inline char *dirname(char *path) {

    /* remember, this is the windows version, so path is bound to contain
       the drive letter. Although, we are merely concerned with removing
       the last \ from the path offered. A new string should be allocated?? */

    
    
    return NULL;
}

static __inline char *basename(char *path) {
    return NULL;
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
