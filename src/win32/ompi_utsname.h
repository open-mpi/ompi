/*
 * $HEADER$
 */

#ifndef OMPI_UTSNAME_H
#define OMPI_UTSNAME_H

#ifdef WIN32
#   define WIN32_LEAN_AND_MEAN
#   include <windows.h>
#   undef WIN32_LEAN_AND_MEAN
#endif

#define OMPI_UTSNAME_LEN 20 
/* cygwin defines this to be 20 as well ... verify */

struct utsname {
    char sysname[OMPI_UTSNAME_LEN];
    char nodename[OMPI_UTSNAME_LEN];
    char release[OMPI_UTSNAME_LEN];
    char version[OMPI_UTSNAME_LEN];
    char machine[OMPI_UTSNAME_LEN];
};

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    int uname(struct utsname *un);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* oMPI_UTSNAME_H */
