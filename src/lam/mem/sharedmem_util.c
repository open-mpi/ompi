/*
 * $HEADER$
 */

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lam/mem/sharedmem_util.h"
#include "lam/util/lam_log.h"

void *lam_zero_alloc(size_t len, int mem_prot, int mem_flags)
{
    void *ptr;
    int fd, flags = mem_flags;
#ifndef __osf__
    fd = -1;
    
#ifndef __APPLE__
    fd = open("/dev/zero", O_RDWR);
    if (fd < 0)
    {
        perror("/dev/zero");
        close(fd);
        return 0;
    }
#else
    flags = flags | MAP_ANON;
#endif		/* __APPLE__ */
    
    ptr = mmap(NULL, len, mem_prot, flags, fd, 0);
    if ( ptr == MAP_FAILED )
    {
        lam_err(("Error: mmap failed (%s)\n", strerror(errno)));
        close(fd);
        return (void *)0;
    }
    close(fd);
    
#else       /* this is __osf__ */
    
    if( mem_flags & MAP_PRIVATE ) {
        /*
         * private memory allocation
         */
        fd = open("/dev/zero", O_RDWR);
        if (fd < 0)
        {
            perror("/dev/zero");
            close(fd);
            return 0;
        }
        ptr = mmap(NULL, len, mem_prot, mem_flags, fd, 0);
        if ( ptr == MAP_FAILED )
        {
            fprintf(stderr,
                    " ZeroAlloc :: error in mmap(\"/dev/zero\") call.  Bytes Allocated :: %ld\n", len);
            fprintf(stderr, " errno :: %d\n", errno);
            perror(" mmap failed");
            close(fd);
            return (void *)0;
        }
        close(fd);
    } else {
        long pageSize = sysconf(_SC_PAGESIZE);
        long long paddedLen = len + (2 * pageSize);
        ptr = ulm_malloc(paddedLen * sizeof(char));
        if (!ptr) {
            ulm_warn(("ZeroAlloc :: padded ulm_malloc() failed for "
                      "%lld bytes\n", paddedLen));
            return (void *)0;
        }
        memset(ptr, 0, paddedLen * sizeof(char));
        ptr = (char *)ptr + (pageSize - 1);
        ptr = (void *)((long)ptr & ~(pageSize - 1));
        /*
         * shared memory allocation
         */
        fd = -1;
        ptr = mmap(ptr, len, mem_prot, MAP_FIXED | mem_flags, fd, 0);
        
        if ( ptr == MAP_FAILED )
        {
            ulm_warn(("ZeroAlloc shared mmap error :: %d fd %d\n", errno, fd));
            perror(" mmap failed");
            return (void *)0;
        }
    }  /* end memory allocation */
#endif      /* __osf__ */
    
    return ptr;
}
