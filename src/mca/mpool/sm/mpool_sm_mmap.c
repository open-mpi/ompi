#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <sys/mman.h>

#include "include/constants.h"
#include "util/output.h"
#include "util/sys_info.h"
#include "mca/pcm/pcm.h"
#include "mpool_sm.h"
#include "mpool_sm_mmap.h"


OBJ_CLASS_INSTANCE(
    mca_mpool_sm_mmap_t,
    ompi_object_t,
    NULL,
    NULL
);


static mca_mpool_sm_mmap_t* mca_mpool_sm_mmap_open(char* path)
{
    mca_mpool_sm_mmap_t* map;
    mca_mpool_sm_segment_t* seg;
    int fd = -1;
    struct stat sbuf;

    while(fd < 0) {
        struct timespec ts;
        fd = open(path, O_CREAT|O_RDWR, 0000); 
        if(fd < 0 && errno != EACCES) {
            ompi_output(0, "mca_ptl_sm_mmap_open: open failed with errno=%d\n", errno);
            return NULL;
        }
        ts.tv_sec = 0; 
        ts.tv_nsec = 500000;
        nanosleep(&ts,NULL);
    }

    if(fstat(fd, &sbuf) != 0) {
        ompi_output(0, "mca_mpool_sm_mmap_open: fstat failed with errno=%d\n", errno);
        return NULL;
    }

    /* map the file and initialize segment state */
    seg = mmap(NULL, sbuf.st_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if(NULL == seg) {
        ompi_output(0, "mca_mpool_sm_mmap_open: mmap failed with errno=%d\n", errno);
        return NULL;
    }
    close(fd);
                                                                                              
    map = OBJ_NEW(mca_mpool_sm_mmap_t);
    map->map_seg = seg;
    map->map_addr = (unsigned char*)(seg + 1);
    map->map_size = seg->seg_size - sizeof(mca_mpool_sm_segment_t);
    close(fd);
    return map;
}


mca_mpool_sm_mmap_t* mca_mpool_sm_mmap_init(size_t size)
{
    int fd;
    mca_mpool_sm_segment_t* seg;
    mca_mpool_sm_mmap_t* map;
    char path[PATH_MAX];

    sprintf(path, "%s/mmap.%s", ompi_system_info.session_dir, ompi_system_info.nodename);
    fd = open(path, O_CREAT|O_RDWR, 0000); 
    if(fd < 0) {
        if(errno == EACCES)
            return mca_mpool_sm_mmap_open(path);
        ompi_output(0, "mca_mpool_sm_mmap_init: open failed with errno=%d\n", errno);
        return NULL;
    }

    /* truncate the file to the requested size */
    if(ftruncate(fd, size) != 0) {
        ompi_output(0, "mca_mpool_sm_mmap_init: ftruncate failed with errno=%d\n", errno);
        return NULL;
    }

    /* map the file and initialize segment state */
    seg = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if(NULL == seg) {
        ompi_output(0, "mca_mpool_sm_mmap_init: mmap failed with errno=%d\n", errno);
        return NULL;
    }

    spinunlock(&seg->seg_lock);
    seg->seg_offset = 0;
    seg->seg_size = size;

    map = OBJ_NEW(mca_mpool_sm_mmap_t);
    strncpy(map->map_path, path, PATH_MAX);
    map->map_seg = seg;
    map->map_addr = (unsigned char*)(seg+1);
    map->map_size = size - sizeof(mca_mpool_sm_segment_t);

    /* enable access by other processes on this host */
    if(fchmod(fd, 0600) != 0) {
        ompi_output(0, "mca_mpool_sm_mmap_init: fchmod failed with errno=%d\n", errno);
        OBJ_RELEASE(map);
        close(fd);
        return NULL;
    }
    close(fd);
    return map;
}


void* mca_mpool_sm_mmap_alloc(size_t* size)
{
    mca_mpool_sm_mmap_t* map = mca_mpool_sm_module.sm_mmap;
    mca_mpool_sm_segment_t* seg = map->map_seg;
    void* addr;

    spinlock(&seg->seg_lock);
    if(seg->seg_offset + *size > map->map_size) {
        addr = NULL;
    } else {
        addr = map->map_addr + seg->seg_offset;
        seg->seg_offset += *size;
    }
    spinunlock(&seg->seg_lock);
    return addr;
}


