#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <sys/mman.h>

#include "constants.h"
#include "mca/pcm/pcm.h"
#include "ptl_sm.h"
#include "ptl_sm_mmap.h"


OBJ_CLASS_INSTANCE(
    mca_ptl_sm_mmap_t,
    lam_object_t,
    NULL,
    NULL
);


static mca_ptl_sm_mmap_t* mca_ptl_sm_mmap_open(size_t size)
{
    mca_ptl_sm_segment_t* seg;
    mca_ptl_sm_mmap_t* map;
    int fd = -1;

    while(fd < 0) {
        struct timespec ts;
        fd = shm_open(mca_ptl_sm_module.sm_mmap_file, O_CREAT|O_RDWR, 0000);
        if(fd < 0 && errno != EACCES) {
            lam_output(0, "mca_ptl_sm_mmap_open: open failed with errno=%d\n", errno);
            return NULL;
        }
        ts.tv_sec = 0; 
        ts.tv_nsec = 500000;
        nanosleep(&ts,NULL);
    }

    /* map the file and initialize segment state */
    seg = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if(NULL == seg) {
        lam_output(0, "mca_ptl_sm_module_mmap: mmap failed with errno=%d\n", errno);
        return NULL;
    }
    close(fd);

    fprintf(stderr, "mapped at %08x", (unsigned int)seg);
    map = OBJ_NEW(mca_ptl_sm_mmap_t);
    map->sm_segment = seg;
    map->sm_addr = (unsigned char*)(seg + 1);
    map->sm_size = seg->seg_size;
    return map;
}


mca_ptl_sm_mmap_t* mca_ptl_sm_mmap_init(size_t size)
{
    static int segnum = 0;

    lam_job_handle_t job_handle = mca_pcm.pcm_handle_get();
    char hostname[64];
    int fd;
    mca_ptl_sm_segment_t* seg;
    mca_ptl_sm_mmap_t* map;

    gethostname(hostname, sizeof(hostname));
    sprintf(mca_ptl_sm_module.sm_mmap_file, "/%s.%s.%d", hostname, job_handle, segnum++);
    fd = shm_open(mca_ptl_sm_module.sm_mmap_file, O_CREAT|O_RDWR, 0000);
    if(fd < 0) {
        if(errno == EACCES)
            return mca_ptl_sm_mmap_open(size);
        lam_output(0, "mca_ptl_sm_module_mmap: open failed with errno=%d\n", errno);
        return NULL;
    }
                                                                                                                                          
    /* truncate the file to the requested size */
    if(ftruncate(fd, size) != 0) {
        lam_output(0, "mca_ptl_sm_module_mmap: ftruncate failed with errno=%d\n", errno);
        return NULL;
    }

    /* map the file and initialize segment state */
    seg = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if(NULL == seg) {
        lam_output(0, "mca_ptl_sm_module_mmap: mmap failed with errno=%d\n", errno);
        return NULL;
    }
    fprintf(stderr, "mapped at %08x", (unsigned int)seg);

    spinunlock(&seg->seg_lock);
    seg->seg_offset = 0;
    seg->seg_size = size;

    map = OBJ_NEW(mca_ptl_sm_mmap_t);
    map->sm_segment = seg;
    map->sm_addr = (unsigned char*)(seg + 1);
    map->sm_size = size;

    /* enable access by other processes on this host */
    if(fchmod(fd, 0600) != 0) {
        lam_output(0, "mca_ptl_sm_module_mmap: fchmod failed with errno=%d\n", errno);
        OBJ_RELEASE(map);
        close(fd);
        return NULL;
    }
    close(fd);
    return map;
}


void* mca_ptl_sm_mmap_alloc(lam_allocator_t* allocator, size_t size)
{
    mca_ptl_sm_mmap_t* map = mca_ptl_sm_module.sm_mmap;
    mca_ptl_sm_segment_t* seg = map->sm_segment;
    void* addr;

    spinlock(&seg->seg_lock);
    addr = map->sm_addr + seg->seg_offset;
    seg->seg_offset += size;
    spinunlock(&seg->seg_lock);
    return addr;
}

                                                                                                                                          
void mca_ptl_sm_mmap_free(lam_allocator_t* allocator, void* ptr)
{
    /* empty for now */
}
                                                                                                                                          
                                                                                                                                          

