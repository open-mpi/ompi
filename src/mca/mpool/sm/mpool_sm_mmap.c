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
#include "util/proc_info.h"
#include "mca/pcm/pcm.h"
#include "mpool_sm.h"
#include "mpool_sm_mmap.h"


OBJ_CLASS_INSTANCE(
    mca_mpool_sm_mmap_t,
    ompi_object_t,
    NULL,
    NULL
);


static int mca_mpool_sm_mmap_open(char* path)
{
    int fd = -1;

    /* loop until file can be opened, or until an erro, other than
     * access error, occurs */
    while(fd < 0) {
        struct timespec ts;
        fd = open(path, O_CREAT|O_RDWR, 0000); 
        if(fd < 0 && errno != EACCES) {
            ompi_output(0, 
            "mca_ptl_sm_mmap_open: open %s failed with errno=%d\n", path, errno);
            return -1;
        }
        ts.tv_sec = 0; 
        ts.tv_nsec = 500000;
        nanosleep(&ts,NULL);
    }

    /* return */
    return fd;

}

/**
 *  This routine is used to set up a shared memory file, backed
 *  by a specified file.  It is assumed that the file does not
 *  exist before any of the current set of processes try and open
 *  it.
 *
 *  @param size - size of the file, in bytes (IN)
 *
 *  @param file_name  name of file to be opened. (IN)
 *
 *  @param size_ctl_structure  size of the control structure at
 *                             the head of the file. The control structure 
 *                             is assumed to have mca_mpool_sm_segment_t
 *                             as its first segment (IN)
 *
 *  @param data_set_alignment  alignment of the data segment.  this
 *                             follows the control structure (IN)
 */

mca_mpool_sm_mmap_t* mca_mpool_sm_mmap_init(size_t size, char *file_name, 
        size_t size_ctl_structure, size_t data_seg_alignment)
{
    int fd,return_code=OMPI_SUCCESS;
    bool file_previously_opened;
    mca_mpool_sm_segment_t* seg;
    mca_mpool_sm_mmap_t* map;
    struct stat s_stat;
    unsigned char *addr;
    size_t tmp;

    /* input parameter error checks */
    if( (size <= sizeof(mca_mpool_sm_segment_t) ) ||
                ( file_name == NULL ) || 
                ( size_ctl_structure < sizeof(mca_mpool_sm_segment_t ) ) ||
                ( data_seg_alignment == 0 ) ) {
        return NULL;
    }

    /* debug */
    fprintf(stderr," open %s \n",file_name);
    fflush(stderr);
    /* end debug */

    /* open the backing file - only the first process accessing this
     * file will succeed. */
    fd=mca_mpool_sm_mmap_open(file_name);
    if( -1 == fd ) {
        ompi_output(0, "mca_mpool_sm_mmap_init: mca_mpool_sm_mmap_open failed \n");
        return NULL;
    }

    /* figure out if I am first to attach to file */
    file_previously_opened=false;
    return_code=fstat(fd,&s_stat);
    if( 0 > return_code ) {
        ompi_output(0, "mca_mpool_sm_mmap_init: fstat failed with errno=%d\n", errno);
        close(fd);
        return NULL;
    }
    if(0 < s_stat.st_size){
        file_previously_opened=true;
    }

    /* first process to open the file, so needs to
     * initialize it */
    if( !file_previously_opened ) {
        /* truncate the file to the requested size */
        if(ftruncate(fd, size) != 0) {
            ompi_output(0, 
                    "mca_mpool_sm_mmap_init: ftruncate failed with errno=%d\n",
                    errno);
            close(fd);
            return NULL;
        }
    }

    /* map the file and initialize segment state */
    seg = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if( (void*)-1 == seg ) {
        ompi_output(0, "mca_mpool_sm_mmap_init: mmap failed with errno=%d\n",
                errno);
        close(fd);
        return NULL;
    }

    /* set up the map object */
    map = OBJ_NEW(mca_mpool_sm_mmap_t);
    strncpy(map->map_path, file_name, PATH_MAX);
    map->map_seg = seg;
    /* allign start of data segment */
    addr=(unsigned char *)seg+size_ctl_structure;
    tmp=(size_t)(addr+1+data_seg_alignment);
    addr=(unsigned char *)( (tmp/data_seg_alignment)*data_seg_alignment);
    /* is addr past end of file ? */
    if( (unsigned char*)seg+size < addr ){
        ompi_output(0, "mca_mpool_sm_mmap_init: memory region too small len %d \n",size);
        close(fd);
        munmap(seg,size);
        return NULL;
    }
    map->map_addr = addr;
    map->map_size = size - (addr-(unsigned char *)seg);
    close(fd);

    /* initialize the segment - only the first process to open the file */
    if( !file_previously_opened ) {
        spinunlock(&seg->seg_lock);
        seg->seg_inited = false;
        seg->seg_offset = 0;
        seg->seg_size = size;
    }

    /* enable access by other processes on this host */
    if(fchmod(fd, 0600) != 0) {
        ompi_output(0, "mca_mpool_sm_mmap_init: fchmod failed with errno=%d\n", errno);
        OBJ_RELEASE(map);
        return NULL;
    }

    return map;
}


void* mca_mpool_sm_mmap_alloc(size_t* size)
{
    mca_mpool_sm_mmap_t* map = mca_mpool_sm_component.sm_mmap;
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


