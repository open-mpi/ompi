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
#include "common_sm_mmap.h"


OBJ_CLASS_INSTANCE(
    mca_common_sm_mmap_t,
    ompi_object_t,
    NULL,
    NULL
);

/*
 * Instance that is shared between components that use shared memory
 */
mca_common_sm_mmap_t *mca_common_sm_mmap = NULL;


static int mca_common_sm_mmap_open(char* path)
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
 *                             is assumed to have mca_common_sm_segment_t
 *                             as its first segment (IN)
 *
 *  @param data_set_alignment  alignment of the data segment.  this
 *                             follows the control structure (IN)
 */

mca_common_sm_mmap_t* mca_common_sm_mmap_init(size_t size, char *file_name, 
        size_t size_ctl_structure, size_t data_seg_alignment)
{
    int fd,return_code=OMPI_SUCCESS;
    bool file_previously_opened;
    mca_common_sm_segment_t* seg;
    mca_common_sm_mmap_t* map;
    struct stat s_stat;
    unsigned char *addr;
    size_t tmp,mem_offset;

    /* input parameter error checks */
    if( (size < sizeof(mca_common_sm_segment_t) ) ||
                ( file_name == NULL ) || 
                ( size_ctl_structure < sizeof(mca_common_sm_segment_t ) ) ||
                ( data_seg_alignment == 0 ) ) {
        return NULL;
    }

    /* debug */
    fprintf(stderr," open %s \n",file_name);
    fflush(stderr);
    /* end debug */

    /* open the backing file - only the first process accessing this
     * file will succeed. */
    fd=mca_common_sm_mmap_open(file_name);
    if( -1 == fd ) {
        ompi_output(0, "mca_common_sm_mmap_init: mca_common_sm_mmap_open failed \n");
        return NULL;
    }

    /* figure out if I am first to attach to file */
    file_previously_opened=false;
    return_code=fstat(fd,&s_stat);
    if( 0 > return_code ) {
        ompi_output(0, "mca_common_sm_mmap_init: fstat failed with errno=%d\n", errno);
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
                    "mca_common_sm_mmap_init: ftruncate failed with errno=%d\n",
                    errno);
            close(fd);
            return NULL;
        }
    }

    /* map the file and initialize segment state */
    seg = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if( (void*)-1 == seg ) {
        ompi_output(0, "mca_common_sm_mmap_init: mmap failed with errno=%d\n",
                errno);
        close(fd);
        return NULL;
    }

    /* set up the map object */
    map = OBJ_NEW(mca_common_sm_mmap_t);
    strncpy(map->map_path, file_name, PATH_MAX);
    /* the first entry in the file is the control strcuture.  the first
       entry in the control structure is an mca_common_sm_segment_t
       element */
    map->map_seg = seg;

    /* allign start of data segment */
    addr=(unsigned char *)seg+size_ctl_structure;
    tmp=(size_t)(addr+1);
    addr=(unsigned char *)( (tmp/data_seg_alignment)*data_seg_alignment);
    /* is addr past end of file ? */
    if( (unsigned char*)seg+size < addr ){
        ompi_output(0, "mca_common_sm_mmap_init: memory region too small len %d  addr %p\n",
                size,addr);
        fchmod(fd, 0600);
        close(fd);
        munmap(seg,size);
        return NULL;
    }
    mem_offset=addr-(unsigned char *)seg;
    map->map_addr = seg;
    map->data_addr = addr;
    map->map_size = size;

    /* initialize the segment - only the first process to open the file */
    if( !file_previously_opened ) {
        spinunlock(&seg->seg_lock);
        seg->seg_inited = false;
        seg->seg_offset = mem_offset;
        seg->seg_size = size;
    }

    /* enable access by other processes on this host */
    if(fchmod(fd, 0600) != 0) {
        ompi_output(0, "mca_common_sm_mmap_init: fchmod failed with errno=%d :: fd %d\n",
                errno,fd);
        OBJ_RELEASE(map);
        close(fd);
        return NULL;
    }
    close(fd);

    return map;
}


/**
 *  allocate memory from a previously allocated shared memory
 *  block.
 *
 *  @param size size of request, in bytes (IN)
 * 
 *  @retval addr virtual address
 */
void* mca_common_sm_mmap_alloc(size_t* size)
{
    mca_common_sm_mmap_t* map = mca_common_sm_mmap;
    mca_common_sm_segment_t* seg = map->map_seg;
    void* addr;

    spinlock(&seg->seg_lock);
    if(seg->seg_offset + *size > map->map_size) {
        addr = NULL;
    } else {
        /* add base address to segment offset */
        addr = map->data_addr + seg->seg_offset;
        seg->seg_offset += *size;
    }
    spinunlock(&seg->seg_lock);
    return addr;
}


