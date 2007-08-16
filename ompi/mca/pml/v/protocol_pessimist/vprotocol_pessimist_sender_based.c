/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "vprotocol_pessimist_sender_based.h"
#include <sys/types.h>
#if defined(HAVE_SYS_MMAN_H)
#include <sys/mman.h>
#endif  /* defined(HAVE_SYS_MMAN_H) */
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif
#include <fcntl.h>

#define sb mca_vprotocol_pessimist.sender_based

int vprotocol_pessimist_sender_based_init(const char *mmapfile, size_t size) 
{
    char path[PATH_MAX];
    sb.sb_offset = 0;
    sb.sb_length = size;
    sb.sb_pagesize = getpagesize();
    sb.sb_cursor = sb.sb_addr = (uintptr_t) NULL;
    sb.sb_available = 0;
    sb.sb_comm = MPI_COMM_NULL;
    
    sprintf(path, "%s"OPAL_PATH_SEP"%s", orte_process_info.proc_session_dir, 
                mmapfile);
    sb.sb_fd = open(path, O_CREAT | O_TRUNC | O_RDWR, 0600);
    if(-1 == sb.sb_fd)
    {
        V_OUTPUT_ERR("pml_v: vprotocol_pessimist: sender_based_init: open (%s): %s", 
                     path, strerror(errno));
        return -1;
    }
    return sb.sb_fd;
}

void vprotocol_pessimist_sender_based_finalize(void)
{
    int ret;
    
    if(sb.sb_comm != MPI_COMM_NULL)
    {
/* TODO: check this has already been freed by MPI_Finalize 
 *         ret = ompi_comm_free(&sb.sb_comm);
 *         if(MPI_SUCCESS != ret) 
 *             opal_output(0, "pml_v: protocol_pessimist: sender_based_finalize: ompi_comm_free failed (%d)", ret);
 */
        ret = munmap((void *) sb.sb_addr, sb.sb_length);
        if(-1 == ret)
            V_OUTPUT_ERR("pml_v: protocol_pessimsit: sender_based_finalize: munmap (%p): %s", 
                         (void *) sb.sb_addr, strerror(errno));
    }
    ret = close(sb.sb_fd);
    if(-1 == ret)
        V_OUTPUT_ERR("pml_v: protocol_pessimist: sender_based_finalize: close (%d): %s", 
                     sb.sb_fd, strerror(errno));
}


/** Manage mmap floating window, allocating enough memory for the message to be 
  * asynchronously copied to disk.
  */
void vprotocol_pessimist_sender_based_alloc(size_t len)
{
    if(sb.sb_comm == MPI_COMM_NULL)
        ompi_comm_dup(MPI_COMM_SELF, &sb.sb_comm, 1);
    else
        munmap((void *) sb.sb_addr, sb.sb_length);

    /* Take care of alignement of sb_offset                             */
    sb.sb_offset += sb.sb_cursor - sb.sb_addr;
    sb.sb_cursor = sb.sb_offset % sb.sb_pagesize;
    sb.sb_offset -= sb.sb_cursor; 

    /* Adjusting sb_length for the largest application message to fit   */
    len += sb.sb_cursor + sizeof(vprotocol_pessimist_sender_based_header_t);
    if(sb.sb_length < len)
        sb.sb_length = len;
    /* How much space left for application data */
    sb.sb_available = sb.sb_length - sb.sb_cursor;

    if(-1 == lseek(sb.sb_fd, sb.sb_offset + sb.sb_length, SEEK_SET))
    {
        V_OUTPUT_ERR("pml_v: vprotocol_pessimist: sender_based_alloc: lseek: %s", 
                     strerror(errno));
        close(sb.sb_fd);
        ompi_mpi_abort(MPI_COMM_NULL, MPI_ERR_NO_SPACE, false);
    }
    if(1 != write(sb.sb_fd, "", 1))
    {
        V_OUTPUT_ERR("pml_v: vprotocol_pessimist: sender_based_alloc: write: %s", 
                     strerror(errno));
        close(sb.sb_fd);
        ompi_mpi_abort(MPI_COMM_NULL, MPI_ERR_NO_SPACE, false);
    }
    sb.sb_addr = (uintptr_t) mmap((void *) sb.sb_addr, sb.sb_length, 
                                  PROT_WRITE | PROT_READ, MAP_SHARED, sb.sb_fd, 
                                  sb.sb_offset);
    if(((uintptr_t) -1) == sb.sb_addr)
    {
        V_OUTPUT_ERR("pml_v: vprotocol_pessimist: sender_based_alloc: mmap: %s", 
                     strerror(errno));
        close(sb.sb_fd);
        ompi_mpi_abort(MPI_COMM_NULL, MPI_ERR_NO_SPACE, false);
    }
    sb.sb_cursor += sb.sb_addr; /* set absolute addr of sender_based buffer */
    V_OUTPUT_VERBOSE(30, "pessimist:\tsb\tgrow\toffset %llu\tlength %llu\tbase %p\tcursor %p", (unsigned long long) sb.sb_offset, (unsigned long long) sb.sb_length, (void *) sb.sb_addr, (void *) sb.sb_cursor);
}   

#undef sb
