#include "ompi_config.h"
#include "vprotocol_pessimist.h"

#ifndef __VPROTOCOL_PESSIMIST_SENDERBASED_H__
#define __VPROTOCOL_PESSIMIST_SENDERBASED_H__

#include "vprotocol_pessimist_request.h"
#if defined(HAVE_SYS_MMAN_H)
#include <sys/mman.h>
#endif  /* defined(HAVE_SYS_MMAN_H) */

typedef struct vprotocol_pessimist_sender_based_t 
{
    int sb_pagesize;    /* size of memory pages on this architecture */
    ompi_communicator_t *sb_comm;
    
    int sb_fd;          /* file descriptor of mapped file */
    off_t sb_offset;    /* offset in mmaped file          */
      
    char *sb_addr;      /* base address of mmaped segment */
    size_t sb_length;   /* length of mmaped segment */
    char *sb_cursor;    /* current pointer to writeable memory */
    size_t sb_vacant;   /* available space before end of segment */
} vprotocol_pessimist_sender_based_t;

typedef struct vprotocol_pessimist_sender_based_header_t
{
    size_t size;
    int dst; 
    int tag;
    uint32_t contextid;
    uint64_t sequence;
} vprotocol_pessimist_sender_based_header_t;

int vprotocol_pessimist_sender_based_init(const char *mmapfile, size_t size);

void vprotocol_pessimist_sender_based_finalize(void);

/** Manage mmap floating window, allocating enough memory for the message to be 
  * asynchronously copied to disk.
  */
void vprotocol_pessimist_sender_based_alloc(size_t len);


#define __SENDER_BASED_IOV_PACK(req) do {                                     \
    if( 0 != req->req_bytes_packed ) {                                        \
        ompi_convertor_t conv;                                                \
        size_t max_data;                                                      \
        size_t zero = 0;                                                      \
        unsigned int iov_count = 1;                                           \
        struct iovec iov;                                                     \
                                                                              \
        iov.iov_len = req->req_bytes_packed;                                  \
        iov.iov_base =                                                        \
            (IOVBASE_TYPE *) mca_vprotocol_pessimist.sender_based.sb_cursor;  \
        ompi_convertor_clone_with_position( &req->req_base.req_convertor,     \
                                            &conv, 0, &zero );                \
        ompi_convertor_pack(&conv, &iov, &iov_count, &max_data);              \
    }                                                                         \
} while(0)

#define __SENDER_BASED_SENDRECV_PACK(req) do {                                \
    mca_pml_v.host_pml.pml_irecv(                                             \
            mca_vprotocol_pessimist.sender_based.sb_cursor,                   \
            req->req_bytes_packed, MPI_PACKED, 0, 0,                          \
            mca_vprotocol_pessimist.sender_based.sb_comm,                     \
            & VPESSIMIST_REQ(req)->sb_reqs[0]);                               \
    mca_pml_v.host_pml.pml_isend(req->req_base.req_addr,                      \
            req->req_base.req_count, req->req_base.req_datatype, 0, 0,        \
            MCA_PML_BASE_SEND_READY,                                          \
            mca_vprotocol_pessimist.sender_based.sb_comm,                     \
            & VPESSIMIST_REQ(req)->sb_reqs[1]);                               \
} while(0);

#define __SENDER_BASED_PACK(req) do {                                         \
    vprotocol_pessimist_sender_based_header_t *sbhdr =                        \
        (vprotocol_pessimist_sender_based_header_t *)                         \
            mca_vprotocol_pessimist.sender_based.sb_cursor;                   \
    sbhdr->size = req->req_bytes_packed;                                      \
    sbhdr->dst = req->req_base.req_peer;                                      \
    sbhdr->tag = req->req_base.req_tag;                                       \
    sbhdr->contextid = req->req_base.req_comm->c_contextid;                   \
    sbhdr->sequence = req->req_base.req_sequence;                             \
    mca_vprotocol_pessimist.sender_based.sb_cursor +=                         \
            sizeof(vprotocol_pessimist_sender_based_header_t);                \
                                                                              \
    __SENDER_BASED_IOV_PACK(req);                                             \
    mca_vprotocol_pessimist.sender_based.sb_cursor += sbhdr->size;            \
    mca_vprotocol_pessimist.sender_based.sb_vacant -= (sbhdr->size +          \
            sizeof(vprotocol_pessimist_sender_based_header_t));               \
    V_OUTPUT_VERBOSE(70, "pessimist:\tsb\twrite\t%x\tsize %ld", VPESSIMIST_REQ(&req->req_base)->reqid, sbhdr->size); \
} while(0)

/** Copy data associated to a pml_base_send_request_t to the sender based 
  * message payload buffer
  */
#define VPROTOCOL_PESSIMIST_SENDER_BASED_COPY(REQ) do {                       \
    mca_pml_base_send_request_t *req = (mca_pml_base_send_request_t *) (REQ); \
    if(req->req_bytes_packed >=                                               \
            mca_vprotocol_pessimist.sender_based.sb_vacant)                   \
    {                                                                         \
        vprotocol_pessimist_sender_based_alloc(req->req_bytes_packed);        \
    }                                                                         \
    __SENDER_BASED_PACK(req);                                                 \
} while(0)


/** Ensure sender based is finished before allowing user to touch send buffer
  */ 
#define VPROTOCOL_PESSIMIST_SENDER_BASED_FLUSH(REQ)
#define DUMMYCOMMENT do {                      \
    if(VPESSIMIST_REQ(REQ)->sb_reqs[0])                                       \
    {                                                                         \
        ompi_request_wait_all(2, VPESSIMIST_REQ(REQ)->sb_reqs,                \
                              MPI_STATUSES_IGNORE);                           \
        VPESSIMIST_REQ(REQ)->sb_reqs[0] = NULL;                               \
    }                                                                         \
} while(0)

#endif
