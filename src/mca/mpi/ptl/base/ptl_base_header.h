/*
 * $HEADER$
 */

#ifndef MCA_PML_BASE_HEADER_H
#define MCA_PML_BASE_HEADER_H

#include "mca/mpi/ptl/ptl.h"

/* define a common set of parameters included in all
 *   point-to-point headers
 */

typedef struct {
	/* communicator index */
	uint32_t hdr_contextid;
	/* source rank */
	int hdr_src_rank;
	/* destination rank */
	int hdr_dst_rank;
	/* user tag */
	int hdr_user_tag;
	/* type of message - send/bsend/ssend/rsend/recv */
	int hdr_msg_type;
	/* message length */
	size_t hdr_msg_length;
	/* fragment length */
	size_t hdr_frag_length;
	/* offset into message */
	size_t hdr_offset;
} mca_ptl_base_hdr_t;

typedef struct {
	/* base header */
	mca_ptl_base_hdr_t hdr_base;
	/* message sequence number */
    mca_ptl_base_sequence_t hdr_msg_seq_num;
	/* fragment sequence number */
	mca_ptl_base_sequence_t hdr_frag_seq_num;
} mca_ptl_base_reliable_hdr_t;


#endif /* MCA_PML_BASE_HEADER_H */
