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
	int32_t hdr_src_rank;
	/* destination rank */
	int32_t hdr_dst_rank;
	/* user tag */
	int32_t hdr_user_tag;
	/* type of message - send/bsend/ssend/rsend/recv */
	int32_t hdr_msg_type;
	/* message length */
	uint32_t hdr_msg_length;
	/* offset into message */
	uint32_t hdr_msg_offset;
	/* fragment length */
	uint32_t hdr_frag_length;
	/* message sequence number */
        mca_ptl_base_sequence_t hdr_msg_seq;
	/* fragment sequence number */
	mca_ptl_base_sequence_t hdr_frag_seq;
} mca_ptl_base_header_t;


#endif /* MCA_PML_BASE_HEADER_H */
