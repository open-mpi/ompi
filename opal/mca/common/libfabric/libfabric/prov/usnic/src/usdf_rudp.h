/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef _USDF_RUDP_H_
#define _USDF_RUDP_H_

#include "usnic_direct.h"

#define USDF_RUDP_SEQ_CREDITS 256
#define USDF_RUDP_ACK_TIMEOUT 5  /* ms */

#define RUDP_SEQ_DIFF(A, B) ((int16_t)((u_int16_t)(A) - (u_int16_t)(B)))
#define RUDP_SEQ_LT(A, B) (RUDP_SEQ_DIFF((A), (B)) < 0)
#define RUDP_SEQ_LE(A, B) (RUDP_SEQ_DIFF((A), (B)) <= 0)
#define RUDP_SEQ_GT(A, B) (RUDP_SEQ_DIFF((A), (B)) > 0)
#define RUDP_SEQ_GE(A, B) (RUDP_SEQ_DIFF((A), (B)) >= 0)

#define RUDP_MSGID_DIFF(A, B) ((int32_t)((u_int32_t)(A) - (u_int32_t)(B)))
#define RUDP_MSGID_LT(A, B) (RUDP_MSGID_DIFF((A), (B)) < 0)
#define RUDP_MSGID_LE(A, B) (RUDP_MSGID_DIFF((A), (B)) <= 0)
#define RUDP_MSGID_GT(A, B) (RUDP_MSGID_DIFF((A), (B)) > 0)
#define RUDP_MSGID_GE(A, B) (RUDP_MSGID_DIFF((A), (B)) >= 0)

enum {
    /* data messages (a bitmask of FIRST and LAST) */
    RUDP_OP_MID     = 0x00,
    RUDP_OP_FIRST   = 0x01,
    RUDP_OP_LAST    = 0x02,
    RUDP_OP_ONLY    = 0x03,

    /* control messages */
    RUDP_OP_CONNECT_REQ  = 0x81,
    RUDP_OP_CONNECT_RESP = 0x82,
    RUDP_OP_NAK       = 0x83,
    RUDP_OP_ACK       = 0x84,
};

#define RUDP_OP_DATA_MASK (RUDP_OP_FIRST | RUDP_OP_LAST)

struct rudp_rc_data_msg {
    u_int32_t offset;  /* 4 */
    u_int16_t rkey;    /* 8 */
    u_int16_t length;  /* 10 */
    u_int16_t seqno;   /* 12 */
    u_int16_t rdma_id; /* 14 */
} __attribute__ ((__packed__));

struct rudp_msg {
    u_int16_t opcode;
    u_int16_t src_peer_id;
    u_int32_t msg_id;
    union {
        struct rudp_rc_data_msg rc_data;
        struct {
            u_int16_t dst_peer_id;
        } connect_req;
        struct {
            u_int16_t dst_peer_id;
        } connect_resp;
        struct {
            u_int16_t ack_seq;
        } ack;
        struct {
            u_int16_t nak_seq;
            u_int32_t seq_mask;
        } nak;
    } __attribute__ ((__packed__)) m;
} __attribute__ ((__packed__));


struct rudp_pkt {
    struct usd_udp_hdr hdr;
    struct rudp_msg msg;
} __attribute__ ((__packed__));


#endif /* _USDF_RUDP_H_ */
