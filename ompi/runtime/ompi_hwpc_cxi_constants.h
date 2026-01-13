/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier:  MIT
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef __OMPI_HWPC_CXI_CONSTANTS_H__
#define __OMPI_HWPC_CXI_CONSTANTS_H__

#include "ompi_config.h"

#include <stdio.h>

#define MAX_DEVS 8
#define MAX_HOSTNAME_SIZE 64
#define MAX_COUNTER_NAME_SIZE 64
#define MAX_COUNTER_GROUP_NAME_SIZE MAX_COUNTER_NAME_SIZE
#define MAX_FILEPATH_LENGTH 256
#define MAX_OUTPUT_REPORT_PREFIX_LENGTH 128
#define MAX_LINE_LENGTH 512

// This enumeration serves as a list of predefined cxi hardware performance counter group mnemonic name ids
typedef enum ompi_hwpc_cxi_predefined_counter_group_mnemonics {
    CXI_PERFSTATS,
    CXI_ERRSTATS,
    CXI_OPCOMMANDS,
    CXI_OPPACKETS,
    CXI_DMAENGINE,
    CXI_WRITESTOHOST,
    CXI_MESSAGEMATCHINGPOOLED,
    CXI_TRANSLATIONUNIT,
    CXI_LATENCYHIST,
    CXI_PCTREQRESPTRACKING,
    CXI_LINKRELIABILITY,
    CXI_CONGESTION,
    // This serves as the number of counter groups. It must be last.
    OMPI_HWPC_CXI_NUM_PREDEFINED_COUNTER_GROUP_MNEMONICS
} ompi_hwpc_cxi_predefined_counter_group_mnemonic_t;

// This enumeration serves as a list of predefined cxi hardware performance counter mnemonic name ids
typedef enum ompi_hwpc_cxi_predefined_counter_mnemonics {
    // Address Translation Unit Performance Counters
    ATU_ATS_PRS_ODP_LATENCY,
    ATU_ATS_TRANS_LATENCY,
    ATU_CACHE_HIT_BASE_PAGE_SIZE,
    ATU_CACHE_HIT_DERIVATIVE1_PAGE_SIZE,
    ATU_CACHE_HIT_DERIVATIVE2_PAGE_SIZE,
    ATU_CACHE_MISS,
    ATU_CACHE_MISS_EE,
    ATU_CACHE_MISS_IXE,
    ATU_CACHE_MISS_OXE,
    ATU_CACHE_EVICTIONS,
    ATU_CLIENT_REQ_EE,
    ATU_CLIENT_REQ_IXE,
    ATU_CLIENT_REQ_OXE,
    ATU_CLIENT_RSP_NOT_OK,
    ATU_NIC_PRI_ODP_LATENCY,
    ATU_NTA_TRANS_LATENCY,
    ATU_ODP_REQUESTS,
    // Command Queue Performance Counters
    CQ_CQ_CMD_COUNTS,
    CQ_DMA_CMD_COUNTS,
    CQ_CYCLES_BLOCKED,
    CQ_NUM_CQ_CMDS,
    CQ_NUM_DMA_CMDS,
    CQ_NUM_IDC_CMDS,
    CQ_NUM_LL_CMDS,
    CQ_NUM_LL_OPS_RECEIVED,
    CQ_NUM_LL_OPS_REJECTED,
    CQ_NUM_LL_OPS_SPLIT,
    CQ_NUM_LL_OPS_SUCCESSFUL,
    CQ_NUM_TGQ_CMD_READS,
    CQ_NUM_TGT_CMDS,
    CQ_NUM_TOU_CMD_READS,
    CQ_NUM_TXQ_CMD_READS,
    CQ_TGT_WAITING_ON_READ,
    CQ_TX_WAITING_ON_READ,
    // Event Engine Performance Counters
    EE_ADDR_TRANS_PREFETCH_CNTR,
    EE_CBS_WRITTEN_CNTR,
    EE_DEFERRED_EQ_SWITCH_CNTR,
    EE_EQ_BUFFER_SWITCH_CNTR,
    EE_EQ_STATUS_UPDATE_CNTR,
    EE_EQ_SW_STATE_WR_CNTR,
    EE_EVENTS_DROPPED_FC_SC_CNTR,
    EE_EVENTS_DROPPED_ORDINARY_CNTR,
    EE_EVENTS_DROPPED_RSRVN_CNTR,
    EE_EVENTS_ENQUEUED_CNTR,
    EE_EXPIRED_CBS_WRITTEN_CNTR,
    EE_PARTIAL_CBS_WRITTEN_CNTR,
    // HNI Performance Counters
    HNI_DISCARD_CNTR,
    HNI_LLR_TX_REPLAY_EVENT,
    HNI_LLR_RX_REPLAY_EVENT,
    HNI_MULTICAST_PKTS_RECV_BY_TC,
    HNI_MULTICAST_PKTS_SENT_BY_TC,
    HNI_PAUSE_RECV,
    HNI_PAUSE_XOFF_SENT,
    HNI_PCS_CORRECTED_CW,
    HNI_PCS_FECL_ERRORS,
    HNI_PCS_GOOD_CW,
    HNI_PCS_UNCORRECTED_CW,
    HNI_PFC_FIFO_OFLW_CNTR,
    HNI_PKTS_RECV_BY_TC,
    HNI_PKTS_SENT_BY_TC,
    // High-Speed Network Interface Receive Performance Counters
    HNI_RX_OK_27,
    HNI_RX_OK_35,
    HNI_RX_OK_64,
    HNI_RX_OK_36_to_63,
    HNI_RX_OK_65_to_127,
    HNI_RX_OK_128_to_255,
    HNI_RX_OK_256_to_511,
    HNI_RX_OK_512_to_1023,
    HNI_RX_OK_1024_to_2047,
    HNI_RX_OK_2048_to_4095,
    HNI_RX_OK_4096_to_8191,
    HNI_RX_OK_8192_to_max,
    HNI_RX_PAUSED,
    HNI_RX_PAUSED_STD,
    HNI_RX_STALL_IXE_PKTBUF,
    // High-Speed Network Interface Transmit Performance Counters
    HNI_TX_OK_27,
    HNI_TX_OK_35,
    HNI_TX_OK_64,
    HNI_TX_OK_36_to_63,
    HNI_TX_OK_65_to_127,
    HNI_TX_OK_128_to_255,
    HNI_TX_OK_256_to_511,
    HNI_TX_OK_512_to_1023,
    HNI_TX_OK_1024_to_2047,
    HNI_TX_OK_2048_to_4095,
    HNI_TX_OK_4096_to_8191,
    HNI_TX_OK_8192_to_max,
    HNI_TX_PAUSED,
    // Inbound Transfer Engine Performance Counters
    IXE_DISP_DMAWR_REQS,
    IXE_DMAWR_STALL_NP_CDT,
    IXE_DMAWR_STALL_P_CDT,
    IXE_POOL_ECN_PKTS,
    IXE_POOL_NO_ECN_PKTS,
    PI_PTI_TARB_MRD_PKTS,
    PI_PTI_TARB_MWR_PKTS,
    IXE_TC_REQ_ECN_PKTS,
    IXE_TC_REQ_NO_ECN_PKTS,
    IXE_TC_RSP_ECN_PKTS,
    IXE_TC_RSP_NO_ECN_PKTS,
    // List Processing Engine Performance Counters
    LPE_APPEND_CMDS,
    LPE_APPEND_SUCCESS,
    LPE_CYC_RRQ_BLOCKED,
    LPE_NET_MATCH_LOCAL,
    LPE_NET_MATCH_OVERFLOW,
    LPE_NET_MATCH_PRIORITY,
    LPE_NET_MATCH_REQUEST,
    LPE_NET_MATCH_REQUESTS,
    LPE_NET_MATCH_SUCCESS,
    LPE_NET_MATCH_USEONCE,
    LPE_NUM_TRUNCATED,
    LPE_RNDZV_PUTS,
    LPE_RNDZV_PUTS_OFFLOADED,
    LPE_SEARCH_NID_ANY,
    LPE_SEARCH_PID_ANY,
    LPE_SEARCH_RANK_ANY,
    LPE_SETSTATE_CMDS,
    LPE_SETSTATE_SUCCESS,
    LPE_UNEXPECTED_GET_AMO,
    LPE_STS_NET_MATCH_ATTEMPTS,
    LPE_STS_APP_MATCH_ATTEMPTS,
    LPE_STS_NET_MAX_ATTEMPTS,
    LPE_STS_APP_MAX_ATTEMPTS,
    LPE_AMO_CMDS,
    LPE_FAMO_CMDS,
    // Management Block Performance Counters
    MB_CMC_AXI_RD_REQUESTS,
    MB_CMC_AXI_WR_REQUESTS,
    MB_CRMC_AXI_RD_REQUESTS,
    MB_CRMC_AXI_WR_REQUESTS,
    MB_CRMC_RD_ERROR,
    MB_CRMC_RING_MBE,
    MB_CRMC_RING_RD_REQUESTS,
    MB_CRMC_RING_SBE,
    MB_CRMC_RING_WR_REQUESTS,
    MB_CRMC_WR_ERROR,
    // Outbound Transfer Engine Performance Counters
    OXE_CHANNEL_IDLE,
    OXE_MCU_MEAS,
    OXE_PTL_TX_GET_MSGS_TSC,
    OXE_PTL_TX_PUT_MSGS_TSC,
    OXE_PTL_TX_PUT_PKTS_TSC,
    OXE_STALL_FGFC_BLK,
    OXE_STALL_FGFC_CNTRL,
    OXE_STALL_FGFC_END,
    OXE_STALL_FGFC_START,
    OXE_STALL_IDC_NO_BUFF_BC,
    OXE_STALL_PBUF_BC,
    OXE_STALL_PCT_BC,
    OXE_STALL_TS_NO_IN_CRD_TSC,
    OXE_STALL_TS_NO_OUT_CRD_TSC,
    OXE_STALL_WR_CONFLICT_PKT_BUFF_BNK,
    // Processor Interface Arbiters Performance Counters
    PARBS_TARB_PI_POSTED_PKTS,
    PARBS_TARB_PI_POSTED_BLOCKED_CNT,
    PARBS_TARB_PI_NON_POSTED_PKTS,
    PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT,
    // Packet Connection and Tracking Performance Counters
    PCT_CONN_SCT_OPEN,
    PCT_HOST_ACCESS_LATENCY,
    PCT_MST_HIT_ON_SOM,
    PCT_NO_TCT_NACKS,
    PCT_NO_TRS_NACKS,
    PCT_NO_MST_NACKS,
    PCT_REQ_ORDERED,
    PCT_REQ_UNORDERED,
    PCT_REQ_RSP_LATENCY,
    PCT_RESPONSES_RECEIVED,
    PCT_RETRY_SRB_REQUESTS,
    PCT_SCT_TIMEOUTS,
    PCT_SPT_TIMEOUTS,
    PCT_TRS_RSP_NACK_DROPS,
    // Triggered Operations Performance Counters
    TOU_CT_CMD_COUNTS,
    TOU_NUM_LIST_REBUILDS,
    TOU_NUM_TRIG_CMDS,
    // Retry-Handler Performance Counters
    SCT_TIMEOUTS,
    SPT_TIMEOUTS,
    SPT_TIMEOUTS_U,
    CONNECTIONS_CANCELLED,
    NACK_NO_MATCHING_CONN,
    NACK_NO_TARGET_CONN,
    NACK_NO_TARGET_MST,
    NACK_NO_TARGET_TRS,
    NACK_RESOURCE_BUSY,
    NACKS,
    NACK_SEQUENCE_ERROR,
    PKTS_CANCELLED_O,
    PKTS_CANCELLED_U,
    SCT_IN_USE,
    TCT_TIMEOUTS,
    // This serves as the number of counters. It must be last.
    OMPI_HWPC_CXI_NUM_PREDEFINED_COUNTER_MNEMONICS
} ompi_hwpc_cxi_predefined_counter_mnemonic_t;


typedef struct ompi_hwpc_cxi_counter_desc_t {
    const char* counter_name;
    const char* counter_description;
    bool is_per_cxi_device;
    bool is_retry_handler_counter;
    bool is_per_category;
    // number of categories, if is_per_category is true
    int num_categories;
    // name of category, if is_per_category is true
    const char* category_name;      // e.g. traffic-class, command-type, etc.
    
} ompi_hwpc_cxi_counter_desc_t;

typedef struct ompi_hwpc_cxi_counter_group_desc_t {
    const char* counter_group_name;
    const char* counter_group_string_name;
    const char* counter_group_description;
    const ompi_hwpc_cxi_predefined_counter_mnemonic_t* counter_mnemonic_list;
    const size_t counter_mnemonic_list_size;
} ompi_hwpc_cxi_counter_group_desc_t;


// Contains the predefined Cassini (CXI) hardware performance counters.
extern const ompi_hwpc_cxi_counter_desc_t OMPI_HWPC_CXI_PREDEFINED_COUNTERS_LIST[];

// Contains the predefined Cassini (CXI) hardware performance counter groups.
extern const ompi_hwpc_cxi_counter_group_desc_t OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[];

// All of these list sizes are in terms of HPE's Cassini (CXI) hardware counter mnemonics, thus the true number of counter registers is much larger.
extern const size_t OMPI_HWPC_CXI_PREDEFINED_COUNTERS_LIST_SIZE;
extern const size_t OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST_SIZE;
extern const size_t OMPI_HWPC_CXI_DEFAULT_COUNTERS_LIST_SIZE;

extern const size_t CXI_PERFSTATS_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_ERRSTATS_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_OPCOMMANDS_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_OPPACKETS_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_DMAENGINE_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_WRITESTOHOST_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_MESSAGEMATCHINGPOOLED_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_TRANSLATIONUNIT_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_LATENCYHIST_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_PCTREQRESPTRACKING_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_LINKRELIABILITY_COUNTER_GROUP_LIST_SIZE;
extern const size_t CXI_CONGESTION_COUNTER_GROUP_LIST_SIZE;

#define GET_PREDEF_COUNTER_DESC(COUNTER_MNEMONIC_NAME) &OMPI_HWPC_CXI_PREDEFINED_COUNTERS_LIST[COUNTER_MNEMONIC_NAME]

int ompi_hwpc_cxi_get_counters_in_group(const char *group_name, char ***counter_names, size_t *num_counters);


#endif // OMPI_HWPC_CXI_CONSTANTS_H
