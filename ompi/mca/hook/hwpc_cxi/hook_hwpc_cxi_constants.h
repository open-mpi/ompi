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

#ifndef OMPI_HWPC_CXI_CONSTANTS_H
#define OMPI_HWPC_CXI_CONSTANTS_H

#include "ompi_config.h"
#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>

#define HWPC_CXI_MAX_DEVS_PER_NODE 16
#define HWPC_CXI_MAX_HOSTNAME_LENGTH 64
#define HWPC_CXI_MAX_OUTPUT_REPORT_PREFIX_LENGTH 64
#define HWPC_CXI_MAX_COUNTER_NAME_LENGTH 64
#define HWPC_CXI_COUNTER_TRACKING_LIST_ALLOC_SIZE 64
#define HWPC_CXI_MAX_FULLPATH_LENGTH 512  /* Absolute path + HWPC_CXI_MAX_OUTPUT_REPORT_PREFIX_LENGTH + HWPC_CXI_MAX_HOSTNAME_LENGTH */
#define HWPC_CXI_MAX_LINE_LENGTH 512

typedef enum ompi_hwpc_cxi_error_code_t {
    HWPC_CXI_SUCCESS                        =  0,
    HWPC_CXI_ERROR                          = -1,   /* general error */
    HWPC_CXI_ERROR_INVALID_ARGUMENTS        = -2,   /* invalid arguments */
    HWPC_CXI_ERROR_OUT_OF_MEMORY            = -3,   /* out of memory */
    HWPC_CXI_COUNTER_GROUP_NOT_FOUND        = -4,   /* counter group not found */
    HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND     = -5    /* counter mnemonic not found */
} ompi_hwpc_cxi_error_code_t;

typedef enum ompi_hwpc_cxi_counter_type_t {
    HWPC_CXI_COUNTER_UNKNOWN_TYPE           = 0,    /* Unknown counter type */
    HWPC_CXI_COUNTER_LOWLEVEL_TYPE          = 1,    /* A low-level counter represents a hardware-specific performance metric */
    HWPC_CXI_COUNTER_MNEMONIC_TYPE          = 2,    /* A counter mnemonic represents a specific performance metric within a counter group */
    HWPC_CXI_COUNTER_GROUP_TYPE             = 3,    /* A counter group is a collection of related counter mnemonics that represent a higher-level performance metric */
    HWPC_CXI_NUM_COUNTER_TYPES                      /* number of counter types */
} ompi_hwpc_cxi_counter_type_t;

/* This enumeration serves as a list of predefined cxi hardware performance counter group mnemonic name ids */
typedef enum ompi_hwpc_cxi_predefined_counter_group_id_t {
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
    /* This serves as the number of counter groups. It must be last. */
    OMPI_HWPC_CXI_NUM_PREDEFINED_COUNTER_GROUPS
} ompi_hwpc_cxi_predefined_counter_group_id_t;

/* This enumeration serves as a list of predefined cxi hardware performance counter mnemonic name ids */
typedef enum ompi_hwpc_cxi_predefined_counter_mnemonic_id_t {
    /* Address Translation Unit Performance Counters */
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
    /* Command Queue Performance Counters */
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
    /* Event Engine Performance Counters */
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
    /* HNI Performance Counters */
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
    /* High-Speed Network Interface Receive Performance Counters */
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
    /* High-Speed Network Interface Transmit Performance Counters */
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
    /* Inbound Transfer Engine Performance Counters */
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
    /* List Processing Engine Performance Counters */
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
    /* Management Block Performance Counters */
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
    /* Outbound Transfer Engine Performance Counters */
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
    /* Processor Interface Arbiters Performance Counters */
    PARBS_TARB_PI_POSTED_PKTS,
    PARBS_TARB_PI_POSTED_BLOCKED_CNT,
    PARBS_TARB_PI_NON_POSTED_PKTS,
    PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT,
    /* Packet Connection and Tracking Performance Counters */
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
    /* Triggered Operations Performance Counters */
    TOU_CT_CMD_COUNTS,
    TOU_NUM_LIST_REBUILDS,
    TOU_NUM_TRIG_CMDS,
    /* Retry-Handler Performance Counters */
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
    /* This serves as the number of counters. It must be last. */
    OMPI_HWPC_CXI_NUM_PREDEFINED_COUNTER_MNEMONICS
} ompi_hwpc_cxi_predefined_counter_mnemonic_id_t;

typedef struct ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t {
    const char* counter_name;
    const char* counter_description;
    const bool is_per_cxi_device;
    const bool is_retry_handler_counter;
    const bool is_per_category;
    /* Number of categories, if is_per_category is true, else 1 */
    const size_t num_categories;
    /* Name of category, if is_per_category is true, else NULL */
    const char* category_name;      /* e.g. traffic-class, command-type, etc. */
} ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t;

typedef struct ompi_hwpc_cxi_predefined_counter_group_obj_t {
    const char* counter_group_name;
    const char* counter_group_pretty_name;
    const char* counter_group_description;
    const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t* counter_mnemonic_list;
    const size_t counter_mnemonic_list_size;
} ompi_hwpc_cxi_predefined_counter_group_obj_t;

/* Simple helper for sanity-checking when processing a counter group object */
extern bool ompi_hwpc_cxi_counter_mnemonic_id_is_valid(const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t counter_mnemonic_id);

/* Function that given a counter group name, returns the corresponding counter group object */
extern ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_get_counter_group_obj_by_name(const ompi_hwpc_cxi_predefined_counter_group_obj_t **counter_group_obj, const char *counter_group_name);

/* Function that given a counter group name, returns the number of counters in the group */
extern ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_get_num_counters_in_counter_group_by_name(int *total_num_counters, const char *counter_group_name);

/* Function that given a counter mnemonic id, returns the corresponding counter mnemonic object */
extern ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_get_counter_mnemonic_obj_by_id(const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t counter_mnemonic_id);

/* Function that given a counter mnemonic name, returns the corresponding counter mnemonic object */
extern ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_get_counter_mnemonic_obj_by_name(const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, const char *counter_mnemonic_name);

/* Function that given a counter mnemonic name, returns the number of counters in the mnemonic */
extern ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_get_num_counters_in_counter_mnemonic_by_name(int *total_num_counters, const char *counter_mnemonic_name);

/*
 * Prints out a description of a counter group, but only the top-level information.
 * Call ompi_hwpc_cxi_print_full_counter_group_description() to print out the full description of a counter group
 * including all of its nested counter mnemonics.
 */
extern ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_print_counter_group_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj);

/* Prints out a detailed description of a counter mnemonic */
extern ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_print_counter_mnemonic_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj);

/* Prints out a detailed description of a counter group plus all of its nested counter mnemonics */
extern ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_print_full_counter_group_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj);

/* Returns a string representation of the given error code */
extern const char* ompi_hwpc_cxi_error_to_string(ompi_hwpc_cxi_error_code_t error_code);

/* Returns a string representation of the given counter type*/
extern const char* ompi_hwpc_cxi_counter_type_to_string(ompi_hwpc_cxi_counter_type_t counter_type);

#endif /* OMPI_HWPC_CXI_CONSTANTS_H */
