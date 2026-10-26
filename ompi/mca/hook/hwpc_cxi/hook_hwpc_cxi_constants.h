/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Copyright (c) 2026       Hewlett Packard Enterprise Development LP. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef HWPC_CXI_CONSTANTS_H
#define HWPC_CXI_CONSTANTS_H

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

typedef enum hwpc_cxi_error_code_t {
    HWPC_CXI_SUCCESS                        =  0,
    HWPC_CXI_ERROR                          = -1,   /* general error */
    HWPC_CXI_ERROR_INVALID_ARGUMENTS        = -2,   /* invalid arguments */
    HWPC_CXI_ERROR_OUT_OF_MEMORY            = -3,   /* out of memory */
    HWPC_CXI_COUNTER_GROUP_NOT_FOUND        = -4,   /* counter group not found */
    HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND     = -5    /* counter mnemonic not found */
} hwpc_cxi_error_code_t;

typedef enum hwpc_cxi_counter_type_t {
    HWPC_CXI_COUNTER_UNKNOWN_TYPE           = 0,    /* Unknown counter type */
    HWPC_CXI_COUNTER_LOWLEVEL_TYPE          = 1,    /* A low-level counter represents a hardware-specific performance metric */
    HWPC_CXI_COUNTER_MNEMONIC_TYPE          = 2,    /* A counter mnemonic represents a specific performance metric within a counter group */
    HWPC_CXI_COUNTER_GROUP_TYPE             = 3,    /* A counter group is a collection of related counter mnemonics that represent a higher-level performance metric */
    HWPC_CXI_NUM_COUNTER_TYPES                      /* number of counter types */
} hwpc_cxi_counter_type_t;

/* This enumeration serves as a list of predefined cxi hardware performance counter group mnemonic name ids */
typedef enum hwpc_cxi_predefined_counter_group_id_t {
    HWPC_CXI_PERFSTATS,
    HWPC_CXI_ERRSTATS,
    HWPC_CXI_OPCOMMANDS,
    HWPC_CXI_OPPACKETS,
    HWPC_CXI_DMAENGINE,
    HWPC_CXI_WRITESTOHOST,
    HWPC_CXI_MESSAGEMATCHINGPOOLED,
    HWPC_CXI_TRANSLATIONUNIT,
    HWPC_CXI_LATENCYHIST,
    HWPC_CXI_PCTREQRESPTRACKING,
    HWPC_CXI_LINKRELIABILITY,
    HWPC_CXI_CONGESTION,
    /* This serves as the number of counter groups. It must be last. */
    HWPC_CXI_NUM_PREDEFINED_COUNTER_GROUPS
} hwpc_cxi_predefined_counter_group_id_t;

/* This enumeration serves as a list of predefined cxi hardware performance counter mnemonic name ids */
typedef enum hwpc_cxi_predefined_counter_mnemonic_id_t {
    /* Address Translation Unit Performance Counters */
    HWPC_CXI_ATU_ATS_PRS_ODP_LATENCY,
    HWPC_CXI_ATU_ATS_TRANS_LATENCY,
    HWPC_CXI_ATU_CACHE_HIT_BASE_PAGE_SIZE,
    HWPC_CXI_ATU_CACHE_HIT_DERIVATIVE1_PAGE_SIZE,
    HWPC_CXI_ATU_CACHE_HIT_DERIVATIVE2_PAGE_SIZE,
    HWPC_CXI_ATU_CACHE_MISS,
    HWPC_CXI_ATU_CACHE_MISS_EE,
    HWPC_CXI_ATU_CACHE_MISS_IXE,
    HWPC_CXI_ATU_CACHE_MISS_OXE,
    HWPC_CXI_ATU_CACHE_EVICTIONS,
    HWPC_CXI_ATU_CLIENT_REQ_EE,
    HWPC_CXI_ATU_CLIENT_REQ_IXE,
    HWPC_CXI_ATU_CLIENT_REQ_OXE,
    HWPC_CXI_ATU_CLIENT_RSP_NOT_OK,
    HWPC_CXI_ATU_NIC_PRI_ODP_LATENCY,
    HWPC_CXI_ATU_NTA_TRANS_LATENCY,
    HWPC_CXI_ATU_ODP_REQUESTS,
    /* Command Queue Performance Counters */
    HWPC_CXI_CQ_CQ_CMD_COUNTS,
    HWPC_CXI_CQ_DMA_CMD_COUNTS,
    HWPC_CXI_CQ_CYCLES_BLOCKED,
    HWPC_CXI_CQ_NUM_CQ_CMDS,
    HWPC_CXI_CQ_NUM_DMA_CMDS,
    HWPC_CXI_CQ_NUM_IDC_CMDS,
    HWPC_CXI_CQ_NUM_LL_CMDS,
    HWPC_CXI_CQ_NUM_LL_OPS_RECEIVED,
    HWPC_CXI_CQ_NUM_LL_OPS_REJECTED,
    HWPC_CXI_CQ_NUM_LL_OPS_SPLIT,
    HWPC_CXI_CQ_NUM_LL_OPS_SUCCESSFUL,
    HWPC_CXI_CQ_NUM_TGQ_CMD_READS,
    HWPC_CXI_CQ_NUM_TGT_CMDS,
    HWPC_CXI_CQ_NUM_TOU_CMD_READS,
    HWPC_CXI_CQ_NUM_TXQ_CMD_READS,
    HWPC_CXI_CQ_TGT_WAITING_ON_READ,
    HWPC_CXI_CQ_TX_WAITING_ON_READ,
    /* Event Engine Performance Counters */
    HWPC_CXI_EE_ADDR_TRANS_PREFETCH_CNTR,
    HWPC_CXI_EE_CBS_WRITTEN_CNTR,
    HWPC_CXI_EE_DEFERRED_EQ_SWITCH_CNTR,
    HWPC_CXI_EE_EQ_BUFFER_SWITCH_CNTR,
    HWPC_CXI_EE_EQ_STATUS_UPDATE_CNTR,
    HWPC_CXI_EE_EQ_SW_STATE_WR_CNTR,
    HWPC_CXI_EE_EVENTS_DROPPED_FC_SC_CNTR,
    HWPC_CXI_EE_EVENTS_DROPPED_ORDINARY_CNTR,
    HWPC_CXI_EE_EVENTS_DROPPED_RSRVN_CNTR,
    HWPC_CXI_EE_EVENTS_ENQUEUED_CNTR,
    HWPC_CXI_EE_EXPIRED_CBS_WRITTEN_CNTR,
    HWPC_CXI_EE_PARTIAL_CBS_WRITTEN_CNTR,
    /* HNI Performance Counters */
    HWPC_CXI_HNI_DISCARD_CNTR,
    HWPC_CXI_HNI_LLR_TX_REPLAY_EVENT,
    HWPC_CXI_HNI_LLR_RX_REPLAY_EVENT,
    HWPC_CXI_HNI_MULTICAST_PKTS_RECV_BY_TC,
    HWPC_CXI_HNI_MULTICAST_PKTS_SENT_BY_TC,
    HWPC_CXI_HNI_PAUSE_RECV,
    HWPC_CXI_HNI_PAUSE_XOFF_SENT,
    HWPC_CXI_HNI_PCS_CORRECTED_CW,
    HWPC_CXI_HNI_PCS_FECL_ERRORS,
    HWPC_CXI_HNI_PCS_GOOD_CW,
    HWPC_CXI_HNI_PCS_UNCORRECTED_CW,
    HWPC_CXI_HNI_PFC_FIFO_OFLW_CNTR,
    HWPC_CXI_HNI_PKTS_RECV_BY_TC,
    HWPC_CXI_HNI_PKTS_SENT_BY_TC,
    /* High-Speed Network Interface Receive Performance Counters */
    HWPC_CXI_HNI_RX_OK_27,
    HWPC_CXI_HNI_RX_OK_35,
    HWPC_CXI_HNI_RX_OK_64,
    HWPC_CXI_HNI_RX_OK_36_to_63,
    HWPC_CXI_HNI_RX_OK_65_to_127,
    HWPC_CXI_HNI_RX_OK_128_to_255,
    HWPC_CXI_HNI_RX_OK_256_to_511,
    HWPC_CXI_HNI_RX_OK_512_to_1023,
    HWPC_CXI_HNI_RX_OK_1024_to_2047,
    HWPC_CXI_HNI_RX_OK_2048_to_4095,
    HWPC_CXI_HNI_RX_OK_4096_to_8191,
    HWPC_CXI_HNI_RX_OK_8192_to_max,
    HWPC_CXI_HNI_RX_PAUSED,
    HWPC_CXI_HNI_RX_PAUSED_STD,
    HWPC_CXI_HNI_RX_STALL_IXE_PKTBUF,
    /* High-Speed Network Interface Transmit Performance Counters */
    HWPC_CXI_HNI_TX_OK_27,
    HWPC_CXI_HNI_TX_OK_35,
    HWPC_CXI_HNI_TX_OK_64,
    HWPC_CXI_HNI_TX_OK_36_to_63,
    HWPC_CXI_HNI_TX_OK_65_to_127,
    HWPC_CXI_HNI_TX_OK_128_to_255,
    HWPC_CXI_HNI_TX_OK_256_to_511,
    HWPC_CXI_HNI_TX_OK_512_to_1023,
    HWPC_CXI_HNI_TX_OK_1024_to_2047,
    HWPC_CXI_HNI_TX_OK_2048_to_4095,
    HWPC_CXI_HNI_TX_OK_4096_to_8191,
    HWPC_CXI_HNI_TX_OK_8192_to_max,
    HWPC_CXI_HNI_TX_PAUSED,
    /* Inbound Transfer Engine Performance Counters */
    HWPC_CXI_IXE_DISP_DMAWR_REQS,
    HWPC_CXI_IXE_DMAWR_STALL_NP_CDT,
    HWPC_CXI_IXE_DMAWR_STALL_P_CDT,
    HWPC_CXI_IXE_POOL_ECN_PKTS,
    HWPC_CXI_IXE_POOL_NO_ECN_PKTS,
    HWPC_CXI_PI_PTI_TARB_MRD_PKTS,
    HWPC_CXI_PI_PTI_TARB_MWR_PKTS,
    HWPC_CXI_IXE_TC_REQ_ECN_PKTS,
    HWPC_CXI_IXE_TC_REQ_NO_ECN_PKTS,
    HWPC_CXI_IXE_TC_RSP_ECN_PKTS,
    HWPC_CXI_IXE_TC_RSP_NO_ECN_PKTS,
    /* List Processing Engine Performance Counters */
    HWPC_CXI_LPE_APPEND_CMDS,
    HWPC_CXI_LPE_APPEND_SUCCESS,
    HWPC_CXI_LPE_CYC_RRQ_BLOCKED,
    HWPC_CXI_LPE_NET_MATCH_LOCAL,
    HWPC_CXI_LPE_NET_MATCH_OVERFLOW,
    HWPC_CXI_LPE_NET_MATCH_PRIORITY,
    HWPC_CXI_LPE_NET_MATCH_REQUEST,
    HWPC_CXI_LPE_NET_MATCH_REQUESTS,
    HWPC_CXI_LPE_NET_MATCH_SUCCESS,
    HWPC_CXI_LPE_NET_MATCH_USEONCE,
    HWPC_CXI_LPE_NUM_TRUNCATED,
    HWPC_CXI_LPE_RNDZV_PUTS,
    HWPC_CXI_LPE_RNDZV_PUTS_OFFLOADED,
    HWPC_CXI_LPE_SEARCH_NID_ANY,
    HWPC_CXI_LPE_SEARCH_PID_ANY,
    HWPC_CXI_LPE_SEARCH_RANK_ANY,
    HWPC_CXI_LPE_SETSTATE_CMDS,
    HWPC_CXI_LPE_SETSTATE_SUCCESS,
    HWPC_CXI_LPE_UNEXPECTED_GET_AMO,
    HWPC_CXI_LPE_STS_NET_MATCH_ATTEMPTS,
    HWPC_CXI_LPE_STS_APP_MATCH_ATTEMPTS,
    HWPC_CXI_LPE_STS_NET_MAX_ATTEMPTS,
    HWPC_CXI_LPE_STS_APP_MAX_ATTEMPTS,
    HWPC_CXI_LPE_AMO_CMDS,
    HWPC_CXI_LPE_FAMO_CMDS,
    /* Management Block Performance Counters */
    HWPC_CXI_MB_CMC_AXI_RD_REQUESTS,
    HWPC_CXI_MB_CMC_AXI_WR_REQUESTS,
    HWPC_CXI_MB_CRMC_AXI_RD_REQUESTS,
    HWPC_CXI_MB_CRMC_AXI_WR_REQUESTS,
    HWPC_CXI_MB_CRMC_RD_ERROR,
    HWPC_CXI_MB_CRMC_RING_MBE,
    HWPC_CXI_MB_CRMC_RING_RD_REQUESTS,
    HWPC_CXI_MB_CRMC_RING_SBE,
    HWPC_CXI_MB_CRMC_RING_WR_REQUESTS,
    HWPC_CXI_MB_CRMC_WR_ERROR,
    /* Outbound Transfer Engine Performance Counters */
    HWPC_CXI_OXE_CHANNEL_IDLE,
    HWPC_CXI_OXE_MCU_MEAS,
    HWPC_CXI_OXE_PTL_TX_GET_MSGS_TSC,
    HWPC_CXI_OXE_PTL_TX_PUT_MSGS_TSC,
    HWPC_CXI_OXE_PTL_TX_PUT_PKTS_TSC,
    HWPC_CXI_OXE_STALL_FGFC_BLK,
    HWPC_CXI_OXE_STALL_FGFC_CNTRL,
    HWPC_CXI_OXE_STALL_FGFC_END,
    HWPC_CXI_OXE_STALL_FGFC_START,
    HWPC_CXI_OXE_STALL_IDC_NO_BUFF_BC,
    HWPC_CXI_OXE_STALL_PBUF_BC,
    HWPC_CXI_OXE_STALL_PCT_BC,
    HWPC_CXI_OXE_STALL_TS_NO_IN_CRD_TSC,
    HWPC_CXI_OXE_STALL_TS_NO_OUT_CRD_TSC,
    HWPC_CXI_OXE_STALL_WR_CONFLICT_PKT_BUFF_BNK,
    /* Processor Interface Arbiters Performance Counters */
    HWPC_CXI_PARBS_TARB_PI_POSTED_PKTS,
    HWPC_CXI_PARBS_TARB_PI_POSTED_BLOCKED_CNT,
    HWPC_CXI_PARBS_TARB_PI_NON_POSTED_PKTS,
    HWPC_CXI_PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT,
    /* Packet Connection and Tracking Performance Counters */
    HWPC_CXI_PCT_CONN_SCT_OPEN,
    HWPC_CXI_PCT_HOST_ACCESS_LATENCY,
    HWPC_CXI_PCT_MST_HIT_ON_SOM,
    HWPC_CXI_PCT_NO_TCT_NACKS,
    HWPC_CXI_PCT_NO_TRS_NACKS,
    HWPC_CXI_PCT_NO_MST_NACKS,
    HWPC_CXI_PCT_REQ_ORDERED,
    HWPC_CXI_PCT_REQ_UNORDERED,
    HWPC_CXI_PCT_REQ_RSP_LATENCY,
    HWPC_CXI_PCT_RESPONSES_RECEIVED,
    HWPC_CXI_PCT_RETRY_SRB_REQUESTS,
    HWPC_CXI_PCT_SCT_TIMEOUTS,
    HWPC_CXI_PCT_SPT_TIMEOUTS,
    HWPC_CXI_PCT_TRS_RSP_NACK_DROPS,
    /* Triggered Operations Performance Counters */
    HWPC_CXI_TOU_CT_CMD_COUNTS,
    HWPC_CXI_TOU_NUM_LIST_REBUILDS,
    HWPC_CXI_TOU_NUM_TRIG_CMDS,
    /* Retry-Handler Performance Counters */
    HWPC_CXI_SCT_TIMEOUTS,
    HWPC_CXI_SPT_TIMEOUTS,
    HWPC_CXI_SPT_TIMEOUTS_U,
    HWPC_CXI_CONNECTIONS_CANCELLED,
    HWPC_CXI_NACK_NO_MATCHING_CONN,
    HWPC_CXI_NACK_NO_TARGET_CONN,
    HWPC_CXI_NACK_NO_TARGET_MST,
    HWPC_CXI_NACK_NO_TARGET_TRS,
    HWPC_CXI_NACK_RESOURCE_BUSY,
    HWPC_CXI_NACKS,
    HWPC_CXI_NACK_SEQUENCE_ERROR,
    HWPC_CXI_PKTS_CANCELLED_O,
    HWPC_CXI_PKTS_CANCELLED_U,
    HWPC_CXI_SCT_IN_USE,
    HWPC_CXI_TCT_TIMEOUTS,
    /* This serves as the number of counters. It must be last. */
    HWPC_CXI_NUM_PREDEFINED_COUNTER_MNEMONICS
} hwpc_cxi_predefined_counter_mnemonic_id_t;

typedef struct hwpc_cxi_predefined_counter_mnemonic_obj_t {
    const char* counter_name;
    const char* counter_description;
    const bool is_per_cxi_device;
    const bool is_retry_handler_counter;
    const bool is_per_category;
    /* Number of categories, if is_per_category is true, else 1 */
    const size_t num_categories;
    /* Name of category, if is_per_category is true, else NULL */
    const char* category_name;      /* e.g. traffic-class, command-type, etc. */
} hwpc_cxi_predefined_counter_mnemonic_obj_t;

typedef struct hwpc_cxi_predefined_counter_group_obj_t {
    const char* counter_group_name;
    const char* counter_group_pretty_name;
    const char* counter_group_description;
    const hwpc_cxi_predefined_counter_mnemonic_id_t* counter_mnemonic_list;
    const size_t counter_mnemonic_list_size;
} hwpc_cxi_predefined_counter_group_obj_t;


/* Simple helper for sanity-checking predefined counter group ids */
OMPI_HIDDEN bool ompi_hook_hwpc_cxi_counter_group_id_is_valid(const hwpc_cxi_predefined_counter_group_id_t counter_group_id);

/* Function that given a predefined counter group id, returns the corresponding predefined counter group object */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_get_counter_group_obj_by_id(const hwpc_cxi_predefined_counter_group_obj_t **counter_group_obj, const hwpc_cxi_predefined_counter_group_id_t counter_group_id);

/* Function that given a predefined counter group object, returns the corresponding predefined counter group id */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_get_counter_group_id_by_obj(hwpc_cxi_predefined_counter_group_id_t *counter_group_id, const hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj);

/* Function that given a counter group name, returns the corresponding counter group object */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_get_counter_group_obj_by_name(const hwpc_cxi_predefined_counter_group_obj_t **counter_group_obj, const char *counter_group_name);

/* Function that given a predefined counter group name, returns the number of counters in the predefined group */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_get_num_counters_in_counter_group_by_name(int *total_num_counters, const char *counter_group_name);


/* Simple helper for sanity-checking predefined counter mnemonic ids */
OMPI_HIDDEN bool ompi_hook_hwpc_cxi_counter_mnemonic_id_is_valid(const hwpc_cxi_predefined_counter_mnemonic_id_t counter_mnemonic_id);

/* Function that given a predefined counter mnemonic id, returns the corresponding predefined counter mnemonic object */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_by_id(const hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, const hwpc_cxi_predefined_counter_mnemonic_id_t counter_mnemonic_id);

/* Function that given a predefined counter mnemonic object, returns the corresponding predefined counter mnemonic id */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_get_counter_mnemonic_id_by_obj(hwpc_cxi_predefined_counter_mnemonic_id_t *counter_mnemonic_id, const hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj);

/* Function that given a predefined counter mnemonic name, returns the corresponding predefined counter mnemonic object */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_by_name(const hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, const char *counter_mnemonic_name);
    
/* Function that given a low-level counter name, returns the corresponding predefined counter mnemonic object */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_for_lowlevel_counter_name(const hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, const char *counter_name);

/* Function that given a counter mnemonic name, returns the number of counters in the mnemonic */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_get_num_counters_in_counter_mnemonic_by_name(int *total_num_counters, const char *counter_mnemonic_name);

/*
 * Prints out a description of a counter group, but only the top-level information.
 * Call ompi_hook_hwpc_cxi_print_full_counter_group_description() to print out the full description of a counter group
 * including all of its nested counter mnemonics.
 */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_print_counter_group_description(FILE *ofp, const hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj);

/* Prints out a detailed description of a counter mnemonic */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_print_counter_mnemonic_description(FILE *ofp, const hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj);

/* Prints out a detailed description of a counter group plus all of its nested counter mnemonics */
OMPI_HIDDEN hwpc_cxi_error_code_t ompi_hook_hwpc_cxi_print_full_counter_group_description(FILE *ofp, const hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj);

/* Returns a string representation of the given error code */
OMPI_HIDDEN const char* ompi_hook_hwpc_cxi_error_to_string(hwpc_cxi_error_code_t error_code);

/* Returns a string representation of the given counter type*/
OMPI_HIDDEN const char* ompi_hook_hwpc_cxi_counter_type_to_string(hwpc_cxi_counter_type_t counter_type);

#endif /* HWPC_CXI_CONSTANTS_H */
