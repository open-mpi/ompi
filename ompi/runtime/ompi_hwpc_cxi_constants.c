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
#ifdef HWPC_CXI_FEATURE_MOVED_TO_MCA_HOOK_MODULE

#include "ompi_config.h"

#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <stdbool.h>

#if defined(HWPC_CXI_ENABLE) && (HWPC_CXI_ENABLE == 1) /* HWPCs for HPE's Cassini (CXI) devices are enabled */

#include "ompi/runtime/ompi_hwpc_cxi_constants.h"

#define GET_PREDEF_COUNTER_GROUP_OBJ(COUNTER_GROUP_ID) (&OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[(COUNTER_GROUP_ID)])
#define GET_PREDEF_COUNTER_MNEMONIC_OBJ(COUNTER_MNEMONIC_ID) (&OMPI_HWPC_CXI_PREDEFINED_COUNTER_MNEMONICS_LIST[(COUNTER_MNEMONIC_ID)])

/* Internal Prototypes */
static bool cxi_counter_name_string_is_valid(const char *counter_name);
static bool cxi_counter_mnemonic_id_is_valid(const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t mnemonic_id);
static bool cxi_counter_mnemonic_obj_is_valid(const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj);
static bool cxi_counter_group_obj_is_valid(const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj);

static ompi_hwpc_cxi_error_code_t cxi_get_counter_group_obj_by_name(const ompi_hwpc_cxi_predefined_counter_group_obj_t **counter_group_obj, const char *counter_group_name);
static ompi_hwpc_cxi_error_code_t cxi_get_num_counters_in_counter_group_obj(int *total_num_counters, const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj);
static ompi_hwpc_cxi_error_code_t cxi_get_num_counters_in_counter_group_by_name(int *total_num_counters, const char *counter_group_name);

static ompi_hwpc_cxi_error_code_t cxi_get_counter_mnemonic_obj_by_id(const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, ompi_hwpc_cxi_predefined_counter_mnemonic_id_t mnemonic_id);
static ompi_hwpc_cxi_error_code_t cxi_get_counter_mnemonic_obj_by_name(const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, const char *mnemonic_name);
static ompi_hwpc_cxi_error_code_t cxi_get_num_counters_in_counter_mnemonic_obj(int *total_num_counters, const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj);
static ompi_hwpc_cxi_error_code_t cxi_get_num_counters_in_counter_mnemonic_by_name(int *total_num_counters, const char *counter_mnemonic_name);

static ompi_hwpc_cxi_error_code_t cxi_print_counter_group_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj);
static ompi_hwpc_cxi_error_code_t cxi_print_counter_mnemonic_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj);
static ompi_hwpc_cxi_error_code_t cxi_print_full_counter_group_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj);

/* External API */

bool ompi_hwpc_cxi_counter_mnemonic_id_is_valid(const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t counter_mnemonic_id)
{
    return cxi_counter_mnemonic_id_is_valid(counter_mnemonic_id);
}

/* Function that, given a counter group name, returns the corresponding counter group object */
ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_get_counter_group_obj_by_name(const ompi_hwpc_cxi_predefined_counter_group_obj_t **counter_group_obj, const char *counter_group_name)
{
    return cxi_get_counter_group_obj_by_name(counter_group_obj, counter_group_name);
}

/* Function that, given a counter group name, returns the number of counters in the group */
ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_get_num_counters_in_counter_group_by_name(int *total_num_counters, const char *counter_group_name)
{
    return cxi_get_num_counters_in_counter_group_by_name(total_num_counters, counter_group_name);
}

/* Function that, given a counter mnemonic id, returns the corresponding counter mnemonic object */
ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_get_counter_mnemonic_obj_by_id(const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t mnemonic_id)
{
    return cxi_get_counter_mnemonic_obj_by_id(counter_mnemonic_obj, mnemonic_id);
}

/* Function that, given a counter mnemonic name, returns the corresponding counter mnemonic object */
ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_get_counter_mnemonic_obj_by_name(const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, const char *mnemonic_name)
{
    return cxi_get_counter_mnemonic_obj_by_name(counter_mnemonic_obj, mnemonic_name);
}

/* Function that, given a counter mnemonic name, returns the number of counters in the mnemonic */
ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_get_num_counters_in_counter_mnemonic_by_name(int *total_num_counters, const char *counter_mnemonic_name)
{
    return cxi_get_num_counters_in_counter_mnemonic_by_name(total_num_counters, counter_mnemonic_name);
}

/* 
 * Prints out a description of a counter group, but only the top-level information.
 * Call ompi_hwpc_cxi_print_full_counter_group_description() to print out the full description of a counter group 
 * including all of its nested counter mnemonics.
 */
ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_print_counter_group_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj)
{
    return cxi_print_counter_group_description(ofp, counter_group_obj);
}

/* Prints out a detailed description of a counter mnemonic */
ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_print_counter_mnemonic_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj)
{
    return cxi_print_counter_mnemonic_description(ofp, counter_mnemonic_obj);
}

/* Prints out a detailed description of a counter group plus all of its nested counter mnemonics */
ompi_hwpc_cxi_error_code_t ompi_hwpc_cxi_print_full_counter_group_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj)
{
    return cxi_print_full_counter_group_description(ofp, counter_group_obj);
}

/* Returns a string representation of the given error code */
const char* ompi_hwpc_cxi_error_to_string(ompi_hwpc_cxi_error_code_t error_code)
{
    switch (error_code) {
        case HWPC_CXI_SUCCESS:                          return "Success";
        case HWPC_CXI_ERROR:                            return "General error";
        case HWPC_CXI_ERROR_INVALID_ARGUMENTS:          return "Invalid arguments";
        case HWPC_CXI_ERROR_OUT_OF_MEMORY:              return "Out of memory";
        case HWPC_CXI_COUNTER_GROUP_NOT_FOUND:          return "Counter group not found";
        case HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND:       return "Counter mnemonic not found";
        default:                                        return "Unknown error code";
    }
}

/* Returns a string representation of the given counter type*/
const char* ompi_hwpc_cxi_counter_type_to_string(ompi_hwpc_cxi_counter_type_t counter_type)
{
    switch (counter_type) {
        case HWPC_CXI_COUNTER_LOWLEVEL_TYPE:            return "Low-Level Counter";
        case HWPC_CXI_COUNTER_MNEMONIC_TYPE:            return "Counter Mnemonic";
        case HWPC_CXI_COUNTER_GROUP_TYPE:               return "Counter Group";
        case HWPC_CXI_COUNTER_UNKNOWN_TYPE:             return "Unknown counter type";
        default:                                        return "Unknown counter type";
    }
}

/* A list containing all of the performance counters for the CxiPerfStats group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_PERFSTATS_COUNTER_GROUP_LIST[] = {
    HNI_PKTS_SENT_BY_TC,
    HNI_PKTS_RECV_BY_TC,
    HNI_TX_PAUSED,
    HNI_RX_PAUSED,
    PARBS_TARB_PI_POSTED_PKTS,
    PARBS_TARB_PI_POSTED_BLOCKED_CNT,
    PARBS_TARB_PI_NON_POSTED_PKTS,
    PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT,
    LPE_NET_MATCH_PRIORITY,
    LPE_NET_MATCH_OVERFLOW,
    ATU_CACHE_MISS,
    ATU_CACHE_EVICTIONS
};

/*  A list containing all of the performance counters for the CxiErrStats group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_ERRSTATS_COUNTER_GROUP_LIST[] = {
    PCT_SPT_TIMEOUTS,
    PCT_SCT_TIMEOUTS,
    PCT_NO_TCT_NACKS,
    PCT_NO_TRS_NACKS,
    PCT_NO_MST_NACKS,
    PCT_RETRY_SRB_REQUESTS,
    PCT_TRS_RSP_NACK_DROPS,
    HNI_PCS_UNCORRECTED_CW,
    HNI_LLR_TX_REPLAY_EVENT,
    HNI_LLR_RX_REPLAY_EVENT
};

/* A list containing all of the performance counters for the CxiOpCommands group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_OPCOMMANDS_COUNTER_GROUP_LIST[] = {
    CQ_DMA_CMD_COUNTS,
    CQ_CQ_CMD_COUNTS,
    CQ_NUM_DMA_CMDS,
    CQ_NUM_IDC_CMDS
};

/* A list containing all of the performance counters for the CxiOpPackets group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_OPPACKETS_COUNTER_GROUP_LIST[] = {
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
    HNI_PKTS_SENT_BY_TC,
    HNI_PKTS_RECV_BY_TC
};

/*  A list containing all of the performance counters for the CxiDmaEngine group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_DMAENGINE_COUNTER_GROUP_LIST[] = {
    OXE_MCU_MEAS,
    OXE_CHANNEL_IDLE,
    IXE_DISP_DMAWR_REQS,
    IXE_DMAWR_STALL_P_CDT,
    IXE_DMAWR_STALL_NP_CDT,
    PI_PTI_TARB_MRD_PKTS,
    PI_PTI_TARB_MWR_PKTS
};

/* A list containing all of the performance counters for the CxiWritesToHost group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_WRITESTOHOST_COUNTER_GROUP_LIST[] = {
    PARBS_TARB_PI_POSTED_PKTS,
    PARBS_TARB_PI_POSTED_BLOCKED_CNT,
    PARBS_TARB_PI_NON_POSTED_PKTS,
    PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT
};

/* A list containing all of the performance counters for the CxiMessageMatchingPooled group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_MESSAGEMATCHINGPOOLED_COUNTER_GROUP_LIST[] = {
    LPE_NET_MATCH_REQUESTS,
    LPE_NET_MATCH_PRIORITY,
    LPE_NET_MATCH_OVERFLOW,
    LPE_NET_MATCH_REQUEST,
    LPE_APPEND_CMDS,
    LPE_SEARCH_NID_ANY,
    LPE_SEARCH_PID_ANY,
    LPE_SEARCH_RANK_ANY,
    LPE_RNDZV_PUTS,
    LPE_AMO_CMDS,
    LPE_FAMO_CMDS
};

/* A list containing all of the performance counters for the CxiTranslationUnit group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_TRANSLATIONUNIT_COUNTER_GROUP_LIST[] = {
    ATU_CACHE_MISS,
    ATU_CLIENT_REQ_EE,
    ATU_CLIENT_REQ_IXE,
    ATU_CLIENT_REQ_OXE,
    ATU_CACHE_MISS_EE,
    ATU_CACHE_MISS_IXE,
    ATU_CACHE_MISS_OXE,
    ATU_CACHE_EVICTIONS,
    ATU_CACHE_HIT_BASE_PAGE_SIZE,
    ATU_CACHE_HIT_DERIVATIVE1_PAGE_SIZE,
    ATU_ATS_TRANS_LATENCY
};

/* A list containing all of the performance counters for the CxiLatencyHist group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_LATENCYHIST_COUNTER_GROUP_LIST[] = {
    PCT_HOST_ACCESS_LATENCY,
    PCT_REQ_RSP_LATENCY
};

/* A list containing all of the performance counters for the CxiPctReqRespTracking group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_PCTREQRESPTRACKING_COUNTER_GROUP_LIST[] = {
    PCT_REQ_ORDERED,
    PCT_REQ_UNORDERED,
    PCT_RESPONSES_RECEIVED,
    PCT_CONN_SCT_OPEN,
    PCT_NO_TRS_NACKS,
    PCT_RETRY_SRB_REQUESTS,
    PCT_SPT_TIMEOUTS,
    PCT_SCT_TIMEOUTS
};

/* A list containing all of the performance counters for the CxiLinkReliability group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_LINKRELIABILITY_COUNTER_GROUP_LIST[] = {
    HNI_PCS_GOOD_CW,
    HNI_PCS_CORRECTED_CW,
    HNI_PCS_UNCORRECTED_CW,
    HNI_LLR_TX_REPLAY_EVENT,
    HNI_LLR_RX_REPLAY_EVENT
};

/* A list containing all of the performance counters for the CxiCongestion group */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t CXI_CONGESTION_COUNTER_GROUP_LIST[] = {
    PARBS_TARB_PI_POSTED_PKTS,
    PARBS_TARB_PI_POSTED_BLOCKED_CNT,
    PARBS_TARB_PI_NON_POSTED_PKTS,
    PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT,
    HNI_RX_PAUSED,
    HNI_RX_PAUSED_STD,
    HNI_TX_PAUSED
};

static const size_t CXI_PERFSTATS_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_PERFSTATS_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_ERRSTATS_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_ERRSTATS_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_OPCOMMANDS_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_OPCOMMANDS_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_OPPACKETS_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_OPPACKETS_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_DMAENGINE_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_DMAENGINE_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_WRITESTOHOST_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_WRITESTOHOST_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_MESSAGEMATCHINGPOOLED_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_MESSAGEMATCHINGPOOLED_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_TRANSLATIONUNIT_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_TRANSLATIONUNIT_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_LATENCYHIST_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_LATENCYHIST_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_PCTREQRESPTRACKING_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_PCTREQRESPTRACKING_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_LINKRELIABILITY_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_LINKRELIABILITY_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);
static const size_t CXI_CONGESTION_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_CONGESTION_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_id_t);

#define HAS_CATEGORIES true
#define IS_STANDALONE false

#define SET_PREDEF_COUNTER(COUNTER_MNEMONIC_NAME, DESC, PER_DEV, RETRY_HANDLER, PER_CAT, NUM_CAT, CAT_NAME)   [COUNTER_MNEMONIC_NAME] = { .counter_name = #COUNTER_MNEMONIC_NAME, \
                                                            .counter_description = DESC, .is_per_cxi_device = PER_DEV, .is_retry_handler_counter = RETRY_HANDLER, \
                                                            .is_per_category = PER_CAT, .num_categories = NUM_CAT, .category_name = CAT_NAME }

/*
 * This array contains all predefined Cassini hardware performance counter descriptions,
 * providing metadata for a short, but incomplete, collection of hardware performance counters supported.
 */
static const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t OMPI_HWPC_CXI_PREDEFINED_COUNTER_MNEMONICS_LIST[] = {
    /* Address Translation Unit Performance Counters */
    SET_PREDEF_COUNTER(ATU_ATS_PRS_ODP_LATENCY, "ATS Page Request Services On-Demand Paging latency histogram. Four bins defined in C_ATU_CFG_ODP_HIST.", true, false, HAS_CATEGORIES, 4, "Histogram-Bin"),
    SET_PREDEF_COUNTER(ATU_ATS_TRANS_LATENCY, "ATS Translation latency histogram. Four bins defined in C_ATU_CFG_XLATION_HIST.", true, false, HAS_CATEGORIES, 4, "Histogram-Bin"),
    SET_PREDEF_COUNTER(ATU_CACHE_HIT_BASE_PAGE_SIZE, "Number of cache hits observed on the Base Page Size.", true, false, HAS_CATEGORIES, 4, "Page-Size"),
    SET_PREDEF_COUNTER(ATU_CACHE_HIT_DERIVATIVE1_PAGE_SIZE, "Number of cache hits observed on the Derivative 1 Page Size.", true, false, HAS_CATEGORIES, 4, "Page-Size"),
    SET_PREDEF_COUNTER(ATU_CACHE_HIT_DERIVATIVE2_PAGE_SIZE, "Number of cache hits observed on the Derivative 2 Page Size.", true, false, HAS_CATEGORIES, 4, "Page-Size"),
    SET_PREDEF_COUNTER(ATU_CACHE_MISS, "Number of cache misses by counter pool. Four counters of which counter 0 counts misses on 4K pages and counter 1 counts misses on 2M pages by default.", true, false, HAS_CATEGORIES, 4, "Page-Size"),
    SET_PREDEF_COUNTER(ATU_CACHE_MISS_EE, "Number of cache misses by client (events).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CACHE_MISS_IXE, "Number of cache misses by client (writes).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CACHE_MISS_OXE, "Number of cache misses by client (reads).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CACHE_EVICTIONS, "Number of times a tag was evicted from the NIC translation cache to make room for a new tag.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CLIENT_REQ_EE, "Number of translation requests by client (events).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CLIENT_REQ_IXE, "Number of translation requests by client (writes).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CLIENT_REQ_OXE, "Number of translation requests by client (reads).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CLIENT_RSP_NOT_OK, "Number of client responses that were not RC_OK.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(ATU_NIC_PRI_ODP_LATENCY, "NIC Page Request Interface On-Demand Paging latency histogram.", true, false, HAS_CATEGORIES, 4, "Histogram-Bin"),
    SET_PREDEF_COUNTER(ATU_NTA_TRANS_LATENCY, "NTA Translation latency histogram.", true, false, HAS_CATEGORIES, 4, "Histogram-Bin"),
    SET_PREDEF_COUNTER(ATU_ODP_REQUESTS, "Number of On-Demand Paging requests.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    /* Command Queue Performance Counters */
    SET_PREDEF_COUNTER(CQ_CQ_CMD_COUNTS, "Number of CQ commands of each type: See Cassini Performance Counter Guide for more details", true, false, HAS_CATEGORIES, 16, "Command-Type"),
    SET_PREDEF_COUNTER(CQ_DMA_CMD_COUNTS, "Number of DMA commands of each type: See Cassini Performance Counter Guide for more details", true, false, HAS_CATEGORIES, 16, "Command-Type"),
    SET_PREDEF_COUNTER(CQ_CYCLES_BLOCKED, "Number of cycles the pool had a command ready to send to OXE and could not make progress because another OCUSET won arbitration.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_CQ_CMDS,"Number of successfully parsed CQ commands processed by the CQ block.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_DMA_CMDS, "Number of successfully parsed DMA commands.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_IDC_CMDS, "Number of successfully parsed immediate data commands.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_LL_CMDS, "Number of successfully parsed CQ commands processed by the CQ block.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_LL_OPS_RECEIVED, "The number of error-free low-latency operations received.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_LL_OPS_REJECTED, "The number of low-latency operations received for which the data of the operation was not accepted.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_LL_OPS_SPLIT, "The number of low-latency operations received for which the data of the operation was not accepted because delivery of that data was split.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_LL_OPS_SUCCESSFUL, "The number of low-latency operations for which the data of the operation was accepted.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_TGQ_CMD_READS,"Number of PCIe command reads issued for target prefetch queues. Four counters, one each for reads of 64, 128, 192, or 256 bytes.", true, false, HAS_CATEGORIES, 4, "Data-Size"),
    SET_PREDEF_COUNTER(CQ_NUM_TGT_CMDS, "Number of successfully parsed CQ commands processed by target command queues. All target commands are single flit. Incremented as target commands are sent to LPE.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_TOU_CMD_READS, "Number of PCIe command reads issued for TOU prefetch queues. Four counters, one each for reads of 64, 128, 192, or 256 bytes.", true, false, HAS_CATEGORIES, 4, "Data-Size"),
    SET_PREDEF_COUNTER(CQ_NUM_TXQ_CMD_READS, "Number of PCIe command reads issued for transmit prefetch queues. Four counters, one each for reads of 64, 128, 192, or 256 bytes.", true, false, HAS_CATEGORIES, 4, "Data-Size"),
    SET_PREDEF_COUNTER(CQ_TGT_WAITING_ON_READ, "Cycles on which target prefetch buffers are empty and pool has read requests pending. Note that this counter does not increment on cycles for which commands are being processed in another pool.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_TX_WAITING_ON_READ, "Cycles in which transmit prefetch buffers are empty and pool has read requests pending. Note that this counter does not increment on cycles for which commands in another pool are being processed.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    /* Event Engine Performance Counters */
    SET_PREDEF_COUNTER(EE_ADDR_TRANS_PREFETCH_CNTR, "Number of prefetched address translations.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(EE_CBS_WRITTEN_CNTR, "Number of combining buffers written to an event queue.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(EE_DEFERRED_EQ_SWITCH_CNTR, "Number of event queue buffer switches not performed as soon as requested due to insufficient old event queue buffer free space immediately available to enqueue the buffer switch event.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(EE_EQ_BUFFER_SWITCH_CNTR, "Number of event queue buffer switches performed.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(EE_EQ_STATUS_UPDATE_CNTR, "Number of status updates written to an event queue.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(EE_EQ_SW_STATE_WR_CNTR, "Number of times the event queue software state is updated using a fast path write.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(EE_EVENTS_DROPPED_FC_SC_CNTR, "Number of flow control state-change full events that were not enqueued because the event queue was full.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(EE_EVENTS_DROPPED_ORDINARY_CNTR, "Number of full events that were not enqueued because the event queue was full.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(EE_EVENTS_DROPPED_RSRVN_CNTR, "Number of full events, subject to an event queue space reservation, which were not enqueued because the event queue was full.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(EE_EVENTS_ENQUEUED_CNTR, "Number of full events enqueued to an event queue.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(EE_EXPIRED_CBS_WRITTEN_CNTR, "Number of partially full combining buffers written to their event queue because too much time elapsed without additional events arriving to fill the buffer.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(EE_PARTIAL_CBS_WRITTEN_CNTR, "Number of partially full combining buffers written to an event queue.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    /* High-Speed Network Interface Performance Counters */
    SET_PREDEF_COUNTER(HNI_DISCARD_CNTR, "Number of packets discarded due to a timeout for each traffic class as indicated by DISCARD_ERR.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_LLR_TX_REPLAY_EVENT, "Number of LLR replays.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_LLR_RX_REPLAY_EVENT, "Number of LLR replays.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_MULTICAST_PKTS_RECV_BY_TC, "Number of multicast packets with good FCS received by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_MULTICAST_PKTS_SENT_BY_TC, "Number of multicast packets with good FCS sent by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_PAUSE_RECV, "Number of pause frames received for each enabled PCP (as identified by the PEV field of the pause frame) when PFC pause is enabled.", true, false, HAS_CATEGORIES, 8, "Unknown"),
    SET_PREDEF_COUNTER(HNI_PAUSE_XOFF_SENT, "Number of pause frames sent where XOFF is indicated for each PCP when PFC pause is enabled.", true, false, HAS_CATEGORIES, 8, "Unknown"),
    SET_PREDEF_COUNTER(HNI_PCS_CORRECTED_CW, "Number of corrected codewords.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_PCS_FECL_ERRORS, "Number of errors in each FECL.", true, false, HAS_CATEGORIES, 8, "Unknown"),
    SET_PREDEF_COUNTER(HNI_PCS_GOOD_CW, "Number of codewords received with no errors.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_PCS_UNCORRECTED_CW, "Number of uncorrected code words received on the switch to NIC link.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_PFC_FIFO_OFLW_CNTR, "Number of packets discarded at the tail of the PFC FIFO.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_PKTS_RECV_BY_TC, "Number of packets received by traffic class <n>.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_PKTS_SENT_BY_TC, "Number of packets sent by traffic class <n>.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    /* High-Speed Network Interface (Receive) Performance Counters */
    SET_PREDEF_COUNTER(HNI_RX_OK_27, "Number of packets received in the 27 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_35, "Number of packets received in the 35 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_64, "Number of packets received in the 64 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_36_to_63, "Number of packets received in the 36 to 63 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_65_to_127, "Number of packets received in the 65 to 127 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_128_to_255, "Number of packets received in the 128 to 255 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_256_to_511, "Number of packets received in the 256 to 511 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_512_to_1023, "Number of packets received in the 512 to 1023 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_1024_to_2047, "Number of packets received in the 1024 to 2047 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_2048_to_4095, "Number of packets received in the 2048 to 4095 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_4096_to_8191, "Number of packets received in the 4096 to 8191 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_8192_to_max, "Number of packets received in the 8192 to max bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_PAUSED, "Number of cycles in which the pause is applied on the receive path for traffic class <n>.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_RX_PAUSED_STD, "Number of cycles in which at least one PCP pause occurred.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_STALL_IXE_PKTBUF, "Number of system clocks for which the Rx path of the corresponding traffic class is stalled due to lack of space in the IXE Packet Buffer.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    /* High-Speed Network Interface (Transmit) Performance Counters */
    SET_PREDEF_COUNTER(HNI_TX_OK_27, "Number of packets sent in the 27 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_35, "Number of packets sent in the 35 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_64, "Number of packets sent in the 64 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_36_to_63, "Number of packets sent in the 36 to 63 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_65_to_127, "Number of packets sent in the 65 to 127 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_128_to_255, "Number of packets sent in the 128 to 255 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_256_to_511, "Number of packets sent in the 256 to 511 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_512_to_1023, "Number of packets sent in the 512 to 1023 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_1024_to_2047, "Number of packets sent in the 1024 to 2047 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_2048_to_4095, "Number of packets sent in the 2048 to 4095 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_4096_to_8191, "Number of packets sent in the 4096 to 8191 bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_8192_to_max, "Number of packets sent in the 8192 to max bytes size bin.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_PAUSED, "Number of cycles in which the transmit path is paused for traffic class <n>.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    /* Inbound Transfer Engine Performance Counters */
    SET_PREDEF_COUNTER(IXE_DISP_DMAWR_REQS, "Number of requests to the IXE dispatcher for DMA write operations.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(IXE_DMAWR_STALL_NP_CDT, "Number of stalls due to no non-posted credits (cycles).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(IXE_DMAWR_STALL_P_CDT, "Number of stalls due to no posted credits (cycles).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(IXE_POOL_ECN_PKTS, "Number of packets with ECN set.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(IXE_POOL_NO_ECN_PKTS, "Number of packets without ECN set.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(PI_PTI_TARB_MRD_PKTS, "Number of memory read TLPs (all source).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PI_PTI_TARB_MWR_PKTS, "Number of memory write TLPs (all source).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(IXE_TC_REQ_ECN_PKTS, "Number of request packets with ECN set, by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(IXE_TC_REQ_NO_ECN_PKTS, "Number of request packets without ECN set, by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(IXE_TC_RSP_ECN_PKTS, "Number of response packets with ECN set, by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(IXE_TC_RSP_NO_ECN_PKTS, "Number of response packets without ECN set, by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    /* List Processing Engine Performance Counters */
    SET_PREDEF_COUNTER(LPE_APPEND_CMDS, "Number of Append commands received. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_APPEND_SUCCESS, "Number of Append commands LPE successfully completed. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_CYC_RRQ_BLOCKED, "Number of cycles in which a PE Match Request Queue dequeue was blocked because a Ready Request Queue was full.", true, false, HAS_CATEGORIES, 4, "PE-Index"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_LOCAL, "Number of network requests LPE successfully matched to locally managed buffers. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_OVERFLOW, "Number of messages where payload data was delivered to a buffer on the overflow list because there was no match on the priority list. Four counters of which 0 is the default.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_PRIORITY, "Number of messages matched on the priority list. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_REQUEST, "The number of network requests LPE successfully matched on the Request list. One counter for each of the four pools of PtlTEs. (software endpoints)", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_REQUESTS, "The number of network requests LPE received. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_SUCCESS, "Number of network requests LPE successfully completed. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_USEONCE, "Number of network requests LPE successfully matched to use-once buffers. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NUM_TRUNCATED, "Number of truncated packets. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_RNDZV_PUTS, "Number of rendezvous puts received. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_RNDZV_PUTS_OFFLOADED, "Number of Rendezvous Puts that LPE was able to offload. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_SEARCH_NID_ANY, "Number of wildcard searches using NID_ANY, physical matching. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_SEARCH_PID_ANY, "Number of wildcard searches using PID_ANY, physical matching. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_SEARCH_RANK_ANY, "Number of wildcard searches using RANK_ANY, logical matching. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_SETSTATE_CMDS, "Number of SetState commands LPE received. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_SETSTATE_SUCCESS, "Number of successful SetState commands. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_UNEXPECTED_GET_AMO, "Number of Get and AMO packets that match on Overflow or Request, resulting in RC_DELAYED.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_STS_NET_MATCH_ATTEMPTS, "Number of match attempts for inbound network headers in each pool. Indexed by CNTR_POOL.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(LPE_STS_APP_MATCH_ATTEMPTS, "Number of comparisons of Appended list entries to unexpected headers in each pool. Indexed by CNTR_POOL.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(LPE_STS_NET_MAX_ATTEMPTS, "Maximum number of match attempts needed for an inbound network header in each pool. Indexed by CNTR_POOL.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(LPE_STS_APP_MAX_ATTEMPTS, "Maximum number of match attempts needed for an Append command in each pool. Indexed by CNTR_POOL.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(LPE_AMO_CMDS, "Number of non-fetching AMO commands received by LPE.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(LPE_FAMO_CMDS, "Number of fetching AMO commands received by LPE.", true, false, IS_STANDALONE, 1, NULL),
    /* Management Block Performance Counters */
    SET_PREDEF_COUNTER(MB_CMC_AXI_RD_REQUESTS, "Number of AXI read requests received at the CMC channels.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(MB_CMC_AXI_WR_REQUESTS, "Number of AXI write requests received at the CMC channels.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(MB_CRMC_AXI_RD_REQUESTS, "Number of CRMC AXI read requests.", true, false, HAS_CATEGORIES, 3, "Unknown"),
    SET_PREDEF_COUNTER(MB_CRMC_AXI_WR_REQUESTS, "Number of CRMC AXI write requests.", true, false, HAS_CATEGORIES, 3, "Unknown"),
    SET_PREDEF_COUNTER(MB_CRMC_RD_ERROR, "Number of CRMC read errors.", true, false, HAS_CATEGORIES, 3, "Unknown"),
    SET_PREDEF_COUNTER(MB_CRMC_RING_MBE, "Number of CRMC ring in multi bit errors.", true, false, HAS_CATEGORIES, 3, "Unknown"),
    SET_PREDEF_COUNTER(MB_CRMC_RING_RD_REQUESTS, "Number of CRMC ring out read requests.", true, false, HAS_CATEGORIES, 3, "Unknown"),
    SET_PREDEF_COUNTER(MB_CRMC_RING_SBE, "Number of CRMC ring in single bit errors.", true, false, HAS_CATEGORIES, 3, "Unknown"),
    SET_PREDEF_COUNTER(MB_CRMC_RING_WR_REQUESTS, "Number of CRMC ring out write requests.", true, false, HAS_CATEGORIES, 3, "Unknown"),
    SET_PREDEF_COUNTER(MB_CRMC_WR_ERROR, "Number of CRMC write errors.", true, false, HAS_CATEGORIES, 3, "Unknown"),
    /* Outbound Transfer Engine Performance Counters */
    SET_PREDEF_COUNTER(OXE_CHANNEL_IDLE, "Number of cycles in which available bandwidth is not used.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(OXE_MCU_MEAS, "Number of flits sent by each MCU (configurable to count packets or messages).", true, false, HAS_CATEGORIES, 96, "Unknown"),
    SET_PREDEF_COUNTER(OXE_PTL_TX_GET_MSGS_TSC, "Number of portal Get messages sent by traffic shaping class. The following OXE pkt types (c_oxe_pkt_type) are included: PKT_GET where SOM is set. To be counted in the MCUOBC.", true, false, HAS_CATEGORIES, 10, "Unknown"),
    SET_PREDEF_COUNTER(OXE_PTL_TX_PUT_MSGS_TSC, "Number of clocks a TSC is not allowed to participate in traffic shaping arbitration because it is waiting for outbound shaping tokens. One counter for each TSC.", true, false, HAS_CATEGORIES, 10, "Unknown"),
    SET_PREDEF_COUNTER(OXE_PTL_TX_PUT_PKTS_TSC, "Number of portal packets by traffic shaping class. Counted either in the MCUOBC after the MCU is granted or at the traffic shaper after packet is selected.", true, false, HAS_CATEGORIES, 10, "Unknown"),
    SET_PREDEF_COUNTER(OXE_STALL_FGFC_BLK, "Number of cycles blocked for FGFC (matching entry exists and Credit >=0). Index given by CSR C_OXE_CFG_FGFC_CNT[4].", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(OXE_STALL_FGFC_CNTRL, "Number of cycles blocked for FGFC (matching entry exists). Index given by CSR C_OXE_CFG_FGFC_CNT[4].", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(OXE_STALL_FGFC_END, "Number of FGFC frames received with matching VNI that end FGFC (period == 0). Index given by CSR C_OXE_CFG_FGFC_CNT[4].", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(OXE_STALL_FGFC_START, "Number of FGFC frames received with matching VNI that start or continue FGFC (period != 0). Index given by CSR C_OXE_CFG_FGFC_CNT[4].", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(OXE_STALL_IDC_NO_BUFF_BC, "Number of cycles IDC command is enqueued (into ENQ FIFO) but cannot get buffer. Counted per BC. This is an OR of all MCUs belonging to the same BC with an IDC command but does not have a cell (buffer) allocated.", true, false, HAS_CATEGORIES, 10, "Unknown"),
    SET_PREDEF_COUNTER(OXE_STALL_PBUF_BC, "Number of clocks a BC is not allowed to participate in MCUOBC arbitration because it is waiting for packet buffer resources. One counter for each BC. Count in PKTBUFF_REQ state.", true, false, HAS_CATEGORIES, 10, "Unknown"),
    SET_PREDEF_COUNTER(OXE_STALL_PCT_BC, "Number of clocks a BC is not allowed to participate in MCUOBC arbitration because it is waiting for PCT resources. One counter for each BC. Count in PKTBUFF_REQ state.", true, false, HAS_CATEGORIES, 10, "Unknown"),
    SET_PREDEF_COUNTER(OXE_STALL_TS_NO_IN_CRD_TSC, "Number of clocks a TSC is not allowed to participate in traffic shaping arbitration because it is waiting for inbound shaping tokens. One counter for each TSC.", true, false, HAS_CATEGORIES, 10, "Unknown"),
    SET_PREDEF_COUNTER(OXE_STALL_TS_NO_OUT_CRD_TSC, "Number of clocks a TSC is not allowed to participate in traffic shaping arbitration because it is waiting for outbound shaping tokens. One counter for each TSC.", true, false, HAS_CATEGORIES, 10, "Unknown"),
    SET_PREDEF_COUNTER(OXE_STALL_WR_CONFLICT_PKT_BUFF_BNK, "Number of cycles header write or IDC write collides with PCIe data return. Per bank of Packet buffer.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    /* Processor Interface Arbiters Performance Counters */
    SET_PREDEF_COUNTER(PARBS_TARB_PI_POSTED_PKTS, "Number of PCIe packets transferred using the posted path (for example, writes)", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PARBS_TARB_PI_POSTED_BLOCKED_CNT, "Number of cycles in which the posted path is blocked. Compute the ratio cycles/pkts. Values of more than a few cycles per packet indicate back pressure from the host. This endpoint is likely to be the cause of congestion.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PARBS_TARB_PI_NON_POSTED_PKTS, "Number of PCIe packets transferred using the non-posted path (for example, reads)", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT, "Number of cycles in which the non-posted path is blocked. Compute the ratio cycles/pkts. Values of more than a few cycles per packet indicate per host performance (high read latencies). This endpoint is likely to be injecting at a low rate.", true, false, IS_STANDALONE, 1, NULL),
    /* Packet Connection and Tracking Performance Counters */
    /* FIXME: description of PCT_HOST_ACCESS_LATENCY and PCT_REQ_RSP_LATENCY are mistakenly swapped in User Guide */
    SET_PREDEF_COUNTER(PCT_CONN_SCT_OPEN, "Number of open requests.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_HOST_ACCESS_LATENCY, "Host access latency histogram, 16 bins.", true, false, HAS_CATEGORIES, 16, "Unknown"),
    SET_PREDEF_COUNTER(PCT_MST_HIT_ON_SOM, "Number of times an MST entry already exists for a request of a new message that needs a new MST entry.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_NO_TCT_NACKS, "Number of resource exhaustion NACKs. Retry handler is invoked.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_NO_TRS_NACKS, "Number of resource exhaustion NACKs. Retry handler is invoked.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_NO_MST_NACKS, "Number of resource exhaustion NACKs. Retry handler is invoked.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_REQ_ORDERED, "Number of ordered requests.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_REQ_UNORDERED, "Number of unordered requests.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_REQ_RSP_LATENCY, "Request/response latency histogram, 32 bins", true, false, HAS_CATEGORIES, 32, "Unknown"),
    SET_PREDEF_COUNTER(PCT_RESPONSES_RECEIVED, "Number of responses received (all unordered).", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_RETRY_SRB_REQUESTS, "Number of retries.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_SCT_TIMEOUTS, "Number of response timeouts (or packet loss in the network). Retry handler is invoked.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_SPT_TIMEOUTS, "Number of response timeouts, (packet loss in the network). Retry handler is invoked.", true, false, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PCT_TRS_RSP_NACK_DROPS, "Number of NACKs dropped. Retry handler is invoked.", true, false, IS_STANDALONE, 1, NULL),
    /* Triggered Operations Performance Counters */
    SET_PREDEF_COUNTER(TOU_CT_CMD_COUNTS, "Number of the successfully validated commands in C_CT_OP_T. Offset into the counter array is equal to the OpCode.", true, false, HAS_CATEGORIES, 16, "Command-OpCode"),
    SET_PREDEF_COUNTER(TOU_NUM_LIST_REBUILDS, "Number of list CT list rebuilds. Pooled counter, pool is equal to the PFQ number.", true, false, HAS_CATEGORIES, 4, "PFQ-Number"),
    SET_PREDEF_COUNTER(TOU_NUM_TRIG_CMDS, "Number of triggered commands. Pooled counter, pool is equal to the PFQ number.", true, false, HAS_CATEGORIES, 4, "PFQ-Number"),
    /* Retry-Handler Performance Counters */
    SET_PREDEF_COUNTER(SCT_TIMEOUTS, "Number of SCT timeouts.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(SPT_TIMEOUTS, "Number of SPT timeouts.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(SPT_TIMEOUTS_U, "Number of SPT timeouts.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(CONNECTIONS_CANCELLED, "Number of connections cancelled.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(NACK_NO_MATCHING_CONN, "Number of NACKs due to no matching connection.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(NACK_NO_TARGET_CONN, "Number of NACKs due to no target connection.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(NACK_NO_TARGET_MST, "Number of NACKs due to no target MST.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(NACK_NO_TARGET_TRS, "Number of NACKs due to no target TRS.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(NACK_RESOURCE_BUSY, "Number of NACKs due to resource busy.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(NACKS, "Total number of NACKs.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(NACK_SEQUENCE_ERROR, "Number of NACKs due to sequence error.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PKTS_CANCELLED_O, "Number of packets cancelled (ordered).", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(PKTS_CANCELLED_U, "Number of packets cancelled (unordered).", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(SCT_IN_USE, "Number of SCTs currently in use.", true, true, IS_STANDALONE, 1, NULL),
    SET_PREDEF_COUNTER(TCT_TIMEOUTS, "Number of TCT timeouts.", true, true, IS_STANDALONE, 1, NULL)
};
static const size_t OMPI_HWPC_CXI_PREDEFINED_COUNTER_MNEMONICS_LIST_SIZE = sizeof(OMPI_HWPC_CXI_PREDEFINED_COUNTER_MNEMONICS_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t);

#undef SET_PREDEF_COUNTER
#undef HAS_CATEGORIES
#undef IS_STANDALONE

#define SET_PREDEF_COUNTER_GROUP(NAME, STRING_NAME, DESC, COUNTER_LIST, COUNTER_LIST_SIZE)   [NAME] = { .counter_group_name = #NAME, .counter_group_pretty_name = STRING_NAME, \
                                                             .counter_group_description = DESC, .counter_mnemonic_list = COUNTER_LIST, \
                                                             .counter_mnemonic_list_size = COUNTER_LIST_SIZE }

/*
 * This array contains all predefined Cassini (CX) hardware performance counter group descriptions, providing metadata for
 * complete collection of hardware performance counter groups supported at the time of writing.
 */
static const ompi_hwpc_cxi_predefined_counter_group_obj_t OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[] = {
    SET_PREDEF_COUNTER_GROUP(CXI_PERFSTATS, "CxiPerfStats", "Traffic Congestion Counter Group", CXI_PERFSTATS_COUNTER_GROUP_LIST, CXI_PERFSTATS_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_ERRSTATS, "CxiErrStats", "Cassini Network Error Counter Group", CXI_ERRSTATS_COUNTER_GROUP_LIST, CXI_ERRSTATS_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_OPCOMMANDS, "CxiOpCommands", "Cassini Operation (Commands) Counter Group", CXI_OPCOMMANDS_COUNTER_GROUP_LIST, CXI_OPCOMMANDS_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_OPPACKETS, "CxiOpPackets", "Cassini Operation (Packets) Counter Group", CXI_OPPACKETS_COUNTER_GROUP_LIST, CXI_OPPACKETS_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_DMAENGINE, "CxiDmaEngine", "Cassini DMA Engine Counter Group", CXI_DMAENGINE_COUNTER_GROUP_LIST, CXI_DMAENGINE_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_WRITESTOHOST, "CxiWritesToHost", "Cassini Writes to Host Counter Group", CXI_WRITESTOHOST_COUNTER_GROUP_LIST, CXI_WRITESTOHOST_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_MESSAGEMATCHINGPOOLED, "CxiMessageMatchingPooled", "Cassini Message Matching of Pooled Counters Group", CXI_MESSAGEMATCHINGPOOLED_COUNTER_GROUP_LIST, CXI_MESSAGEMATCHINGPOOLED_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_TRANSLATIONUNIT, "CxiTranslationUnit", "Cassini Translation Unit Counter Group", CXI_TRANSLATIONUNIT_COUNTER_GROUP_LIST, CXI_TRANSLATIONUNIT_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_LATENCYHIST, "CxiLatencyHist", "Cassini Latency Histogram Counter Group", CXI_LATENCYHIST_COUNTER_GROUP_LIST, CXI_LATENCYHIST_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_PCTREQRESPTRACKING, "CxiPctReqRespTracking", "Cassini Packet Connection and Tracking Counter Group", CXI_PCTREQRESPTRACKING_COUNTER_GROUP_LIST, CXI_PCTREQRESPTRACKING_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_LINKRELIABILITY, "CxiLinkReliability", "Cassini Link Reliability Counter Group", CXI_LINKRELIABILITY_COUNTER_GROUP_LIST, CXI_LINKRELIABILITY_COUNTER_GROUP_LIST_SIZE),
    SET_PREDEF_COUNTER_GROUP(CXI_CONGESTION, "CxiCongestion", "Cassini Congestion Counter Group", CXI_CONGESTION_COUNTER_GROUP_LIST, CXI_CONGESTION_COUNTER_GROUP_LIST_SIZE)
};
static const size_t OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST_SIZE = sizeof(OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_group_obj_t);

#undef SET_PREDEF_COUNTER_GROUP


/* Function that given a ompi_hwpc_cxi_predefined_counter_mnemonic_id_t determines if the mnemonic id is valid and within range of possible values.
 * Returns true if valid, otherwise returns false
 */
static bool cxi_counter_mnemonic_id_is_valid(const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t counter_mnemonic_id)
{
    if (counter_mnemonic_id < 0 || OMPI_HWPC_CXI_PREDEFINED_COUNTER_MNEMONICS_LIST_SIZE <= counter_mnemonic_id) {
        return false;
    }
    return true;
}

/* Function that given a ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t determines if the mnemonic obj is valid and within range of possible values */
static bool cxi_counter_mnemonic_obj_is_valid(const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj)
{
    if (NULL == counter_mnemonic_obj || !cxi_counter_name_string_is_valid(counter_mnemonic_obj->counter_name)) {
        return false;
    }
    return true;
}

/* Function that given a ompi_hwpc_cxi_predefined_counter_group_obj_t determines if the group obj is valid and within range of possible values */
static bool cxi_counter_group_obj_is_valid(const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj)
{
    if (NULL == counter_group_obj || !cxi_counter_name_string_is_valid(counter_group_obj->counter_group_name) || 
        NULL == counter_group_obj->counter_mnemonic_list || counter_group_obj->counter_mnemonic_list_size == 0) {
        return false;
    }
    return true;
}

/* Function that given a counter (group, mnemonic, low-level, etc.) name string determines if it is valid and within range of possible values for a string */
static bool cxi_counter_name_string_is_valid(const char *counter_name)
{
    if (NULL == counter_name) {
        return false;
    }
    size_t counter_name_len = strnlen(counter_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH+1); /* +1 to check for null-termination */
    if (counter_name_len == 0 || counter_name_len > HWPC_CXI_MAX_COUNTER_NAME_LENGTH) {
        return false;
    }

    return true;
}

/* Function that given a counter mnemonic object, returns the number of (per-NIC) low-level cxi counters that the counter mnemonic represents */
ompi_hwpc_cxi_error_code_t cxi_get_num_counters_in_counter_mnemonic_obj(int *total_num_counters, const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj)
{
    if (NULL == total_num_counters || !cxi_counter_mnemonic_obj_is_valid(counter_mnemonic_obj)) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }
    /* Initialize for any error path */
    *total_num_counters = 0;

    if (counter_mnemonic_obj->is_per_cxi_device) {
        *total_num_counters += (counter_mnemonic_obj->is_per_category ? counter_mnemonic_obj->num_categories : 1); /* Add the number of categories for this counter mnemonic */
    }
    return HWPC_CXI_SUCCESS; /* Found */
}

/* Function that given a counter group object, returns the number of (per-NIC) low-level cxi counters that the counter group represents */
ompi_hwpc_cxi_error_code_t cxi_get_num_counters_in_counter_group_obj(int *total_num_counters, const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj)
{
    if (NULL == total_num_counters || !cxi_counter_group_obj_is_valid(counter_group_obj)) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }
    /* Initialize for any error path */
    ompi_hwpc_cxi_error_code_t rc = HWPC_CXI_ERROR;
    *total_num_counters = 0;

    bool found_valid_mnemonic = false;
    ompi_hwpc_cxi_predefined_counter_mnemonic_id_t counter_mnemonic_id;
    const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj;
    int num_counters_in_mnemonic = 0;

    for (size_t i = 0; i < counter_group_obj->counter_mnemonic_list_size; i++) {
        counter_mnemonic_id = counter_group_obj->counter_mnemonic_list[i];
        if (!cxi_counter_mnemonic_id_is_valid(counter_mnemonic_id)) {
            continue; /* Skip if mnemonic id is out of range */
        }

        counter_mnemonic_obj = GET_PREDEF_COUNTER_MNEMONIC_OBJ(counter_mnemonic_id);
        if (!cxi_counter_mnemonic_obj_is_valid(counter_mnemonic_obj)) {
            continue; /* Skip if mnemonic object is invalid */
        }

        num_counters_in_mnemonic = 0;
        rc = cxi_get_num_counters_in_counter_mnemonic_obj(&num_counters_in_mnemonic, counter_mnemonic_obj);
        if (HWPC_CXI_SUCCESS != rc) {
            return rc;
        }

        *total_num_counters += num_counters_in_mnemonic;
        found_valid_mnemonic = true;    /* Found (at least one) counter mnemonic to be valid and counted */
    }

    if (!found_valid_mnemonic) {
        *total_num_counters = 0; /* No valid counter mnemonics found in the counter group */
        return HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND;
    }

    return HWPC_CXI_SUCCESS;
}

/* Function that given a counter group name, returns the corresponding counter group's counter_group_obj */
ompi_hwpc_cxi_error_code_t cxi_get_counter_group_obj_by_name(const ompi_hwpc_cxi_predefined_counter_group_obj_t **counter_group_obj, const char *counter_group_name)
{
    if (NULL == counter_group_obj || !cxi_counter_name_string_is_valid(counter_group_name)) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }
    /* Initialize for any error path */
    *counter_group_obj = NULL;

    size_t input_group_name_len = strnlen(counter_group_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);

    const ompi_hwpc_cxi_predefined_counter_group_obj_t *predef_counter_group_obj = NULL;
    const char *predef_group_name;
    const char *predef_group_pretty_name;
    size_t predef_group_name_len;
    size_t predef_group_pretty_name_len;

    /* Cycle over counter groups and find one with a name that case-insensitively matches the input group name */
    for (size_t i = 0; i < OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST_SIZE; i++) {
        predef_counter_group_obj = GET_PREDEF_COUNTER_GROUP_OBJ(i);

        /* Compare against both the counter group name and the counter group pretty name */
        predef_group_name = predef_counter_group_obj->counter_group_name;
        predef_group_pretty_name = predef_counter_group_obj->counter_group_pretty_name;

        if (NULL == predef_group_name && NULL == predef_group_pretty_name) {
            continue; /* Skip if both predef group names are NULL */
        }
        /* Accept either spelling of the group name */
        if (predef_group_name) {
            predef_group_name_len = strnlen(predef_group_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
            if (predef_group_name_len == input_group_name_len && strncasecmp(predef_group_name, counter_group_name, predef_group_name_len) == 0) {
                *counter_group_obj = predef_counter_group_obj;
                return HWPC_CXI_SUCCESS; /* Group found */
            }
        }
        if (predef_group_pretty_name) {
            predef_group_pretty_name_len = strnlen(predef_group_pretty_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
            if (predef_group_pretty_name_len == input_group_name_len && strncasecmp(predef_group_pretty_name, counter_group_name, predef_group_pretty_name_len) == 0) {
                *counter_group_obj = predef_counter_group_obj;
                return HWPC_CXI_SUCCESS; /* Group found */
            }
        }
    }
    return HWPC_CXI_COUNTER_GROUP_NOT_FOUND; /* Group not found */
}

/* Function that given a counter group name, returns the number of (per-NIC) low-level cxi counters that the counter group represents */
ompi_hwpc_cxi_error_code_t cxi_get_num_counters_in_counter_group_by_name(int *total_num_counters, const char *counter_group_name)
{
    if (NULL == total_num_counters || !cxi_counter_name_string_is_valid(counter_group_name)) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }
    /* Initialize for any error path */
    *total_num_counters = 0;

    ompi_hwpc_cxi_error_code_t rc = HWPC_CXI_ERROR;

    const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj;
    rc = cxi_get_counter_group_obj_by_name(&counter_group_obj, counter_group_name);
    if (HWPC_CXI_SUCCESS != rc) {
        return rc;
    }

    return cxi_get_num_counters_in_counter_group_obj(total_num_counters, counter_group_obj);
}



/* Function that given a counter mnemonic id, returns the corresponding counter mnemonic object */
ompi_hwpc_cxi_error_code_t cxi_get_counter_mnemonic_obj_by_id(const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, const ompi_hwpc_cxi_predefined_counter_mnemonic_id_t counter_mnemonic_id)
{
    if (NULL == counter_mnemonic_obj) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }
    /* Initialize for any error path */
    *counter_mnemonic_obj = NULL;
    if (!cxi_counter_mnemonic_id_is_valid(counter_mnemonic_id)) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }

    *counter_mnemonic_obj = GET_PREDEF_COUNTER_MNEMONIC_OBJ(counter_mnemonic_id);
    if (!cxi_counter_mnemonic_obj_is_valid(*counter_mnemonic_obj)) {
        *counter_mnemonic_obj = NULL;
        return HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND; /* Mnemonic id not found */
    }

    return HWPC_CXI_SUCCESS; /* Found */
}

/* Function that given a counter mnemonic name, returns the corresponding counter mnemonic object */
ompi_hwpc_cxi_error_code_t cxi_get_counter_mnemonic_obj_by_name(const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t **counter_mnemonic_obj, const char *counter_mnemonic_name)
{
    if (NULL == counter_mnemonic_obj || !cxi_counter_name_string_is_valid(counter_mnemonic_name)) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS; /* Invalid arguments */
    }
    /* Initialize for any error path */
    *counter_mnemonic_obj = NULL;

    size_t input_mnemonic_name_len = strnlen(counter_mnemonic_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);

    const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *predef_mnemonic_obj = NULL;
    const char *predef_mnemonic_name = NULL;
    size_t predef_mnemonic_name_len;

    /* Cycle over counter mnemonics */
    for (size_t i = 0; i < OMPI_HWPC_CXI_PREDEFINED_COUNTER_MNEMONICS_LIST_SIZE; i++) {
        predef_mnemonic_obj = GET_PREDEF_COUNTER_MNEMONIC_OBJ(i);

        predef_mnemonic_name = predef_mnemonic_obj->counter_name;
        if (NULL == predef_mnemonic_name) {
            continue; /* Skip if known mnemonic name is NULL */
        }
        predef_mnemonic_name_len = strnlen(predef_mnemonic_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
        if (predef_mnemonic_name_len == input_mnemonic_name_len
            && strncasecmp(predef_mnemonic_name, counter_mnemonic_name, predef_mnemonic_name_len) == 0) {
            *counter_mnemonic_obj = predef_mnemonic_obj;
            return HWPC_CXI_SUCCESS; /* Mnemonic found */
        }
    }
    return HWPC_CXI_COUNTER_MNEMONIC_NOT_FOUND; /* Mnemonic not found */
}

/* Function that, given a counter mnemonic name, returns the number of counters in the mnemonic */
ompi_hwpc_cxi_error_code_t cxi_get_num_counters_in_counter_mnemonic_by_name(int *total_num_counters, const char *counter_mnemonic_name)
{
    if (NULL == total_num_counters || !cxi_counter_name_string_is_valid(counter_mnemonic_name)) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS; /* Invalid arguments */
    }
    /* Initialize for any error path */
    *total_num_counters = 0;

    ompi_hwpc_cxi_error_code_t rc = HWPC_CXI_ERROR;

    const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj;
    rc = cxi_get_counter_mnemonic_obj_by_name(&counter_mnemonic_obj, counter_mnemonic_name);
    if (HWPC_CXI_SUCCESS != rc) {
        return rc;
    }

    return cxi_get_num_counters_in_counter_mnemonic_obj(total_num_counters, counter_mnemonic_obj);
}

/*
 * Prints out a helpful description of a counter group, but only the top-level information.
 * Call ompi_hwpc_cxi_print_full_counter_group_description() to print out the full description of a counter group,
 * including all of its nested counter mnemonics.
 */
ompi_hwpc_cxi_error_code_t cxi_print_counter_group_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj)
{
    if (NULL == ofp || !cxi_counter_group_obj_is_valid(counter_group_obj)) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS; /* Invalid arguments */
    }

    const char *group_name = counter_group_obj->counter_group_name ? counter_group_obj->counter_group_name : "Unknown";
    const char *group_pretty_name = counter_group_obj->counter_group_pretty_name ? counter_group_obj->counter_group_pretty_name : "Unknown";
    const char *group_description = counter_group_obj->counter_group_description ? counter_group_obj->counter_group_description : "Unknown";

    fprintf(ofp, "Counter Group Name: %s\n", group_name);
    fprintf(ofp, "Counter Group Pretty Name: %s\n", group_pretty_name);
    fprintf(ofp, "Counter Group Description: %s\n", group_description);
    fprintf(ofp, "Number of Counter Mnemonics in Group: %zu\n", counter_group_obj->counter_mnemonic_list_size);

    return HWPC_CXI_SUCCESS; /* Found */
}

/* Prints out a helpful, detailed description of a counter mnemonic */
ompi_hwpc_cxi_error_code_t cxi_print_counter_mnemonic_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj)
{
    if (NULL == ofp || !cxi_counter_mnemonic_obj_is_valid(counter_mnemonic_obj)) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS; /* Invalid arguments */
    }

    const char *mnemonic_name = counter_mnemonic_obj->counter_name ? counter_mnemonic_obj->counter_name : "Unknown";
    const char *mnemonic_description = counter_mnemonic_obj->counter_description ? counter_mnemonic_obj->counter_description : "Unknown";

    fprintf(ofp, "Counter Name: %s\n", mnemonic_name);
    fprintf(ofp, "Description: %s\n", mnemonic_description);
    fprintf(ofp, "Is Per Cassini Device: %s\n", counter_mnemonic_obj->is_per_cxi_device ? "Yes" : "No");
    fprintf(ofp, "Is Retry Handler Counter: %s\n", counter_mnemonic_obj->is_retry_handler_counter ? "Yes" : "No");
    fprintf(ofp, "Is Segmented Into Categories: %s\n", counter_mnemonic_obj->is_per_category ? "Yes" : "No");
    if (counter_mnemonic_obj->is_per_category) {
        fprintf(ofp, "Number of Categories: %zu\n", counter_mnemonic_obj->num_categories);
        if (counter_mnemonic_obj->category_name != NULL) {
            fprintf(ofp, "Category Name: %s\n", counter_mnemonic_obj->category_name);
        }
    }
    fprintf(ofp, "\n");

    return HWPC_CXI_SUCCESS;
}

/* Prints out a helpful, detailed description of a counter group plus all of its nested counter mnemonics */
ompi_hwpc_cxi_error_code_t cxi_print_full_counter_group_description(FILE *ofp, const ompi_hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj)
{
    if (NULL == ofp || NULL == counter_group_obj) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS; /* Invalid arguments */
    }
    /* Initialize for any error path */
    ompi_hwpc_cxi_error_code_t rc = HWPC_CXI_ERROR;

    rc = cxi_print_counter_group_description(ofp, counter_group_obj);
    if (HWPC_CXI_SUCCESS != rc) {
        return rc; /* Error printing group description */
    }

    /* Cycle over all counter mnemonics in the group */
    const ompi_hwpc_cxi_predefined_counter_mnemonic_obj_t *cxi_counter_mnemonic_obj = NULL;
    ompi_hwpc_cxi_predefined_counter_mnemonic_id_t cxi_counter_mnemonic_id;
    for (size_t i = 0; i < counter_group_obj->counter_mnemonic_list_size; i++) {
        cxi_counter_mnemonic_id = counter_group_obj->counter_mnemonic_list[i];
        rc = cxi_get_counter_mnemonic_obj_by_id(&cxi_counter_mnemonic_obj, cxi_counter_mnemonic_id);
        if (HWPC_CXI_SUCCESS != rc) {
            return rc;
        }
        rc = cxi_print_counter_mnemonic_description(ofp, cxi_counter_mnemonic_obj);
        if (HWPC_CXI_SUCCESS != rc) {
            return rc; /* Error printing counter description */
        }
    }
    return HWPC_CXI_SUCCESS; /* Success */
}

#undef GET_PREDEF_COUNTER_GROUP_OBJ
#undef GET_PREDEF_COUNTER_MNEMONIC_OBJ

#endif /* HWPC_CXI_ENABLE */

#endif /* HWPC_CXI_FEATURE_MOVED_TO_MCA_HOOK_MODULE */
