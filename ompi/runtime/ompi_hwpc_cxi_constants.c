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

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>
#include "ompi/runtime/ompi_hwpc_cxi_constants.h"

// A list containing all of the performance counters for the CxiPerfStats group
const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_PERFSTATS_COUNTER_GROUP_LIST[] = {
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

// A list containing all of the performance counters for the CxiErrStats group
const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_ERRSTATS_COUNTER_GROUP_LIST[] = {
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

// A list containing all of the performance counters for the CxiOpCommands group
const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_OPCOMMANDS_COUNTER_GROUP_LIST[] = {
    CQ_DMA_CMD_COUNTS,
    CQ_CQ_CMD_COUNTS,
    CQ_NUM_DMA_CMDS,
    CQ_NUM_IDC_CMDS
};

// A list containing all of the performance counters for the CxiOpPackets group
const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_OPPACKETS_COUNTER_GROUP_LIST[] = {
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

// A list containing all of the performance counters for the CxiDmaEngine group
static const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_DMAENGINE_COUNTER_GROUP_LIST[] = {
    OXE_MCU_MEAS,
    OXE_CHANNEL_IDLE,
    IXE_DISP_DMAWR_REQS,
    IXE_DMAWR_STALL_P_CDT,
    IXE_DMAWR_STALL_NP_CDT,
    PI_PTI_TARB_MRD_PKTS,
    PI_PTI_TARB_MWR_PKTS
};

// A list containing all of the performance counters for the CxiWritesToHost group
static const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_WRITESTOHOST_COUNTER_GROUP_LIST[] = {
    PARBS_TARB_PI_POSTED_PKTS,
    PARBS_TARB_PI_POSTED_BLOCKED_CNT,
    PARBS_TARB_PI_NON_POSTED_PKTS,
    PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT
};

// A list containing all of the performance counters for the CxiMessageMatchingPooled group
static const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_MESSAGEMATCHINGPOOLED_COUNTER_GROUP_LIST[] = {
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

// A list containing all of the performance counters for the CxiTranslationUnit group
const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_TRANSLATIONUNIT_COUNTER_GROUP_LIST[] = {
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

// A list containing all of the performance counters for the CxiLatencyHist group
const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_LATENCYHIST_COUNTER_GROUP_LIST[] = {
    PCT_HOST_ACCESS_LATENCY,
    PCT_REQ_RSP_LATENCY
};

// A list containing all of the performance counters for the CxiPctReqRespTracking group
const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_PCTREQRESPTRACKING_COUNTER_GROUP_LIST[] = {
    PCT_REQ_ORDERED,
    PCT_REQ_UNORDERED,
    PCT_RESPONSES_RECEIVED,
    PCT_CONN_SCT_OPEN,
    PCT_NO_TRS_NACKS,
    PCT_RETRY_SRB_REQUESTS,
    PCT_SPT_TIMEOUTS,
    PCT_SCT_TIMEOUTS
};

// A list containing all of the performance counters for the CxiLinkReliability group
const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_LINKRELIABILITY_COUNTER_GROUP_LIST[] = {
    HNI_PCS_GOOD_CW,
    HNI_PCS_CORRECTED_CW,
    HNI_PCS_UNCORRECTED_CW,
    HNI_LLR_TX_REPLAY_EVENT,
    HNI_LLR_RX_REPLAY_EVENT
};

// A list containing all of the performance counters for the CxiCongestion group
const ompi_hwpc_cxi_predefined_counter_mnemonic_t CXI_CONGESTION_COUNTER_GROUP_LIST[] = {
    PARBS_TARB_PI_POSTED_PKTS,
    PARBS_TARB_PI_POSTED_BLOCKED_CNT,
    PARBS_TARB_PI_NON_POSTED_PKTS,
    PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT,
    HNI_RX_PAUSED,
    HNI_RX_PAUSED_STD,
    HNI_TX_PAUSED
};

const size_t CXI_PERFSTATS_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_PERFSTATS_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_ERRSTATS_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_ERRSTATS_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_OPCOMMANDS_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_OPCOMMANDS_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_OPPACKETS_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_OPPACKETS_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_DMAENGINE_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_DMAENGINE_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_WRITESTOHOST_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_WRITESTOHOST_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_MESSAGEMATCHINGPOOLED_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_MESSAGEMATCHINGPOOLED_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_TRANSLATIONUNIT_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_TRANSLATIONUNIT_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_LATENCYHIST_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_LATENCYHIST_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_PCTREQRESPTRACKING_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_PCTREQRESPTRACKING_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_LINKRELIABILITY_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_LINKRELIABILITY_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);
const size_t CXI_CONGESTION_COUNTER_GROUP_LIST_SIZE = sizeof(CXI_CONGESTION_COUNTER_GROUP_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);

#define HAS_CATEGORIES true
#define IS_SINGLETON false

#define SET_PREDEF_COUNTER(COUNTER_MNEMONIC_NAME, DESC, PER_DEV, RETRY_HANDLER, PER_CAT, NUM_CAT, CAT_NAME)   [COUNTER_MNEMONIC_NAME] = { .counter_name = #COUNTER_MNEMONIC_NAME, \
                                                            .counter_description = DESC, .is_per_cxi_device = PER_DEV, .is_retry_handler_counter = RETRY_HANDLER, \
                                                            .is_per_category = PER_CAT, .num_categories = NUM_CAT, .category_name = CAT_NAME }

/*
 * This array contains all predefined Cassini hardware performance counter descriptions,
 * providing metadata for a short, but incomplete, collection of hardware performance counters supported.
 */
const ompi_hwpc_cxi_counter_desc_t OMPI_HWPC_CXI_PREDEFINED_COUNTERS_LIST[] = {
    // Address Translation Unit Performance Counters
    SET_PREDEF_COUNTER(ATU_ATS_PRS_ODP_LATENCY, "ATS Page Request Services On-Demand Paging latency histogram. Four bins defined in C_ATU_CFG_ODP_HIST.", true, false, HAS_CATEGORIES, 4, "Histogram-Bin"),
    SET_PREDEF_COUNTER(ATU_ATS_TRANS_LATENCY, "ATS Translation latency histogram. Four bins defined in C_ATU_CFG_XLATION_HIST.", true, false, HAS_CATEGORIES, 4, "Histogram-Bin"),
    SET_PREDEF_COUNTER(ATU_CACHE_HIT_BASE_PAGE_SIZE, "Number of cache hits observed on the Base Page Size.", true, false, HAS_CATEGORIES, 4, "Page-Size"),
    SET_PREDEF_COUNTER(ATU_CACHE_HIT_DERIVATIVE1_PAGE_SIZE, "Number of cache hits observed on the Derivative 1 Page Size.", true, false, HAS_CATEGORIES, 4, "Page-Size"),
    SET_PREDEF_COUNTER(ATU_CACHE_HIT_DERIVATIVE2_PAGE_SIZE, "Number of cache hits observed on the Derivative 2 Page Size.", true, false, HAS_CATEGORIES, 4, "Page-Size"),
    SET_PREDEF_COUNTER(ATU_CACHE_MISS, "Number of cache misses by counter pool. Four counters of which counter 0 counts misses on 4K pages and counter 1 counts misses on 2M pages by default.", true, false, HAS_CATEGORIES, 4, "Page-Size"),
    SET_PREDEF_COUNTER(ATU_CACHE_MISS_EE, "Number of cache misses by client (events).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CACHE_MISS_IXE, "Number of cache misses by client (writes).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CACHE_MISS_OXE, "Number of cache misses by client (reads).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CACHE_EVICTIONS, "Number of times a tag was evicted from the NIC translation cache to make room for a new tag.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CLIENT_REQ_EE, "Number of translation requests by client (events).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CLIENT_REQ_IXE, "Number of translation requests by client (writes).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CLIENT_REQ_OXE, "Number of translation requests by client (reads).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(ATU_CLIENT_RSP_NOT_OK, "Number of client responses that were not RC_OK.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(ATU_NIC_PRI_ODP_LATENCY, "NIC Page Request Interface On-Demand Paging latency histogram.", true, false, HAS_CATEGORIES, 4, "Histogram-Bin"),
    SET_PREDEF_COUNTER(ATU_NTA_TRANS_LATENCY, "NTA Translation latency histogram.", true, false, HAS_CATEGORIES, 4, "Histogram-Bin"),
    SET_PREDEF_COUNTER(ATU_ODP_REQUESTS, "Number of On-Demand Paging requests.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    // Command Queue Performance Counters
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
    SET_PREDEF_COUNTER(CQ_NUM_TGQ_CMD_READS," Number of PCIe command reads issued for target prefetch queues. Four counters, one each for reads of 64, 128, 192, or 256 bytes.", true, false, HAS_CATEGORIES, 4, "Data-Size"),
    SET_PREDEF_COUNTER(CQ_NUM_TGT_CMDS, "Number of successfully parsed CQ commands processed by target command queues. All target commands are single flit. Incremented as target commands are sent to LPE.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_NUM_TOU_CMD_READS, "Number of PCIe command reads issued for TOU prefetch queues. Four counters, one each for reads of 64, 128, 192, or 256 bytes.", true, false, HAS_CATEGORIES, 4, "Data-Size"),
    SET_PREDEF_COUNTER(CQ_NUM_TXQ_CMD_READS, "Number of PCIe command reads issued for transmit prefetch queues. Four counters, one each for reads of 64, 128, 192, or 256 bytes.", true, false, HAS_CATEGORIES, 4, "Data-Size"),
    SET_PREDEF_COUNTER(CQ_TGT_WAITING_ON_READ, "Cycles on which target prefetch buffers are empty and pool has read requests pending. Note that this counter does not increment on cycles for which commands are being processed in another pool.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    SET_PREDEF_COUNTER(CQ_TX_WAITING_ON_READ, "Cycles in which transmit prefetch buffers are empty and pool has read requests pending. Note that this counter does not increment on cycles for which commands in another pool are being processed.", true, false, HAS_CATEGORIES, 4, "Unknown"),
    // Event Engine Performance Counters
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
    // High-Speed Network Interface Performance Counters
    SET_PREDEF_COUNTER(HNI_DISCARD_CNTR, "Number of packets discarded due to a timeout for each traffic class as indicated by DISCARD_ERR.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_LLR_TX_REPLAY_EVENT, "Number of LLR replays.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_LLR_RX_REPLAY_EVENT, "Number of LLR replays.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_MULTICAST_PKTS_RECV_BY_TC, "Number of multicast packets with good FCS received by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_MULTICAST_PKTS_SENT_BY_TC, "Number of multicast packets with good FCS sent by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_PAUSE_RECV, "Number of pause frames received for each enabled PCP (as identified by the PEV field of the pause frame) when PFC pause is enabled.", true, false, HAS_CATEGORIES, 8, "Unknown"),
    SET_PREDEF_COUNTER(HNI_PAUSE_XOFF_SENT, "Number of pause frames sent where XOFF is indicated for each PCP when PFC pause is enabled.", true, false, HAS_CATEGORIES, 8, "Unknown"),
    SET_PREDEF_COUNTER(HNI_PCS_CORRECTED_CW, "Number of corrected codewords.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_PCS_FECL_ERRORS, "Number of errors in each FECL.", true, false, HAS_CATEGORIES, 8, "Unknown"),
    SET_PREDEF_COUNTER(HNI_PCS_GOOD_CW, "Number of codewords received with no errors.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_PCS_UNCORRECTED_CW, "Number of uncorrected code words received on the switch to NIC link.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_PFC_FIFO_OFLW_CNTR, "Number of packets discarded at the tail of the PFC FIFO.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_PKTS_RECV_BY_TC, "Number of packets received by traffic class <n>.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_PKTS_SENT_BY_TC, "Number of packets sent by sent by traffic class <n>.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    // High-Speed Network Interface (Receive) Performance Counters
    SET_PREDEF_COUNTER(HNI_RX_OK_27, "Number of packets received in the 27 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_35, "Number of packets received in the 35 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_64, "Number of packets received in the 64 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_36_to_63, "Number of packets received in the 36 to 63 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_65_to_127, "Number of packets received in the 65 to 127 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_128_to_255, "Number of packets received in the 128 to 255 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_256_to_511, "Number of packets received in the 256 to 511 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_512_to_1023, "Number of packets received in the 512 to 1023 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_1024_to_2047, "Number of packets received in the 1024 to 2047 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_2048_to_4095, "Number of packets received in the 2048 to 4095 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_4096_to_8191, "Number of packets received in the 4096 to 8191 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_OK_8192_to_max, "Number of packets received in the 8192 to max bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_PAUSED, "Number of cycles in which the pause is applied on the receive path for traffic class <n>.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(HNI_RX_PAUSED_STD, "Number of cycles in which at least one PCP pause occurred.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_RX_STALL_IXE_PKTBUF, "Number of system clocks for which the Rx path of the corresponding traffic class is stalled due to lack of space in the IXE Packet Buffer.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    // High-Speed Network Interface (Transmit) Performance Counters
    SET_PREDEF_COUNTER(HNI_TX_OK_27, "Number of packets sent in the 27 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_35, "Number of packets sent in the 35 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_64, "Number of packets sent in the 64 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_36_to_63, "Number of packets sent in the 36 to 63 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_65_to_127, "Number of packets sent in the 65 to 127 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_128_to_255, "Number of packets sent in the 128 to 255 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_256_to_511, "Number of packets sent in the 256 to 511 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_512_to_1023, "Number of packets sent in the 512 to 1023 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_1024_to_2047, "Number of packets sent in the 1024 to 2047 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_2048_to_4095, "Number of packets sent in the 2048 to 4095 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_4096_to_8191, "Number of packets sent in the 4096 to 8191 bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_OK_8192_to_max, "Number of packets sent in the 8192 to max bytes size bin.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(HNI_TX_PAUSED, "Number of cycles in which the transmit path is paused for traffic class <n>.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    // Inbound Transfer Engine Performance Counters
    SET_PREDEF_COUNTER(IXE_DISP_DMAWR_REQS, "Number of requests to the IXE dispatcher for DMA write operations.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(IXE_DMAWR_STALL_NP_CDT, "Number of stalls due to no non-posted credits (cycles).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(IXE_DMAWR_STALL_P_CDT, "Number of stalls due to no posted credits (cycles).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(IXE_POOL_ECN_PKTS, "Number of packets with ECN set.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(IXE_POOL_NO_ECN_PKTS, "Number of packets without ECN set.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(PI_PTI_TARB_MRD_PKTS, "Number of memory read TLPs (all source).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PI_PTI_TARB_MWR_PKTS, "Number of memory write TLPs (all source).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(IXE_TC_REQ_ECN_PKTS, "Number of request packets with ECN set, by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(IXE_TC_REQ_NO_ECN_PKTS, "Number of request packets without ECN set, by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(IXE_TC_RSP_ECN_PKTS, "Number of response packets with ECN set, by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    SET_PREDEF_COUNTER(IXE_TC_RSP_NO_ECN_PKTS, "Number of response packets without ECN set, by TC.", true, false, HAS_CATEGORIES, 8, "Traffic-Class"),
    //List Processing Engine Performance Counters
    SET_PREDEF_COUNTER(LPE_APPEND_CMDS, "Number of Append commands received. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_APPEND_SUCCESS, "Number of Append commands LPE successfully completed. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_CYC_RRQ_BLOCKED, "Number of cycles in which a PE Match Request Queue dequeue was blocked because a Ready Request Queue was full.", true, false, HAS_CATEGORIES, 4, "PE-Index"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_LOCAL, "Number of network requests LPE successfully matched to locally managed buffers. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_OVERFLOW, "Number of messages where payload data was delivered to a buffer on the overflow list because there was no match on the priority list. Four counters of which 0 is the default.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_PRIORITY, "Number of messages matched on the priority list. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_REQUEST, "The number of network requests LPE successfully matched on the Request list. One counter for each of the four pools of PtlTEs.(software endpoints).", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_REQUESTS, "The number of network requests LPE received. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_SUCCESS, "Number of network requests LPE successfully completed. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NET_MATCH_USEONCE, "Number of network requests LPE successfully matched to use-once buffers. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_NUM_TRUNCATED, "Number of truncated packets. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_RNDZV_PUTS, "Number of rendezvous puts received. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_RNDZV_PUTS_OFFLOADED, "Number of Rendezvous Puts that LPE was able to offload. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_SEARCH_NID_ANY, "Number of wildcard searches using NID_ANY, physical matching. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_SEARCH_PID_ANY, "Number of wildcard searches using NID_PID, physical matching. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_SEARCH_RANK_ANY, "Number of wildcard searches using RANK_ANY, logical matching. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_SETSTATE_CMDS, "Number of SetState commands LPE received. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_SETSTATE_SUCCESS, "Number of successful SetState commands. One counter for each of the four pools of PtlTEs.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_UNEXPECTED_GET_AMO, "Number of Get and AMO packets that match on Overflow or Request, resulting in RC_DELAYED.", true, false, HAS_CATEGORIES, 4, "PtlTE-Pool"),
    SET_PREDEF_COUNTER(LPE_STS_NET_MATCH_ATTEMPTS, "Number of match attempts for inbound network headers in each pool. Indexed by CNTR_POOL.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(LPE_STS_APP_MATCH_ATTEMPTS, "Number of comparisons of Appended list entries to unexpected headers in each pool. Indexed by CNTR_POOL.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(LPE_STS_NET_MAX_ATTEMPTS, "Maximum number of match attempts needed for an inbound network header in each pool. Indexed by CNTR_POOL.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(LPE_STS_APP_MAX_ATTEMPTS, "Maximum number of match attempts needed for an Append command in each pool. Indexed by CNTR_POOL.", true, false, HAS_CATEGORIES, 4, "Counter-Pool"),
    SET_PREDEF_COUNTER(LPE_AMO_CMDS, "Number of non-fetching AMO commands received by LPE.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(LPE_FAMO_CMDS, "Number of fetching AMO commands received by LPE.", true, false, IS_SINGLETON, 1, NULL),
    // Management Block Performance Counters
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
    // Outbound Transfer Engine Performance Counters
    SET_PREDEF_COUNTER(OXE_CHANNEL_IDLE, "Number of cycles in which available bandwidth is not used.", true, false, IS_SINGLETON, 1, NULL),
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
    // Processor Interface Arbiters Performance Counters
    SET_PREDEF_COUNTER(PARBS_TARB_PI_POSTED_PKTS, "Number of PCIe packets transferred using the posted path (for example, writes),", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PARBS_TARB_PI_POSTED_BLOCKED_CNT, "and the number of cycles in which this path is blocked. Compute the ratio cycles/pkts. Values of more than a few cycles per packet indicate back pressure from the host. This endpoint is likely to be the cause of congestion.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PARBS_TARB_PI_NON_POSTED_PKTS, "Number of PCIe packets transferred using the non-posted path (for example, reads),", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT, "and the number of cycles in which this path is blocked. Compute the ratio cycles/pkts. Values of more than a few cycles per packet indicate per host performance (high read latencies). This endpoint is likely to be injecting at a low rate.", true, false, IS_SINGLETON, 1, NULL),
    // Packet Connection and Tracking Performance Counters
    // FIXME: description of PCT_HOST_ACCESS_LATENCY and PCT_REQ_RSP_LATENCY are mistakenly swapped in User Guide
    SET_PREDEF_COUNTER(PCT_CONN_SCT_OPEN, "Number of open requests.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_HOST_ACCESS_LATENCY, "Host access latency histogram, 16 bins.", true, false, HAS_CATEGORIES, 16, "Unknown"),
    SET_PREDEF_COUNTER(PCT_MST_HIT_ON_SOM, "Number of times an MST entry already exists for a request of a new message that needs a new MST entry.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_NO_TCT_NACKS, "Number of resource exhaustion NACKs. Retry handler is invoked.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_NO_TRS_NACKS, "Number of resource exhaustion NACKs. Retry handler is invoked.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_NO_MST_NACKS, "Number of resource exhaustion NACKs. Retry handler is invoked.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_REQ_ORDERED, "Number of ordered requests.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_REQ_UNORDERED, "Number of unordered requests.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_REQ_RSP_LATENCY, "Request/response latency histogram, 32 bins", true, false, HAS_CATEGORIES, 32, "Unknown"),
    SET_PREDEF_COUNTER(PCT_RESPONSES_RECEIVED, "Number of responses received (all unordered).", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_RETRY_SRB_REQUESTS, "Number of retries.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_SCT_TIMEOUTS, "Number of response timeouts (or packet loss in the network). Retry handler is invoked.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_SPT_TIMEOUTS, "Number of response timeouts, (packet loss in the network). Retry handler is invoked.", true, false, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PCT_TRS_RSP_NACK_DROPS, "Number of NACKs dropped. Retry handler is invoked.", true, false, IS_SINGLETON, 1, NULL),
    // Triggered Operations Performance Counters
    SET_PREDEF_COUNTER(TOU_CT_CMD_COUNTS, "Number of the successfully validated commands in C_CT_OP_T. Offset into the counter array is equal to the OpCode.", true, false, HAS_CATEGORIES, 16, "Command-OpCode"),
    SET_PREDEF_COUNTER(TOU_NUM_LIST_REBUILDS, "Number of list CT list rebuilds. Pooled counter, pool is equal to the PFQ number.", true, false, HAS_CATEGORIES, 4, "PFQ-Number"),
    SET_PREDEF_COUNTER(TOU_NUM_TRIG_CMDS, "Number of triggered commands. Pooled counter, pool is equal to the PFQ number.", true, false, HAS_CATEGORIES, 4, "PFQ-Number"),
    // Retry-Handler Performance Counters
    SET_PREDEF_COUNTER(SCT_TIMEOUTS, "Number of SCT timeouts.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(SPT_TIMEOUTS, "Number of SPT timeouts.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(SPT_TIMEOUTS_U, "Number of SPT timeouts.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(CONNECTIONS_CANCELLED, "Number of connections cancelled.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(NACK_NO_MATCHING_CONN, "Number of NACKs due to no matching connection.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(NACK_NO_TARGET_CONN, "Number of NACKs due to no target connection.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(NACK_NO_TARGET_MST, "Number of NACKs due to no target MST.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(NACK_NO_TARGET_TRS, "Number of NACKs due to no target TRS.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(NACK_RESOURCE_BUSY, "Number of NACKs due to resource busy.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(NACKS, "Total number of NACKs.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(NACK_SEQUENCE_ERROR, "Number of NACKs due to sequence error.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PKTS_CANCELLED_O, "Number of packets cancelled (ordered).", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(PKTS_CANCELLED_U, "Number of packets cancelled (unordered).", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(SCT_IN_USE, "Number of SCTs currently in use.", true, true, IS_SINGLETON, 1, NULL),
    SET_PREDEF_COUNTER(TCT_TIMEOUTS, "Number of TCT timeouts.", true, true, IS_SINGLETON, 1, NULL)
};
const size_t OMPI_HWPC_CXI_PREDEFINED_COUNTERS_LIST_SIZE = sizeof(OMPI_HWPC_CXI_PREDEFINED_COUNTERS_LIST) / sizeof(ompi_hwpc_cxi_counter_desc_t);

#undef HAS_CATEGORIES
#undef IS_SINGLETON

#define SET_PREDEF_COUNTER_GROUP(NAME, STRING_NAME, DESC, COUNTER_LIST, COUNTER_LIST_SIZE)   [NAME] = { .counter_group_name = #NAME, .counter_group_string_name = STRING_NAME, \
                                                             .counter_group_description = DESC, .counter_mnemonic_list = COUNTER_LIST, \
                                                             .counter_mnemonic_list_size = COUNTER_LIST_SIZE }

#define SET_PREDEF_COUNTER_GROUP2(NAME, STRING_NAME, DESC)   [NAME] = { .counter_group_name = #NAME, .counter_group_string_name = STRING_NAME, \
                                                             .counter_group_description = DESC, .counter_mnemonic_list = GET_COUNTER_GROUP_LIST(NAME), \
                                                             .counter_mnemonic_list_size = sizeof(GET_COUNTER_GROUP_LIST(NAME)) / sizeof(ompi_hwpc_cxi_counter_desc_t) }

#define SET_PREDEF_COUNTER_GROUP3(NAME, STRING_NAME, DESC)   [NAME] = { .counter_group_name = #NAME, .counter_group_string_name = STRING_NAME, \
                                                             .counter_group_description = DESC, .counter_mnemonic_list = GET_COUNTER_GROUP_LIST(NAME), \
                                                             .counter_mnemonic_list_size = GET_COUNTER_GROUP_LIST_SIZE(NAME) }

/*
 * This array contains all predefined Cassini (CX) hardware performance counter group descriptions, providing metadata for
 * complete collection of hardware performance counter groups supported at the time of writing.
 */
const ompi_hwpc_cxi_counter_group_desc_t OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[] = {
    SET_PREDEF_COUNTER_GROUP(CXI_PERFSTATS, "CxiPerfStats", "Traffic Congestion Counter Group", CXI_PERFSTATS_COUNTER_GROUP_LIST, 12),
    SET_PREDEF_COUNTER_GROUP(CXI_ERRSTATS, "CxiErrStats", "Cassini Network Error Counter Group", CXI_ERRSTATS_COUNTER_GROUP_LIST, 10),
    SET_PREDEF_COUNTER_GROUP(CXI_OPCOMMANDS, "CxiOpCommands", "Cassini Operation (Commands) Counter Group", CXI_OPCOMMANDS_COUNTER_GROUP_LIST, 4),
    SET_PREDEF_COUNTER_GROUP(CXI_OPPACKETS, "CxiOpPackets", "Cassini Operation (Packets) Counter Group", CXI_OPPACKETS_COUNTER_GROUP_LIST, 26),
    SET_PREDEF_COUNTER_GROUP(CXI_DMAENGINE, "CxiDmaEngine", "Cassini DMA Engine Counter Group", CXI_DMAENGINE_COUNTER_GROUP_LIST, 7),
    SET_PREDEF_COUNTER_GROUP(CXI_WRITESTOHOST, "CxiWritesToHost", "Cassini Writes to Host Counter Group", CXI_WRITESTOHOST_COUNTER_GROUP_LIST, 4),
    SET_PREDEF_COUNTER_GROUP(CXI_MESSAGEMATCHINGPOOLED, "CxiMessageMatchingPooled", "Cassini Message Matching of Pooled Counters Group", CXI_MESSAGEMATCHINGPOOLED_COUNTER_GROUP_LIST, 11),
    SET_PREDEF_COUNTER_GROUP(CXI_TRANSLATIONUNIT, "CxiTranslationUnit", "Cassini Translation Unit Counter Group", CXI_TRANSLATIONUNIT_COUNTER_GROUP_LIST, 11),
    SET_PREDEF_COUNTER_GROUP(CXI_LATENCYHIST, "CxiLatencyHist", "Cassini Latency Histogram Counter Group", CXI_LATENCYHIST_COUNTER_GROUP_LIST, 2),
    SET_PREDEF_COUNTER_GROUP(CXI_PCTREQRESPTRACKING, "CxiPctReqRespTracking", "Cassini Packet Connection and Tracking Counter Group", CXI_PCTREQRESPTRACKING_COUNTER_GROUP_LIST, 8),
    SET_PREDEF_COUNTER_GROUP(CXI_LINKRELIABILITY, "CxiLinkReliability", "Cassini Link Reliability Counter Group", CXI_LINKRELIABILITY_COUNTER_GROUP_LIST, 5),
    SET_PREDEF_COUNTER_GROUP(CXI_CONGESTION, "CxiCongestion", "Cassini Congestion Counter Group", CXI_CONGESTION_COUNTER_GROUP_LIST, 7)
};
const size_t OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST_SIZE = sizeof(OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST) / sizeof(ompi_hwpc_cxi_counter_group_desc_t);

#undef SET_PREDEF_COUNTER_GROUP

const ompi_hwpc_cxi_predefined_counter_mnemonic_t OMPI_HWPC_CXI_DEFAULT_COUNTERS_MNEMONICS_LIST[] = {
    SCT_TIMEOUTS,
    SPT_TIMEOUTS,
    SPT_TIMEOUTS_U,
    ATU_CACHE_EVICTIONS,
    ATU_CACHE_HIT_BASE_PAGE_SIZE,
    ATU_CACHE_HIT_DERIVATIVE1_PAGE_SIZE,
    LPE_NET_MATCH_PRIORITY,
    LPE_NET_MATCH_OVERFLOW,
    LPE_NET_MATCH_REQUEST,
    LPE_RNDZV_PUTS,
    LPE_RNDZV_PUTS_OFFLOADED,
    HNI_RX_PAUSED,
    HNI_TX_PAUSED,
    PARBS_TARB_PI_POSTED_PKTS,
    PARBS_TARB_PI_POSTED_BLOCKED_CNT,
    PARBS_TARB_PI_NON_POSTED_PKTS,
    PARBS_TARB_PI_NON_POSTED_BLOCKED_CNT,
    PCT_NO_TCT_NACKS,
    PCT_TRS_RSP_NACK_DROPS,
    PCT_MST_HIT_ON_SOM,
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
    TCT_TIMEOUTS
};
const size_t OMPI_HWPC_CXI_DEFAULT_COUNTERS_LIST_SIZE = sizeof(OMPI_HWPC_CXI_DEFAULT_COUNTERS_MNEMONICS_LIST) / sizeof(ompi_hwpc_cxi_predefined_counter_mnemonic_t);

// Function that given a string of a counter group, returns a list of strings of all the counters in that group.
int ompi_hwpc_cxi_get_counters_in_group(const char *group_name, char ***counter_names, size_t *num_counters) {

    if (group_name == NULL) {
        return -1; // Invalid arguments
    }

    int total_found_counters = 0;
    char full_counter_name[MAX_COUNTER_NAME_SIZE];

    // Cycle over counter groups
    for (size_t i = 0; i < OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST_SIZE; i++) {
        if (strncmp(OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[i].counter_group_name, group_name, MAX_COUNTER_NAME_SIZE) == 0) {
            // Cycle over all counter mnemonics in the group
            for (size_t j = 0; j < OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[i].counter_mnemonic_list_size; j++) {

                ompi_hwpc_cxi_predefined_counter_mnemonic_t counter_mnemonic = OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[i].counter_mnemonic_list[j];
                const ompi_hwpc_cxi_counter_desc_t *counter_desc = GET_PREDEF_COUNTER_DESC(counter_mnemonic);

                total_found_counters += counter_desc->num_categories;
            }
            break;
        }
    }
    *counter_names = malloc((size_t)total_found_counters * sizeof(char *));
    if (*counter_names == NULL) {
        return -1; // Memory allocation failed
    }
    int total_written_counters = 0; // Reset to use as index
    // Cycle over counter groups
    for (size_t i = 0; i < OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST_SIZE; i++) {
        if (strncmp(OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[i].counter_group_name, group_name, MAX_COUNTER_NAME_SIZE) == 0) {
            // Cycle over all counter mnemonics in the group
            for (size_t j = 0; j < OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[i].counter_mnemonic_list_size; j++) {

                ompi_hwpc_cxi_predefined_counter_mnemonic_t counter_mnemonic = OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[i].counter_mnemonic_list[j];
                const ompi_hwpc_cxi_counter_desc_t *counter_desc = GET_PREDEF_COUNTER_DESC(counter_mnemonic);
                
                // Create a string for every category or classification of this counter_mnemonic
                for (int k = 0; k < counter_desc->num_categories; k++) {
                    if (counter_desc->num_categories > 1) {
                        // Create string of the base counter name with the category number appended
                        snprintf(full_counter_name, MAX_COUNTER_NAME_SIZE, "%s_%d", counter_desc->counter_name, k);
                    } else {
                        // Single category counter; use the base name as-is
                        snprintf(full_counter_name, MAX_COUNTER_NAME_SIZE, "%s", counter_desc->counter_name);
                    }
                    (*counter_names)[(total_written_counters + k)] = strndup(full_counter_name, MAX_COUNTER_NAME_SIZE);
                }
                total_written_counters += counter_desc->num_categories;
            }
            break;
        }
    }

    *num_counters = total_written_counters;

    return 0; // Group found
}
