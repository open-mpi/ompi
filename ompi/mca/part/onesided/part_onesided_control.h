/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * $HEADER$
 */

#ifndef PART_ONESIDED_CONTROL_H
#define PART_ONESIDED_CONTROL_H

#include "ompi_config.h"
#include "opal/types.h"

BEGIN_C_DECLS

/**
 * Versioned common control header for onesided part.
 */
typedef struct {
    uint32_t version;
    uint32_t msg_type;
    uint64_t request_id;
} part_onesided_control_hdr_t;

typedef enum {
    PART_ONESIDED_CTRL_INIT_SEND = 1,
    PART_ONESIDED_CTRL_INIT_RECV,
    PART_ONESIDED_CTRL_MATCH_ACK,
    PART_ONESIDED_CTRL_EPOCH_OPEN,
    PART_ONESIDED_CTRL_PART_COMPLETE,
    PART_ONESIDED_CTRL_ERROR
} part_onesided_ctrl_type_t;

typedef struct {
    uint64_t epoch;
} part_onesided_epoch_open_t;

#define PART_ONESIDED_CTRL_VERSION 1

END_C_DECLS

#endif
