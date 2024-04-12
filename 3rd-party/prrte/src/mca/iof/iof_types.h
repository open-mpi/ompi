/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */

#ifndef PRTE_IOF_TYPES_H
#define PRTE_IOF_TYPES_H

#include "prte_config.h"
#include "types.h"

BEGIN_C_DECLS

/* Predefined tag values */
typedef uint16_t prte_iof_tag_t;
#define PRTE_IOF_TAG_T PRTE_UINT16

#define PRTE_IOF_STDIN     0x0001
#define PRTE_IOF_STDOUT    0x0002
#define PRTE_IOF_STDERR    0x0004
#define PRTE_IOF_STDMERGE  0x0006
#define PRTE_IOF_STDDIAG   0x0008
#define PRTE_IOF_STDOUTALL 0x000e
#define PRTE_IOF_STDALL    0x000f
#define PRTE_IOF_EXCLUSIVE 0x0100

/* flow control flags */
#define PRTE_IOF_XON  0x1000
#define PRTE_IOF_XOFF 0x2000
/* tool requests */
#define PRTE_IOF_PULL  0x4000
#define PRTE_IOF_CLOSE 0x8000

END_C_DECLS

#endif /* PRTE_IOF_TYPES_H */
