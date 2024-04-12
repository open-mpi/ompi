/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * I/O Forwarding Service
 * The I/O forwarding service (IOF) is used to connect stdin, stdout, and
 * stderr file descriptor streams from MPI processes to the user
 *
 * The design is fairly simple: when a proc is spawned, the IOF establishes
 * connections between its stdin, stdout, and stderr to a
 * corresponding IOF stream. In addition, the IOF designates a separate
 * stream for passing OMPI/PRTE internal diagnostic/help output to mpirun.
 * This is done specifically to separate such output from the user's
 * stdout/err - basically, it allows us to present it to the user in
 * a separate format for easier recognition. Data read from a source
 * on any stream (e.g., printed to stdout by the proc) is relayed
 * by the local daemon to the other end of the stream - i.e., stdin
 * is relayed to the local proc, while stdout/err is relayed to mpirun.
 * Thus, the eventual result is to connect ALL streams to/from
 * the application process and mpirun.
 *
 * Note: By default, data read from stdin is forwarded -only- to rank=0.
 * Stdin for all other procs is tied to "/dev/null".
 *
 * External tools can "pull" copies of stdout/err and
 * the diagnostic stream from mpirun for any process. In this case,
 * mpirun will send a copy of the output to the "pulling" process. Note that external tools
 * cannot "push" something into stdin unless the user specifically directed
 * that stdin remain open, nor under any conditions "pull" a copy of the
 * stdin being sent to rank=0.
 *
 * Tools can exploit either of two mechanisms for this purpose:
 *
 * (a) call prte_init themselves and utilize the PRTE tool comm
 *     library to access the IOF. This also provides access to
 *     other tool library functions - e.g., to order that a job
 *     be spawned; or
 *
 * (b) fork/exec the "prte-iof" tool and let it serve as the interface
 *     to mpirun. This lets the tool avoid calling prte_init, and means
 *     the tool will not have to compile against the PRTE/OMPI libraries.
 *     However, the prte-iof tool is limited solely to interfacing
 *     stdio and cannot be used for other functions included in
 *     the tool comm library
 *
 * Thus, mpirun acts as a "switchyard" for IO, taking input from stdin
 * and passing it to rank=0 of the job, and taking stdout/err/diag from all
 * ranks and passing it to its own stdout/err/diag plus any "pull"
 * requestors.
 *
 * Streams are identified by PRTE process name (to include wildcards,
 * such as "all processes in PRTE job X") and tag.  There are
 * currently only 4 allowed predefined tags:
 *
 * - PRTE_IOF_STDIN (value 0)
 * - PRTE_IOF_STDOUT (value 1)
 * - PRTE_IOF_STDERR (value 2)
 * - PRTE_IOF_INTERNAL (value 3): for "internal" messages
 *   from the infrastructure, just to differentiate them from user job
 *   stdout/stderr
 *
 * Note that since streams are identified by PRTE process name, the
 * caller has no idea whether the stream is on the local node or a
 * remote node -- it's just a stream.
 *
 * IOF components are selected on a "one of many" basis, meaning that
 * only one IOF component will be selected for a given process.
 * Details for the various components are given in their source code
 * bases.
 *
 * Each IOF component must support the following API:
 *
 * push: Tie a local file descriptor (*not* a stream!) to the stdin
 * of the specified process. If the user has not specified that stdin
 * of the specified process is to remain open, this will return an error.
 *
 * pull: Tie a local file descriptor (*not* a stream!) to a stream.
 * Subsequent input that appears via the stream will
 * automatically be sent to the target file descriptor until the
 * stream is "closed" or an EOF is received on the local file descriptor.
 * Valid source values include PRTE_IOF_STDOUT, PRTE_IOF_STDERR, and
 * PRTE_IOF_INTERNAL
 *
 * close: Closes a stream, flushing any pending data down it and
 * terminating any "push/pull" connections against it. Unclear yet
 * if this needs to be blocking, or can be done non-blocking.
 *
 * flush: Block until all pending data on all open streams has been
 * written down local file descriptors and/or completed sending across
 * the OOB to remote process targets.
 *
 */

#ifndef PRTE_IOF_H
#define PRTE_IOF_H

#include "prte_config.h"
#include "types.h"

#include "src/mca/mca.h"
#include "src/pmix/pmix-internal.h"

#include "src/runtime/prte_globals.h"

#include "iof_types.h"

BEGIN_C_DECLS

/* Initialize the selected module */
typedef int (*prte_iof_base_init_fn_t)(void);

/**
 * Explicitly push data from the specified input file descriptor to
 * the stdin of the indicated peer(s). The provided peer name can
 * include wildcard values.
 *
 * @param peer  Name of target peer(s)
 * @param fd    Local file descriptor for input.
 */
typedef int (*prte_iof_base_push_fn_t)(const pmix_proc_t *peer, prte_iof_tag_t src_tag, int fd);

/**
 * Explicitly pull data from the specified set of SOURCE peers and
 * dump to the indicated output file descriptor. Any fragments that
 * arrive on the stream will automatically be written down the fd.
 *
 * @param peer          Name used to qualify set of origin peers.
 * @param source_tag    Indicates the output streams to be forwarded
 * @param fd            Local file descriptor for output.
 */
typedef int (*prte_iof_base_pull_fn_t)(const pmix_proc_t *peer, prte_iof_tag_t source_tag, int fd);

/**
 * Close the specified iof stream(s) from the indicated peer(s)
 */
typedef int (*prte_iof_base_close_fn_t)(const pmix_proc_t *peer, prte_iof_tag_t source_tag);

typedef int (*prte_iof_base_push_stdin_fn_t)(const pmix_proc_t *dst_name, uint8_t *data, size_t sz);

/* Flag that a job is complete */
typedef void (*prte_iof_base_complete_fn_t)(const prte_job_t *jdata);

/* finalize the selected module */
typedef int (*prte_iof_base_finalize_fn_t)(void);

/**
 *  IOF module.
 */
struct prte_iof_base_module_2_0_0_t {
    prte_iof_base_init_fn_t init;
    prte_iof_base_push_fn_t push;
    prte_iof_base_pull_fn_t pull;
    prte_iof_base_close_fn_t close;
    prte_iof_base_complete_fn_t complete;
    prte_iof_base_finalize_fn_t finalize;
    prte_iof_base_push_stdin_fn_t push_stdin;
};

typedef struct prte_iof_base_module_2_0_0_t prte_iof_base_module_2_0_0_t;
typedef prte_iof_base_module_2_0_0_t prte_iof_base_module_t;
PRTE_EXPORT extern prte_iof_base_module_t prte_iof;

typedef pmix_mca_base_component_t prte_iof_base_component_t;

END_C_DECLS

/*
 * Macro for use in components that are of type iof
 */
#define PRTE_IOF_BASE_VERSION_2_0_0 PRTE_MCA_BASE_VERSION_3_0_0("iof", 2, 0, 0)

#endif /* PRTE_IOF_H */
