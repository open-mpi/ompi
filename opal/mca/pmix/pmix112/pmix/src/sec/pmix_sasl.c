/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>
#include <private/pmix_stdint.h>
#include <private/pmix_socket_errno.h>

#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include PMIX_EVENT_HEADER
#include <sasl/sasl.h>

#include "src/class/pmix_list.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/util/progress_threads.h"
#include "src/usock/usock.h"

#include "pmix_sasl.h"

static int sasl_init(void);
static void sasl_finalize(void);
static int client_handshake(int sd);
static int server_handshake(pmix_peer_t *peer);

pmix_sec_base_module_t pmix_sasl_module = {
    "sasl",
    sasl_init,
    sasl_finalize,
    NULL,
    client_handshake,
    NULL,
    server_handshake
};


static int sasl_init(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec: sasl init");

    return PMIX_ERR_NOT_SUPPORTED;
}

static void sasl_finalize(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec: sasl finalize");
}

static int client_handshake(int sd)
{
    return PMIX_ERR_NOT_SUPPORTED;
}


static int server_handshake(pmix_peer_t *peer)
{
    return PMIX_ERR_NOT_IMPLEMENTED;
}
