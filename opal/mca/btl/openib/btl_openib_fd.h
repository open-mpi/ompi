/*
 * Copyright (c) 2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009 Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_BTL_OPENIB_FD_H_
#define OPAL_BTL_OPENIB_FD_H_

#include "opal_config.h"

BEGIN_C_DECLS

/**
 * Typedef for fd callback function
 */
typedef void *(opal_btl_openib_fd_event_callback_fn_t)(int fd, int flags,
                                                       void *context);

/**
 * Typedef for generic callback function
 */
typedef void *(opal_btl_openib_fd_main_callback_fn_t)(void *context);

/**
 * Initialize fd monitoring.
 * Called by the main thread.
 */
int opal_btl_openib_fd_init(void);

/**
 * Start monitoring an fd.
 * Called by main or service thread; callback will be in service thread.
 */
int opal_btl_openib_fd_monitor(int fd, int flags,
                               opal_btl_openib_fd_event_callback_fn_t *callback,
                               void *context);

/**
 * Stop monitoring an fd.
 * Called by main or service thread; callback will be in service thread.
 */
int opal_btl_openib_fd_unmonitor(int fd,
                                 opal_btl_openib_fd_event_callback_fn_t *callback,
                                 void *context);

/**
 * Run a function in the service thread.
 * Called by the main thread.
 */
int opal_btl_openib_fd_run_in_service(opal_btl_openib_fd_main_callback_fn_t callback,
                                      void *context);

/**
 * Run a function in the main thread.
 * Called by the service thread.
 */
int opal_btl_openib_fd_run_in_main(opal_btl_openib_fd_main_callback_fn_t callback,
                                   void *context);

/**
 * Drain all pending messages from the main thread's pipe.
 * Likely only useful during finalize, when the event library
 * won't fire callbacks.
 */
int opal_btl_openib_fd_main_thread_drain(void);

/**
 * Finalize fd monitoring.
 * Called by the main thread.
 */
int opal_btl_openib_fd_finalize(void);

END_C_DECLS

#endif  /* OPAL_BTL_OPENIB_FD_H_ */
