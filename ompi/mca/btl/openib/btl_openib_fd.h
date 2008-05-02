/*
 * Copyright (c) 2008 Cisco, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_BTL_OPENIB_FD_H_
#define OMPI_BTL_OPENIB_FD_H_

#include "ompi_config.h"

BEGIN_C_DECLS

/**
 * Typedef for fd callback function
 */
typedef void *(ompi_btl_openib_fd_callback_fn_t)(int fd, int flags, 
                                                 void *context);

/**
 * Typedef for generic callback function
 */
typedef void *(ompi_btl_openib_schedule_callback_fn_t)(void *context);

/**
 * Initialize fd monitoring
 */
int ompi_btl_openib_fd_init(void);

/**
 * Start monitoring an fd
 */
int ompi_btl_openib_fd_monitor(int fd, int flags, 
                               ompi_btl_openib_fd_callback_fn_t *callback,
                               void *context);

/**
 * Stop monitoring an fd
 */
int ompi_btl_openib_fd_unmonitor(int fd, 
                                 ompi_btl_openib_fd_callback_fn_t *callback,
                                 void *context);

/**
 * Run a function in the main thread
 */
int ompi_btl_openib_fd_schedule(ompi_btl_openib_schedule_callback_fn_t callback,
                                void *context);

/**
 * Finalize fd monitoring
 */
int ompi_btl_openib_fd_finalize(void);

END_C_DECLS

#endif
