/*
 * Copyright (c) 2007 Cisco, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * This interface is designed to hide the back-end details of how IB
 * RC connections are made from the rest of the openib BTL.  There are
 * module-like instances of the implemented functionality (dlopen and
 * friends are not used, but all the functionality is accessed through
 * struct's of function pointers, so you can swap between multiple
 * different implementations at run time, just like real components).
 *
 * Currently, the connect functions are referenced by their names
 * (e.g., "oob", "rdma_cm").  The decision which to use is made during
 * the openib BTL init() function call.
 *
 * Note that the openib BTL's open() function calls the
 * connect_base_open() function, which registers an MCA parameter, and
 * scans all the connect modules to see if they have open() functions.
 * If they do, they are called.  In this way, the connect modules can
 * register MCA parameters that show up in ompi_info output.
 *
 * There are four main functions to this interface:
 *
 * - open: as described above, used to register MCA params for connect
 * modules
 *
 * - init: to select a connect module.  The module is responsible for
 * setting itself up for asynchronous operation for incoming
 * connection requests (e.g., putting fd's in the progress engine,
 * posting non-blocking RML requests, spawning a background thread,
 * etc.).
 *
 * - start_connect: initiate a connection to a remote peer.  Similar
 * to init, the module is responsible for setting itself up for
 * asyncronous operation for progressing the outgoing connection
 * request.
 *
 * - finalize: shut down all asynchronous handling.  No need to clean
 * up the connections that were made; that's the responsibility of the
 * main openib BTL.
 *
 * There are two functions in the main openib BTL that the module will
 * call:
 *
 * - ompi_btl_openib_post_recvs(endpoint): once a QP is locally
 * connected to the remote side (but we don't know if the remote side
 * is connected to us yet), this function is invoked to post buffers
 * on the QP, setup credits for the endpoint, etc.
 *
 * - ompi_btl_openib_connected(endpoint): once we know that a QP is
 * connected on *both* sides, this function is invoked to tell the
 * main openib BTL "ok, you can use this connection now." (e.g., the
 * main openib BTL will start sending out fragments that were queued
 * while the connection was establing, etc.).
 */

#ifndef BTL_OPENIB_CONNECT_H
#define BTL_OPENIB_CONNECT_H

#include "btl_openib_endpoint.h"

BEGIN_C_DECLS

/**
 * Function to register MCA params in the connect functions
 */
typedef int (*ompi_btl_openib_connect_base_func_open_t)(void);

/**
 * Function to intialize the connection functions (i.e., it's been
 * selected, so do whatever setup is necessary).
 */
typedef int (*ompi_btl_openib_connect_base_func_init_t)(void);

/**
 * Function to initiate a connection to a remote process
 */
typedef int (*ompi_btl_openib_connect_base_func_start_connect_t)
    (mca_btl_base_endpoint_t *e);

/**
 * Function to finalize the connection functions
 */
typedef int (*ompi_btl_openib_connect_base_func_finalize_t)(void);

#define BCF_MAX_NAME 64

struct ompi_btl_openib_connect_base_funcs_t {
    /** Name of this set of connection functions */
    char bcf_name[BCF_MAX_NAME];

    /** Open function */
    ompi_btl_openib_connect_base_func_open_t bcf_open;

    /** Init function */
    ompi_btl_openib_connect_base_func_init_t bcf_init;

    /** Connect function */
    ompi_btl_openib_connect_base_func_start_connect_t bcf_start_connect;

    /** Finalize function */
    ompi_btl_openib_connect_base_func_open_t bcf_finalize;
};
typedef struct ompi_btl_openib_connect_base_funcs_t ompi_btl_openib_connect_base_funcs_t;

END_C_DECLS

#endif
