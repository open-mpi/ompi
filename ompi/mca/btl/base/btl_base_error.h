/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_BASE_ERROR_H
#define MCA_BTL_BASE_ERROR_H

#include "ompi_config.h"

#include <errno.h>
#include <stdio.h>

#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"

extern int mca_btl_base_debug;

extern int mca_btl_base_err(const char*, ...);
extern int mca_btl_base_out(const char*, ...);

#define BTL_OUTPUT(args)                                     \
do {                                                         \
    mca_btl_base_out("[%s][%lu,%lu,%lu][%s:%d:%s] ",         \
            orte_system_info.nodename,                       \
            ORTE_NAME_ARGS(orte_process_info.my_name),       \
            __FILE__, __LINE__, __func__);                   \
    mca_btl_base_out args;                                   \
    mca_btl_base_out("\n");                                  \
} while(0);


#define BTL_ERROR(args)                                      \
do {                                                         \
    mca_btl_base_err("[%s][%lu,%lu,%lu][%s:%d:%s] ",         \
            orte_system_info.nodename,                       \
            ORTE_NAME_ARGS(orte_process_info.my_name),       \
            __FILE__, __LINE__, __func__);                   \
    mca_btl_base_err args;                                   \
    mca_btl_base_err("\n");                                  \
} while(0);

#define BTL_PEER_ERROR(proc, args)                               \
do {                                                             \
    mca_btl_base_err("[%lu,%lu,%lu][%s:%d:%s] from %s ",         \
                     ORTE_NAME_ARGS(orte_process_info.my_name),  \
                     __FILE__, __LINE__, __func__,               \
                     orte_system_info.nodename);                 \
    if(proc && proc->proc_hostname) {                            \
        mca_btl_base_err("to: %s ", proc->proc_hostname);        \
    }                                                            \
    mca_btl_base_err args;                                       \
    mca_btl_base_err("\n");                                      \
} while(0);


#if OMPI_ENABLE_DEBUG
#define BTL_DEBUG(args)                                      \
do {                                                         \
   if(mca_btl_base_debug) {                                  \
        mca_btl_base_err("[%s][%lu,%lu,%lu][%s:%d:%s] ",     \
                orte_system_info.nodename,                   \
                ORTE_NAME_ARGS(orte_process_info.my_name),   \
                __FILE__, __LINE__, __func__);               \
        mca_btl_base_err args;                               \
        mca_btl_base_err("\n");                              \
   }                                                         \
} while(0); 
#define BTL_VERBOSE(args)                                    \
do {                                                         \
   if(mca_btl_base_debug > 1) {                              \
        mca_btl_base_err("[%s][%lu,%lu,%lu][%s:%d:%s] ",     \
                orte_system_info.nodename,                   \
                ORTE_NAME_ARGS(orte_process_info.my_name),   \
                __FILE__, __LINE__, __func__);               \
        mca_btl_base_err args;                               \
        mca_btl_base_err("\n");                              \
   }                                                         \
} while(0); 
#else
#define BTL_DEBUG(args) 
#define BTL_VERBOSE(args) 
#endif

#endif


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern void mca_btl_base_error_no_nics(const char* transport, 
                                       const char* nic_name);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
