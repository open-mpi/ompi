/* -*- C -*-
 *
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2020 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#ifndef PMIX_PTL_BASE_H_
#define PMIX_PTL_BASE_H_

#include "src/include/pmix_config.h"

#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h> /* for struct timeval */
#endif
#ifdef HAVE_STRING_H
#    include <string.h>
#endif

#include "src/class/pmix_pointer_array.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/mca.h"

#include "src/include/pmix_globals.h"
#include "src/include/pmix_stdatomic.h"
#include "src/mca/ptl/base/ptl_base_handshake.h"
#include "src/mca/ptl/ptl.h"

BEGIN_C_DECLS

/*
 * MCA Framework
 */
PMIX_EXPORT extern pmix_mca_base_framework_t pmix_ptl_base_framework;
/**
 * PTL select function
 *
 * Cycle across available components and construct the list
 * of active modules
 */
PMIX_EXPORT pmix_status_t pmix_ptl_base_select(void);

/* framework globals */
struct pmix_ptl_base_t {
    bool initialized;
    bool selected;
    pmix_list_t posted_recvs; // list of pmix_ptl_posted_recv_t
    pmix_list_t unexpected_msgs;
    pmix_listener_t listener;
    struct sockaddr_storage *connection;
    uint32_t current_tag;
    size_t max_msg_size;
    char *session_tmpdir;
    char *system_tmpdir;
    char *report_uri;
    char *uri;
    char *urifile;
    char *sysctrlr_filename;
    char *scheduler_filename;
    char *system_filename;
    char *session_filename;
    char *nspace_filename;
    char *pid_filename;
    char *rendezvous_filename;
    bool created_rendezvous_file;
    bool created_session_tmpdir;
    bool created_system_tmpdir;
    bool created_sysctrlr_filename;
    bool created_scheduler_filename;
    bool created_system_filename;
    bool created_session_filename;
    bool created_nspace_filename;
    bool created_pid_filename;
    bool created_urifile;
    bool remote_connections;
    bool system_tool;
    bool session_tool;
    bool tool_support;
    char *if_include;
    char *if_exclude;
    int ipv4_port;
    bool disable_ipv4_family;
    int ipv6_port;
    bool disable_ipv6_family;
    int max_retries;
    int wait_to_connect;
    int handshake_wait_time;
    int handshake_max_retries;
};
typedef struct pmix_ptl_base_t pmix_ptl_base_t;

PMIX_EXPORT extern pmix_ptl_base_t pmix_ptl_base;

typedef struct {
    pmix_list_item_t super;
    int sd;
    char *nspace;
    pmix_rank_t rank;
    char *uri;
    char *version;
} pmix_connection_t;
PMIX_CLASS_DECLARATION(pmix_connection_t);

/* API stubs */
PMIX_EXPORT pmix_status_t pmix_ptl_base_set_notification_cbfunc(pmix_ptl_cbfunc_t cbfunc);
PMIX_EXPORT pmix_status_t pmix_ptl_base_connect_to_peer(struct pmix_peer_t *peer,
                                                        pmix_info_t info[], size_t ninfo);
PMIX_EXPORT pmix_status_t pmix_ptl_base_parse_uri_file(char *filename,
                                                       bool optional,
                                                       pmix_list_t *connections);

PMIX_EXPORT pmix_status_t pmix_ptl_base_setup_connection(char *uri,
                                                         struct sockaddr_storage *connection,
                                                         size_t *len);

PMIX_EXPORT void pmix_ptl_base_post_recv(int fd, short args, void *cbdata);
PMIX_EXPORT void pmix_ptl_base_cancel_recv(int sd, short args, void *cbdata);

PMIX_EXPORT pmix_status_t pmix_ptl_base_start_listening(pmix_info_t info[], size_t ninfo);
PMIX_EXPORT void pmix_ptl_base_stop_listening(void);
PMIX_EXPORT pmix_status_t pmix_base_write_rndz_file(char *filename, char *uri, bool *created);

/* base support functions */
PMIX_EXPORT pmix_status_t pmix_ptl_base_check_server_uris(pmix_peer_t *peer, char **evar);
PMIX_EXPORT pmix_status_t pmix_ptl_base_check_directives(pmix_info_t *info, size_t ninfo);
PMIX_EXPORT pmix_status_t pmix_ptl_base_setup_fork(const pmix_proc_t *proc, char ***env);
PMIX_EXPORT void pmix_ptl_base_send_handler(int sd, short flags, void *cbdata);
PMIX_EXPORT void pmix_ptl_base_recv_handler(int sd, short flags, void *cbdata);
PMIX_EXPORT void pmix_ptl_base_process_msg(int fd, short flags, void *cbdata);
PMIX_EXPORT pmix_status_t pmix_ptl_base_set_nonblocking(int sd);
PMIX_EXPORT pmix_status_t pmix_ptl_base_set_blocking(int sd);
PMIX_EXPORT pmix_status_t pmix_ptl_base_send_blocking(int sd, char *ptr, size_t size);
PMIX_EXPORT pmix_status_t pmix_ptl_base_recv_blocking(int sd, char *data, size_t size);
PMIX_EXPORT pmix_status_t pmix_ptl_base_connect(struct sockaddr_storage *addr, pmix_socklen_t len,
                                                int *fd);
PMIX_EXPORT void pmix_ptl_base_connection_handler(int sd, short args, void *cbdata);
PMIX_EXPORT pmix_status_t pmix_ptl_base_setup_listener(pmix_info_t info[], size_t ninfo);
PMIX_EXPORT pmix_status_t pmix_ptl_base_send_connect_ack(int sd);
PMIX_EXPORT pmix_status_t pmix_ptl_base_recv_connect_ack(int sd);
PMIX_EXPORT void pmix_ptl_base_lost_connection(pmix_peer_t *peer, pmix_status_t err);
PMIX_EXPORT bool pmix_ptl_base_peer_is_earlier(pmix_peer_t *peer, uint8_t major, uint8_t minor,
                                               uint8_t release);
PMIX_EXPORT void pmix_ptl_base_query_servers(int sd, short args, void *cbdata);
PMIX_EXPORT pmix_status_t pmix_ptl_base_parse_uri(const char *evar, char **nspace,
                                                  pmix_rank_t *rank, char **suri);
PMIX_EXPORT pmix_status_t pmix_ptl_base_df_search(char *dirname, char *prefix, pmix_info_t info[],
                                                  size_t ninfo, bool optional, pmix_list_t *connections);
PMIX_EXPORT pmix_rnd_flag_t pmix_ptl_base_set_flag(size_t *sz);
PMIX_EXPORT pmix_status_t pmix_ptl_base_make_connection(pmix_peer_t *peer, char *suri,
                                                        pmix_info_t *iptr, size_t niptr);
PMIX_EXPORT void pmix_ptl_base_complete_connection(pmix_peer_t *peer, char *nspace,
                                                   pmix_rank_t rank, char *uri);
PMIX_EXPORT pmix_status_t pmix_ptl_base_set_timeout(pmix_peer_t *peer, struct timeval *save,
                                                    pmix_socklen_t *sz, bool *sockopt);
PMIX_EXPORT void pmix_ptl_base_setup_socket(pmix_peer_t *peer);
PMIX_EXPORT pmix_status_t pmix_ptl_base_client_handshake(pmix_peer_t *peer, pmix_status_t reply);
PMIX_EXPORT pmix_status_t pmix_ptl_base_tool_handshake(pmix_peer_t *peer, pmix_status_t rp);
PMIX_EXPORT char **pmix_ptl_base_split_and_resolve(const char *orig_str,
                                                   const char *name);
PMIX_EXPORT pmix_status_t pmix_ptl_base_connect_to_peer(struct pmix_peer_t *pr, pmix_info_t *info,
                                                        size_t ninfo);
PMIX_EXPORT pmix_status_t pmix_ptl_base_set_peer(pmix_peer_t *peer, char *evar);
PMIX_EXPORT char *pmix_ptl_base_get_cmd_line(void);

END_C_DECLS

#endif
