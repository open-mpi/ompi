/*
 *
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c)      2012 UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#ifndef OPAL_RENAME_H_
#define OPAL_RENAME_H_

#ifdef __cplusplus
extern "C" {
#endif

/* buffer.c */
#define _evbuffer_chain_pin                                  opal_libevent2022_evbuffer_chain_pin
#define _evbuffer_chain_unpin                                opal_libevent2022_evbuffer_chain_unpin
#define _evbuffer_decref_and_unlock                          opal_libevent2022_evbuffer_decref_and_unlock
#define _evbuffer_expand_fast                                opal_libevent2022_evbuffer_expand_fast
#define _evbuffer_incref                                     opal_libevent2022_evbuffer_incref
#define _evbuffer_incref_and_lock                            opal_libevent2022_evbuffer_incref_and_lock
#define _evbuffer_read_setup_vecs                            opal_libevent2022_evbuffer_read_setup_vecs
#define _evbuffer_testing_use_linear_file_access             opal_libevent2022_evbuffer_testing_use_linear_file_access
#define _evbuffer_testing_use_mmap                           opal_libevent2022_evbuffer_testing_use_mmap
#define _evbuffer_testing_use_sendfile                       opal_libevent2022_evbuffer_testing_use_sendfile
#define evbuffer_add                                         opal_libevent2022_evbuffer_add
#define evbuffer_add_buffer                                  opal_libevent2022_evbuffer_add_buffer
#define evbuffer_add_cb                                      opal_libevent2022_evbuffer_add_cb
#define evbuffer_add_file                                    opal_libevent2022_evbuffer_add_file
#define evbuffer_add_printf                                  opal_libevent2022_evbuffer_add_printf
#define evbuffer_add_reference                               opal_libevent2022_evbuffer_add_reference
#define evbuffer_add_vprintf                                 opal_libevent2022_evbuffer_add_vprintf
#define evbuffer_cb_clear_flags                              opal_libevent2022_evbuffer_cb_clear_flags
#define evbuffer_cb_set_flags                                opal_libevent2022_evbuffer_cb_set_flags
#define evbuffer_clear_flags                                 opal_libevent2022_evbuffer_clear_flags
#define evbuffer_commit_space                                opal_libevent2022_evbuffer_commit_space
#define evbuffer_copyout                                     opal_libevent2022_evbuffer_copyout
#define evbuffer_defer_callbacks                             opal_libevent2022_evbuffer_defer_callbacks
#define evbuffer_drain                                       opal_libevent2022_evbuffer_drain
#define evbuffer_enable_locking                              opal_libevent2022_evbuffer_enable_locking
#define evbuffer_expand                                      opal_libevent2022_evbuffer_expand
#define evbuffer_find                                        opal_libevent2022_evbuffer_find
#define evbuffer_free                                        opal_libevent2022_evbuffer_free
#define evbuffer_freeze                                      opal_libevent2022_evbuffer_freeze
#define evbuffer_get_contiguous_space                        opal_libevent2022_evbuffer_get_contiguous_space
#define evbuffer_get_length                                  opal_libevent2022_evbuffer_get_length
#define evbuffer_invoke_callbacks                            opal_libevent2022_evbuffer_invoke_callbacks
#define evbuffer_lock                                        opal_libevent2022_evbuffer_lock
#define evbuffer_new                                         opal_libevent2022_evbuffer_new
#define evbuffer_peek                                        opal_libevent2022_evbuffer_peek
#define evbuffer_prepend                                     opal_libevent2022_evbuffer_prepend
#define evbuffer_prepend_buffer                              opal_libevent2022_evbuffer_prepend_buffer
#define evbuffer_ptr_set                                     opal_libevent2022_evbuffer_ptr_set
#define evbuffer_pullup                                      opal_libevent2022_evbuffer_pullup
#define evbuffer_read                                        opal_libevent2022_evbuffer_read
#define evbuffer_readline                                    opal_libevent2022_evbuffer_readline
#define evbuffer_readln                                      opal_libevent2022_evbuffer_readln
#define evbuffer_remove                                      opal_libevent2022_evbuffer_remove
#define evbuffer_remove_buffer                               opal_libevent2022_evbuffer_remove_buffer
#define evbuffer_remove_cb                                   opal_libevent2022_evbuffer_remove_cb
#define evbuffer_remove_cb_entry                             opal_libevent2022_evbuffer_remove_cb_entry
#define evbuffer_reserve_space                               opal_libevent2022_evbuffer_reserve_space
#define evbuffer_search                                      opal_libevent2022_evbuffer_search
#define evbuffer_search_eol                                  opal_libevent2022_evbuffer_search_eol
#define evbuffer_search_range                                opal_libevent2022_evbuffer_search_range
#define evbuffer_set_flags                                   opal_libevent2022_evbuffer_set_flags
#define evbuffer_set_parent                                  opal_libevent2022_evbuffer_set_parent
#define evbuffer_setcb                                       opal_libevent2022_evbuffer_setcb
#define evbuffer_unfreeze                                    opal_libevent2022_evbuffer_unfreeze
#define evbuffer_unlock                                      opal_libevent2022_evbuffer_unlock
#define evbuffer_write                                       opal_libevent2022_evbuffer_write
#define evbuffer_write_atmost                                opal_libevent2022_evbuffer_write_atmost
#define _bufferevent_add_event                               opal_libevent2022__bufferevent_add_event
#define _bufferevent_decref_and_unlock                       opal_libevent2022__bufferevent_decref_and_unlock
#define _bufferevent_del_generic_timeout_cbs                 opal_libevent2022__bufferevent_del_generic_timeout_cbs
#define _bufferevent_generic_adj_timeouts                    opal_libevent2022__bufferevent_generic_adj_timeouts
#define _bufferevent_incref_and_lock                         opal_libevent2022__bufferevent_incref_and_lock
#define _bufferevent_init_generic_timeout_cbs                opal_libevent2022__bufferevent_init_generic_timeout_cbs
#define _bufferevent_run_eventcb                             opal_libevent2022__bufferevent_run_eventcb
#define _bufferevent_run_readcb                              opal_libevent2022__bufferevent_run_readcb
#define _bufferevent_run_writecb                             opal_libevent2022__bufferevent_run_writecb
#define bufferevent_decref                                   opal_libevent2022_bufferevent_decref
#define bufferevent_disable                                  opal_libevent2022_bufferevent_disable
#define bufferevent_disable_hard                             opal_libevent2022_bufferevent_disable_hard
#define bufferevent_enable                                   opal_libevent2022_bufferevent_enable
#define bufferevent_enable_locking                           opal_libevent2022_bufferevent_enable_locking
#define bufferevent_flush                                    opal_libevent2022_bufferevent_flush
#define bufferevent_free                                     opal_libevent2022_bufferevent_free
#define bufferevent_get_base                                 opal_libevent2022_bufferevent_get_base
#define bufferevent_get_enabled                              opal_libevent2022_bufferevent_get_enabled
#define bufferevent_get_input                                opal_libevent2022_bufferevent_get_input
#define bufferevent_get_output                               opal_libevent2022_bufferevent_get_output
#define bufferevent_get_underlying                           opal_libevent2022_bufferevent_get_underlying
#define bufferevent_getfd                                    opal_libevent2022_bufferevent_getfd
#define bufferevent_incref                                   opal_libevent2022_bufferevent_incref
#define bufferevent_init_common                              opal_libevent2022_bufferevent_init_common
#define bufferevent_lock                                     opal_libevent2022_bufferevent_lock
#define bufferevent_read                                     opal_libevent2022_bufferevent_read
#define bufferevent_read_buffer                              opal_libevent2022_bufferevent_read_buffer
#define bufferevent_set_timeouts                             opal_libevent2022_bufferevent_set_timeouts
#define bufferevent_setcb                                    opal_libevent2022_bufferevent_setcb
#define bufferevent_setfd                                    opal_libevent2022_bufferevent_setfd
#define bufferevent_settimeout                               opal_libevent2022_bufferevent_settimeout
#define bufferevent_setwatermark                             opal_libevent2022_bufferevent_setwatermark
#define bufferevent_suspend_read                             opal_libevent2022_bufferevent_suspend_read
#define bufferevent_suspend_write                            opal_libevent2022_bufferevent_suspend_write
#define bufferevent_unlock                                   opal_libevent2022_bufferevent_unlock
#define bufferevent_unsuspend_read                           opal_libevent2022_bufferevent_unsuspend_read
#define bufferevent_unsuspend_write                          opal_libevent2022_bufferevent_unsuspend_write
#define bufferevent_write                                    opal_libevent2022_bufferevent_write
#define bufferevent_write_buffer                             opal_libevent2022_bufferevent_write_buffer
#define bufferevent_filter_new                               opal_libevent2022_bufferevent_filter_new
#define bufferevent_get_openssl_error                        opal_libevent2022_bufferevent_get_openssl_error
#define bufferevent_openssl_filter_new                       opal_libevent2022_bufferevent_openssl_filter_new
#define bufferevent_openssl_get_ssl                          opal_libevent2022_bufferevent_openssl_get_ssl
#define bufferevent_openssl_socket_new                       opal_libevent2022_bufferevent_openssl_socket_new
#define bufferevent_ssl_renegotiate                          opal_libevent2022_bufferevent_ssl_renegotiate
#define bufferevent_pair_get_partner                         opal_libevent2022_bufferevent_pair_get_partner
#define bufferevent_pair_new                                 opal_libevent2022_bufferevent_pair_new
#define _bufferevent_decrement_read_buckets                  opal_libevent2022__bufferevent_decrement_read_buckets
#define _bufferevent_decrement_write_buckets                 opal_libevent2022__bufferevent_decrement_write_buckets
#define _bufferevent_get_read_max                            opal_libevent2022__bufferevent_get_read_max
#define _bufferevent_get_write_max                           opal_libevent2022__bufferevent_get_write_max
#define bufferevent_add_to_rate_limit_group                  opal_libevent2022_bufferevent_add_to_rate_limit_group
#define bufferevent_decrement_read_limit                     opal_libevent2022_bufferevent_decrement_read_limit
#define bufferevent_decrement_write_limit                    opal_libevent2022_bufferevent_decrement_write_limit
#define bufferevent_get_max_to_read                          opal_libevent2022_bufferevent_get_max_to_read
#define bufferevent_get_max_to_write                         opal_libevent2022_bufferevent_get_max_to_write
#define bufferevent_get_read_limit                           opal_libevent2022_bufferevent_get_read_limit
#define bufferevent_get_write_limit                          opal_libevent2022_bufferevent_get_write_limit
#define bufferevent_rate_limit_group_decrement_read          opal_libevent2022_bufferevent_rate_limit_group_decrement_read
#define bufferevent_rate_limit_group_decrement_write         opal_libevent2022_bufferevent_rate_limit_group_decrement_write
#define bufferevent_rate_limit_group_free                    opal_libevent2022_bufferevent_rate_limit_group_free
#define bufferevent_rate_limit_group_get_read_limit          opal_libevent2022_bufferevent_rate_limit_group_get_read_limit
#define bufferevent_rate_limit_group_get_totals              opal_libevent2022_bufferevent_rate_limit_group_get_totals
#define bufferevent_rate_limit_group_get_write_limit         opal_libevent2022_bufferevent_rate_limit_group_get_write_limit
#define bufferevent_rate_limit_group_new                     opal_libevent2022_bufferevent_rate_limit_group_new
#define bufferevent_rate_limit_group_reset_totals            opal_libevent2022_bufferevent_rate_limit_group_reset_totals
#define bufferevent_rate_limit_group_set_cfg                 opal_libevent2022_bufferevent_rate_limit_group_set_cfg
#define bufferevent_rate_limit_group_set_min_share           opal_libevent2022_bufferevent_rate_limit_group_set_min_share
#define bufferevent_remove_from_rate_limit_group             opal_libevent2022_bufferevent_remove_from_rate_limit_group
#define bufferevent_remove_from_rate_limit_group_internal    opal_libevent2022_bufferevent_remove_from_rate_limit_group_internal
#define bufferevent_set_rate_limit                           opal_libevent2022_bufferevent_set_rate_limit
#define bufferevent_base_set                                 opal_libevent2022_bufferevent_base_set
#define bufferevent_new                                      opal_libevent2022_bufferevent_new
#define bufferevent_priority_set                             opal_libevent2022_bufferevent_priority_set
#define bufferevent_socket_connect                           opal_libevent2022_bufferevent_socket_connect
#define bufferevent_socket_connect_hostname                  opal_libevent2022_bufferevent_socket_connect_hostname
#define bufferevent_socket_get_dns_error                     opal_libevent2022_bufferevent_socket_get_dns_error
#define bufferevent_socket_new                               opal_libevent2022_bufferevent_socket_new


/* tokens */
#define ev_token_bucket_cfg_free                             opal_libevent2022_ev_token_bucket_cfg_free
#define ev_token_bucket_cfg_new                              opal_libevent2022_ev_token_bucket_cfg_new
#define ev_token_bucket_get_tick                             opal_libevent2022_ev_token_bucket_get_tick
#define ev_token_bucket_init                                 opal_libevent2022_ev_token_bucket_init
#define ev_token_bucket_update                               opal_libevent2022_ev_token_bucket_update


/* debug */
#define _event_debug_map_HT_REP_IS_BAD                       opal_libevent2022__event_debug_map_HT_REP_IS_BAD
#define event_debug_map_HT_CLEAR                             opal_libevent2022_event_debug_map_HT_CLEAR
#define event_debug_map_HT_GROW                              opal_libevent2022_event_debug_map_HT_GROW
#define event_debug_unassign                                 opal_libevent2022_event_debug_unassign
#define _event_debugx                                        opal_libevent2022__event_debugx
#define _event_debug_mode_on                                 opal_libevent2022__event_debug_mode_on

/* event.c */
#define event_active                                         opal_libevent2022_event_active
#define event_active_nolock                                  opal_libevent2022_event_active_nolock
#define event_add                                            opal_libevent2022_event_add
#define event_assign                                         opal_libevent2022_event_assign
#define event_base_add_virtual                               opal_libevent2022_event_base_add_virtual
#define event_base_assert_ok                                 opal_libevent2022_event_base_assert_ok
#define event_base_del_virtual                               opal_libevent2022_event_base_del_virtual
#define event_base_dispatch                                  opal_libevent2022_event_base_dispatch
#define event_base_dump_events                               opal_libevent2022_event_base_dump_events
#define event_base_free                                      opal_libevent2022_event_base_free
#define event_base_get_deferred_cb_queue                     opal_libevent2022_event_base_get_deferred_cb_queue
#define event_base_get_features                              opal_libevent2022_event_base_get_features
#define event_base_get_method                                opal_libevent2022_event_base_get_method
#define event_base_gettimeofday_cached                       opal_libevent2022_event_base_gettimeofday_cached
#define event_base_got_break                                 opal_libevent2022_event_base_got_break
#define event_base_got_exit                                  opal_libevent2022_event_base_got_exit
#define event_base_init_common_timeout                       opal_libevent2022_event_base_init_common_timeout
#define event_base_loop                                      opal_libevent2022_event_base_loop
#define event_base_loopexit                                  opal_libevent2022_event_base_loopexit
#define event_base_new                                       opal_libevent2022_event_base_new
#define event_base_new_with_config                           opal_libevent2022_event_base_new_with_config
#define event_base_once                                      opal_libevent2022_event_base_once
#define event_base_priority_init                             opal_libevent2022_event_base_priority_init
#define event_base_set                                       opal_libevent2022_event_base_set
#define event_base_start_iocp                                opal_libevent2022_event_base_start_iocp
#define event_base_stop_iocp                                 opal_libevent2022_event_base_stop_iocp
#define event_config_avoid_method                            opal_libevent2022_event_config_avoid_method
#define event_config_free                                    opal_libevent2022_event_config_free
#define event_config_new                                     opal_libevent2022_event_config_new
#define event_config_require_features                        opal_libevent2022_event_config_require_features
#define event_config_set_flag                                opal_libevent2022_event_config_set_flag
#define event_config_set_num_cpus_hint                       opal_libevent2022_event_config_set_num_cpus_hint
#define event_deferred_cb_cancel                             opal_libevent2022_event_deferred_cb_cancel
#define event_deferred_cb_init                               opal_libevent2022_event_deferred_cb_init
#define event_deferred_cb_queue_init                         opal_libevent2022_event_deferred_cb_queue_init
#define event_deferred_cb_schedule                           opal_libevent2022_event_deferred_cb_schedule
#define event_del                                            opal_libevent2022_event_del
#define event_dispatch                                       opal_libevent2022_event_dispatch
#define event_enable_debug_mode                              opal_libevent2022_event_enable_debug_mode
#define event_enable_debug_output                            opal_libevent2022_event_enable_debug_output
#define event_free                                           opal_libevent2022_event_free
#define event_get_assignment                                 opal_libevent2022_event_get_assignment
#define event_get_base                                       opal_libevent2022_event_get_base
#define event_get_callback                                   opal_libevent2022_event_get_callback
#define event_get_callback_arg                               opal_libevent2022_event_get_callback_arg
#define event_get_events                                     opal_libevent2022_event_get_events
#define event_get_fd                                         opal_libevent2022_event_get_fd
#define event_get_method                                     opal_libevent2022_event_get_method
#define event_get_struct_event_size                          opal_libevent2022_event_get_struct_event_size
#define event_get_supported_methods                          opal_libevent2022_event_get_supported_methods
#define event_get_version                                    opal_libevent2022_event_get_version
#define event_get_version_number                             opal_libevent2022_event_get_version_number
#define event_init                                           opal_libevent2022_event_init
#define _event_initialized                                   opal_libevent2022__event_initialized
#define event_initialized                                    opal_libevent2022_event_initialized
#define event_loop                                           opal_libevent2022_event_loop
#define event_loopbreak                                      opal_libevent2022_event_loopbreak
#define event_loopexit                                       opal_libevent2022_event_loopexit
#define event_mm_calloc_                                     opal_libevent2022_event_mm_calloc_
#define event_mm_free_                                       opal_libevent2022_event_mm_free_
#define event_mm_malloc_                                     opal_libevent2022_event_mm_malloc_
#define event_mm_realloc_                                    opal_libevent2022_event_mm_realloc_
#define event_mm_strdup_                                     opal_libevent2022_event_mm_strdup_
#define event_new                                            opal_libevent2022_event_new
#define event_once                                           opal_libevent2022_event_once
#define event_pending                                        opal_libevent2022_event_pending
#define event_priority_init                                  opal_libevent2022_event_priority_init
#define event_priority_set                                   opal_libevent2022_event_priority_set
#define event_reinit                                         opal_libevent2022_event_reinit
#define event_set                                            opal_libevent2022_event_set
#define event_set_mem_functions                              opal_libevent2022_event_set_mem_functions
#define event_changelist_add                                 opal_libevent2022_event_changelist_add
#define event_changelist_del                                 opal_libevent2022_event_changelist_del
#define event_changelist_freemem                             opal_libevent2022_event_changelist_freemem
#define event_changelist_init                                opal_libevent2022_event_changelist_init
#define event_changelist_remove_all                          opal_libevent2022_event_changelist_remove_all
#define event_err                                            opal_libevent2022_event_err
#define event_errx                                           opal_libevent2022_event_errx
#define event_msgx                                           opal_libevent2022_event_msgx
#define event_set_fatal_callback                             opal_libevent2022_event_set_fatal_callback
#define event_set_log_callback                               opal_libevent2022_event_set_log_callback
#define event_sock_err                                       opal_libevent2022_event_sock_err
#define event_sock_warn                                      opal_libevent2022_event_sock_warn
#define event_warn                                           opal_libevent2022_event_warn
#define event_warnx                                          opal_libevent2022_event_warnx

/* evutil.c*/
#define EVUTIL_ISALNUM                                       opal_libevent2022_EVUTIL_ISALNUM
#define EVUTIL_ISALPHA                                       opal_libevent2022_EVUTIL_ISALPHA
#define EVUTIL_ISDIGIT                                       opal_libevent2022_EVUTIL_ISDIGIT
#define EVUTIL_ISLOWER                                       opal_libevent2022_EVUTIL_ISLOWER
#define EVUTIL_ISPRINT                                       opal_libevent2022_EVUTIL_ISPRINT
#define EVUTIL_ISSPACE                                       opal_libevent2022_EVUTIL_ISSPACE
#define EVUTIL_ISUPPER                                       opal_libevent2022_EVUTIL_ISUPPER
#define EVUTIL_ISXDIGIT                                      opal_libevent2022_EVUTIL_ISXDIGIT
#define EVUTIL_TOLOWER                                       opal_libevent2022_EVUTIL_TOLOWER
#define EVUTIL_TOUPPER                                       opal_libevent2022_EVUTIL_TOUPPER
#define _evutil_weakrand                                     opal_libevent2022__evutil_weakrand
#define evutil_addrinfo_append                               opal_libevent2022_evutil_addrinfo_append
#define evutil_adjust_hints_for_addrconfig                   opal_libevent2022_evutil_adjust_hints_for_addrconfig
#define evutil_ascii_strcasecmp                              opal_libevent2022_evutil_ascii_strcasecmp
#define evutil_ascii_strncasecmp                             opal_libevent2022_evutil_ascii_strncasecmp
#define evutil_closesocket                                   opal_libevent2022_evutil_closesocket
#define evutil_ersatz_socketpair                             opal_libevent2022_evutil_ersatz_socketpair
#define evutil_format_sockaddr_port                          opal_libevent2022_evutil_format_sockaddr_port
#define evutil_freeaddrinfo                                  opal_libevent2022_evutil_freeaddrinfo
#define evutil_gai_strerror                                  opal_libevent2022_evutil_gai_strerror
#define evutil_getaddrinfo                                   opal_libevent2022_evutil_getaddrinfo
#define evutil_getaddrinfo_async                             opal_libevent2022_evutil_getaddrinfo_async
#define evutil_getaddrinfo_common                            opal_libevent2022_evutil_getaddrinfo_common
#define evutil_getenv                                        opal_libevent2022_evutil_getenv
#define evutil_hex_char_to_int                               opal_libevent2022_evutil_hex_char_to_int
#define evutil_inet_ntop                                     opal_libevent2022_evutil_inet_ntop
#define evutil_inet_pton                                     opal_libevent2022_evutil_inet_pton
#define evutil_make_listen_socket_reuseable                  opal_libevent2022_evutil_make_listen_socket_reuseable
#define evutil_make_socket_closeonexec                       opal_libevent2022_evutil_make_socket_closeonexec
#define evutil_make_socket_nonblocking                       opal_libevent2022_evutil_make_socket_nonblocking
#define evutil_new_addrinfo                                  opal_libevent2022_evutil_new_addrinfo
#define evutil_open_closeonexec                              opal_libevent2022_evutil_open_closeonexec
#define evutil_parse_sockaddr_port                           opal_libevent2022_evutil_parse_sockaddr_port
#define evutil_read_file                                     opal_libevent2022_evutil_read_file
#define evutil_secure_rng_get_bytes                          opal_libevent2022_evutil_secure_rng_get_bytes
#define evutil_secure_rng_global_setup_locks_                opal_libevent2022_evutil_secure_rng_global_setup_locks_
#define evutil_secure_rng_init                               opal_libevent2022_evutil_secure_rng_init
#define evutil_set_evdns_getaddrinfo_fn                      opal_libevent2022_evutil_set_evdns_getaddrinfo_fn
#define evutil_snprintf                                      opal_libevent2022_evutil_snprintf
#define evutil_sockaddr_cmp                                  opal_libevent2022_evutil_sockaddr_cmp
#define evutil_sockaddr_is_loopback                          opal_libevent2022_evutil_sockaddr_is_loopback
#define evutil_socket_connect                                opal_libevent2022_evutil_socket_connect
#define evutil_socket_finished_connecting                    opal_libevent2022_evutil_socket_finished_connecting
#define evutil_socketpair                                    opal_libevent2022_evutil_socketpair
#define evutil_strtoll                                       opal_libevent2022_evutil_strtoll
#define evutil_tv_to_msec                                    opal_libevent2022_evutil_tv_to_msec
#define evutil_vsnprintf                                     opal_libevent2022_evutil_vsnprintf


/* threads */
#define evthread_make_base_notifiable                        opal_libevent2022_evthread_make_base_notifiable
#define _evthread_debug_get_real_lock                        opal_libevent2022__evthread_debug_get_real_lock
#define _evthread_is_debug_lock_held                         opal_libevent2022__evthread_is_debug_lock_held
#define evthread_enable_lock_debuging                        opal_libevent2022_evthread_enable_lock_debuging
#define evthread_set_condition_callbacks                     opal_libevent2022_evthread_set_condition_callbacks
#define evthread_set_id_callback                             opal_libevent2022_evthread_set_id_callback
#define evthread_set_lock_callbacks                          opal_libevent2022_evthread_set_lock_callbacks
#define evthread_use_pthreads                                opal_libevent2022_evthread_use_pthreads


/* tags */
#define evtag_consume                                        opal_libevent2022_evtag_consume
#define evtag_decode_int                                     opal_libevent2022_evtag_decode_int
#define evtag_decode_int64                                   opal_libevent2022_evtag_decode_int64
#define evtag_decode_tag                                     opal_libevent2022_evtag_decode_tag
#define evtag_encode_int                                     opal_libevent2022_evtag_encode_int
#define evtag_encode_int64                                   opal_libevent2022_evtag_encode_int64
#define evtag_encode_tag                                     opal_libevent2022_evtag_encode_tag
#define evtag_init                                           opal_libevent2022_evtag_init
#define evtag_marshal                                        opal_libevent2022_evtag_marshal
#define evtag_marshal_buffer                                 opal_libevent2022_evtag_marshal_buffer
#define evtag_marshal_int                                    opal_libevent2022_evtag_marshal_int
#define evtag_marshal_int64                                  opal_libevent2022_evtag_marshal_int64
#define evtag_marshal_string                                 opal_libevent2022_evtag_marshal_string
#define evtag_marshal_timeval                                opal_libevent2022_evtag_marshal_timeval
#define evtag_payload_length                                 opal_libevent2022_evtag_payload_length
#define evtag_peek                                           opal_libevent2022_evtag_peek
#define evtag_peek_length                                    opal_libevent2022_evtag_peek_length
#define evtag_unmarshal                                      opal_libevent2022_evtag_unmarshal
#define evtag_unmarshal_fixed                                opal_libevent2022_evtag_unmarshal_fixed
#define evtag_unmarshal_header                               opal_libevent2022_evtag_unmarshal_header
#define evtag_unmarshal_int                                  opal_libevent2022_evtag_unmarshal_int
#define evtag_unmarshal_int64                                opal_libevent2022_evtag_unmarshal_int64
#define evtag_unmarshal_string                               opal_libevent2022_evtag_unmarshal_string
#define evtag_unmarshal_timeval                              opal_libevent2022_evtag_unmarshal_timeval

/* map */
#define evmap_check_integrity                                opal_libevent2022_evmap_check_integrity
#define evmap_io_active                                      opal_libevent2022_evmap_io_active
#define evmap_io_add                                         opal_libevent2022_evmap_io_add
#define evmap_io_clear                                       opal_libevent2022_evmap_io_clear
#define evmap_io_del                                         opal_libevent2022_evmap_io_del
#define evmap_io_get_fdinfo                                  opal_libevent2022_evmap_io_get_fdinfo
#define evmap_io_initmap                                     opal_libevent2022_evmap_io_initmap
#define evmap_signal_active                                  opal_libevent2022_evmap_signal_active
#define evmap_signal_add                                     opal_libevent2022_evmap_signal_add
#define evmap_signal_clear                                   opal_libevent2022_evmap_signal_clear
#define evmap_signal_del                                     opal_libevent2022_evmap_signal_del
#define evmap_signal_initmap                                 opal_libevent2022_evmap_signal_initmap


/* connections */
#define evconnlistener_disable                               opal_libevent2022_evconnlistener_disable
#define evconnlistener_enable                                opal_libevent2022_evconnlistener_enable
#define evconnlistener_free                                  opal_libevent2022_evconnlistener_free
#define evconnlistener_get_base                              opal_libevent2022_evconnlistener_get_base
#define evconnlistener_get_fd                                opal_libevent2022_evconnlistener_get_fd
#define evconnlistener_new                                   opal_libevent2022_evconnlistener_new
#define evconnlistener_new_bind                              opal_libevent2022_evconnlistener_new_bind
#define evconnlistener_set_cb                                opal_libevent2022_evconnlistener_set_cb
#define evconnlistener_set_error_cb                          opal_libevent2022_evconnlistener_set_error_cb

/* signal */
#define _evsig_restore_handler                               opal_libevent2022__evsig_restore_handler
#define _evsig_set_handler                                   opal_libevent2022__evsig_set_handler
#define evsig_dealloc                                        opal_libevent2022_evsig_dealloc
#define evsig_init                                           opal_libevent2022_evsig_init
#define evsig_process                                        opal_libevent2022_evsig_process
#define evsig_set_base                                       opal_libevent2022_evsig_set_base

/* eventop */
#ifdef _EVENT_HAVE_EVENT_PORTS
#define evportops                                            opal_libevent2022_evportops
#endif
#ifdef _EVENT_HAVE_SELECT
#define selectops                                            opal_libevent2022_selectops
#endif
#ifdef _EVENT_HAVE_POLL
#define pollops                                              opal_libevent2022_pollops
#endif
#ifdef _EVENT_HAVE_EPOLL
#define epollops                                             opal_libevent2022_epollops
#endif
#ifdef _EVENT_HAVE_WORKING_KQUEUE
#define kqops                                                opal_libevent2022_kqops
#endif
#ifdef _EVENT_HAVE_DEVPOLL
#define devpollops                                           opal_libevent2022_devpollops
#endif
#ifdef WIN32
#define win32ops                                             opal_libevent2022_win32ops
#endif

#ifdef __cplusplus
}
#endif

#endif
