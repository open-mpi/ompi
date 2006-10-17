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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_RML_CNOS_H
#define MCA_RML_CNOS_H

#include "orte/mca/rml/rml.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C"
{
#endif

  ORTE_DECLSPEC extern orte_rml_component_t mca_rml_cnos_component;

  int orte_rml_cnos_open(void);
  int orte_rml_cnos_close(void);
  orte_rml_module_t * orte_rml_cnos_init(int *priority);

  int orte_rml_cnos_module_init(void);
  int orte_rml_cnos_module_fini(void);

  char *orte_rml_cnos_get_uri(void);
  int orte_rml_cnos_set_uri(const char *);
  int orte_rml_cnos_parse_uris(const char *uri,
				 orte_process_name_t * peer, char ***uris);
  int orte_rml_cnos_ping(const char *uri, const struct timeval *tv);

  int orte_rml_cnos_send(orte_process_name_t * peer,
			   struct iovec *msg,
			   int count, int tag, int flags);

  int orte_rml_cnos_send_buffer(orte_process_name_t * peer,
				  orte_buffer_t * buffer,
				  orte_rml_tag_t tag, int flags);

  int orte_rml_cnos_send_nb(orte_process_name_t * peer,
			      struct iovec *msg,
			      int count,
			      orte_rml_tag_t tag,
			      int flags,
			      orte_rml_callback_fn_t cbfunc, void *cbdata);

  int orte_rml_cnos_send_buffer_nb(orte_process_name_t * peer,
				     orte_buffer_t * buffer,
				     orte_rml_tag_t tag,
				     int flags,
				     orte_rml_buffer_callback_fn_t cbfunc,
				     void *cbdata);

  int orte_rml_cnos_recv(orte_process_name_t * peer,
			   struct iovec *msg,
			   int count, orte_rml_tag_t tag, int flags);

  int orte_rml_cnos_recv_buffer(orte_process_name_t * peer,
				  orte_buffer_t * buf, orte_rml_tag_t tag);

  int orte_rml_cnos_recv_nb(orte_process_name_t * peer,
			      struct iovec *msg,
			      int count,
			      orte_rml_tag_t tag,
			      int flags,
			      orte_rml_callback_fn_t cbfunc, void *cbdata);

  int orte_rml_cnos_recv_buffer_nb(orte_process_name_t * peer,
				     orte_rml_tag_t tag,
				     int flags,
				     orte_rml_buffer_callback_fn_t cbfunc,
                                     void *cbdata);

  int orte_rml_cnos_recv_cancel(orte_process_name_t * peer,
				  orte_rml_tag_t tag);


  int orte_rml_cnos_barrier(void);

  int orte_rml_cnos_xcast(orte_process_name_t * root,
                          orte_process_name_t * peers,
                          size_t num_peers,
                          orte_buffer_t * buffer,
                          orte_gpr_trigger_cb_fn_t cbfunc,
                          void *user_tag);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
