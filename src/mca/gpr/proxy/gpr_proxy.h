/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#ifndef GPR_PROXY_H
#define GPR_PROXY_H


#include "ompi_config.h"
#include "include/types.h"
#include "include/constants.h"

#include "threads/mutex.h"

#include "class/ompi_list.h"
#include "mca/gpr/base/base.h"

/*
 * Module open / close
 */
int mca_gpr_proxy_open(void);
int mca_gpr_proxy_close(void);


/*
 * Startup / Shutdown
 */
mca_gpr_base_module_t* mca_gpr_proxy_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority);
int mca_gpr_proxy_finalize(void);

/*
 * proxy-local types
 */


/*
 * globals used within proxy component
 */

extern ompi_process_name_t *mca_gpr_my_replica;
extern ompi_list_t mca_gpr_proxy_notify_request_tracker;
extern mca_gpr_notify_id_t mca_gpr_proxy_last_notify_id_tag;
extern ompi_list_t mca_gpr_proxy_free_notify_id_tags;
extern int mca_gpr_proxy_debug;
extern ompi_mutex_t mca_gpr_proxy_mutex;

/*
 * Implementation of delete_segment().
 */
 int gpr_proxy_delete_segment(char *segment);

/*
 * Implementation of put()
 */
int gpr_proxy_put(ompi_registry_mode_t mode, char *segment,
		  char **tokens, ompi_registry_object_t object,
		  ompi_registry_object_size_t size);

/*
 * Implementation of delete()
 */
int gpr_proxy_delete_object(ompi_registry_mode_t mode,
			    char *segment, char **tokens);

/*
 * Implementation of index()
 */
ompi_list_t* gpr_proxy_index(char *segment);


/*
 * Implementation of subscribe()
 */
int gpr_proxy_subscribe(ompi_registry_mode_t mode,
			ompi_registry_notify_action_t action,
			char *segment, char **tokens,
			ompi_registry_notify_cb_fn_t cb_func, void *user_tag);

int gpr_proxy_unsubscribe(ompi_registry_mode_t mode,
			  ompi_registry_notify_action_t action,
			  char *segment, char **tokens);

int gpr_proxy_synchro(ompi_registry_synchro_mode_t synchro_mode,
		      ompi_registry_mode_t mode,
		      char *segment, char **tokens, int trigger,
		      ompi_registry_notify_cb_fn_t cb_func, void *user_tag);

int gpr_proxy_cancel_synchro(ompi_registry_synchro_mode_t synchro_mode,
			     ompi_registry_mode_t addr_mode,
			     char *segment, char **tokens, int trigger);
/*
 * Implementation of get()
 */
ompi_list_t* gpr_proxy_get(ompi_registry_mode_t mode,
			   char *segment, char **tokens);


ompi_list_t* gpr_proxy_test_internals(int level);

void mca_gpr_proxy_notify_recv(int status, ompi_process_name_t* sender,
			       ompi_buffer_t buffer, int tag,
			       void* cbdata);

#endif
