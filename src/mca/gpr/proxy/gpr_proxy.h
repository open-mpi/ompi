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
mca_ns_t* mca_gpr_proxy_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority);
int mca_gpr_proxy_finalize(void);

/*
 * globals used within proxy component
 */

extern ompi_process_name_t *mca_gpr_my_replica;


/*
 * Implementation of define_segment().
 */
int gpr_proxy_define_segment(char *segment);

/*
 * Implementation of delete_segment().
 */
 int gpr_proxy_delete_segment(char *segment);

/*
 * Implementation of put()
 */
int gpr_proxy_put(ompi_registry_mode_t mode, char *segment,
		  char **tokens, ompi_registry_object_t *object,
		  int size);

/*
 * Implementation of delete()
 */
int gpr_proxy_delete(ompi_registry_mode_t mode,
		     char *segment, char **tokens);

/*
 * Implementation of index()
 */
ompi_keytable_t* gpr_proxy_index(char *segment);


/*
 * Implementation of subscribe()
 */
int gpr_proxy_subscribe(ompi_registry_mode_t mode,
			ompi_registry_action_t action,
			char *segment, char **tokens);

/*
 * Implementation of unsubscribe()
 */
int gpr_proxy_unsubscribe(ompi_registry_mode_t mode,
			  char *segment, char **tokens);

/*
 * Implementation of get()
 */
ompi_registry_value_t* gpr_proxy_get(ompi_registry_mode_t mode,
				     char *segment, char **tokens);


#endif
