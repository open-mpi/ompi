/*
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry.
 *
 * The Open MPI system contains a general purpose registry for use by both
 * applications and internal systems to dynamically share information. For
 * speed purposes, the registry is divided into "segments", each labelled
 * with an appropriate "token" string that describes its contents. Segments
 * are automatically provided for the "universe" and for each MPI CommWorld.
 * At this time, all segments may be accessed by any application within the universe, thus
 * providing a mechanism for cross-CommWorld communications (with the requirement
 * that all participating CommWorlds must reside within the same universe). In the future,
 * some form of security may be provided to limit access privileges between
 * segments.
 *
 * Within each registry segment, there exists a list of objects that have
 * been "put" onto the registry. Each object must be tagged with at least
 * one token, but may be tagged with as many tokens as the creator desires.
 * Retrieval requests must specify the segment and at least one token, but
 * can specify an arbitrary number of tokens to describe the search. The registry
 * will return a list of all objects that meet the search criteria.
 *
 * Tokens are defined as character strings, thus allowing for clarity in
 * the program. However, for speed purposes, tokens are translated into
 * integer keys prior to storing an object. A table of token-key pairs
 * is independently maintained for each registry segment. Users can obtain
 * an index of tokens within a dictionary by requesting it through the ompi_registry_index()
 * function.
 *
 * The registry also provides a subscription capability whereby a caller
 * can subscribe to a stored object and receive notification when various actions
 * are performed on that object. Currently supported actions include modification,
 * the addition of another subscriber, and deletion. Notifications are sent via
 * the OOB communication channel.
 *
 * 
 */

#ifndef MCA_GPR_BASE_H_
#define MCA_GRP_BASE_H_

/*
 * includes
 */
#include <sys/types.h>
#include <stdint.h>

#include "ompi_config.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/gpr/gpr.h"

/*
* Global functions for MCA overall collective open and close
*/
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    int mca_gpr_base_open(void);
    int mca_gpr_base_select(bool *allow_multi_user_threads,
			    bool *have_hidden_threads);
    int mca_gpr_base_close(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * globals that might be needed
 */
extern int mca_gpr_base_output;
extern mca_gpr_base_module_t ompi_registry; /* holds selected module's function pointers */
extern bool mca_gpr_base_selected;
extern ompi_list_t mca_gpr_base_components_available;
extern mca_gpr_base_component_t mca_gpr_base_selected_component;


#endif
