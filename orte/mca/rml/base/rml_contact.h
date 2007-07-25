/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Interface for manipulating how the RML receives contact information
 *
 * Interface for manipulating how the RML receives contact
 * information.  These functions are generally used during orte_init
 * and orte_finalize.
 */


#include "orte_config.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/gpr/gpr_types.h"

BEGIN_C_DECLS


/* ******************************************************************** */


/**
 * Create packed RML contact information for the given process names
 *
 * Create packed RML contact information for the given process names.
 * The information is provided packed in an orte_gpr_notify_data_t
 * structure.
 *
 * @note If the vpid in the process name is WILDCARD, then it returns
 * the contact info for all processes in the specified job. If the
 * jobid is WILDCARD, then it returns the contact info for processes
 * of the specified vpid across all jobs. Obviously, combining the two
 * WILDCARD values will return contact info for everyone!
 *
 * @param[in] name      Process name specifying contact information needed
 * @param[out] data     Contact information in GPR notify format for
 *                      \c name.
 *
 * @retval ORTE_SUCCESS Successfully found contact information
 * @retval ORTE_ERROR   Contact information could not be found or shared
 */
ORTE_DECLSPEC int orte_rml_base_get_contact_info(orte_process_name_t *name, 
                                                 orte_gpr_notify_data_t **data);


/**
 * Update the RML with contact information
 *
 * Update the RML with contact information provided from a call to
 * orte_rml_base_get_contact_info(), likely on another host.
 *
 * @note The function signature of this function is strange because it
 * also acts as a callback from the GPR on information update.
 *
 * @param[in] data      Contact information in GPR notify format,
 *                      obtained by call to orte_rml_base_get_contact_info()
 * @prarm[in] cbdata    Unused
 */
ORTE_DECLSPEC void orte_rml_base_contact_info_notify(orte_gpr_notify_data_t* data,
                                                     void* cbdata);


/**
 * Register a subscription for contact information updates
 *
 * Register a subscription with the GPR to receive all contact
 * information and updates associated with \c jobid.
 *
 * @param[in] jobid     Jobid for which infromation is needed
 * @param[in] trigger   Trigger on which the subscription should
 *                      be attached
 *
 * @retval ORTE_SUCCESS Successfully subscribed to information
 * @retval ORTE_ERROR   An unspecified error occurred
 */
ORTE_DECLSPEC int orte_rml_base_register_subscription(orte_jobid_t jobid, 
                                                      char *trigger);

/**
 * Publish local contact information
 *
 * Publish local contact information into the GPR.  The published
 * contact information is the same string returned from
 * orte_rml.get_contact_info().
 *
 * @retval ORTE_SUCCESS Successfully published contact information
 * @retval ORTE_ERR_OUT_OF_RESOURCE No memory for contact information available
 * @retval ORTE_ERROR   An unspecified error occurred
 */
ORTE_DECLSPEC int orte_rml_base_register_contact_info(void);


/**
 * Parse a contact information string
 *
 * Parse a contact infromation string, such as that returned by
 * orte_rml.get_contact_info().  Generally used to extract the peer
 * name from a contact information string.  It can also be used to
 * extract the contact URI strings, although this is slightly less
 * useful as the URIs may be RML componenent specific and not have
 * general meaning.
 *
 * @param[in] contact_info  Contact information string for peer
 * @param[out] peer         Peer name in contact_info
 * @param[out] uris         URI strings for peer.  May be NULL if
 *                          information is not needed
 *
 * @retval ORTE_SUCCESS     Information successfully extraced
 * @retval ORTE_ERR_BAD_PARAM The contact_info was not a valid string
 * @retval ORTE_ERROR       An unspecified error occurred
 */
ORTE_DECLSPEC int orte_rml_base_parse_uris(const char* contact_inf,
                                           orte_process_name_t* peer, 
                                           char*** uris);

END_C_DECLS
