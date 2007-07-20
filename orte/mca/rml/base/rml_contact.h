#include "orte/mca/ns/ns.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/gpr/gpr_types.h"


BEGIN_C_DECLS

/*
 * Get contact info for a process or job
 * Returns contact info for the specified process. If the vpid in the process name
 * is WILDCARD, then it returns the contact info for all processes in the specified
 * job. If the jobid is WILDCARD, then it returns the contact info for processes
 * of the specified vpid across all jobs. Obviously, combining the two WILDCARD
 * values will return contact info for everyone!
 */
int orte_rml_base_get_contact_info(orte_process_name_t *name, 
                                   orte_gpr_notify_data_t **data);

int orte_rml_base_register_subscription(orte_jobid_t jobid, char *trigger);

int orte_rml_base_register_contact_info(void);

void orte_rml_base_contact_info_notify(orte_gpr_notify_data_t* data,
                                       void* cbdata);

int orte_rml_base_parse_uris(const char* uri,
                             orte_process_name_t* peer, 
                             char*** uris);

END_C_DECLS
