/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - support functions.
 *
 */

#ifndef MCA_GPR_REPLICA_TL_H_
#define MCA_GPR_REPLICA_TL_H_

#include "orte_config.h"

#include "mca/gpr/replica/gpr_replica.h"

/*
 * DICTIONARY OPERATIONS
 */
 
/** Add a name to a segment's dictionary.
 * This function allows the addition of a new definition to
 * the registry's dictionaries. The specified string is assigned an integer
 * value within the specified segment, and the entry is added to the segment's token-itag
 * dictionary.
 *
 * @param *seg Pointer to the segment of the registry.
 * @param name Pointer to a character string containing the string to be defined.
 *
 * @param *itag (OUT) orte_gpr_replica_itag_t value of corresponding name.
 * 
 * @retval ORTE_SUCCESS Operation successful
 * @retval ORTE_ERROR(s) Indicates that the dictionary is full or some other error.
 */
int orte_gpr_replica_create_itag(orte_gpr_replica_itag_t *itag,
                                 orte_gpr_replica_segment_t *seg, char *name);

/** Delete a name from a segment's dictionary.
 * This function allows the removal of a definition from the
 * registry's dictionaries. This should be used with caution! Deletion of
 * a name causes the registry to search through all entries within that segment
 * for entries that include the specified name in their description. The reference
 * is subsequently removed, and any entry for which this was the SOLE descriptor will also
 * be removed from the registry!
 *
 * @param *seg Pointer to the segment of the registry.
 * @param name Pointer to a character string containing the name to be deleted.
 *
 * @retval ORTE_SUCCESS Indicating that the operation was successful.
 * @retval ORTE_ERROR Indicates that the operation failed - most likely caused by specifying
 * a name that did not exist within the specified segment, or a non-existent segment.
 */
int orte_gpr_replica_delete_itag(orte_gpr_replica_segment_t *seg, char *name);


int orte_gpr_replica_dict_lookup(orte_gpr_replica_itag_t *itag,
                        orte_gpr_replica_segment_t *seg, char *name);


int orte_gpr_replica_dict_reverse_lookup(char **name,
        orte_gpr_replica_segment_t *seg, orte_gpr_replica_itag_t itag);


/*
 * Get a list of itags for a list of string names
 * Given a list of string names, this function will look them up in the specified
 * segment's dictionary to see if they exist. If they do, then the function adds the
 * corresponding itag to the list of itag values that will be returned to the caller.
 * If the string doesn't exist in the dictionary, then the function has a dictionary
 * entry created and includes that itag in the return list.
 * 
 * Providing a value of NULL for the list of string names will return a NULL list of
 * itags. This corresponds to a wildcard case. The function will return the NULL list
 * and provide an ORTE_SUCCESS response to the caller.
 * 
 * Providing a name with a wildcard character in it will cause the function to look
 * for all itag values that match the specified pattern. In this case, no new dictionary
 * elements will be created - calls to get_itag_list that include wildcards and generate
 * no itag values to be returned to the caller will result in an ORTE_ERR_NOT_FOUND
 * response.
 * 
 */
int orte_gpr_replica_get_itag_list(orte_gpr_replica_itag_t **itaglist,
                    orte_gpr_replica_segment_t *seg, char **names,
                    int *num_names);



/*
 * SEGMENT OPERATIONS
 */
 
/** Find a requested registry segment.
 * The function finds the registry segment corresponding to
 * the specified name.
 *
 * @param *seg (OUT) Pointer to the segment
 * @param create (IN) A boolean that indicates whether or not to create the segment if it
 * doesn't already exist. TRUE => create it, FALSE => don't create it.
 * @param segment (IN) Pointer to a string containing the name of the segment to be found.
 *
 * @retval ORTE_SUCCESS Operation successful
 * @retval ORTE_ERROR(s) Appropriate error code returned
 */
int orte_gpr_replica_find_seg(orte_gpr_replica_segment_t **seg,
                              bool create, char *segment);


#endif
