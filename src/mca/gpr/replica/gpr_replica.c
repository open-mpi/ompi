/*
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>

#include "ompi_config.h"
#include "include/constants.h"
#include "mca/gpr/base/base.h"
#include "gpr_replica.h"
#include "gpr_replica_internals.h"

int gpr_replica_define_segment(char *segment)
{
    mca_gpr_registry_segment_t *seg;
    mca_gpr_replica_key_t key;

    key = gpr_replica_define_key(segment, NULL);
     if (MCA_GPR_REPLICA_KEY_MAX == key) {
 	return(OMPI_ERROR);
     }

     /* need to add the segment to the registry */
     seg = OBJ_NEW(mca_gpr_registry_segment_t);
     seg->segment = key;
     ompi_list_append(&mca_gpr_replica_head.registry, &seg->item);

     return(OMPI_SUCCESS);
}

int gpr_replica_delete_segment(char *segment)
{
    mca_gpr_registry_segment_t *seg;

    seg = gpr_replica_find_seg(segment);

    if (NULL == seg) {  /* couldn't locate segment */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != gpr_replica_empty_segment(seg)) {  /* couldn't empty the segment */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != gpr_replica_delete_key(segment, NULL)) { /* couldn't remove dictionary entry */
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int gpr_replica_put(ompi_registry_mode_t mode, char *segment,
		    char **tokens, ompi_registry_object_t *object,
		    ompi_registry_object_size_t size)
{
    ompi_list_t *keys;
    mca_gpr_keytable_t *keyptr;
    mca_gpr_registry_segment_t *seg;
    int return_code;

    /* protect ourselves against errors */
    if (NULL == segment || NULL == object || 0 == size || NULL == tokens) {
	return OMPI_ERROR;
    }

    /* convert tokens to list of keys */
    keys = gpr_replica_get_key_list(segment, tokens);
    if (NULL == keys) {
	return OMPI_ERROR;
    }
    /* traverse the list to find undefined tokens - get new keys for them */
    for (keyptr = (mca_gpr_keytable_t*)ompi_list_get_first(keys);
	 keyptr != (mca_gpr_keytable_t*)ompi_list_get_end(keys);
	 keyptr = (mca_gpr_keytable_t*)ompi_list_get_next(keyptr)) {
	if (MCA_GPR_REPLICA_KEY_MAX == keyptr->key) { /* need to get new key */
	    keyptr->key = gpr_replica_define_key(segment, keyptr->token);
	}
    }

    /* find the segment */
    seg = gpr_replica_find_seg(segment);
    if (NULL == seg) { /* couldn't find segment */
	return_code = OMPI_ERROR;
	goto CLEANUP;
    }

    /* see if specified entry already exists */

 CLEANUP:
    /* release list of keys */

    return return_code;
}

int gpr_replica_delete_object(ompi_registry_mode_t mode,
			      char *segment, char **tokens)
{
    return 0;
}

ompi_list_t* gpr_replica_index(char *segment)
{
    ompi_list_t *answer;
    mca_gpr_keytable_t *ptr;
    mca_gpr_registry_segment_t *seg;
    ompi_registry_index_value_t *ans;

    answer = OBJ_NEW(ompi_list_t);

    if (NULL == segment) { /* looking for index of global registry */
	for (ptr = (mca_gpr_keytable_t*)ompi_list_get_first(&mca_gpr_replica_head.segment_dict);
	     ptr != (mca_gpr_keytable_t*)ompi_list_get_end(&mca_gpr_replica_head.segment_dict);
	     ptr = (mca_gpr_keytable_t*)ompi_list_get_next(ptr)) {
	    ans = OBJ_NEW(ompi_registry_index_value_t);
	    ans->token = strdup(ptr->token);
	    ompi_list_append(answer, &ans->item);
	}
    } else {
    }

    return answer;
}

int gpr_replica_subscribe(ompi_process_name_t *caller, ompi_registry_mode_t mode,
			  ompi_registry_notify_action_t action,
			  char *segment, char **tokens)
{
    return 0;
}

int gpr_replica_unsubscribe(ompi_process_name_t *caller, ompi_registry_mode_t mode,
			    char *segment, char **tokens)
{
    return 0;
}

ompi_list_t* gpr_replica_get(ompi_registry_mode_t mode,
				       char *segment, char **tokens)
{
    mca_gpr_registry_segment_t *seg;
    ompi_list_t *answer;
    ompi_registry_value_t *ans;
    ompi_list_t *keys;
    mca_gpr_keytable_t *keyptr;
    mca_gpr_registry_core_t *reg;

    answer = OBJ_NEW(ompi_list_t);

    /* protect against errors */
    if (NULL == segment || NULL == tokens) {
	return answer;
    }

    /* find the specified segment */
    seg = gpr_replica_find_seg(segment);
    if (NULL == seg) {  /* segment not found */
	return answer;
    }

    /* convert tokens to list of keys */
    keys = gpr_replica_get_key_list(segment, tokens);
    if (0 == ompi_list_get_size(keys)) {
	return answer;
    }

    /* traverse the list to find undefined tokens - error if found */
    for (keyptr = (mca_gpr_keytable_t*)ompi_list_get_first(keys);
	 keyptr != (mca_gpr_keytable_t*)ompi_list_get_end(keys);
	 keyptr = (mca_gpr_keytable_t*)ompi_list_get_next(keyptr)) {
	if (MCA_GPR_REPLICA_KEY_MAX == keyptr->key) { /* unknown token */
	    goto CLEANUP;
	}
    }

    OBJ_CONSTRUCT(&answer, ompi_list_t);
    /* traverse the segment's registry, looking for matching tokens per the specified mode */
    for (reg = (mca_gpr_registry_core_t*)ompi_list_get_first(&seg->registry_entries);
	 reg != (mca_gpr_registry_core_t*)ompi_list_get_end(&seg->registry_entries);
	 reg = (mca_gpr_registry_core_t*)ompi_list_get_next(reg)) {

	/* for each registry entry, check the key list */
	for (keyptr = (mca_gpr_keytable_t*)ompi_list_get_first(&reg->keys);
	     keyptr != (mca_gpr_keytable_t*)ompi_list_get_end(&reg->keys);
	     keyptr = (mca_gpr_keytable_t*)ompi_list_get_next(keyptr)) {
	    if (gpr_replica_check_key_list(keys, keyptr->key)) { /* found the key on the list */
		if (OMPI_REGISTRY_OR == mode) {  /* just needed to find one - add entry to list */
		    ans = OBJ_NEW(ompi_registry_value_t);
		    ans->object_size = reg->object_size;
		    ans->object = (ompi_buffer_t*)malloc(ans->object_size);
		    memcpy(ans->object, reg->object, ans->object_size);
		    ompi_list_append(answer, &ans->item);
		}
	    } else if (OMPI_REGISTRY_XAND == mode || OMPI_REGISTRY_XOR == mode) { /* key not on the list */
		break; /* don't include entry if mode was exclusive */
	    }
	}
    }

 CLEANUP:
    /* empty the list of keys and release it */
    while (0 < ompi_list_get_size(keys)) {
	ompi_list_remove_last(keys);
    }
    OBJ_DESTRUCT(keys);

    return answer;
}

