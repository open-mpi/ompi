/*
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - support functions.
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
#include "util/output.h"
#include "mca/mca.h"
#include "mca/oob/base/base.h"
#include "mca/gpr/base/base.h"
#include "gpr_replica.h"
#include "gpr_replica_internals.h"

/*
 *
 */
mca_gpr_registry_segment_t *gpr_replica_find_seg(char *segment)
{
    mca_gpr_keytable_t *ptr_seg;
    mca_gpr_registry_segment_t *seg;

    /* search the registry segments to find which one is being referenced */
    for (ptr_seg = (mca_gpr_keytable_t*)ompi_list_get_first(&mca_gpr_replica_head.segment_dict);
	 ptr_seg != (mca_gpr_keytable_t*)ompi_list_get_end(&mca_gpr_replica_head.segment_dict);
	 ptr_seg = (mca_gpr_keytable_t*)ompi_list_get_next(ptr_seg)) {
	if (0 == strcmp(segment, ptr_seg->token)) {
	    fprintf(stderr, "findseg: found segment token %s key %d\n", ptr_seg->token, (int)ptr_seg->key);
	    /* search mca_gpr_replica_head to find segment */
	    for (seg=(mca_gpr_registry_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
		 seg != (mca_gpr_registry_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);
		 seg = (mca_gpr_registry_segment_t*)ompi_list_get_next(seg)) {
		fprintf(stderr, "findseg: checking seg\n");
		if(seg->segment == ptr_seg->key) {
		    fprintf(stderr, "findseg: found segment key %d\n", (int)seg->segment);
		    return(seg);
		}
	    }
	}
    }
    return(NULL); /* couldn't find the specified segment */
}

mca_gpr_keytable_t *gpr_replica_find_dict_entry(char *segment, char *token)
{
    mca_gpr_keytable_t *ptr_seg;
    mca_gpr_keytable_t *ptr_key;
    mca_gpr_registry_segment_t *seg;

    /* search the registry segments to find which one is being referenced */
    for (ptr_seg = (mca_gpr_keytable_t*)ompi_list_get_first(&mca_gpr_replica_head.segment_dict);
	 ptr_seg != (mca_gpr_keytable_t*)ompi_list_get_end(&mca_gpr_replica_head.segment_dict);
	 ptr_seg = (mca_gpr_keytable_t*)ompi_list_get_next(ptr_seg)) {
	if (0 == strcmp(segment, ptr_seg->token)) {
	    if (NULL == token) { /* just want segment token-key pair */
		return(ptr_seg);
	    }
	    /* search registry to find segment */
	    for (seg=(mca_gpr_registry_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
		 seg != (mca_gpr_registry_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);
		 seg = (mca_gpr_registry_segment_t*)ompi_list_get_next(seg)) {
		if(seg->segment == ptr_seg->key) {
		    /* got segment - now find specified token-key pair in that dictionary */
		    for (ptr_key = (mca_gpr_keytable_t*)ompi_list_get_first(&seg->keytable);
			 ptr_key != (mca_gpr_keytable_t*)ompi_list_get_end(&seg->keytable);
			 ptr_key = (mca_gpr_keytable_t*)ompi_list_get_next(ptr_key)) {
			if (0 == strcmp(token, ptr_key->token)) {
			    return(ptr_key);
			}
		    }
		    return(NULL); /* couldn't find the specified entry */
		}
	    }
	    return(NULL); /* couldn't find segment, even though we found entry in registry dict */
	}
    }
    return(NULL); /* couldn't find segment token-key pair */
}


mca_gpr_replica_key_t gpr_replica_get_key(char *segment, char *token)
{
    mca_gpr_keytable_t *ptr_key;

    /* find registry segment */
    ptr_key = gpr_replica_find_dict_entry(segment, NULL);
    if (NULL != ptr_key) {
	if (NULL == token) { /* only want segment key */
	    return(ptr_key->key);
	}
	/* if token specified, find the dictionary entry that matches token */
	ptr_key = gpr_replica_find_dict_entry(segment, token);
	if (NULL != ptr_key) {
	    return(ptr_key->key);
	}
	return MCA_GPR_REPLICA_KEY_MAX; /* couldn't find dictionary entry */
    }
    return MCA_GPR_REPLICA_KEY_MAX; /* couldn't find segment */
}


ompi_list_t *gpr_replica_get_key_list(char *segment, char **tokens)
{
    ompi_list_t *keys;
    char **token;
    mca_gpr_keytable_t *keyptr, *dict_entry;

    /* protect against errors */
    if (NULL == segment || NULL == tokens) {
	return NULL;
    }

    token = tokens;
    keys = OBJ_NEW(ompi_list_t);
    while (NULL != *token) {  /* traverse array of tokens until NULL */
	keyptr = OBJ_NEW(mca_gpr_keytable_t);
	if (NULL == (dict_entry = gpr_replica_find_dict_entry(segment, *token))) {
	    keyptr->key = MCA_GPR_REPLICA_KEY_MAX;  /* indicate unknown token */
	} else { /* found existing dictionary entry */
	    keyptr->key = dict_entry->key;
	    keyptr->token = strdup(dict_entry->token);
	}
	ompi_list_append(keys, &keyptr->item);
	token++;
    }
    return keys;
}

mca_gpr_replica_key_t gpr_replica_define_key(char *segment, char *token)
{
    mca_gpr_registry_segment_t *seg;
    mca_gpr_keytable_t *ptr_seg, *ptr_key, *new;

    /* protect against errors */
    if (NULL == segment) {
	return MCA_GPR_REPLICA_KEY_MAX;
    }

    /* if token is NULL, then this is defining a segment name. Check dictionary to ensure uniqueness */
    if (NULL == token) {
	for (ptr_seg = (mca_gpr_keytable_t*)ompi_list_get_first(&mca_gpr_replica_head.segment_dict);
	     ptr_seg != (mca_gpr_keytable_t*)ompi_list_get_end(&mca_gpr_replica_head.segment_dict);
	     ptr_seg = (mca_gpr_keytable_t*)ompi_list_get_next(ptr_seg)) {
	    if (0 == strcmp(segment, ptr_seg->token)) {
		return MCA_GPR_REPLICA_KEY_MAX;
	    }
	}

	/* okay, name is not previously taken. Define a key value for it and return */
	new = OBJ_NEW(mca_gpr_keytable_t);
	new->token = strdup(segment);
	if (0 == ompi_list_get_size(&mca_gpr_replica_head.freekeys)) { /* no keys waiting for reuse */
	    if (MCA_GPR_REPLICA_KEY_MAX-2 > mca_gpr_replica_head.lastkey) {  /* have a key left */
	    mca_gpr_replica_head.lastkey++;
	    new->key = mca_gpr_replica_head.lastkey;
	    } else {  /* out of keys */
		return MCA_GPR_REPLICA_KEY_MAX;
	    }
	} else {
	    ptr_key = (mca_gpr_keytable_t*)ompi_list_remove_first(&mca_gpr_replica_head.freekeys);
	    new->key = ptr_key->key;
	}
	ompi_list_append(&mca_gpr_replica_head.segment_dict, &new->item);
	return(new->key);
    }

    /* okay, token is specified */
    /* search the registry segments to find which one is being referenced */
    seg = gpr_replica_find_seg(segment);
    if (NULL != seg) {
	/* using that segment, check dictionary to ensure uniqueness */
	for (ptr_key = (mca_gpr_keytable_t*)ompi_list_get_first(&seg->keytable);
	     ptr_key != (mca_gpr_keytable_t*)ompi_list_get_end(&seg->keytable);
	     ptr_key = (mca_gpr_keytable_t*)ompi_list_get_next(ptr_key)) {
	    if (0 == strcmp(token, ptr_key->token)) {
		return MCA_GPR_REPLICA_KEY_MAX; /* already taken, report error */
	    }
	}
	/* okay, token is unique - create dictionary entry */
	new = OBJ_NEW(mca_gpr_keytable_t);
	new->token = strdup(token);
	if (0 == ompi_list_get_size(&seg->freekeys)) { /* no keys waiting for reuse */
	    seg->lastkey++;
	    new->key = seg->lastkey;
	} else {
	    ptr_key = (mca_gpr_keytable_t*)ompi_list_remove_first(&seg->freekeys);
	    new->key = ptr_key->key;
	}
	ompi_list_append(&seg->keytable, &new->item);
	return(new->key);
    }
    /* couldn't find segment */
    return MCA_GPR_REPLICA_KEY_MAX;
}

int gpr_replica_delete_key(char *segment, char *token)
{
    mca_gpr_registry_segment_t *seg;
    mca_gpr_registry_core_t *reg, *prev;
    mca_gpr_keytable_t *ptr_seg, *ptr_key, *new, *regkey;

    /* protect ourselves against errors */
    if (NULL == segment) {
	return(OMPI_ERROR);
    }

    /* find the segment */
    seg = gpr_replica_find_seg(segment);
    if (NULL != seg) {

	/* if specified token is NULL, then this is deleting a segment name.*/
	if (NULL == token) {
	    if (OMPI_SUCCESS != gpr_replica_empty_segment(seg)) { /* couldn't empty segment */
		return OMPI_ERROR;
	    }
	    /* now remove the dictionary entry from the global registry dictionary*/
	    ptr_seg = gpr_replica_find_dict_entry(segment, NULL);
	    if (NULL == ptr_seg) { /* failed to find dictionary entry */
		return OMPI_ERROR;
	    }
	    /* add key to global registry's freekey list */
	    new = OBJ_NEW(mca_gpr_keytable_t);
	    new->token = NULL;
	    new->key = ptr_seg->key;
	    ompi_list_append(&mca_gpr_replica_head.freekeys, &new->item);

	    /* remove the dictionary entry */
	    ompi_list_remove_item(&mca_gpr_replica_head.segment_dict, &ptr_seg->item);
	    return(OMPI_SUCCESS);

	} else {  /* token not null, so need to find dictionary element to delete */
	    ptr_key = gpr_replica_find_dict_entry(segment, token);
	    if (NULL != ptr_key) {
		/* found key in dictionary */
		/* need to search this segment's registry to find all instances of key - then delete them */
		for (reg = (mca_gpr_registry_core_t*)ompi_list_get_first(&seg->registry_entries);
		     reg != (mca_gpr_registry_core_t*)ompi_list_get_end(&seg->registry_entries);
		     reg = (mca_gpr_registry_core_t*)ompi_list_get_next(reg)) {

		    /* check the key list */
		    for (regkey = (mca_gpr_keytable_t*)ompi_list_get_first(&reg->keys);
			 (regkey != (mca_gpr_keytable_t*)ompi_list_get_end(&reg->keys))
			     && (regkey->key != ptr_key->key);
			 regkey = (mca_gpr_keytable_t*)ompi_list_get_next(regkey));
		    if (regkey != (mca_gpr_keytable_t*)ompi_list_get_end(&reg->keys)) {
			ompi_list_remove_item(&reg->keys, &regkey->item);
		    }
		    /* if this was the last key, then remove the registry entry itself */
		    if (0 == ompi_list_get_size(&reg->keys)) {
			while (0 < ompi_list_get_size(&reg->subscriber)) {
			    ompi_list_remove_last(&reg->subscriber);
			}
			prev = (mca_gpr_registry_core_t*)ompi_list_get_prev(reg);
			ompi_list_remove_item(&seg->registry_entries, &reg->item);
			reg = prev;
		    }
		}

		/* add key to this segment's freekey list */
		new = OBJ_NEW(mca_gpr_keytable_t);
		new->token = NULL;
		new->key = ptr_key->key;
		ompi_list_append(&seg->freekeys, &new->item);

		/* now remove the dictionary entry from the segment's dictionary */
		ompi_list_remove_item(&seg->keytable, &ptr_key->item);
		return(OMPI_SUCCESS);
	    }
	    return(OMPI_ERROR); /* if we get here, then we couldn't find token in dictionary */
	}
    }
    return(OMPI_ERROR); /* if we get here, then we couldn't find segment */
}

int gpr_replica_empty_segment(mca_gpr_registry_segment_t *seg)
{

    /* empty the segment's registry */
    while (0 < ompi_list_get_size(&seg->registry_entries)) {
	ompi_list_remove_last(&seg->registry_entries);
    }

    /* empty the segment's dictionary */
    while (0 < ompi_list_get_size(&seg->keytable)) {
	ompi_list_remove_last(&seg->keytable);
    }
    /* empty the list of free keys */
    while (0 < ompi_list_get_size(&seg->freekeys)) {
	ompi_list_remove_last(&seg->freekeys);
    }
    /* now remove segment from global registry */
    ompi_list_remove_item(&mca_gpr_replica_head.registry, &seg->item);

    return OMPI_SUCCESS;
}

bool gpr_replica_check_key_list(ompi_list_t *key_list, mca_gpr_replica_key_t key)
{
    return true;
}
