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
ompi_registry_segment_t *ompi_registry_findseg(char *segment)
{
    ompi_keytable_t *ptr_seg;
    ompi_registry_segment_t *seg;

    /* search the registry segments to find which one is being referenced */
    for (ptr_seg = (ompi_keytable_t*)ompi_list_get_first(&ompi_registry.segment_dict);
	 ptr_seg != (ompi_keytable_t*)ompi_list_get_end(&ompi_registry.segment_dict);
	 ptr_seg = (ompi_keytable_t*)ompi_list_get_next(ptr_seg)) {
	if (0 == strcmp(segment, ptr_seg->token)) {
	    fprintf(stderr, "findseg: found segment token %s key %ld\n", ptr_seg->token, ptr_seg->key);
	    /* search ompi_registry to find segment */
	    for (seg=(ompi_registry_segment_t*)ompi_list_get_first(&ompi_registry.registry);
		 seg != (ompi_registry_segment_t*)ompi_list_get_end(&ompi_registry.registry);
		 seg = (ompi_registry_segment_t*)ompi_list_get_next(seg)) {
		fprintf(stderr, "findseg: checking seg\n");
		if(seg->segment == ptr_seg->key) {
		    fprintf(stderr, "findseg: found segment key %ld\n", seg->segment);
		    return(seg);
		}
	    }
	}
    }
    return(NULL); /* couldn't find the specified segment */
}

ompi_keytable_t *ompi_registry_finddictentry(char *segment, char *token)
{
    ompi_keytable_t *ptr_seg;
    ompi_keytable_t *ptr_key;
    ompi_registry_segment_t *seg;

    /* search the registry segments to find which one is being referenced */
    for (ptr_seg = (ompi_keytable_t*)ompi_list_get_first(&ompi_registry.segment_dict);
	 ptr_seg != (ompi_keytable_t*)ompi_list_get_end(&ompi_registry.segment_dict);
	 ptr_seg = (ompi_keytable_t*)ompi_list_get_next(ptr_seg)) {
	if (0 == strcmp(segment, ptr_seg->token)) {
	    if (NULL == token) { /* just want segment token-key pair */
		return(ptr_seg);
	    }
	    /* search ompi_registry to find segment */
	    for (seg=(ompi_registry_segment_t*)ompi_list_get_first(&ompi_registry.registry);
		 seg != (ompi_registry_segment_t*)ompi_list_get_end(&ompi_registry.registry);
		 seg = (ompi_registry_segment_t*)ompi_list_get_next(seg)) {
		if(seg->segment == ptr_seg->key) {
		    /* got segment - now find specified token-key pair in that dictionary */
		    for (ptr_key = (ompi_keytable_t*)ompi_list_get_first(&seg->keytable);
			 ptr_key != (ompi_keytable_t*)ompi_list_get_end(&seg->keytable);
			 ptr_key = (ompi_keytable_t*)ompi_list_get_next(ptr_key)) {
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


mca_gpr_replica_key_t ompi_registry_getkey(char *segment, char *token)
{
    ompi_keytable_t *ptr_key;

    /* find registry segment */
    ptr_key = ompi_registry_finddictentry(segment, NULL);
    if (NULL != ptr_key) {
	if (NULL == token) { /* only want segment key */
	    return(ptr_key->key);
	}
	/* if token specified, find the dictionary entry that matches token */
	ptr_key = ompi_registry_finddictentry(segment, token);
	if (NULL != ptr_key) {
	    return(ptr_key->key);
	}
	return(0); /* couldn't find dictionary entry */
    }
    return(0); /* couldn't find segment */
}


mca_gpr_relica_key_t gpr_replica_definekey(char *segment, char *token)
{
    ompi_registry_segment_t *seg;
    ompi_keytable_t *ptr_seg, *ptr_key, *new;

    /* protect against errors */
    if (NULL == segment) {
	return(0);
    }

    /* if token is NULL, then this is defining a segment name. Check dictionary to ensure uniqueness */
    if (NULL == token) {
	for (ptr_seg = (ompi_keytable_t*)ompi_list_get_first(&ompi_registry.segment_dict);
	     ptr_seg != (ompi_keytable_t*)ompi_list_get_end(&ompi_registry.segment_dict);
	     ptr_seg = (ompi_keytable_t*)ompi_list_get_next(ptr_seg)) {
	    if (0 == strcmp(segment, ptr_seg->token)) {
		return(0);
	    }
	}

	/* okay, name is not previously taken. Define a key value for it and return */
	new = OBJ_NEW(ompi_keytable_t);
	new->token = strdup(segment);
	if (0 == ompi_list_get_size(&ompi_registry.freekeys)) { /* no keys waiting for reuse */
	    ompi_registry.lastkey++;
	    new->key = ompi_registry.lastkey;
	} else {
	    ptr_key = (ompi_keytable_t*)ompi_list_remove_first(&ompi_registry.freekeys);
	    new->key = ptr_key->key;
	}
	ompi_list_append(&ompi_registry.segment_dict, &new->item);
	return(new->key);
    }

    /* okay, token is specified */
    /* search the registry segments to find which one is being referenced */
    seg = ompi_registry_findseg(segment);
    if (NULL != seg) {
	/* using that segment, check dictionary to ensure uniqueness */
	for (ptr_key = (ompi_keytable_t*)ompi_list_get_first(&seg->keytable);
	     ptr_key != (ompi_keytable_t*)ompi_list_get_end(&seg->keytable);
	     ptr_key = (ompi_keytable_t*)ompi_list_get_next(ptr_key)) {
	    if (0 == strcmp(token, ptr_key->token)) {
		return(0); /* already taken, report error */
	    }
	}
	/* okay, token is unique - create dictionary entry */
	new = OBJ_NEW(ompi_keytable_t);
	new->token = strdup(token);
	if (0 == ompi_list_get_size(&seg->freekeys)) { /* no keys waiting for reuse */
	    seg->lastkey++;
	    new->key = seg->lastkey;
	} else {
	    ptr_key = (ompi_keytable_t*)ompi_list_remove_first(&seg->freekeys);
	    new->key = ptr_key->key;
	}
	ompi_list_append(&seg->keytable, &new->item);
	return(new->key);
    }
    /* couldn't find segment */
    return(0);
}

int ompi_registry_deletekey(char *segment, char *token)
{
    ompi_registry_segment_t *seg;
    ompi_registry_core_t *reg, *prev;
    ompi_keytable_t *ptr_seg, *ptr_key, *new, *regkey;
    ompi_subscribe_list_t *subscriber;

    /* protect ourselves against errors */
    if (NULL == segment) {
	return(OMPI_ERROR);
    }

    /* find the segment */
    seg = ompi_registry_findseg(segment);
    if (NULL != seg) {
	/* if specified token is NULL, then this is deleting a segment name.*/
	if (NULL == token) {
	    /* empty the segment's registry */
	    while (0 < ompi_list_get_size(&seg->reg_list)) {
		ompi_list_remove_last(&seg->reg_list);
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
	    ompi_list_remove_item(&ompi_registry.registry, &seg->item);
	    /* add key to global registry's freekey list */
	    new = OBJ_NEW(ompi_keytable_t);
	    new->token = NULL;
	    new->key = ptr_seg->key;
	    ompi_list_append(&ompi_registry.freekeys, &new->item);
	    /* NEED TO RE-FIND PTR_SEG */
	    /* now remove the dictionary entry from the global registry dictionary*/
	    ompi_list_remove_item(&ompi_registry.segment_dict, &ptr_seg->item);
	    return(OMPI_SUCCESS);
	} else {  /* token not null, so need to find dictionary element to delete */
	    ptr_key = ompi_registry_finddictentry(segment, token);
	    if (NULL != ptr_key) {
		/* found key in dictionary */
		/* need to search this segment's registry to find all instances of key - then delete them */
		for (reg = (ompi_registry_core_t*)ompi_list_get_first(&seg->reg_list);
		     reg != (ompi_registry_core_t*)ompi_list_get_end(&seg->reg_list);
		     reg = (ompi_registry_core_t*)ompi_list_get_next(reg)) {
		    /* check the subscriber list */
		    for (subscriber = (ompi_subscribe_list_t*)ompi_list_get_first(&reg->subscriber);
			 (subscriber != (ompi_subscribe_list_t*)ompi_list_get_end(&reg->subscriber)
			  && (subscriber->id != ptr_key->key));
			 subscriber = (ompi_subscribe_list_t*)ompi_list_get_next(subscriber));
		    if (subscriber != (ompi_subscribe_list_t*)ompi_list_get_end(&reg->subscriber)) {
			ompi_list_remove_item(&reg->subscriber, &subscriber->item);
		    }
		    /* check the key list */
		    for (regkey = (ompi_keytable_t*)ompi_list_get_first(&reg->keys);
			 (regkey != (ompi_keytable_t*)ompi_list_get_end(&reg->keys))
			     && (regkey->key != ptr_key->key);
			 regkey = (ompi_keytable_t*)ompi_list_get_next(regkey));
		    if (regkey != (ompi_keytable_t*)ompi_list_get_end(&reg->keys)) {
			ompi_list_remove_item(&reg->keys, &regkey->item);
		    }
		    /* if this was the last key, then remove the registry entry itself */
		    if (0 == ompi_list_get_size(&reg->keys)) {
			while (0 < ompi_list_get_size(&reg->subscriber)) {
			    ompi_list_remove_last(&reg->subscriber);
			}
			prev = (ompi_registry_core_t*)ompi_list_get_prev(reg);
			ompi_list_remove_item(&seg->reg_list, &reg->item);
			reg = prev;
		    }
		}
		/* add key to this segment's freekey list */
		new = OBJ_NEW(ompi_keytable_t);
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
