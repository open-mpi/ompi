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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "util/output.h"
#include "util/proc_info.h"

#include "mca/errmgr/errmgr.h"

#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

#include "gpr_replica_fn.h"
 
/*
 * Local typedef for storing a list of itagvals
 * - used exclusively by "get" routines
 */
typedef struct {
    ompi_list_item_t item;              /* required for this to be on list */
    orte_gpr_replica_itag_t itag;       /* itag for this value's key */
    orte_data_type_t type;              /* the type of value stored */
    orte_gpr_value_union_t value;
} orte_gpr_replica_ival_list_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_ival_list_t);

/* constructor */
static void orte_gpr_replica_ival_list_constructor(orte_gpr_replica_ival_list_t* ptr)
{
    ptr->itag = 0;
    ptr->type = 0;
    (ptr->value).strptr = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_ival_list_destructor(orte_gpr_replica_ival_list_t* ptr)
{
    if (ORTE_BYTE_OBJECT == ptr->type) {
        free(((ptr->value).byteobject).bytes);
    }

}

/* define instance of ival_list_t */
OBJ_CLASS_INSTANCE(
          orte_gpr_replica_ival_list_t,  /* type name */
          ompi_list_item_t, /* parent "class" name */
          orte_gpr_replica_ival_list_constructor, /* constructor */
          orte_gpr_replica_ival_list_destructor); /* destructor */

/*
 * Local typedef for storing a list of containers
 * - used exclusively by "get" routines
 */
typedef struct {
    ompi_list_item_t item;              /* required for this to be on list */
    orte_gpr_replica_container_t *cptr;   /* pointer to the container */
    ompi_list_t *ival_list;             /* list of ival_list_t of values found by get */
} orte_gpr_replica_get_list_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_get_list_t);

/* constructor */
static void orte_gpr_replica_get_list_constructor(orte_gpr_replica_get_list_t* ptr)
{
    ptr->cptr = NULL;
    ptr->ival_list = OBJ_NEW(ompi_list_t);
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_get_list_destructor(orte_gpr_replica_get_list_t* ptr)
{
    orte_gpr_replica_ival_list_t *iptr;
    
    while (NULL != (iptr = (orte_gpr_replica_ival_list_t*)ompi_list_remove_first(ptr->ival_list))) {
        OBJ_RELEASE(iptr);
    }

}

/* define instance of get_list_t */
OBJ_CLASS_INSTANCE(
          orte_gpr_replica_get_list_t,  /* type name */
          ompi_list_item_t, /* parent "class" name */
          orte_gpr_replica_get_list_constructor, /* constructor */
          orte_gpr_replica_get_list_destructor); /* destructor */



/*
 * FUNCTIONS
 */
 
int orte_gpr_replica_put_fn(orte_gpr_addr_mode_t addr_mode,
                            orte_gpr_replica_segment_t *seg,
                            orte_gpr_replica_itag_t *token_itags, int num_tokens,
                            int cnt, orte_gpr_keyval_t **keyvals,
                            int8_t *action_taken)
{
    orte_gpr_replica_container_t **cptr, *cptr2;
    orte_gpr_replica_itag_t itag;
    orte_gpr_replica_addr_mode_t tok_mode;
    orte_gpr_keyval_t **kptr;
    orte_gpr_replica_itagval_t *iptr;
    bool overwrite;
    char **tmp;
    int rc, i, j, num_found;

    if (orte_gpr_replica_globals.debug) {
	    ompi_output(0, "[%d,%d,%d] gpr replica: put entered on segment %s\nValues:",
		    ORTE_NAME_ARGS(orte_process_info.my_name), seg->name);
        for (i=0; i < cnt; i++) {
            ompi_output(0, "\tKey: %s", keyvals[i]->key);
        }
        ompi_output(0, "Tokens:");
        for (i=0; i < num_tokens; i++) {
            orte_gpr_replica_dict_reverse_lookup(tmp, seg, token_itags[i]);
            ompi_output(0, "\t%s", *tmp);
        }
    }

    /* initialize action */
    *action_taken = 0;
    
    /* extract the token address mode and overwrite permissions */
    overwrite = false;
    if (addr_mode & ORTE_GPR_OVERWRITE) {
        overwrite = true;
    }
    tok_mode = 0x004f & addr_mode;
    if (0x00 == tok_mode) {  /* default tokens addressing mode to AND */
        tok_mode = ORTE_GPR_REPLICA_AND;
    }

    /* find the specified container(s) */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_containers(&num_found, seg, tok_mode,
                                    token_itags, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (NULL == token_itags && 0 == num_found) { /* wildcard tokens but nothing found */
        /* no ERROR_LOG entry created as this is not a system failure */
        return ORTE_ERR_NOT_FOUND;
    }
    
    if (0 == num_found) {  /* existing container not found - create one */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_container(&cptr2, seg,
                                            num_tokens, token_itags))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
 
        /* ok, store all the keyvals in the container */
        kptr = keyvals;
        for (i=0; i < cnt; i++) {
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_add_keyval(&iptr, seg, cptr2, keyvals[i]))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        *action_taken = ORTE_GPR_REPLICA_ENTRY_ADDED;
    } else {  /* otherwise, go through list of containers. For each one,
                 see if entry already exists in container - overwrite if allowed */
        cptr = (orte_gpr_replica_container_t**)(orte_gpr_replica_globals.srch_cptr)->addr;
        for (j=0; j < (orte_gpr_replica_globals.srch_cptr)->size; j++) {
            if (NULL != cptr[j]) {
                for (i=0; i < cnt; i++) {
                    if (ORTE_SUCCESS == orte_gpr_replica_create_itag(&itag, seg, keyvals[i]->key) &&
                        ORTE_SUCCESS == orte_gpr_replica_search_container(&num_found,
                                                ORTE_GPR_REPLICA_OR,
                                                &itag, 1, cptr[j])) {
                        if (0 < num_found) {
                            /* this key already exists - overwrite, if permission given
                             * else add this keyval to the container as a new entry
                             */
                             if (overwrite) {
                                if (ORTE_SUCCESS != (rc = orte_gpr_replica_update_keyval(seg, cptr[j], keyvals[i]))) {
                                    return rc;
                                }
                                overwrite = false;  /* only do it for the first one - rest get added */
                                *action_taken = *action_taken | ORTE_GPR_REPLICA_ENTRY_CHANGED;
                             } else {
                                if (ORTE_SUCCESS != (rc = orte_gpr_replica_add_keyval(&iptr, seg, cptr[j], keyvals[i]))) {
                                    return rc;
                                }
                                *action_taken = *action_taken | ORTE_GPR_REPLICA_ENTRY_ADDED;
                             }
                        } else { /* new key - add to container */
                            if (ORTE_SUCCESS != (rc = orte_gpr_replica_add_keyval(&iptr, seg, cptr[j], keyvals[i]))) {
                                return rc;
                            }
                            *action_taken = *action_taken | ORTE_GPR_REPLICA_ENTRY_ADDED;
                        }
                    }
                }
            }
        }
    }

    if (orte_gpr_replica_globals.debug) {
	    ompi_output(0, "[%d,%d,%d] gpr replica-put: complete", ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    return ORTE_SUCCESS;
}


int orte_gpr_replica_put_nb_fn(orte_gpr_addr_mode_t addr_mode,
                orte_gpr_replica_segment_t *seg,
                orte_gpr_replica_itag_t *token_itags, int num_tokens,
                int cnt, orte_gpr_keyval_t **keyvals,
                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}
                      

int orte_gpr_replica_get_fn(orte_gpr_addr_mode_t addr_mode,
                            orte_gpr_replica_segment_t *seg,
                            orte_gpr_replica_itag_t *tokentags, int num_tokens,
                            orte_gpr_replica_itag_t *keytags, int num_keys,
                            int *cnt, orte_gpr_value_t ***values)
{
    ompi_list_t *get_list;
    orte_gpr_replica_get_list_t *gptr;
    orte_gpr_replica_ival_list_t *ival_list;
    orte_gpr_replica_container_t **cptr, *cptr2;
    orte_gpr_replica_itagval_t **iptr;
    orte_gpr_keyval_t **kptr;
    orte_gpr_replica_addr_mode_t tokmode, keymode;
    int rc, i, j, num_found;
    
    if (orte_gpr_replica_globals.debug) {
        	ompi_output(0, "[%d,%d,%d] gpr replica: get entered", ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    /* initialize the list of findings */
    get_list = OBJ_NEW(ompi_list_t);
    if (NULL == get_list) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    *cnt = 0;
    *values = NULL;
    tokmode = 0x004f & addr_mode;
    if (0x00 == tokmode) {  /* default token addressing mode to AND */
        tokmode = ORTE_GPR_REPLICA_AND;
    }
    keymode = ((0x4f00 & addr_mode) >> 8) & 0x004f;
    if (0x00 == keymode) {  /* default key addressing mode to OR */
        keymode = ORTE_GPR_REPLICA_OR;
    }
    
    /* find all containers that meet search criteria for tokens */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_containers(&num_found, seg, tokmode,
                                    tokentags, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* if nothing found, then can return */
    if (0 == num_found) {
        OBJ_RELEASE(get_list);
        return ORTE_SUCCESS;
    }
    
    /* for each container that was found, search it to find all matching keytags
     * subject to specified mode. Collect the results on get_list
     */
    cptr = (orte_gpr_replica_container_t**)((orte_gpr_replica_globals.srch_cptr)->addr);
    for (i=0; i < (orte_gpr_replica_globals.srch_cptr)->size; i++) {
        if ((NULL != cptr[i]) &&
            (ORTE_SUCCESS == orte_gpr_replica_search_container(&num_found, keymode,
                                    keytags, num_keys, cptr[i])) && 0 < num_found) {
            gptr = OBJ_NEW(orte_gpr_replica_get_list_t);
            gptr->cptr = cptr[i];
            iptr = (orte_gpr_replica_itagval_t**)((orte_gpr_replica_globals.srch_ival)->addr);
            for (j=0; j < (orte_gpr_replica_globals.srch_ival)->size; j++) {
                if (NULL != iptr[j]) {
                    ival_list = OBJ_NEW(orte_gpr_replica_ival_list_t);
                    ival_list->itag = iptr[j]->itag;
                    ival_list->type = iptr[j]->type;
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_xfer_payload(
                                &(ival_list->value), &(iptr[j]->value), iptr[j]->type))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(ival_list);
                        return rc;
                    }
                    ompi_list_append(gptr->ival_list, &ival_list->item);
                }
            }
            ompi_list_append(get_list, &gptr->item);
            (*cnt)++; /* update number of containers that had something found */
        }
    }
    
    if (0 == *cnt) {  /* nothing found - report that */
        rc = ORTE_SUCCESS;
        goto CLEANUP;
    }
    
    /* if something found, convert it to array of values */
    *values = (orte_gpr_value_t**)malloc((*cnt) * sizeof(orte_gpr_value_t*));
    if (NULL == *values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    for (i=0; i < *cnt; i++) {
        gptr = (orte_gpr_replica_get_list_t*)ompi_list_remove_first(get_list);
        if (NULL == gptr) {
            rc = ORTE_ERROR;
            goto CLEANUP;
        }
        (*values)[i] = OBJ_NEW(orte_gpr_value_t);
        if (NULL == (*values)[i]) {
            *cnt = 0;
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
        (*values)[i]->segment = strdup(seg->name);
        (*values)[i]->cnt = ompi_list_get_size(gptr->ival_list);
        cptr2 = gptr->cptr;
        (*values)[i]->num_tokens = cptr2->num_itags;
        (*values)[i]->tokens = (char **)malloc(cptr2->num_itags * sizeof(char*));
        if (NULL == (*values)[i]->tokens) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
        for (j=0; j < cptr2->num_itags; j++) {
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_dict_reverse_lookup(
                                        &((*values)[i]->tokens[j]), seg, cptr2->itags[j]))) {
                goto CLEANUP;
            }
        }
        (*values)[i]->keyvals = (orte_gpr_keyval_t**)malloc((*values)[i]->cnt * sizeof(orte_gpr_keyval_t*));
        if (NULL == (*values)[i]->keyvals) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
        
        kptr = (*values)[i]->keyvals;
        for (j=0; j < (*values)[i]->cnt; j++) {
            ival_list = (orte_gpr_replica_ival_list_t*)ompi_list_remove_first(gptr->ival_list);
            if (NULL == ival_list) {
                rc = ORTE_ERROR;
                goto CLEANUP;
            }
            kptr[j] = OBJ_NEW(orte_gpr_keyval_t);
            if (NULL == kptr[j]) {
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_dict_reverse_lookup(
                                        &(kptr[j]->key), seg, ival_list->itag))) {
                goto CLEANUP;
            }
            kptr[j]->type = ival_list->type;
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_xfer_payload(
                        &(kptr[j]->value), &(ival_list->value), ival_list->type))) {
                goto CLEANUP;
            }
            OBJ_RELEASE(ival_list);
        }
        OBJ_RELEASE(gptr);
    }

CLEANUP:
    
    while (NULL != (gptr = (orte_gpr_replica_get_list_t*)ompi_list_remove_first(get_list))) {
        OBJ_RELEASE(gptr);
    }
    OBJ_RELEASE(get_list);
    
    if (orte_gpr_replica_globals.debug) {
        	ompi_output(0, "[%d,%d,%d] gpr replica-get: finished search", ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    return rc;
}

int orte_gpr_replica_get_nb_fn(orte_gpr_addr_mode_t addr_mode,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t *tokentags, int num_tokens,
                                orte_gpr_replica_itag_t *keytags, int num_keys,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

