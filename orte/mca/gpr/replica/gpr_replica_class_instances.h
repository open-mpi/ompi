/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
/** @file
 */

#ifndef ORTE_GPR_REPLICA_CLASS_INSTANCES_H_
#define ORTE_GPR_REPLICA_CLASS_INSTANCES_H_

#include "orte_config.h"

#include "orte/class/orte_bitmap.h"
#include "opal/class/opal_object.h"

#include "gpr_replica.h"

/*
 * CONSTRUCTORS, DESTRUCTORS, AND CLASS INSTANTIATIONS
 * FOR GPR REPLICA CLASSES
 */

/*  LOCAL_SUBSCRIBER */
static void orte_gpr_replica_local_subscriber_constructor(orte_gpr_replica_local_subscriber_t *ptr)
{
    ptr->name = NULL;
    ptr->callback = NULL;
    ptr->user_tag = NULL;
}

static void orte_gpr_replica_local_subscriber_destructor(orte_gpr_replica_local_subscriber_t *ptr)
{
    if (NULL != ptr->name) free(ptr->name);
}

OBJ_CLASS_INSTANCE(
          orte_gpr_replica_local_subscriber_t,  /* type name */
          opal_object_t, /* parent "class" name */
          orte_gpr_replica_local_subscriber_constructor, /* constructor */
          orte_gpr_replica_local_subscriber_destructor); /* destructor */


/*  LOCAL_TRIGGER */
static void orte_gpr_replica_local_trigger_constructor(orte_gpr_replica_local_trigger_t *ptr)
{
    ptr->name = NULL;
    ptr->callback = NULL;
    ptr->user_tag = NULL;
}

static void orte_gpr_replica_local_trigger_destructor(orte_gpr_replica_local_trigger_t *ptr)
{
    if (NULL != ptr->name) free(ptr->name);
}

/* define instance */
OBJ_CLASS_INSTANCE(
          orte_gpr_replica_local_trigger_t,  /* type name */
          opal_object_t, /* parent "class" name */
          orte_gpr_replica_local_trigger_constructor, /* constructor */
          orte_gpr_replica_local_trigger_destructor); /* destructor */


/*  SEGMENT */
/* constructor - used to initialize state of segment instance */
static void orte_gpr_replica_segment_construct(orte_gpr_replica_segment_t* seg)
{
    seg->name = NULL;
    seg->itag = ORTE_GPR_REPLICA_ITAG_MAX;

    seg->num_dict_entries = 0;
    orte_pointer_array_init(&(seg->dict), (orte_std_cntr_t)orte_gpr_array_block_size,
                            (orte_std_cntr_t)orte_gpr_array_max_size,
                            (orte_std_cntr_t)orte_gpr_array_block_size);

    seg->num_containers = 0;
    orte_pointer_array_init(&(seg->containers), (orte_std_cntr_t)orte_gpr_array_block_size,
                            (orte_std_cntr_t)orte_gpr_array_max_size,
                            (orte_std_cntr_t)orte_gpr_array_block_size);

}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_segment_destructor(orte_gpr_replica_segment_t* seg)
{
    orte_std_cntr_t i, k;
    char **dptr;
    orte_gpr_replica_itag_t j;
    orte_gpr_replica_container_t **cptr;

    if (NULL != seg->name) {
        free(seg->name);
    }

    if (NULL != seg->dict) {
        dptr = (char**)(seg->dict)->addr;
        for (i=0, j=0; j < seg->num_dict_entries &&
                       i < (seg->dict)->size; i++) {
            if (NULL != dptr[i]) {
                j++;
                free(dptr[i]);
            }
        }
        OBJ_RELEASE(seg->dict);
    }

    if (NULL != seg->containers) {
        cptr = (orte_gpr_replica_container_t**)((seg->containers)->addr);
        for (i=0, k=0; k < seg->num_containers &&
                       i < (seg->containers)->size; i++) {
            if (NULL != cptr[i]) {
                k++;
                OBJ_RELEASE(cptr[i]);
            }
        }
        OBJ_RELEASE(seg->containers);
    }
}

/* define instance of orte_gpr_replica_segment_t */
OBJ_CLASS_INSTANCE(
          orte_gpr_replica_segment_t,  /* type name */
          opal_object_t, /* parent "class" name */
          orte_gpr_replica_segment_construct, /* constructor */
          orte_gpr_replica_segment_destructor); /* destructor */


/* CONTAINER */
/* constructor - used to initialize state of registry container instance */
static void orte_gpr_replica_container_construct(orte_gpr_replica_container_t* reg)
{
    reg->index = 0;
    reg->itags = NULL;
    reg->num_itags = 0;

    orte_pointer_array_init(&(reg->itagvals), (orte_std_cntr_t)orte_gpr_array_block_size,
                            (orte_std_cntr_t)orte_gpr_array_max_size,
                            (orte_std_cntr_t)orte_gpr_array_block_size);
    reg->num_itagvals = 0;

    OBJ_CONSTRUCT(&(reg->itaglist), orte_value_array_t);
    orte_value_array_init(&(reg->itaglist), sizeof(orte_gpr_replica_itag_t));

}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_container_destructor(orte_gpr_replica_container_t* reg)
{
    orte_gpr_replica_itagval_t **ptr;
    orte_std_cntr_t i, k;

    if (NULL != reg->itags) {
         free(reg->itags);
    }

    if (NULL != reg->itagvals) {
        ptr = (orte_gpr_replica_itagval_t**)((reg->itagvals)->addr);
        for (i=0, k=0; k < reg->num_itagvals &&
                       i < (reg->itagvals)->size; i++) {
            if (NULL != ptr[i]) {
                k++;
                OBJ_RELEASE(ptr[i]);
            }
        }
        OBJ_RELEASE(reg->itagvals);
    }

    OBJ_DESTRUCT(&(reg->itaglist));

}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_container_t,  /* type name */
         opal_object_t, /* parent "class" name */
         orte_gpr_replica_container_construct, /* constructor */
         orte_gpr_replica_container_destructor); /* destructor */


/* ITAG-VALUE PAIR */
/* constructor - used to initialize state of itagval instance */
static void orte_gpr_replica_itagval_construct(orte_gpr_replica_itagval_t* ptr)
{
    ptr->index = 0;
    ptr->itag = ORTE_GPR_REPLICA_ITAG_MAX;
    ptr->value = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_itagval_destructor(orte_gpr_replica_itagval_t* ptr)
{
    if (NULL != ptr->value) OBJ_RELEASE(ptr->value);
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_itagval_t,  /* type name */
         opal_object_t, /* parent "class" name */
         orte_gpr_replica_itagval_construct, /* constructor */
         orte_gpr_replica_itagval_destructor); /* destructor */


/* IVALUE */
/* constructor - used to initialize state of ivalue instance */
static void orte_gpr_replica_ivalue_construct(orte_gpr_replica_ivalue_t* ptr)
{
    ptr->index = 0;
    ptr->seg = NULL;
    ptr->addr_mode = 0;

    OBJ_CONSTRUCT(&(ptr->tokentags), orte_value_array_t);
    orte_value_array_init(&(ptr->tokentags), sizeof(orte_gpr_replica_itag_t));

    OBJ_CONSTRUCT(&(ptr->keytags), orte_value_array_t);
    orte_value_array_init(&(ptr->keytags), sizeof(orte_gpr_replica_itag_t));

}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_ivalue_destructor(orte_gpr_replica_ivalue_t* ptr)
{
    OBJ_DESTRUCT(&(ptr->tokentags));
    OBJ_DESTRUCT(&(ptr->keytags));
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_ivalue_t,  /* type name */
         opal_object_t, /* parent "class" name */
         orte_gpr_replica_ivalue_construct, /* constructor */
         orte_gpr_replica_ivalue_destructor); /* destructor */


/* COUNTERS */
/* constructor - used to initialize state of counter instance */
static void orte_gpr_replica_counter_construct(orte_gpr_replica_counter_t* cntr)
{
    cntr->seg = NULL;
    cntr->cptr = NULL;
    cntr->iptr = NULL;
    OBJ_CONSTRUCT(&(cntr->trigger_level), orte_gpr_replica_itagval_t);
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_counter_destructor(orte_gpr_replica_counter_t* cntr)
{
    OBJ_DESTRUCT(&(cntr->trigger_level));
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_counter_t,           /* type name */
         opal_object_t,                 /* parent "class" name */
         orte_gpr_replica_counter_construct,   /* constructor */
         orte_gpr_replica_counter_destructor); /* destructor */


/* REQUESTOR */
/* constructor - used to initialize state of requestor instance */
static void orte_gpr_replica_requestor_construct(orte_gpr_replica_requestor_t* ptr)
{
    ptr->index = 0;
    ptr->requestor = NULL;
    ptr->idtag = 0;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_requestor_destructor(orte_gpr_replica_requestor_t* ptr)
{
    if (NULL != ptr->requestor) free(ptr->requestor);
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_requestor_t,           /* type name */
         opal_object_t,                 /* parent "class" name */
         orte_gpr_replica_requestor_construct,   /* constructor */
         orte_gpr_replica_requestor_destructor); /* destructor */


/* SUBSCRIPTION */
/* constructor - used to initialize state of subscription instance */
static void orte_gpr_replica_subscription_construct(orte_gpr_replica_subscription_t* sub)
{
    sub->index = 0;
    sub->idtag = ORTE_GPR_SUBSCRIPTION_ID_MAX;
    sub->name = NULL;
    sub->active = false;
    sub->processing = false;
    sub->cleanup = false;
    sub->action = ORTE_GPR_REPLICA_NO_ACTION;

    sub->num_values = 0;
    orte_pointer_array_init(&(sub->values), (orte_std_cntr_t)orte_gpr_array_block_size,
                            (orte_std_cntr_t)orte_gpr_array_max_size,
                            (orte_std_cntr_t)orte_gpr_array_block_size);

    sub->num_requestors = 0;
    orte_pointer_array_init(&(sub->requestors), (orte_std_cntr_t)orte_gpr_array_block_size,
                            (orte_std_cntr_t)orte_gpr_array_max_size,
                            (orte_std_cntr_t)orte_gpr_array_block_size);
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_subscription_destructor(orte_gpr_replica_subscription_t* sub)
{
    orte_gpr_replica_requestor_t **ptr;
    orte_gpr_replica_ivalue_t **ivals;
    orte_std_cntr_t i, k;

    if (NULL != sub->name) free(sub->name);

    if (NULL != sub->requestors) {
        ptr = (orte_gpr_replica_requestor_t**)((sub->requestors)->addr);
        for (i=0, k=0; k < sub->num_requestors &&
                       i < (sub->requestors)->size; i++) {
            if (NULL != ptr[i]) {
                k++;
                OBJ_RELEASE(ptr[i]);
            }
        }
        OBJ_RELEASE(sub->requestors);
    }

    if (NULL != sub->values) {
        ivals = (orte_gpr_replica_ivalue_t**)((sub->values)->addr);
        for (i=0, k=0; k < sub->num_values &&
                       i < (sub->values)->size; i++) {
            if (NULL != ivals[i]) {
                k++;
                OBJ_RELEASE(ivals[i]);
            }
        }
        OBJ_RELEASE(sub->values);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_subscription_t,           /* type name */
         opal_object_t,                 /* parent "class" name */
         orte_gpr_replica_subscription_construct,   /* constructor */
         orte_gpr_replica_subscription_destructor); /* destructor */


/* TRIGGER REQUESTOR */
/* constructor - used to initialize state of instance */
static void orte_gpr_replica_trigger_requestor_construct(orte_gpr_replica_trigger_requestor_t* ptr)
{
    ptr->index = 0;
    ptr->idtag = ORTE_GPR_TRIGGER_ID_MAX;
    ptr->requestor = NULL;
    ptr->idtag = 0;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_trigger_requestor_destructor(orte_gpr_replica_trigger_requestor_t* ptr)
{
    if (NULL != ptr->requestor) free(ptr->requestor);
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_trigger_requestor_t,           /* type name */
         opal_object_t,                 /* parent "class" name */
         orte_gpr_replica_trigger_requestor_construct,   /* constructor */
         orte_gpr_replica_trigger_requestor_destructor); /* destructor */


/* TRIGGER */
/* constructor - used to initialize state of trigger instance */
static void orte_gpr_replica_trigger_construct(orte_gpr_replica_trigger_t* trig)
{
    trig->name = NULL;
    trig->index = 0;
    trig->idtag = ORTE_GPR_TRIGGER_ID_MAX;

    trig->num_attached = 0;
    orte_pointer_array_init(&(trig->attached), (orte_std_cntr_t)orte_gpr_array_block_size,
                            (orte_std_cntr_t)orte_gpr_array_max_size,
                            (orte_std_cntr_t)orte_gpr_array_block_size);

    trig->master = NULL;;

    trig->action = ORTE_GPR_REPLICA_NO_ACTION;
    trig->one_shot_fired = false;
    trig->processing = false;

    trig->num_counters = 0;
    orte_pointer_array_init(&(trig->counters), (orte_std_cntr_t)orte_gpr_array_block_size,
                            (orte_std_cntr_t)orte_gpr_array_max_size,
                            (orte_std_cntr_t)orte_gpr_array_block_size);

    trig->num_subscriptions = 0;
    orte_pointer_array_init(&(trig->subscriptions), (orte_std_cntr_t)orte_gpr_array_block_size,
                            (orte_std_cntr_t)orte_gpr_array_max_size,
                            (orte_std_cntr_t)orte_gpr_array_block_size);

}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_trigger_destructor(orte_gpr_replica_trigger_t* trig)
{
    orte_std_cntr_t i, cnt;
    orte_gpr_replica_counter_t **cntrs;
    orte_gpr_replica_trigger_requestor_t **att;

    if (NULL != trig->name) {
        free(trig->name);
    }

    /* must go through the array of atached and release
     * the memory for each one prior to releasing the array
     */
    if (NULL != trig->attached) {
        att = (orte_gpr_replica_trigger_requestor_t**)((trig->attached)->addr);
        cnt = 0;
        for (i=0; cnt < trig->num_attached && i < (trig->attached)->size; i++) {
            if (NULL != att[i]) {
                cnt++;
                OBJ_RELEASE(att[i]);
            }
        }
        OBJ_RELEASE(trig->attached);
    }

    /* must go through the array of counters and release
     * the memory for each one prior to releasing the array
     */
    if (NULL != trig->counters) {
        cntrs = (orte_gpr_replica_counter_t**)((trig->counters)->addr);
        cnt = 0;
        for (i=0; cnt < trig->num_counters && i < (trig->counters)->size; i++) {
            if (NULL != cntrs[i]) {
                cnt++;
                OBJ_RELEASE(cntrs[i]);
            }
        }
        OBJ_RELEASE(trig->counters);
    }

    /* the array of subscriptions is separately maintained, so we
     * do NOT release the subscription memory here. We only release
     * the array of pointers we were using to reference into the
     * subscription array
     */
    if (NULL != trig->subscriptions) {
       OBJ_RELEASE(trig->subscriptions);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_trigger_t,           /* type name */
         opal_object_t,                 /* parent "class" name */
         orte_gpr_replica_trigger_construct,   /* constructor */
         orte_gpr_replica_trigger_destructor); /* destructor */


/* ACTION_TAKEN */
/* constructor - used to initialize state of action_take instance */
static void orte_gpr_replica_action_taken_construct(orte_gpr_replica_action_taken_t* ptr)
{
    ptr->action = ORTE_GPR_REPLICA_NO_ACTION;
    ptr->seg = NULL;
    ptr->cptr = NULL;
    ptr->iptr = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_action_taken_destructor(orte_gpr_replica_action_taken_t* ptr)
{
    /* since we did a "RETAIN" on the objects pointed to by this object,
     * we need to "RELEASE" them to indicate we are done with them
     */
    if (NULL != ptr->seg) OBJ_RELEASE(ptr->seg);
    if (NULL != ptr->cptr) OBJ_RELEASE(ptr->cptr);
    if (NULL != ptr->iptr) OBJ_RELEASE(ptr->iptr);
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_action_taken_t,           /* type name */
         opal_object_t,                 /* parent "class" name */
         orte_gpr_replica_action_taken_construct,   /* constructor */
         orte_gpr_replica_action_taken_destructor); /* destructor */


/* CALLBACKS */
/* constructor - used to initialize state of callback list instance */
static void orte_gpr_replica_callbacks_construct(orte_gpr_replica_callbacks_t* cb)
{
    cb->message = NULL;
    cb->requestor = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_callbacks_destructor(orte_gpr_replica_callbacks_t* cb)
{
    if (NULL != cb->message) OBJ_RELEASE(cb->message);

    if (NULL != cb->requestor) {
        free(cb->requestor);
    }

}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_replica_callbacks_t,           /* type name */
         opal_list_item_t,            /* parent "class" name */
         orte_gpr_replica_callbacks_construct,   /* constructor */
         orte_gpr_replica_callbacks_destructor); /* destructor */


/* REPLICA LIST - NOT IMPLEMENTED YET! */
/* constructor - used to initialize state of replica list instance */
static void orte_gpr_replica_list_construct(orte_gpr_replica_list_t* replica)
{
    replica->replica = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_replica_list_destructor(orte_gpr_replica_list_t* replica)
{
    if (NULL != replica->replica) {
       free(replica->replica);
       replica->replica = NULL;
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
           orte_gpr_replica_list_t,           /* type name */
           opal_list_item_t,                 /* parent "class" name */
           orte_gpr_replica_list_construct,   /* constructor */
           orte_gpr_replica_list_destructor); /* destructor */


/* WRITE INVALIDATE - NOT IMPLEMENTED YET! */
/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
           orte_gpr_replica_write_invalidate_t,            /* type name */
           opal_list_item_t,                          /* parent "class" name */
           NULL,    /* constructor */
           NULL);  /* destructor */


#endif /* _GPR_REPLICA_CLASS_INSTANCES_H_ */

