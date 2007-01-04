/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 */
/** @file
 */

/**
 *  \brief General Purpose Registry (GPR) API
 *
 * The Open MPI General Purpose Registry (GPR)
 *
 * This file contains the public type definitions supporting the GPR
 */

#ifndef ORTE_GPR_TYPES_H_
#define ORTE_GPR_TYPES_H_

#include "orte_config.h"
#include "orte/orte_types.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "orte/mca/schema/schema.h"
#include "opal/class/opal_object.h"
#include "orte/class/orte_pointer_array.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/ns/ns_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/** Define the notify actions for the subscription system - can be OR'd
 * to create multiple actions
 */
#define ORTE_GPR_NOTIFY_NONE                (uint8_t)0x00   /**< No trigger action */
#define ORTE_GPR_NOTIFY_VALUE_CHG_TO        (uint8_t)0x01   /**< Notifies subscriber when value changes to specified value */
#define ORTE_GPR_NOTIFY_VALUE_CHG_FRM       (uint8_t)0x02   /**< Notifies subscriber when value changes away from specified value */
#define ORTE_GPR_NOTIFY_VALUE_CHG           (uint8_t)0x03   /**< Notifies subscriber when value changes */
#define ORTE_GPR_NOTIFY_ADD_ENTRY           (uint8_t)0x04   /**< Notifies subscriber when entry added */
#define ORTE_GPR_NOTIFY_DEL_ENTRY           (uint8_t)0x08   /**< Notifies subscriber when entry deleted */
#define ORTE_GPR_NOTIFY_ALL                 (uint8_t)0x0f   /**< Notifies subscriber upon any action */
#define ORTE_GPR_NOTIFY_PRE_EXISTING        (uint8_t)0x10   /**< Provide list of all pre-existing data */
#define ORTE_GPR_NOTIFY_STARTS_AFTER_TRIG   (uint8_t)0x20   /**< Notifies are off when subscription entered - turned on when trigger fires */
#define ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG   (uint8_t)0x40   /**< Delete this subscription after associated trigger fires */
#define ORTE_GPR_NOTIFY_ANY                 (uint8_t)0xff   /**< Used to test if any action flags set */

typedef uint8_t orte_gpr_notify_action_t;
#define ORTE_GPR_NOTIFY_ACTION_T ORTE_UINT8

typedef int32_t orte_gpr_subscription_id_t;
#define ORTE_GPR_SUBSCRIPTION_ID_T ORTE_INT32
#define ORTE_GPR_SUBSCRIPTION_ID_MAX INT32_MAX


#define ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS    (uint8_t)0x01   /**< Include the trigger data in the notification msg */
#define ORTE_GPR_TRIG_ONE_SHOT              (uint8_t)0x02   /**< Only trigger once - then delete trigger */
#define ORTE_GPR_TRIG_ROUTE_DATA_THRU_ME    (uint8_t)0x04   /**< send all associated data to trigger callback fn */
#define ORTE_GPR_TRIG_AT_LEVEL              (uint8_t)0x08   /**< Trigger whenever count reaches specified level */
#define ORTE_GPR_TRIG_CMP_LEVELS            (uint8_t)0x80   /**< Trigger when all the specified values are equal */
#define ORTE_GPR_TRIG_ALL_AT                (uint8_t)0x7b   /**< Use all trig defs except include trig data with AT - a typical situation */
#define ORTE_GPR_TRIG_ALL_CMP               (uint8_t)0xf3   /**< Use all trig defs except include trig data with CMP */
#define ORTE_GPR_TRIG_ANY                   (uint8_t)0xff   /**< Used to test if any trigs are set */

typedef uint8_t orte_gpr_trigger_action_t;
#define ORTE_GPR_TRIGGER_ACTION_T ORTE_UINT8

typedef int32_t orte_gpr_trigger_id_t;
#define ORTE_GPR_TRIGGER_ID_T ORTE_INT32
#define ORTE_GPR_TRIGGER_ID_MAX INT32_MAX


/** Define the addressing mode bit-masks for registry operations.
 *
 * Token modes
 */
#define ORTE_GPR_TOKENS_AND     (uint16_t)0x0001    /**< AND tokens together for search results */
#define ORTE_GPR_TOKENS_OR      (uint16_t)0x0002    /**< OR tokens for search results */
#define ORTE_GPR_TOKENS_XAND    (uint16_t)0x0004    /**< All tokens required, nothing else allowed */
#define ORTE_GPR_TOKENS_XOR     (uint16_t)0x0008    /**< Any one of the tokens required, nothing else allowed */
#define ORTE_GPR_TOKENS_NOT     (uint16_t)0x0010    /**< Everything except those that meet specs */
/*
 * Key modes
 */
#define ORTE_GPR_KEYS_AND       (uint16_t)0x0100    /**< AND keys together */
#define ORTE_GPR_KEYS_OR        (uint16_t)0x0200    /**< OR keys together */
#define ORTE_GPR_KEYS_XAND      (uint16_t)0x0400    /**< All keys required, nothing else allowed */
#define ORTE_GPR_KEYS_XOR       (uint16_t)0x0800    /**< Any one of the keys required, nothing else allowed */
#define ORTE_GPR_KEYS_NOT       (uint16_t)0x1000    /**< Everything except those that meet specs */
/*
 * General modes
 */
#define ORTE_GPR_STRIPPED       (uint16_t)0x2000    /**< Return values should contain no descriptive info */
#define ORTE_GPR_OVERWRITE      (uint16_t)0x8000    /**< Allow overwrite of existing info */
#define ORTE_GPR_NO_OVERWRITE   (uint16_t)0x0000    /**< Do not allow overwrite of existing info */

typedef uint16_t orte_gpr_addr_mode_t;
#define ORTE_GPR_ADDR_MODE_T ORTE_UINT16
/*
 * typedefs
 */
 /*
  * Key-value pairs for registry operations
  */
typedef struct {
    opal_list_item_t super;             /* required for this to be on a list */
    char *key;                          /* string key for this value */
    orte_data_value_t *value;           /* value */
} orte_gpr_keyval_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_gpr_keyval_t);


/** Return value structure for registry requests.
 * A request for information stored within the registry returns an array of values that
 * correspond to the provided tokens. Each object in the array contains an array of
 * keyvals from a specific container. Note that the array
 * contains \em copies of the data in the registry. This prevents inadvertent
 * modification of the registry, but requires the recipient to release the data's
 * memory when done.
 *
 * The address mode and segment fields are included here for convenience and so that
 * the structure can be re-used by the put command.
 */
typedef struct {
    opal_object_t super;                    /**< Makes this an object */
    orte_gpr_addr_mode_t addr_mode;         /**< Address mode that was used for combining keys/tokens */
    char *segment;                          /**< Name of the segment this came from */
    orte_std_cntr_t cnt;                             /**< Number of keyval objects returned */
    orte_gpr_keyval_t **keyvals;            /**< Contiguous array of keyval object pointers */
    orte_std_cntr_t num_tokens;                      /**< Number of tokens from the container that held these keyvals */
    char **tokens;                          /**< List of tokens that described the container */
} orte_gpr_value_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_gpr_value_t);
#define ORTE_GPR_VALUE_EMPTY {{OBJ_CLASS(orte_gpr_value_t),0}, 0, NULL, 0, NULL, 0, NULL}

/** Return structure for notification messages
 * A notification message contains data from each registered subscription structure.
 * Each block of data is associated with a specified callback function and contains
 * data from a single segment, one or more containers with one or more keyvals/container.
 */
typedef struct orte_gpr_notify_data_t {
    opal_object_t super;            /**< Makes this an object */
    char *target;                   /**< Name of the associated subscripton, if provided */
    orte_gpr_subscription_id_t id;  /**< Number of the associated subscription */
    bool remove;                    /**< Remove this subscription from recipient's tracker */
    orte_std_cntr_t cnt;                     /**< Number of value objects returned, one per container */
    orte_pointer_array_t *values;   /**< Array of value objects returned */
} orte_gpr_notify_data_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_gpr_notify_data_t);

/** Return message for notify requests
 */
typedef uint8_t orte_gpr_notify_msg_type_t;
#define ORTE_GPR_NOTIFY_MSG_TYPE_T ORTE_UINT8
#define ORTE_GPR_TRIGGER_MSG        (orte_gpr_notify_msg_type_t)0x01
#define ORTE_GPR_SUBSCRIPTION_MSG   (orte_gpr_notify_msg_type_t)0x02

typedef struct {
    opal_object_t super;                        /**< Make this an object */
    orte_gpr_notify_msg_type_t msg_type;    /**< trigger or subscription msg */
    char *target;               /**< Name of the associated trigger, if provided */
    orte_gpr_trigger_id_t id;   /**< trigger id, if message comes from trigger
                                    (ORTE_GPR_TRIGGER_ID_MAX otherwise) */
    bool remove;                /**< Remove this trigger from recipient's tracker */
    orte_std_cntr_t cnt;                 /**< number of data objects */
    orte_pointer_array_t *data; /**< Contiguous array of pointers to data objects */
} orte_gpr_notify_message_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_gpr_notify_message_t);

/** Notify callback function
 * notify_msg = message containing data provided by trigger
 *
 * user_tag = whatever tag data the user provided when filing the subscription
 */
typedef void (*orte_gpr_notify_cb_fn_t)(orte_gpr_notify_data_t *notify_data, void *user_tag);

/** Trigger callback function
 * notify_msg = message containing multiple blocks of data provided by trigger
 *
 * user_tag = whatever tag data the user provided when filing the subscription
 *
 * Since this only takes place locally, we CAN get a status code from the callback!
 */
typedef int (*orte_gpr_trigger_cb_fn_t)(orte_gpr_notify_message_t *msg);

/** Structure for registering subscriptions
 * A request to be notified when certain events occur, or when counters reach specified
 * values, is registered on the registry via a subscription request. This structure
 * is provided to concisely provide the required information. The information in this
 * structure describes the data that is to be sent when the subscription "fires". It includes
 * the segment upon which the data resides, the tokens that describe the containers, and
 * the keys that describe the keyvals to be returned. These are combined via the
 * addr_mode to locate and return the data.
 */
typedef struct {
    opal_object_t super;                    /**< Makes this an object */
    char *name;                             /**< A unique name for this subscription - can be NULL */
    orte_gpr_subscription_id_t id;          /**< id number of this subscription, as assigned by system */
    orte_gpr_notify_action_t action;        /**< what causes subscription to fire */
    orte_std_cntr_t cnt;                             /**< Number of values included */
    orte_gpr_value_t **values;              /**< Contiguous array of pointers to value objects
                                                 describing the data to be returned */
    orte_gpr_notify_cb_fn_t cbfunc;         /**< the callback function */
    void *user_tag;                         /**< User-provided tag to be used in cbfunc */
} orte_gpr_subscription_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_gpr_subscription_t);
#define ORTE_GPR_SUBSCRIPTION_EMPTY {{OBJ_CLASS(orte_gpr_subscription_t),0}, NULL, ORTE_GPR_SUBSCRIPTION_ID_MAX, 0, 0, NULL, 0, NULL}

/** Structure for registering triggers
 * A trigger causes the associated subscriptions to be executed at a specified event,
 * such as when counters reach specified values. The data provided here specifies
 * which objects on the registry are to be monitored, and what conditions must
 * exist between those objects for the trigger to be "fired".
 */
typedef struct {
    opal_object_t super;                    /**< Makes this an object */
    char *name;                             /**< A unique name for this trigger - can be NULL */
    orte_gpr_trigger_id_t id;               /**< id number of this trigger, as assigned by system */
    orte_gpr_trigger_action_t action;       /**< trigger characteristics */
    orte_std_cntr_t cnt;                             /**< Number of values included */
    orte_gpr_value_t **values;              /**< Contiguous array of pointers to value objects
                                                 describing the objects to be monitored */
    orte_gpr_trigger_cb_fn_t cbfunc;        /**< the callback function */
    void *user_tag;                         /**< User-provided tag to be used in cbfunc */
} orte_gpr_trigger_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_gpr_trigger_t);
#define ORTE_GPR_TRIGGER_EMPTY {{OBJ_CLASS(orte_gpr_trigger_t),0}, NULL, ORTE_GPR_TRIGGER_ID_MAX, 0, 0, NULL, 0, NULL}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* GPR_TYPES_H */
