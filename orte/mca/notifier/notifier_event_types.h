/*
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
 * Copyright (c) 2009      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_NOTIFIER_BASE_EVENTS_H
#define ORTE_NOTIFIER_BASE_EVENTS_H

#include "orte_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/sys/atomic.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_object.h"

BEGIN_C_DECLS


typedef struct {
    opal_list_item_t super;
    volatile int32_t ev_cnt;
    int ev_id;
    int ev_already_traced;
    time_t ev_time_trc;
    char *ev_msg;
} orte_notifier_event_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_notifier_event_t);


END_C_DECLS

#endif /* ORTE_NOTIFIER_BASE_EVENTS_H */
