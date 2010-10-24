/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#ifndef MCA_LIBEVENT207_H
#define MCA_LIBEVENT207_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"


BEGIN_C_DECLS

/**
 * Globally exported variable
 */
OPAL_DECLSPEC extern const opal_event_component_t mca_event_libevent207_component;

OPAL_DECLSPEC extern const opal_event_module_t opal_event_libevent207;

END_C_DECLS
#endif /* MCA_LIBEVENT207_H */
