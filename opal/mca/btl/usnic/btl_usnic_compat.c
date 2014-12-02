/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if BTL_IN_OPAL
#include "opal_config.h"
#else
#include "ompi_config.h"
#endif

#include "opal/mca/mca.h"
#include "opal_stdint.h"

#include "btl_usnic_compat.h"
#include "btl_usnic_endpoint.h"

/************************************************************************/

/* v1.9 and beyond */

#if (OPAL_MAJOR_VERSION == 1 && OPAL_MINOR_VERSION >= 9) || \
    (OPAL_MAJOR_VERSION >= 2)

#include "opal/util/proc.h"

/**************************
 * usNIC BTL-specific functions to hide differences between master and
 * v1.8
 **************************/
void usnic_compat_modex_send(int *rc,
                             mca_base_component_t *component,
                             opal_btl_usnic_modex_t *modexes,
                             size_t size)
{
    OPAL_MODEX_SEND(*rc, PMIX_SYNC_REQD, PMIX_REMOTE, component,
                    modexes, size);
}

void usnic_compat_modex_recv(int *rc,
                             mca_base_component_t *component,
                             opal_proc_t *proc,
                             opal_btl_usnic_modex_t **modexes,
                             size_t *size)
{
    OPAL_MODEX_RECV(*rc, component, proc, (uint8_t**) modexes, size);
}

uint64_t usnic_compat_rte_hash_name(opal_process_name_t *pname)
{
    uint64_t name = pname->jobid;
    name <<= 32;
    name += pname->vpid;
    return name;
}

const char *usnic_compat_proc_name_print(opal_process_name_t *pname)
{
    return OPAL_NAME_PRINT(*pname);
}

/************************************************************************/

/* v1.7 and v1.8 */

#elif (OPAL_MAJOR_VERSION == 1 && OPAL_MINOR_VERSION >= 7)

#include "ompi/proc/proc.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/mca/rte/base/base.h"
#include "ompi/runtime/ompi_module_exchange.h"


/**************************
 * Replicate functions that exist on master
 **************************/
char* opal_get_proc_hostname(opal_proc_t *proc)
{
    return proc->proc_hostname;
}

/**************************
 * usNIC BTL-specific functions to hide differences between master and
 * v1.8
 **************************/
void usnic_compat_modex_send(int *rc,
                             mca_base_component_t *component,
                             struct opal_btl_usnic_modex_t *modexes,
                             size_t size)
{
    *rc = ompi_modex_send(component, modexes, size);
}

void usnic_compat_modex_recv(int *rc,
                             mca_base_component_t *component,
                             opal_proc_t *proc,
                             struct opal_btl_usnic_modex_t **modexes,
                             size_t *size)
{
    *rc = ompi_modex_recv(component, proc, (void*) modexes, size);
}

uint64_t usnic_compat_rte_hash_name(opal_process_name_t *pname)
{
    return ompi_rte_hash_name(pname);
}

const char *usnic_compat_proc_name_print(opal_process_name_t *pname)
{
    return OMPI_NAME_PRINT(pname);
}

#endif
