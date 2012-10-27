/*
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * memchecker (memory checker) pin framework component interface.
 *
 * Intent
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/memchecker/memchecker.h"
#include "opal/mca/memchecker/base/base.h"
#include "memchecker_pin.h"


/*
 * Local functions
 */
static int pin_module_init(void);
static int pin_module_runindebugger(void);
static int pin_module_isaddressable(void * p, size_t len);
static int pin_module_isdefined(void * p, size_t len);
static int pin_module_mem_noaccess(void * p, size_t len);
static int pin_module_mem_undefined(void * p, size_t len);
static int pin_module_mem_defined(void * p, size_t len);
static int pin_module_mem_defined_if_addressable(void * p, size_t len);
static int pin_module_create_block(void * p, size_t len, char * description);
static int pin_module_discard_block(void * p); /* Here, we need to do some mapping for pin */
static int pin_module_leakcheck(void);
static int pin_module_get_vbits(void * p, char * vbits, size_t len);
static int pin_module_set_vbits(void * p, char * vbits, size_t len);
static int pin_module_reg_mem_watch(void * p, size_t len, int op, void *cb, void *info);
static int pin_module_unreg_mem_watch(void * p, size_t len);
static int pin_module_unreg_all_mem_watch();
static int pin_module_search_mem_index(void * p, size_t len, size_t *index);
static int pin_module_mem_watch_count();
static int pin_module_print_callstack();

/*
 * pin memchecker module
 */
static const opal_memchecker_base_module_1_0_0_t loc_module = {
    /* Initialization function */
    pin_module_init,

    /* Module function pointers */
    pin_module_runindebugger,
    pin_module_isaddressable,
    pin_module_isdefined,
    pin_module_mem_noaccess,
    pin_module_mem_undefined,
    pin_module_mem_defined,
    pin_module_mem_defined_if_addressable,
    pin_module_create_block,
    pin_module_discard_block,
    pin_module_leakcheck,
    pin_module_get_vbits,
    pin_module_set_vbits,
    pin_module_reg_mem_watch,
    pin_module_unreg_mem_watch,
    pin_module_unreg_all_mem_watch,
    pin_module_search_mem_index,
    pin_module_mem_watch_count,
    pin_module_print_callstack
};

int MEMPIN_RUNNING_WITH_PIN(int *pin_alive)
{
    return OPAL_SUCCESS;
}

/*     MEMPIN_REG_MEM_WATCH(addr, size, watch_type, cb_func, cb_args); */
int MEMPIN_REG_MEM_WATCH(void* addr, int size, int op, void * cb_func, void* cb_args)
{
    /* assert the parameters */
    return OPAL_SUCCESS;
}


/*     MEMPIN_UNREG_WATCH(addr, size); */
int MEMPIN_UNREG_MEM_WATCH(void* addr, int size)
{
    /* assert the parameters */
    return OPAL_SUCCESS;
}


/*     MEMPIN_UNREG_MEM_WATCH(); */
int MEMPIN_UNREG_ALL_MEM_WATCH()
{
    /* assert the parameters */
    return OPAL_SUCCESS;
}

/*     MEMPIN_SEARCH_MEM_INDEX(); */
int MEMPIN_SEARCH_MEM_INDEX(void* addr, int size, size_t *index)
{
    /* assert the parameters */
    return OPAL_SUCCESS;
}

/*     MEMPIN_UNREG_MEM_WATCH(); */
int MEMPIN_MEM_WATCH_COUNT()
{
    /* assert the parameters */
    return OPAL_SUCCESS;
}

/*     MEMPIN_UNREG_PRINT_CALLSTACK(); */
int MEMPIN_PRINT_CALLSTACK()
{
    /* assert the parameters */
    return OPAL_SUCCESS;
}


int opal_memchecker_pin_component_query(mca_base_module_t **module, int *priority)
{
    int param;

    param = mca_base_param_find("memchecker", "pin", "priority");
    mca_base_param_lookup_int(param, priority);

    *module = (mca_base_module_t *)&loc_module;

    return OPAL_SUCCESS;
}


static int pin_module_init(void)
{
    /* Nothing to do yet, possibly update the amount of memory blocks. */

    return OPAL_SUCCESS;
}


static int pin_module_runindebugger(void)
{
    int pin_alive=0;

    MEMPIN_RUNNING_WITH_PIN(&pin_alive);

    if(pin_alive) {
        return 1;
    } else {
        return 0;
    }
}


static int pin_module_isaddressable(void * p, size_t len)
{
/*     if (len > 0) { */
/*         PIN_CHECK_MEM_IS_ADDRESSABLE(p, len); */
/*     } */

    return OPAL_SUCCESS;
}


static int pin_module_isdefined(void * p, size_t len)
{
/*     if (len > 0) { */
/*         PIN_CHECK_MEM_IS_DEFINED(p, len); */
/*     } */

    return OPAL_SUCCESS;
}


static int pin_module_mem_noaccess(void * p, size_t len)
{
/*     if (len > 0) { */
/*         PIN_MAKE_MEM_NOACCESS(p, len); */
/*     } */

    return OPAL_SUCCESS;
}


static int pin_module_mem_undefined(void * p, size_t len)
{
/*     if (len > 0) { */
/*         PIN_MAKE_MEM_UNDEFINED(p, len); */
/*     } */

    return OPAL_SUCCESS;
}


static int pin_module_mem_defined(void * p, size_t len)
{
/*     if (len > 0) { */
/*         PIN_MAKE_MEM_DEFINED(p, len); */
/*     } */

    return OPAL_SUCCESS;
}


static int pin_module_mem_defined_if_addressable(void * p, size_t len)
{
/*     if (len > 0) { */
/*         PIN_MAKE_MEM_DEFINED_IF_ADDRESSABLE(p, len); */
/*     } */

    return OPAL_SUCCESS;
}


static int pin_module_create_block(void * p, size_t len, char * description)
{
/*     if (len > 0) { */
/*         PIN_CREATE_BLOCK (p, len, description); */
/*         /\* */
/*          * Add p to some list atomically */
/*          *\/ */
/*     } */
    return OPAL_SUCCESS;
}


static int pin_module_discard_block(void * p)
{
    /* Here, we need to do some mapping for pin */
    /*
     * If p in list, then get rid of name
     */
    return OPAL_SUCCESS;
}


static int pin_module_leakcheck(void)
{
/*     PIN_DO_LEAK_CHECK; */
    return OPAL_SUCCESS;
}


static int pin_module_get_vbits(void * p, char * vbits, size_t len)
{
/*     if (len > 0) { */
/*         PIN_GET_VBITS(p, vbits, len); */
/*     } */

    return OPAL_SUCCESS;
}


static int pin_module_set_vbits(void * p, char * vbits, size_t len)
{
/*     if (len > 0) { */
/*         PIN_SET_VBITS(p, vbits, len); */
/*     } */

    return OPAL_SUCCESS;
}


static int pin_module_reg_mem_watch(void * p, size_t len, int op, void *cb, void *info)
{
    if (len > 0) {
        return MEMPIN_REG_MEM_WATCH(p, len, op, cb, info);
    }

    return OPAL_SUCCESS;
}


static int pin_module_unreg_mem_watch(void * p, size_t len)
{
    if (len > 0) {
        return MEMPIN_UNREG_MEM_WATCH(p, len);
    }

    return OPAL_SUCCESS;
}


static int pin_module_unreg_all_mem_watch()
{
    return MEMPIN_UNREG_ALL_MEM_WATCH();
}


static int pin_module_search_mem_index(void * p, size_t len, size_t *index)
{
    return MEMPIN_SEARCH_MEM_INDEX(p, len, index);
}


static int pin_module_mem_watch_count()
{
    return MEMPIN_MEM_WATCH_COUNT();
}


static int pin_module_print_callstack()
{
    return MEMPIN_PRINT_CALLSTACK();
}
