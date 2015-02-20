/*
 * Copyright (c) 2013-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "orte/mca/errmgr/errmgr.h"

mca_memheap_base_module_t mca_memheap;

/**
 * Function for weeding out memheap components that shouldn't be executed.
 * Implementation inspired by btl/base.
 *
 * Call the init function on all available components to find out if
 * they want to run. Select all components that don't fail.  Failing
 * components will be closed and unloaded.  The selected modules will
 * be pointed to by mca_memheap_base_module_t.
 */

static memheap_context_t* _memheap_create(void);

/**
 * Choose to init one component with the highest priority.
 * If the include list if it is not empty choose a component that appear in the list. 
 * O/W choose the highest priority component not in the exclude list.
 * Include and exclude lists may be given in the shmem launcher command line.
 */
int mca_memheap_base_select()
{
    int priority = 0;
    int max_priority = 0;
    mca_base_component_list_item_t *cli, *next;
    mca_memheap_base_component_t *component = NULL;
    mca_memheap_base_component_t *max_priority_component = NULL;
    mca_memheap_base_module_t *module = NULL;
    memheap_context_t *context = NULL;

    char** include = opal_argv_split(mca_memheap_base_include, ',');
    char** exclude = opal_argv_split(mca_memheap_base_exclude, ',');

    context = _memheap_create();
    if (!context) {
        opal_argv_free(include);
        opal_argv_free(exclude);
        return OSHMEM_ERROR;
    }

    OPAL_LIST_FOREACH_SAFE(cli, next, &oshmem_memheap_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (mca_memheap_base_component_t *) cli->cli_component;

        /* Verify if the component is in the include or the exclude list. */
        /* If there is an include list - item must be in the list to be included */
        if (NULL != include) {
            char** argv = include;
            bool found = false;
            while (argv && *argv) {
                if (strcmp(component->memheap_version.mca_component_name, *argv)
                        == 0) {
                    found = true;
                    break;
                }
                argv++;
            }
            /* If not in the list do not choose this component */
            if (found == false) {
                continue;
            }

            /* Otherwise - check the exclude list to see if this item has been specifically excluded */
        } else if (NULL != exclude) {
            char** argv = exclude;
            bool found = false;
            while (argv && *argv) {
                if (strcmp(component->memheap_version.mca_component_name, *argv)
                        == 0) {
                    found = true;
                    break;
                }
                argv++;
            }
            if (found == true) {
                continue;
            }
        }

        /* Verify that the component has an init function */
        if (NULL == component->memheap_init) {
            MEMHEAP_VERBOSE(10,
                            "select: no init function; for component %s. No component selected",
                            component->memheap_version.mca_component_name);
        } else {

            MEMHEAP_VERBOSE(5,
                            "select: component %s size : user %d private: %d",
                            component->memheap_version.mca_component_name, (int)context->user_size, (int)context->private_size);

            /* Init the component in order to get its priority */
            module = component->memheap_init(context, &priority);

            /* If the component didn't initialize, remove it from the opened                                                                                                list, remove it from the component repository and return an error */
            if (NULL == module) {
                MEMHEAP_VERBOSE(10,
                                "select: init of component %s returned failure",
                                component->memheap_version.mca_component_name);

                opal_list_remove_item(&oshmem_memheap_base_framework.framework_components, &cli->super);
                mca_base_component_close((mca_base_component_t *) component,
                                         oshmem_memheap_base_framework.framework_output);
            } else {
                /* Calculate memheap size in case it was not set during component initialization */
                module->memheap_size = context->user_size;
            }
        }

        /* Init max priority component */
        if (NULL == max_priority_component) {
            max_priority_component = component;
            mca_memheap_base_module_initialized = module;
            max_priority = priority;
        }

        /* Update max priority component if current component has greater priority */
        if (priority > max_priority) {
            max_priority = priority;
            max_priority_component = component;
            mca_memheap_base_module_initialized = module;
        }
    }

    opal_argv_free(include);
    opal_argv_free(exclude);

    /* Verify that a component was selected */
    if (NULL == max_priority_component) {
        MEMHEAP_VERBOSE(10, "select: no component selected");
        return OSHMEM_ERROR;
    }

    /* Verify that some module was initialized */
    if (NULL == mca_memheap_base_module_initialized) {
        opal_show_help("help-oshmem-memheap.txt",
                       "find-available:none-found",
                       true,
                       "memheap");
        orte_errmgr.abort(1, NULL );
    }

    MEMHEAP_VERBOSE(10,
                    "SELECTED %s component %s",
                    max_priority_component->memheap_version.mca_type_name, max_priority_component->memheap_version.mca_component_name);

    setenv(SHMEM_HEAP_TYPE,
           max_priority_component->memheap_version.mca_component_name,
           1);

    mca_memheap = *mca_memheap_base_module_initialized;

    return OSHMEM_SUCCESS;
}

static size_t _memheap_size(void)
{
    char *p;
    long long factor = 1;
    int idx;
    long long size = 0;

    p = getenv(OSHMEM_ENV_SYMMETRIC_SIZE);
    if (!p) {
        p = getenv(SHMEM_HEAP_SIZE);
    } else {
        char *p1 = getenv(SHMEM_HEAP_SIZE);
        if (p1 && strcmp(p, p1)) {
            MEMHEAP_ERROR("Found conflict between env '%s' and '%s'.\n",
                          OSHMEM_ENV_SYMMETRIC_SIZE, SHMEM_HEAP_SIZE);
            return 0;
        }
    }

    if (!p) {
        size = SIZE_IN_MEGA_BYTES(DEFAULT_SYMMETRIC_HEAP_SIZE);
    } else if (1 == sscanf(p, "%lld%n", &size, &idx)) {
        if (p[idx] != '\0') {
            if (p[idx + 1] == '\0') {
                if (p[idx] == 'k' || p[idx] == 'K') {
                    factor = 1024;
                } else if (p[idx] == 'm' || p[idx] == 'M') {
                    factor = 1024 * 1024;
                } else if (p[idx] == 'g' || p[idx] == 'G') {
                    factor = 1024 * 1024 * 1024;
                } else if (p[idx] == 't' || p[idx] == 'T') {
                    factor = 1024UL * 1024UL * 1024UL * 1024UL;
                } else {
                    size = 0;
                }
            } else {
                size = 0;
            }
        }
    }

    if (size <= 0) {
        MEMHEAP_ERROR("Set incorrect symmetric heap size\n");
        return 0;
    } else {
        char *tmp = p;

        if(!p) {
            asprintf(&tmp, "%lld", size);
        }

        if (tmp) {
            setenv(OSHMEM_ENV_SYMMETRIC_SIZE, tmp, 1);
            setenv(SHMEM_HEAP_SIZE, tmp, 1);
        }

        if (!p && tmp) {
            free(tmp);
        }
    }

    return (size_t) memheap_align(size * factor);
}

static memheap_context_t* _memheap_create(void)
{
    int rc = OSHMEM_SUCCESS;
    static memheap_context_t context;
    size_t user_size;

    user_size = _memheap_size();
    if (user_size < MEMHEAP_BASE_MIN_SIZE) {
        MEMHEAP_ERROR("Requested memheap size is less than minimal meamheap size (%llu < %llu)",
                      (unsigned long long)user_size, MEMHEAP_BASE_MIN_SIZE);
        return NULL ;
    }
    /* Inititialize symmetric area */
    if (OSHMEM_SUCCESS == rc) {
        rc = mca_memheap_base_alloc_init(&mca_memheap_base_map,
                                         user_size + MEMHEAP_BASE_PRIVATE_SIZE);
    }

    /* Inititialize static/global variables area */
    if (OSHMEM_SUCCESS == rc) {
        rc = mca_memheap_base_static_init(&mca_memheap_base_map);
    }

    /* Memory Registration */
    if (OSHMEM_SUCCESS == rc) {
        rc = mca_memheap_base_reg(&mca_memheap_base_map);
    }

    /* Init OOB channel */
    if (OSHMEM_SUCCESS == rc) {
        rc = memheap_oob_init(&mca_memheap_base_map);
    }

    if (OSHMEM_SUCCESS == rc) {
        context.user_size = user_size;
        context.private_size = MEMHEAP_BASE_PRIVATE_SIZE;
        context.user_base_addr =
                (void*) ((unsigned char*) mca_memheap_base_map.mem_segs[HEAP_SEG_INDEX].seg_base_addr
                        + 0);
        context.private_base_addr =
                (void*) ((unsigned char*) mca_memheap_base_map.mem_segs[HEAP_SEG_INDEX].seg_base_addr
                        + context.user_size);
    }

    return ((OSHMEM_SUCCESS == rc) ? &context : NULL );
}
