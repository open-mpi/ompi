/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/class/opal_value_array.h"
#include "opal/util/show_help.h"
#include "opal/class/opal_hash_table.h"
#include "opal/util/printf.h"
#include "opal/util/argv.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/base/mca_base_param_internal.h"
#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/runtime/opal.h"

#include "opal/mca/base/mca_base_var.h"

/*
 * local variables
 */
static opal_value_array_t mca_base_params;
static const char *mca_prefix = "OMPI_MCA_";
static bool initialized = false;

/*
 * local functions
 */
static void param_constructor(mca_base_param_t *p);
static void param_destructor(mca_base_param_t *p);
static void info_constructor(mca_base_param_info_t *p);
static void info_destructor(mca_base_param_info_t *p);

/*
 * Make the class instance for mca_base_param_t
 */
OBJ_CLASS_INSTANCE(mca_base_param_t, opal_object_t, 
                   param_constructor, param_destructor);
OBJ_CLASS_INSTANCE(mca_base_param_info_t, opal_list_item_t,
                   info_constructor, info_destructor);

/*
 * Set it up
 */
int mca_base_param_init(void)
{
    int ret;

    if (!initialized) {
        initialized = true;

        OBJ_CONSTRUCT(&mca_base_params, opal_value_array_t);
        opal_value_array_init (&mca_base_params, sizeof (mca_base_param_t));

        ret = mca_base_var_init ();
        if (OPAL_SUCCESS != ret) {
            return ret;
        }
    }

    return OPAL_SUCCESS;
}

/* Leave file caching up to the variable system */
int mca_base_param_recache_files(bool rel_path_search)
{
    return OPAL_SUCCESS;
}

/*
 * Register an MCA parameter 
 */
static int register_param (const char *type_name, const char *component_name,
                           const char *param_name, const char *help_msg,
                           bool internal, bool read_only, mca_base_param_type_t type,
                           void *default_value, void *current_value)
{
    mca_base_var_flag_t flags = 0;
    mca_base_var_type_t var_type;
    mca_base_param_t param;
    int ret, var_index;

    if (!initialized) {
        mca_base_param_init ();
    }

    OBJ_CONSTRUCT(&param, mca_base_param_t);

    if (internal) {
        flags |= MCA_BASE_VAR_FLAG_INTERNAL;
    }
    if (read_only) {
        flags |= MCA_BASE_VAR_FLAG_DEFAULT_ONLY;
    }

    /* Create a backing store for this parameter (needs to be malloc since the param
       will be memcpy'd into the parameter list) */
    param.param_value = calloc (1, sizeof (*param.param_value));
    if (NULL == param.param_value) {
        OBJ_DESTRUCT(&param);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    switch (type) {
    case MCA_BASE_PARAM_TYPE_INT:
        var_type = MCA_BASE_VAR_TYPE_INT;
        param.param_value->intval = ((int *)default_value)[0];
        break;
    case MCA_BASE_PARAM_TYPE_STRING:
        var_type = MCA_BASE_VAR_TYPE_STRING;
        if (default_value) {
            param.param_value->stringval = (char *) default_value;
        }
        break;
    case MCA_BASE_PARAM_TYPE_MAX:
        OBJ_DESTRUCT(&param);
        return OPAL_ERROR;
    }

    var_index = mca_base_var_register (NULL, type_name, component_name,
                                       param_name, help_msg, var_type, NULL,
                                       0, flags, OPAL_INFO_LVL_9,
                                       MCA_BASE_VAR_SCOPE_READONLY,
                                       param.param_value);

    param.var_index = var_index;

    if (0 > var_index) {
        return OPAL_ERROR;
    }

    ret = opal_value_array_append_item (&mca_base_params, &param);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    if (current_value) {
        switch (type) {
        case MCA_BASE_PARAM_TYPE_INT:
            ((int *) current_value)[0] = param.param_value->intval;
            break;
        case MCA_BASE_PARAM_TYPE_STRING:

            if (NULL != param.param_value->stringval) {
                ((char **) current_value)[0] = strdup (param.param_value->stringval);
            } else {
                ((char **) current_value)[0] = NULL;
            }
        case MCA_BASE_PARAM_TYPE_MAX:
            /* Impossible */
            break;
        }
    }

    return var_index;
}

int mca_base_param_reg_int(const mca_base_component_t *component,
                           const char *param_name, 
                           const char *help_msg,
                           bool internal,
                           bool read_only,
                           int default_value,
                           int *current_value)
{
    return register_param (component->mca_type_name, component->mca_component_name,
                           param_name, help_msg, internal, read_only,
                           MCA_BASE_PARAM_TYPE_INT, (void *) &default_value,
                           (void *) current_value);
}

/*
 * Register an integer MCA parameter that is not associated with a
 * component
 */
int mca_base_param_reg_int_name(const char *type,
                                const char *param_name, 
                                const char *help_msg,
                                bool internal,
                                bool read_only,
                                int default_value,
                                int *current_value)
{
    return register_param (type, NULL, param_name, help_msg, internal, read_only,
                           MCA_BASE_PARAM_TYPE_INT, (void *) &default_value,
                           (void *) current_value);
}

int mca_base_param_reg_string(const mca_base_component_t *component,
                              const char *param_name, 
                              const char *help_msg,
                              bool internal,
                              bool read_only,
                              const char *default_value,
                              char **current_value)
{
    return register_param (component->mca_type_name, component->mca_component_name,
                           param_name, help_msg, internal, read_only,
                           MCA_BASE_PARAM_TYPE_STRING, (void *) default_value,
                           (void *) current_value);
}

/*
 * Register a string MCA parameter that is not associated with a
 * component
 */
int mca_base_param_reg_string_name(const char *type,
                                   const char *param_name, 
                                   const char *help_msg,
                                   bool internal,
                                   bool read_only,
                                   const char *default_value,
                                   char **current_value)
{
    return register_param (type, NULL, param_name, help_msg, internal, read_only,
                           MCA_BASE_PARAM_TYPE_STRING, (void *) default_value,
                           (void *) current_value);
}

/*
 * Register a synonym name for an existing MCA parameter
 */
static int reg_syn (int index_orig, const char *type_name, const char *component_name,
                    const char *syn_param_name, bool deprecated)
{
    return mca_base_var_register_synonym (index_orig, NULL, type_name,
                                          component_name, syn_param_name,
                                          deprecated ? MCA_BASE_VAR_SYN_FLAG_DEPRECATED : 0);
}

int mca_base_param_reg_syn(int index_orig,
                           const mca_base_component_t *syn_component,
                           const char *syn_param_name, bool deprecated)
{
    return reg_syn (index_orig, syn_component->mca_type_name,
                    syn_component->mca_component_name, syn_param_name,
                    deprecated);
}

/*
 * Register a synonym name for an existing MCA parameter
 */
int mca_base_param_reg_syn_name(int index_orig,
                                const char *syn_type_name,
                                const char *syn_param_name, bool deprecated)
{
    return reg_syn (index_orig, syn_type_name, NULL, syn_param_name,
                    deprecated);
}

/*
 * Look up an integer MCA parameter.
 */
int mca_base_param_lookup_int(int index, int *value)
{
    const mca_base_var_t *var;
    const mca_base_var_storage_t *tmp;
    int ret;

    ret = mca_base_var_get (index, &var);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    ret = mca_base_var_get_value (index, &tmp, NULL, NULL);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    if (MCA_BASE_VAR_TYPE_BOOL == var->mbv_type) {
        *value = tmp->boolval;
    } else if (MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG == var->mbv_type) {
        *value = (int) tmp->ullval;
    } else if (MCA_BASE_VAR_TYPE_SIZE_T == var->mbv_type) {
        *value = (int) tmp->sizetval;
    } else {
        *value = tmp->intval;
    }

    return OPAL_SUCCESS;
}


/*
 * Set an integer parameter
 */
int mca_base_param_set_int(int index, int value)
{
    const mca_base_var_t *var;
    mca_base_var_storage_t tmp;

    int ret;

    ret = mca_base_var_get (index, &var);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    if (MCA_BASE_VAR_TYPE_BOOL == var->mbv_type) {
        tmp.boolval = !!value;
    } else if (MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG == var->mbv_type) {
        tmp.ullval = (unsigned long long) value;
    } else if (MCA_BASE_VAR_TYPE_SIZE_T == var->mbv_type) {
        tmp.sizetval = (size_t) value;
    } else {
        tmp.intval = value;
    }

    return mca_base_var_set_value (index, &tmp, sizeof (tmp),
                                   MCA_BASE_VAR_SOURCE_SET, NULL);
}

/*
 * Deregister a parameter
 */
int mca_base_param_deregister(int index)
{
    return mca_base_var_deregister (index);
}

/*
 * Look up a string MCA parameter.
 */
int mca_base_param_lookup_string(int index, char **value)
{
    const char **tmp;
    int ret;

    *value = NULL;

   ret = mca_base_var_get_value (index, &tmp, NULL, NULL);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* MCA param users expect us to return a copy of the string */
    if (tmp && tmp[0]) {
        *value = strdup (tmp[0]);
        if (NULL == *value) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    return OPAL_SUCCESS;
}


/*
 * Set an string parameter
 */
int mca_base_param_set_string(int index, char *value)
{
    return mca_base_var_set_value (index, value, value ? strlen (value) : 0,
                                   MCA_BASE_VAR_SOURCE_SET, NULL);
}


/*
 * Lookup the source of an MCA param's value
 */
int mca_base_param_lookup_source(int index, mca_base_param_source_t *source, const char **source_file)
{
    mca_base_var_source_t var_source;
    int ret;

    ret = mca_base_var_get_value (index, NULL, &var_source, source_file);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    if (NULL != source) {
        switch (var_source) {
        case MCA_BASE_VAR_SOURCE_ENV:
        case MCA_BASE_VAR_SOURCE_COMMAND_LINE:
            *source = MCA_BASE_PARAM_SOURCE_ENV;
            break;
        case MCA_BASE_VAR_SOURCE_FILE:
        case MCA_BASE_VAR_SOURCE_OVERRIDE:
            *source = MCA_BASE_PARAM_SOURCE_FILE;
            break;
        case MCA_BASE_VAR_SOURCE_SET:
            *source = MCA_BASE_PARAM_SOURCE_OVERRIDE;
            break;
        case MCA_BASE_VAR_SOURCE_DEFAULT:
            *source = MCA_BASE_PARAM_SOURCE_DEFAULT;
            break;
        case MCA_BASE_VAR_SOURCE_MAX:
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}

/*
 * Unset a parameter
 */
int mca_base_param_unset(int index)
{
    /* It is possible to support the semantics of unset by:
     *  1) When registering the parameter, save the default.
     *  2) On calling unset, lookup the default (can only be done for
     *     parameters that use the old system).
     *  3) Deregister the parameter
     *  4) Set the default
     *  5) Register the parameter.
     *
     * The mca_base_var system will ensure the parameter keeps the
     * same index and will do the lookup (env, file, default) again.
     */
    return OPAL_ERR_NOT_SUPPORTED;
}


char *mca_base_param_env_var(const char *param_name)
{
    char *var_name;
    int ret;

    ret = mca_base_var_env_name (param_name, &var_name);
    if (OPAL_SUCCESS != ret) {
        return NULL;
    }

    return var_name;
}


/*
 * Find the index for an MCA parameter based on its names.
 */
int mca_base_param_find(const char *type_name, const char *component_name, 
                        const char *param_name) 
{
    return mca_base_var_find (NULL, type_name, component_name, param_name);
}

int mca_base_param_set_internal (int index, bool internal)
{
    return mca_base_var_set_flag (index, MCA_BASE_VAR_FLAG_INTERNAL, internal);
}

/*
 * Return a list of info of all currently registered parameters
 */
int mca_base_param_dump(opal_list_t **info, bool internal)
{
    mca_base_param_info_t *p, *q;
    size_t i, j, len;
    int *synonyms;
    int ret;

    /* Check for bozo cases */
    
    if (!initialized || NULL == info) {
        return OPAL_ERROR;
    }

    *info = OBJ_NEW(opal_list_t);
    if (NULL == *info) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Iterate through all the registered parameters */

    len = mca_base_var_get_count ();

    for (i = 0; i < len; ++i) {
        const mca_base_var_t *var, *syn;

        ret = mca_base_var_get (i, &var);
        if (OPAL_SUCCESS != ret) {
            continue;
        }

        /* Dump this variable only if it is not a synonym and either it
           is not internal or internal variables were requested */
        if ((internal || !(var->mbv_flags & MCA_BASE_VAR_FLAG_INTERNAL)) &&
            0 > var->mbv_synonym_for) {
            const mca_base_var_group_t *group;

            ret = mca_base_var_group_get (var->mbv_group_index, &group);
            if (OPAL_SUCCESS != ret) {
                continue;
            }

            p = OBJ_NEW(mca_base_param_info_t);
            if (NULL == p) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }

            p->mbpp_index = i;
            p->mbpp_type_name = group->group_framework;
            p->mbpp_component_name = group->group_component;
            p->mbpp_param_name = var->mbv_variable_name;
            p->mbpp_full_name = var->mbv_full_name;
            p->mbpp_deprecated = !!(var->mbv_flags & MCA_BASE_VAR_FLAG_DEPRECATED);
            p->mbpp_internal = !!(var->mbv_flags & MCA_BASE_VAR_FLAG_INTERNAL);
            p->mbpp_read_only = !!(var->mbv_flags & MCA_BASE_VAR_FLAG_DEFAULT_ONLY);
            if (var->mbv_type == MCA_BASE_VAR_TYPE_INT ||
                var->mbv_type == MCA_BASE_VAR_TYPE_UNSIGNED_INT ||
                var->mbv_type == MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG ||
                var->mbv_type == MCA_BASE_VAR_TYPE_SIZE_T ||
                var->mbv_type == MCA_BASE_VAR_TYPE_BOOL) {
                p->mbpp_type = MCA_BASE_PARAM_TYPE_INT;
            } else {
                p->mbpp_type = MCA_BASE_PARAM_TYPE_STRING;
            }
            p->mbpp_help_msg = var->mbv_description;

            /* Save this entry to the list */
            opal_list_append(*info, &p->super);

            p->mbpp_synonyms_len = opal_value_array_get_size ((opal_value_array_t *) &var->mbv_synonyms);

            if (p->mbpp_synonyms_len) {
                p->mbpp_synonyms = calloc(p->mbpp_synonyms_len,
                                          sizeof (mca_base_param_info_t *));
                if (NULL == p->mbpp_synonyms) {
                    return OPAL_ERR_OUT_OF_RESOURCE;
                }

                synonyms = OPAL_VALUE_ARRAY_GET_BASE(&var->mbv_synonyms, int);

                for (j = 0 ; j < (size_t) p->mbpp_synonyms_len ; ++j) {
                    ret = mca_base_var_get (synonyms[j], &syn);
                    if (OPAL_SUCCESS != ret) {
                        p->mbpp_synonyms[j] = NULL;
                        continue;
                    }

                    ret = mca_base_var_group_get (syn->mbv_group_index, &group);
                    if (OPAL_SUCCESS != ret) {
                        continue;
                    }

                    q = OBJ_NEW(mca_base_param_info_t);
                    if (NULL == q) {
                        p->mbpp_synonyms_len = j;
                        return OPAL_ERR_OUT_OF_RESOURCE;
                    }

                    q->mbpp_index = (int) i;
                    q->mbpp_type_name = group->group_framework;
                    q->mbpp_component_name = group->group_component;
                    q->mbpp_param_name = syn->mbv_variable_name;
                    q->mbpp_full_name = syn->mbv_full_name;
                    q->mbpp_deprecated = !!(syn->mbv_flags & MCA_BASE_VAR_FLAG_DEPRECATED);
                    q->mbpp_internal = !!(syn->mbv_flags & MCA_BASE_VAR_FLAG_INTERNAL);
                    q->mbpp_read_only = !!(syn->mbv_flags & MCA_BASE_VAR_FLAG_DEFAULT_ONLY);
                    q->mbpp_type = syn->mbv_type;
                    q->mbpp_help_msg = syn->mbv_description;

                    /* Let this one point to the original */
                    q->mbpp_synonym_parent = p;

                    /* Let the original point to this one */
                    p->mbpp_synonyms[j] = q;

                    /* Save this entry to the list */
                    opal_list_append(*info, &q->super);
                }
            }
        }
    }

    /* All done */

    return OPAL_SUCCESS;
}


/*
 * Make an argv-style list of strings suitable for an environment
 */
int mca_base_param_build_env(char ***env, int *num_env, bool internal)
{
    return mca_base_var_build_env (env, num_env, internal);
}


/*
 * Free a list -- and all associated memory -- that was previously
 * returned from mca_base_param_dump()
 */
int mca_base_param_dump_release(opal_list_t *info)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first(info))) {
        OBJ_RELEASE(item);
    }

    OBJ_RELEASE(info);

    return OPAL_SUCCESS;
}


/*
 * Shut down the MCA parameter system (normally only invoked by the
 * MCA framework itself).
 */
int mca_base_param_finalize(void)
{
    mca_base_param_t *array;
    size_t size, i;
    int ret;

    if (initialized) {
        ret = mca_base_var_finalize ();
        if (OPAL_SUCCESS != ret) {
            return ret;
        }

        /* This is slow, but effective :-) */

        size = opal_value_array_get_size(&mca_base_params);
        array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
        for (i = 0 ; i < size ; ++i) {
            OBJ_DESTRUCT(&array[i]);
        }
        OBJ_DESTRUCT(&mca_base_params);

        initialized = false;
    }

    /* All done */

    return OPAL_SUCCESS;
}

/*
 * Create an empty param container
 */
static void param_constructor(mca_base_param_t *p)
{
    memset ((char *) p + sizeof (p->super), 0, sizeof (*p) - sizeof (p->super));
}


/*
 * Free all the contents of a param container
 */
static void param_destructor(mca_base_param_t *p)
{
    if (NULL != p->param_value) {
        free (p->param_value);
    }

#if OPAL_ENABLE_DEBUG
    /* Cheap trick to reset everything to NULL */
    param_constructor(p);
#endif
}

static void info_constructor(mca_base_param_info_t *p)
{
    p->mbpp_index = -1;
    p->mbpp_type = MCA_BASE_PARAM_TYPE_MAX;

    p->mbpp_type_name = NULL;
    p->mbpp_component_name = NULL;
    p->mbpp_param_name = NULL;
    p->mbpp_full_name = NULL;

    p->mbpp_deprecated = false;

    p->mbpp_synonyms = NULL;
    p->mbpp_synonyms_len = 0;
    p->mbpp_synonym_parent = NULL;

    p->mbpp_help_msg = NULL;
}

static void info_destructor(mca_base_param_info_t *p)
{
    if (NULL != p->mbpp_synonyms) {
        free(p->mbpp_synonyms);
    }
    /* No need to free any of the strings -- the pointers were copied
       by value from their corresponding parameter registration */

    info_constructor(p);
}

int mca_base_param_find_int(const mca_base_component_t *component,
                            const char *param_name,
                            char **env,
                            int *current_value)
{
    char *tmp, *ptr;
    int len, i;
    int rc=OPAL_ERR_NOT_FOUND;
    
    if (NULL == env) {
        return OPAL_ERR_NOT_FOUND;
    }
    
    asprintf(&tmp, "%s%s_%s_%s", mca_prefix, component->mca_type_name,
             component->mca_component_name, param_name);
    len = strlen(tmp);
    for (i=0; NULL != env[i]; i++) {
        if (0 == strncmp(tmp, env[i], len)) {
            ptr = strchr(env[i], '=');
            ptr++;
            *current_value = strtol(ptr, NULL, 10);
            rc = OPAL_SUCCESS;
            break;
        }
    }
    free(tmp);
    return rc;
}

int mca_base_param_find_int_name(const char *type,
                                 const char *param_name,
                                 char **env,
                                 int *current_value)
{
    char *tmp, *ptr;
    int len, i;
    int rc=OPAL_ERR_NOT_FOUND;
    
    if (NULL == env) {
        return OPAL_ERR_NOT_FOUND;
    }
    
    asprintf(&tmp, "%s%s_%s", mca_prefix, type, param_name);
    len = strlen(tmp);
    for (i=0; NULL != env[i]; i++) {
        if (0 == strncmp(tmp, env[i], len)) {
            ptr = strchr(env[i], '=');
            ptr++;
            *current_value = strtol(ptr, NULL, 10);
            rc = OPAL_SUCCESS;
            break;
        }
    }
    free(tmp);
    return rc;
}

int mca_base_param_find_string(const mca_base_component_t *component,
                               const char *param_name,
                               char **env,
                               char **current_value)
{
    char *tmp, *ptr;
    int len, i;
    int rc=OPAL_ERR_NOT_FOUND;
    
    if (NULL == env) {
        return OPAL_ERR_NOT_FOUND;
    }
    
    asprintf(&tmp, "%s%s_%s_%s", mca_prefix, component->mca_type_name,
             component->mca_component_name, param_name);
    len = strlen(tmp);
    for (i=0; NULL != env[i]; i++) {
        if (0 == strncmp(tmp, env[i], len)) {
            ptr = strchr(env[i], '=');
            ptr++;
            *current_value = ptr;
            rc = OPAL_SUCCESS;
            break;
        }
    }
    free(tmp);
    return rc;
}

int mca_base_param_find_string_name(const char *type,
                                    const char *param_name,
                                    char **env,
                                    char **current_value)
{
    char *tmp, *ptr;
    int len, i;
    int rc=OPAL_ERR_NOT_FOUND;
    
    if (NULL == env) {
        return OPAL_ERR_NOT_FOUND;
    }
    
    asprintf(&tmp, "%s%s_%s", mca_prefix, type, param_name);
    len = strlen(tmp);
    for (i=0; NULL != env[i]; i++) {
        if (0 == strncmp(tmp, env[i], len)) {
            ptr = strchr(env[i], '=');
            ptr++;
            *current_value = ptr;
            rc = OPAL_SUCCESS;
            break;
        }
    }
    free(tmp);
    return rc;
}

int mca_base_param_check_exclusive_string(const char *type_a,
                                          const char *component_a,
                                          const char *param_a,
                                          const char *type_b,
                                          const char *component_b,
                                          const char *param_b)
{
    return mca_base_var_check_exclusive (NULL, type_a, component_a,
                                         param_a, type_b, component_b,
                                         param_b);
}
