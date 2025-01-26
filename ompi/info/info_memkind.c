/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include "info/info_memkind.h"
#include "opal/util/argv.h"
#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/util/printf.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/constants.h"

static opal_mutex_t ompi_info_memkind_mutex = OPAL_MUTEX_STATIC_INIT;
static ompi_memkind_t *ompi_info_memkind_available;
static int ompi_info_memkind_num_available = 0;

#define FREE_STR_ARR(_str_arr) { \
    int _i=0;                    \
    char *_t = _str_arr[_i];     \
    while (NULL != _t) {         \
        free (_t);               \
        _t = _str_arr[++_i];     \
    }                            \
    free (_str_arr);             \
}

static int ompi_info_memkind_get_num_unique (char **memkind_strs, int num_max)
{
    int iter = 0, pos = 0;
    int num_unique = 0;
    char **tmp_arr;

    tmp_arr = (char**) malloc ( (num_max + 1) * sizeof(char*));

    char *m = memkind_strs[iter];
    while (m != NULL) {
        char **tmp_str = opal_argv_split (m, ':');
        tmp_arr[pos++] = strdup (tmp_str[0]);

        FREE_STR_ARR(tmp_str);
        m = memkind_strs[++iter];
    }
    tmp_arr[pos] = NULL;

    iter = 0;
    m = tmp_arr[iter];
    while (NULL != m) {
        bool already_seen = false;

        for (int i = 0; i < iter; i++) {
            if (!strncmp(m, tmp_arr[i], strlen(m))) {
                already_seen = true;
                break;
            }
        }
        if (!already_seen)  {
            num_unique++;
        }
        m = tmp_arr[++iter];
    }
    FREE_STR_ARR(tmp_arr);

    return num_unique;
}

#if 0
static void ompi_info_memkind_dump (const char *var_name, int num_memkinds, ompi_memkind_t *memkinds)
{
    for (int i = 0; i < num_memkinds; i++) {
        printf("[%d] %s memkind[%d].name: %s ", getpid(), var_name, i, memkinds[i].im_name);
        if (memkinds[i].im_num_restrictors > 0) {
            printf("restrictors: ");
            for (int j = 0; j < memkinds[i].im_num_restrictors; j++) {
                printf("%c %s", (j == 0 ? ' ': ','), memkinds[i].im_restrictors[j]);
            }
        } else {
            printf("no restrictors.");
        }
        printf("\n");
    }
}
#endif

static void ompi_info_memkind_extract (const char* memkind_str, int *num_memkinds, ompi_memkind_t **memkinds)
{
    /* The memkind string is a comma-separated list of memkinds, which can have two forms:
    **   - standalone memkind type, which implies that all restrictors of the memkind are requested
    **     (or looking at it the other way around, no restrictions are imposed on that memory kind)
    **   - memkind:restrictor
    ** The same memkind type can appear multiple times, e.g.
    **    memkind_a:restrictor_1,memkind_a:restrictor_2;
    */

    /* Separate requested_str into an array of individual entries */
    char **memkind_combos = opal_argv_split(memkind_str, ',');
    int max_num_memkinds = opal_argv_count(memkind_combos);
    int num_unique_memkinds = ompi_info_memkind_get_num_unique (memkind_combos, max_num_memkinds);

    ompi_memkind_t *memkind_arr = NULL;
    memkind_arr = (ompi_memkind_t *) malloc(num_unique_memkinds * sizeof(ompi_memkind_t));
    if (NULL == memkind_arr) {
        goto err_exit;
    }
    for (int i = 0; i < num_unique_memkinds; i++) {
        memkind_arr[i].im_num_restrictors = 0;
    }

    int iter = 0;
    char *m = memkind_combos[iter];
    int pos = 0, current_max = 0;
    while (m != NULL) {
        bool already_seen = false;
        char **tmp_str = opal_argv_split (m, ':');

        for (int i = 0; i < current_max; i++) {
            if (!strncmp(tmp_str[0], memkind_arr[i].im_name, strlen(tmp_str[0]))) {
                already_seen = true;
                pos = i;
                break;
            }
        }

        if (!already_seen)  {
            pos = current_max;
            memkind_arr[pos].im_name = strdup (tmp_str[0]);
            current_max++;
        }
        if (NULL != tmp_str[1]) {
            memkind_arr[pos].im_restrictors[memkind_arr[pos].im_num_restrictors++] = strdup(tmp_str[1]);
        } else {
            memkind_arr[pos].im_num_restrictors = 0;
        }
        FREE_STR_ARR(tmp_str);
        m = memkind_combos[++iter];
    }

 err_exit:
    *num_memkinds = num_unique_memkinds;
    *memkinds = memkind_arr;

    return;
}

static int ompi_info_memkind_get_available(int *num_memkinds, ompi_memkind_t **memkinds)
{
    int ret = OMPI_SUCCESS;
    if (ompi_info_memkind_num_available > 0) {
        goto exit_no_lock;
    }

    OPAL_THREAD_LOCK (&ompi_info_memkind_mutex);
    if (ompi_info_memkind_num_available > 0) {
        goto exit;
    }

    int tmp_num = 2;
    if (0 != strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null")) {
        tmp_num++;
    }

    ompi_info_memkind_available = (ompi_memkind_t *) malloc (tmp_num * sizeof(ompi_memkind_t));
    if (NULL == ompi_info_memkind_available) {
        *num_memkinds = 0;
        *memkinds = NULL;
        OPAL_THREAD_UNLOCK(&ompi_info_memkind_mutex);
        return OMPI_ERROR;
    }

    /* The system and mpi memory kinds are defined in MPI 4.1 section 12.4.3 */
    ompi_info_memkind_available[0].im_name = strdup ("system");
    ompi_info_memkind_available[0].im_num_restrictors = 0;

    ompi_info_memkind_available[1].im_name = strdup ("mpi");
    ompi_info_memkind_available[1].im_num_restrictors = 3;
    ompi_info_memkind_available[1].im_restrictors[0] = strdup ("alloc_mem");
    ompi_info_memkind_available[1].im_restrictors[1] = strdup ("win_allocate");
    ompi_info_memkind_available[1].im_restrictors[2] = strdup ("win_allocate_shared");

    if (tmp_num > 2) {
        ompi_info_memkind_available[2].im_num_restrictors = OMPI_MAX_NUM_MEMKIND_RESTRICTORS;
        opal_accelerator.get_memkind (&(ompi_info_memkind_available[2].im_name),
                                      &(ompi_info_memkind_available[2].im_num_restrictors),
                                      (char**)ompi_info_memkind_available[2].im_restrictors);
    }
    ompi_info_memkind_num_available = tmp_num;

 exit:
    OPAL_THREAD_UNLOCK(&ompi_info_memkind_mutex);
 exit_no_lock:
    *num_memkinds = ompi_info_memkind_num_available;
    *memkinds = ompi_info_memkind_available;
    return ret;
}

static void ompi_info_memkind_free (int num, ompi_memkind_t *memkind_arr)
{
    for (int i = 0; i < num; i++) {
        free (memkind_arr[i].im_name);
        for (int j = 0; j < memkind_arr[i].im_num_restrictors; j++) {
            free (memkind_arr[i].im_restrictors[j]);
        }
    }
    free (memkind_arr);
}

static void ompi_info_memkind_str_create (int num_memkinds, ompi_memkind_t *memkinds, char** memkind_str)
{
    int num_elems = 0;

    for (int i = 0; i < num_memkinds; i++) {
        if (memkinds[i].im_num_restrictors == 0) {
            num_elems++;
        } else {
            num_elems += memkinds[i].im_num_restrictors;
        }
    }

    char **tmp_str_arr = (char**) malloc ((num_elems+1) * sizeof (char**));
    if (NULL == tmp_str_arr) {
        *memkind_str = NULL;
        return;
    }

    int c = 0;
    for (int i = 0; i < num_memkinds; i++) {
        if (memkinds[i].im_num_restrictors == 0) {
            opal_asprintf(&tmp_str_arr[c++], "%s",memkinds[i].im_name);
        } else {
            for (int j = 0; j < memkinds[i].im_num_restrictors; j++) {
                opal_asprintf(&tmp_str_arr[c++], "%s:%s",memkinds[i].im_name,
                              memkinds[i].im_restrictors[j]);
            }
        }
    }
    tmp_str_arr[num_elems] = NULL;

    char *tmp_str = opal_argv_join(tmp_str_arr, ',');
    FREE_STR_ARR (tmp_str_arr);

    *memkind_str = tmp_str;
    return;
}

#define COPY_MEMKIND(_to,_from) {                                   \
    _to.im_name = strdup(_from.im_name);                            \
    _to.im_num_restrictors = _from.im_num_restrictors;              \
    for (int _i = 0; _i < _from.im_num_restrictors; _i++) {         \
        _to.im_restrictors[_i] = strdup (_from.im_restrictors[_i]); \
    }                                                               \
}

static int ompi_info_memkind_remove_unsupported (int num_requested, ompi_memkind_t *requested_memkinds,
                                                 int num_available, ompi_memkind_t *available_memkinds,
                                                 int *num_provided, ompi_memkind_t **provided_memkinds)
{
    bool have_system_memkind = false;
    bool have_mpi_memkind = false;
    int pos = 0;
    int *apos = malloc (num_requested *sizeof(int));
    if (NULL == apos) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /*
    ** Check whether we support the memkinds requested by the user
    ** In addition, keep track whether user requested "system" and "mpi"
    ** memory_alloc_kinds, since we always add those to the list
    ** of support memory_alloc_kinds
    */
    for (int i = 0; i < num_requested; i++) {
        bool found_name = false;
        bool found_all_requested_restrictors = true;
        int j = -1;

        if (!have_system_memkind && !strncmp(requested_memkinds[i].im_name, "system", strlen("system"))) {
            have_system_memkind = true;
        }
        if (!have_mpi_memkind && !strncmp(requested_memkinds[i].im_name, "mpi", strlen("mpi"))) {
            have_mpi_memkind = true;
        }

        // Check for memory_alloc_kind name first
        for (j = 0; j < num_available; j++) {
            if (!strncmp(requested_memkinds[i].im_name, available_memkinds[j].im_name,
                         strlen(requested_memkinds[i].im_name))) {
                found_name = true;
                break;
            }
        }
        if (found_name) {
            // Check whether we recognize all restrictors requested by user for
            // this memory_alloc_kind
            bool found_this_restrictor = false;
            for (int k = 0; k < requested_memkinds[i].im_num_restrictors; k++) {
                for (int l = 0; l < available_memkinds[j].im_num_restrictors; l++) {
                    if (!strncmp(requested_memkinds[i].im_restrictors[k], available_memkinds[j].im_restrictors[l],
                                 strlen(requested_memkinds[i].im_restrictors[k]))) {
                        found_this_restrictor = true;
                        break;
                    }
                }
                if (!found_this_restrictor) {
                    found_all_requested_restrictors = false;
                    break;
                }
            }
            if (found_all_requested_restrictors) {
                apos[pos++] = i;
            }
        }
    }

    // Add "system" and "mpi" memkinds as supported, even if not requested by user
    int total_len = pos;
    if (!have_system_memkind) {
        total_len++;
    }
    if (!have_mpi_memkind) {
        total_len++;
    }

    ompi_memkind_t *final = (ompi_memkind_t*) malloc (total_len * sizeof(ompi_memkind_t));
    if (NULL == final) {
        free (apos);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    int offset = 0;
    // assert (!strncmp(available_memkinds[0].im_name, "system", strlen("system")));
    COPY_MEMKIND(final[0], available_memkinds[0]);
    offset++;

    // assert (!strncmp(available_memkinds[1].im_name, "mpim", strlen("mpi")));
    COPY_MEMKIND(final[offset], available_memkinds[1]);
    offset++;

    for (int i = 0; i < pos; i++) {
        if (!strncmp(requested_memkinds[apos[i]].im_name, "system", strlen("system"))) {
            continue;
        }
        if (!strncmp(requested_memkinds[apos[i]].im_name, "mpi", strlen("mpi"))) {
            continue;
        }
        COPY_MEMKIND (final[offset], requested_memkinds[apos[i]]);
        offset++;
    }

    *num_provided = total_len;
    *provided_memkinds = final;
    return OMPI_SUCCESS;
}

static bool ompi_info_memkind_is_subset (int num_subset, ompi_memkind_t *subset,
                                         int num_superset, ompi_memkind_t *superset)
{
    bool ret = true;

    for (int i = 0; i < num_subset; i++) {
        bool found_name = false;
        int j = -1;

        // Check for memory_alloc_kind name first
        for (j = 0; j < num_superset; j++) {
            if (!strncmp(subset[i].im_name, superset[j].im_name,
                         strlen(subset[i].im_name))) {
                found_name = true;
                break;
            }
        }
        if (found_name) {
            /* Check whether we recognize all restrictors requested listed in
            ** the subset in the superset. Note, that the superset might not
            ** have any restrictors set, in which case all restrictors are accepted
            */
            if (0 == superset[j].im_num_restrictors) {
                continue;
            }
            for (int k = 0; k < subset[i].im_num_restrictors; k++) {
                bool found_this_restrictor = false;
                for (int l = 0; l < superset[j].im_num_restrictors; l++) {
                    if (!strncmp(subset[i].im_restrictors[k], superset[j].im_restrictors[l],
                                 strlen(subset[i].im_restrictors[k]))) {
                        found_this_restrictor = true;
                        break;
                    }
                }
                if (!found_this_restrictor) {
                    ret = false;
                    goto exit;
                }
            }
        } else {
            ret = false;
            goto exit;
        }
    }

 exit:
    return ret;
}

static bool ompi_info_memkind_validate (const char *assert_str, const char *parent_str)
{
    int num_assert_memkinds = 0, num_parent_memkinds = 0;
    ompi_memkind_t *assert_memkinds = NULL;
    ompi_memkind_t *parent_memkinds = NULL;
    bool ret;

    ompi_info_memkind_extract (assert_str, &num_assert_memkinds, &assert_memkinds);
    ompi_info_memkind_extract (parent_str, &num_parent_memkinds, &parent_memkinds);

    ret = ompi_info_memkind_is_subset (num_assert_memkinds, assert_memkinds,
                                       num_parent_memkinds, parent_memkinds);

    if (NULL != assert_memkinds) {
        ompi_info_memkind_free(num_assert_memkinds, assert_memkinds);
    }
    if (NULL != parent_memkinds) {
        ompi_info_memkind_free(num_parent_memkinds, parent_memkinds);
    }

    return ret;
}


int ompi_info_memkind_process (const char* requested_str, char **provided_str)
{
    int err;
    char *tmp_str = NULL;

    int num_requested_memkinds, num_available_memkinds, num_provided_memkinds;
    ompi_memkind_t *requested_memkinds = NULL ;
    ompi_memkind_t *available_memkinds = NULL;
    ompi_memkind_t *provided_memkinds = NULL;

    if (NULL == requested_str) {
        *provided_str = NULL;
        return OMPI_SUCCESS;
    }

    ompi_info_memkind_extract (requested_str, &num_requested_memkinds, &requested_memkinds);
    err = ompi_info_memkind_get_available (&num_available_memkinds, &available_memkinds);
    if (OMPI_SUCCESS != err) {
        goto exit;
    }

    err = ompi_info_memkind_remove_unsupported (num_requested_memkinds, requested_memkinds,
                                                num_available_memkinds, available_memkinds,
                                                &num_provided_memkinds, &provided_memkinds);
    if (OMPI_SUCCESS != err) {
        goto exit;
    }

    ompi_info_memkind_str_create(num_provided_memkinds, provided_memkinds, &tmp_str);

 exit:
    if (NULL != requested_memkinds) {
        ompi_info_memkind_free(num_requested_memkinds, requested_memkinds);
    }
    if (NULL != provided_memkinds) {
        ompi_info_memkind_free(num_provided_memkinds, provided_memkinds);
    }
    // Don't free the available_memkinds, they will be released in info_finalize;
    
    *provided_str = tmp_str;
    return err;
}

const char *ompi_info_memkind_cb (opal_infosubscriber_t *obj, const char *key, const char *value)
{
    char *result;
    ompi_info_memkind_process (value, &result);
    return result;
}

/*
** Algorithm is a bit convoluted:
**
** - retrieve mpi_memory_alloc_kinds from parent object.
** - if info object passed in as argument to this routine contains
**   mpi_assert_memory_alloc_kinds key/value pair:
**     - validate that we recognize all memory kinds listed
**     - if that is the case, use the value of the of mpi_assert_memory_alloc_kinds
**       when setting mpi_memory_alloc_kinds on the child object
**     - else ignore the mpi_assert_memory_alloc_kinds. (Quote:
**       "If the MPI library does not support one or more of the allocation kinds associated
**        with the mpi_assert_memory_alloc_kinds info key, it will ignore this info key".
**       So we are supposed to drop the entire key, not just the memory kinds that we did
**       recognize.)
** - else use the same memkinds as in mpi_memory_alloc_kinds on the parent object on the
**   child object (i.e. we just copy it over)
**
** To summerize, the value of one info key (mpi_assert_memory_alloc_kinds) can influence the
** value of another info key (mpi_memory_alloc_kinds).
*/
int ompi_info_memkind_copy_or_set (opal_infosubscriber_t *parent, opal_infosubscriber_t *child,
                                   opal_info_t *info)
{
    opal_cstring_t *parent_val;
    opal_cstring_t *assert_val;
    char *final_str = NULL;
    int flag;

    opal_info_get(parent->s_info, "mpi_memory_alloc_kinds", &parent_val, &flag);
    if (0 == flag) {
        return OMPI_SUCCESS;
    }
    final_str = (char*) parent_val->string;

    if (NULL != info) {
        opal_info_get(info, "mpi_assert_memory_alloc_kinds", &assert_val, &flag);
        if (0 == flag) {
            // assert_memory_alloc_kinds was not set by code
            goto exit;
        }

        // Validate asserted memory kind
        bool ret = ompi_info_memkind_validate (assert_val->string, parent_val->string);
        if (ret) {
            final_str = (char*) assert_val->string;
        }
    }

 exit:
    opal_infosubscribe_subscribe (child, "mpi_memory_alloc_kinds", final_str,
                                  ompi_info_memkind_cb);
    return OMPI_SUCCESS;
}

void ompi_info_memkind_free_available (void)
{
    ompi_info_memkind_free (ompi_info_memkind_num_available, ompi_info_memkind_available);
}
