/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2019-2021 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include <string.h>
#include <errno.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <limits.h>
#include <ctype.h>
#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#include <assert.h>

#include "opal/runtime/opal.h"
#include "opal/util/argv.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/util/info.h"

#include "ompi/info/info.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"
#include "ompi/runtime/ompi_rte.h"
#include "ompi/instance/instance.h"

/*
 * Global variables
 */
ompi_predefined_info_t ompi_mpi_info_null = {{{{{0}}}}};
ompi_predefined_info_t *ompi_mpi_info_null_addr = &ompi_mpi_info_null;
ompi_predefined_info_t ompi_mpi_info_env = {{{{{0}}}}};

/*
 * Local functions
 */
static void info_constructor(ompi_info_t *info);
static void info_destructor(ompi_info_t *info);

/*
 * ompi_info_t classes
 */
OBJ_CLASS_INSTANCE(ompi_info_t,
                   opal_info_t,
                   info_constructor,
                   info_destructor);

/*
 * The global fortran <-> C translation table
 */
opal_pointer_array_t ompi_info_f_to_c_table = {{0}};

/*
 * This function is called during ompi_init and initializes the
 * fortran to C translation table. It also fills in the values
 * for the MPI_INFO_GET_ENV object
 */

int ompi_mpiinfo_init(void)
{

    /* initialize table */

    OBJ_CONSTRUCT(&ompi_info_f_to_c_table, opal_pointer_array_t);
    if( OPAL_SUCCESS != opal_pointer_array_init(&ompi_info_f_to_c_table, 0,
                                                OMPI_FORTRAN_HANDLE_MAX, 16) ) {
        return OMPI_ERROR;
    }

    /* Create MPI_INFO_NULL */
    OBJ_CONSTRUCT(&ompi_mpi_info_null.info, ompi_info_t);
    assert(ompi_mpi_info_null.info.i_f_to_c_index == 0);

    /* Create MPI_INFO_ENV  - we create here for the f_to_c.  Can't fill in 
       here because most info needed is only available after a call to
       ompi_rte_init. */
    OBJ_CONSTRUCT(&ompi_mpi_info_env.info, ompi_info_t);
    assert(ompi_mpi_info_env.info.i_f_to_c_index == 1);

    ompi_mpi_instance_append_finalize (ompi_mpiinfo_finalize);

    /* All done */

    return OMPI_SUCCESS;
}

/*
 * Fill in the MPI_INFO_ENV if using MPI3 initialization
 */
int ompi_mpiinfo_init_mpi3(void)
{
    char *cptr, **tmp;

    /* fill the env info object */

    /* command for this app_context */
    if (NULL != ompi_process_info.command) {
        tmp = opal_argv_split(ompi_process_info.command, ' ');
        opal_info_set(&ompi_mpi_info_env.info.super, "command", tmp[0]);

        /* space-separated list of argv for this command */
        if (1 < opal_argv_count(tmp)) {
            cptr = opal_argv_join(&tmp[1], ' ');
        } else {
            cptr = strdup(tmp[0]);
        }
        opal_argv_free(tmp);
        opal_info_set(&ompi_mpi_info_env.info.super, "argv", cptr);
        free(cptr);
    }

    /* max procs for the entire job */
    opal_asprintf(&cptr, "%u", ompi_process_info.num_procs);
    opal_info_set(&ompi_mpi_info_env.info.super, "maxprocs", cptr);
    /* Open MPI does not support the "soft" option, so set it to maxprocs */
    opal_info_set(&ompi_mpi_info_env.info.super, "soft", cptr);
    free(cptr);

    /* the initial error handler, set it as requested (nothing if not
     * requested) */
    if (NULL != ompi_process_info.initial_errhandler) {
        opal_info_set(&ompi_mpi_info_env.info.super, "mpi_initial_errhandler", ompi_process_info.initial_errhandler);
    }

    /* local host name */
    opal_info_set(&ompi_mpi_info_env.info.super, "host", ompi_process_info.nodename);

#ifdef HAVE_SYS_UTSNAME_H
    {
        struct utsname sysname;
        uname(&sysname);
        cptr = sysname.machine;
        opal_info_set(&ompi_mpi_info_env.info.super, "arch", cptr);
    }
#endif

    /* initial working dir of this process, if provided */
    if (NULL != ompi_process_info.initial_wdir) {
        opal_info_set(&ompi_mpi_info_env.info.super, "wdir", ompi_process_info.initial_wdir);
    }

    /* provide the REQUESTED thread level - may be different
     * than the ACTUAL thread level you get.
     * ugly, but have to do a switch to find the string representation */
    switch (ompi_mpi_thread_requested) {
    case MPI_THREAD_SINGLE:
        opal_info_set(&ompi_mpi_info_env.info.super, "thread_level", "MPI_THREAD_SINGLE");
        break;
    case MPI_THREAD_FUNNELED:
        opal_info_set(&ompi_mpi_info_env.info.super, "thread_level", "MPI_THREAD_FUNNELED");
        break;
    case MPI_THREAD_SERIALIZED:
        opal_info_set(&ompi_mpi_info_env.info.super, "thread_level", "MPI_THREAD_SERIALIZED");
        break;
    case MPI_THREAD_MULTIPLE:
        opal_info_set(&ompi_mpi_info_env.info.super, "thread_level", "MPI_THREAD_MULTIPLE");
        break;
    default:
        /* do nothing - don't know the value */
        break;
    }

    /**** now some OMPI-specific values that other MPIs may not provide ****/

    /* the number of app_contexts in this job */
    opal_asprintf(&cptr, "%u", ompi_process_info.num_apps);
    opal_info_set(&ompi_mpi_info_env.info.super, "ompi_num_apps", cptr);
    free(cptr);

    /* space-separated list of first MPI rank of each app_context */
    if (NULL != ompi_process_info.app_ldrs) {
        opal_info_set(&ompi_mpi_info_env.info.super, "ompi_first_rank", ompi_process_info.app_ldrs);
    }

    /* space-separated list of num procs for each app_context */
    if (NULL != ompi_process_info.app_sizes) {
        opal_info_set(&ompi_mpi_info_env.info.super, "ompi_np", ompi_process_info.app_sizes);
    }

    /* location of the directory containing any prepositioned files
     * the user may have requested
     */
    if (NULL != ompi_process_info.proc_session_dir) {
        opal_info_set(&ompi_mpi_info_env.info.super, "ompi_positioned_file_dir", ompi_process_info.proc_session_dir);
    }

    /* All done */

    return OMPI_SUCCESS;
}

// Generally ompi_info_t processing is handled by opal_info_t now.
// But to avoid compiler warnings and to avoid having to constantly
// change code to mpiinfo->super to make MPI code use the opal_info_t
// it's convenient to have ompi_info_t wrappers for some of the opal_info_t
// related calls:

int ompi_info_dup (ompi_info_t *info, ompi_info_t **newinfo) {
    return opal_info_dup (&(info->super), (opal_info_t **)newinfo);
}
int ompi_info_set (ompi_info_t *info, const char *key, const char *value) {
    return opal_info_set (&(info->super), key, value);
}
int ompi_info_set_value_enum (ompi_info_t *info, const char *key, int value,
                              mca_base_var_enum_t *var_enum)
{
    return opal_info_set_value_enum (&(info->super), key, value, var_enum);
}
int ompi_info_get (ompi_info_t *info, const char *key,
                   opal_cstring_t **value, int *flag)
{
    return opal_info_get (&(info->super), key, value, flag);
}
int ompi_info_get_value_enum (ompi_info_t *info, const char *key, int *value,
                              int default_value, mca_base_var_enum_t *var_enum,
                              int *flag)
{
    return opal_info_get_value_enum (&(info->super), key, value,
                              default_value, var_enum, flag);
}
int ompi_info_get_bool(ompi_info_t *info, const char *key, bool *value, int *flag) {
    return opal_info_get_bool(&(info->super), key, value, flag);
}
int ompi_info_delete (ompi_info_t *info, const char *key) {
    return opal_info_delete (&(info->super), key);
}
int ompi_info_get_valuelen (ompi_info_t *info, const char *key, int *valuelen,
                            int *flag)
{
    return opal_info_get_valuelen (&(info->super), key, valuelen, flag);
}
int ompi_info_get_nthkey (ompi_info_t *info, int n, opal_cstring_t **key) {
    return opal_info_get_nthkey (&(info->super), n, key);
}
int ompi_info_get_nkeys(ompi_info_t *info, int *nkeys)
{
    return opal_info_get_nkeys (&(info->super), nkeys);
}


/*
 * Shut down MPI_Info handling
 */
int ompi_mpiinfo_finalize(void)
{
    size_t i, max;
    ompi_info_t *info;
    opal_list_item_t *item;
    opal_info_entry_t *entry;
    bool found = false;

    OBJ_DESTRUCT(&ompi_mpi_info_null);
    OBJ_DESTRUCT(&ompi_mpi_info_env);

    /* Go through the f2c table and see if anything is left.  Free them
       all. */

    max = opal_pointer_array_get_size(&ompi_info_f_to_c_table);
    for (i = 2; i < max; ++i) {
        info = (ompi_info_t *)opal_pointer_array_get_item(&ompi_info_f_to_c_table, i);

        /* If the info was freed but still exists because the user
           told us to never free handles, then do an OBJ_RELEASE it
           and all is well.  Then get the value again and see if it's
           actually been freed. */

        if (NULL != info && ompi_debug_no_free_handles && info->i_freed) {
            OBJ_RELEASE(info);
            info = (ompi_info_t *)opal_pointer_array_get_item(&ompi_info_f_to_c_table, i);
        }

        /* If it still exists here and was never freed, then it's an
           orphan */

        if (NULL != info) {

            /* If the user wanted warnings about MPI object leaks, print out
               a message */

            if (!info->i_freed && ompi_debug_show_handle_leaks) {
                if (ompi_debug_show_handle_leaks) {
                    opal_output(0, "WARNING: MPI_Info still allocated at MPI_FINALIZE");

                    for (item = opal_list_get_first(&info->super.super);
                         opal_list_get_end(&(info->super.super)) != item;
                         item = opal_list_get_next(item)) {
                        entry = (opal_info_entry_t *) item;
                        opal_output(0, "WARNING:   key=\"%s\", value=\"%s\"",
                                    entry->ie_key->string,
                                    NULL != entry->ie_value ? entry->ie_value->string : "(null)");
                        found = true;
                    }
                }
                OBJ_RELEASE(info);
            }

            /* Don't bother setting each element back down to NULL; it
               would just take a lot of thread locks / unlocks and
               since we're destroying everything, it isn't worth it */

            if (!found && ompi_debug_show_handle_leaks) {
                opal_output(0, "WARNING:   (no keys)");
            }
        }
    }

    /* All done -- destroy the table */

    OBJ_DESTRUCT(&ompi_info_f_to_c_table);
    return OPAL_SUCCESS;
}



/*
 * This function is invoked when OBJ_NEW() is called. Here, we add this
 * info pointer to the table and then store its index as the handle
 */
static void info_constructor(ompi_info_t *info)
{
    info->i_f_to_c_index = opal_pointer_array_add(&ompi_info_f_to_c_table,
                                                  info);
    info->i_freed = false;

/*
 * If the user doesn't want us to ever free it, then add an extra
 * RETAIN here
 */
    if (ompi_debug_no_free_handles) {
        OBJ_RETAIN(&(info->super));
    }
}

/*
 *  * This function is called during OBJ_DESTRUCT of "info". When this
 *   * done, we need to remove the entry from the opal fortran to C
 *    * translation table
 *     */
static void info_destructor(ompi_info_t *info)
{
   /* reset the &ompi_info_f_to_c_table entry - make sure that the
      entry is in the table */

    if (MPI_UNDEFINED != info->i_f_to_c_index &&
        NULL != opal_pointer_array_get_item(&ompi_info_f_to_c_table,
                                            info->i_f_to_c_index)){
        opal_pointer_array_set_item(&ompi_info_f_to_c_table,
                                    info->i_f_to_c_index, NULL);
    }

}

ompi_info_t *ompi_info_allocate (void)
{
    ompi_info_t *new_info;
    int rc;

    rc = ompi_mpi_instance_retain ();
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        /* NTH: seriously, what can we do other than abort () or return? we failed to
         * set up the most basic infrastructure! */
        return NULL;
    }

    /*
     * Call the object create function. This function not only
     * allocates the space for MPI_Info, but also calls all the
     * relevant init functions. Should I check if the fortran
     * handle is valid
     */
    new_info = OBJ_NEW(ompi_info_t);
    if (NULL == new_info) {
        return NULL;
    }

    return new_info;
}

/*
 * Free an info handle and all of its keys and values.
 */
int ompi_info_free (ompi_info_t **info)
{
    (*info)->i_freed = true;
    OBJ_RELEASE(*info);
    *info = MPI_INFO_NULL;

    /* release the retain() from info create/dup */
    ompi_mpi_instance_release ();

    return MPI_SUCCESS;
}
