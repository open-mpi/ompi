/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024 Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"

#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>

#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/dl/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"

#include "common_ubcl.h"

/**
 * Common UBCL component
 */
mca_opal_common_ubcl_component_t mca_opal_common_ubcl_component = {
    .output = 0,
    .verbose = 0,
    .ld_library_path_fail_warn = true,
    .search_opt_ubcl = true,
    .force_ld_lib_dlopen = false,
    .ubcl_search_path = NULL,

    .is_init = 0,
    .is_registered = 0,
    .is_dlopen = 0,
};
const char *default_search_path = "/opt/ubcl/";

/*
 * Version of the UBCL API we need
 */
ubcl_api_version_t my_api_version = {
    .major = UBCL_API_VERSION_MAJOR,
    .minor = UBCL_API_VERSION_MINOR,
};

/* Handle to libubcl.so */
opal_dl_handle_t *libubcl_handle = NULL;

static int mca_common_ubcl_scandir_filter(const struct dirent *dir)
{
    char* dirname_copy = NULL;
    char* saved_ptr = NULL;
    char* digit_str = NULL;
    char* endptr = NULL;
    unsigned long digit = 0;
    int digit_position = 0;

    /* Filter out '.' and '..' */
    if (0 == strcmp(dir->d_name, ".") || 0 == strcmp(dir->d_name, "..")) {
        return 0;
    }

    /* Only keep directories and unknown */
    if (DT_DIR != dir->d_type && DT_UNKNOWN != dir->d_type) {
        return 0;
    }

    /* Filter out folders that don't look like X.Y.Z */
    dirname_copy = strdup(dir->d_name);
    digit_str = strtok_r(dirname_copy, ".", &saved_ptr);
    while (digit_str != NULL) {
       digit = strtol(digit_str, &endptr, 10);
       if (digit_str == endptr) {
           common_ubcl_log_verbose(95, "DIGIT: '%s' doesn't start by a number\n",
                                   digit_str);
           goto free_and_fail;
       } else if ('\0' != *endptr) {
           common_ubcl_log_verbose(95, "DIGIT: '%s' contains non-number\n",
                                   digit_str);
           goto free_and_fail;
       } else {
           switch (digit_position) {
               case 0:
                   if (digit != my_api_version.major) {
                       common_ubcl_log_verbose(95, "Wrong API_MAJOR version: "
                                               "%lu != %u\n", digit,
                                               my_api_version.major);
                       goto free_and_fail;
                   }
                   break;
               case 1:
                   if (digit < my_api_version.minor) {
                       common_ubcl_log_verbose(95, "Wrong API_MINOR version: "
                                               "%lu < %u\n", digit,
                                               my_api_version.minor);
                       goto free_and_fail;
                   }
                   break;
               case 2:
                   break;
               default:
                   common_ubcl_log_verbose(95, "'%s' has more than 3 digits",
                                           dir->d_name);
                   goto free_and_fail;
           }
       }
       digit_position++;
       digit_str = strtok_r(NULL, ".", &saved_ptr);
    }

    free(dirname_copy);
    return 1;

free_and_fail:
    common_ubcl_log_verbose(95, "Filtering out '%s'", dir->d_name);
    free(dirname_copy);
    return 0;
}

static int mca_common_ubcl_find_ubcl_install(char*** searchpaths)
{
    int nb_dir, i;
    int inv_i;
    struct dirent **verslist;
    const char* ubcl_search_path = *mca_opal_common_ubcl_component.ubcl_search_path;

    nb_dir = scandir(ubcl_search_path, &verslist, mca_common_ubcl_scandir_filter, versionsort);
    if (-1 == nb_dir) {
        common_ubcl_warning("Failed to scan %s, error: %s", ubcl_search_path, strerror(errno));
        return nb_dir;
    }

    /* Allocate two more to append the search path itself and 'NULL' as the last */
    (*searchpaths) = malloc( (2 + nb_dir) * sizeof(char*));
    asprintf((*searchpaths)+nb_dir, "%s/lib", ubcl_search_path);
    (*searchpaths)[nb_dir + 1] = NULL;

    /* Iterate backwards to get higher versions first */
    inv_i = 0;
    for (i = nb_dir - 1; i >= 0; i--) {
        asprintf((*searchpaths)+inv_i, "%s/%s/lib", ubcl_search_path, verslist[i]->d_name);
        free(verslist[i]);
        inv_i++;
    }
    free(verslist);

    return nb_dir;
}

static void mca_common_ubcl_free_found_searchpaths(char*** searchpaths, int nb_dir) {
    int i;

    for (i = 0; i < nb_dir; i++) {
        free((*searchpaths)[i]);
    }
    free((*searchpaths)[nb_dir]);

    free(*searchpaths);
    (*searchpaths) = NULL;
}

static bool mca_common_ubcl_test_lib_version(char* filename) {
    int ret;
    char *err_msg;
    const char *ubcl_api_symbol = "ubcl_api_version";
    ubcl_api_version_t ubcl_lib_api_version;
    void *symbol = NULL;

    ret = opal_dl_lookup(libubcl_handle, ubcl_api_symbol, &symbol, &err_msg);
    if (OPAL_SUCCESS != ret) {
        common_ubcl_warning("Library %s opened but no %s symbols found."
                            " It probably is an older version, skipping.\n",
                            filename, ubcl_api_symbol);
        return OPAL_ERROR;
    }
    ubcl_lib_api_version = *(ubcl_api_version_t*)symbol;

    if (ubcl_lib_api_version.major != my_api_version.major) {
        common_ubcl_warning("Library %s opened but API version major digit"
                            " '%d' isn't the wanted: '%d'. Skipping\n",
                            filename, ubcl_lib_api_version.major, my_api_version.major);
        return OPAL_ERROR;
    }

    if (ubcl_lib_api_version.minor < my_api_version.minor) {
        common_ubcl_warning("Library %s opened but API version minor '%d' "
                            "inferior to the minimum wanted: '%d'. Skipping\n",
                            filename, ubcl_lib_api_version.minor, my_api_version.minor);
        return OPAL_ERROR;
    }

    common_ubcl_log_verbose(20, " Accepting library %s with API version: '%d.%d',"
                            " (wanted: '%d.%d')\n", filename,
                            ubcl_lib_api_version.major, ubcl_lib_api_version.minor,
                            my_api_version.major, my_api_version.minor);
    return OPAL_SUCCESS;
}

static bool mca_common_ubcl_try_dlopen(char** searchpaths, char** ubcllibs, char*** errmsgs) {
    int retval;
    int errsize;
    bool dlopen_success = false;
    int j = 0;

    while (searchpaths[j] != NULL) {
        int i = 0;
        while (ubcllibs[i] != NULL) {
            char *filename = NULL;
            char *str = NULL;

            /* If there's a non-empty search path, prepend it
               to the library filename */
            if (strlen(searchpaths[j]) > 0) {
                asprintf(&filename, "%s/%s", searchpaths[j], ubcllibs[i]);
            } else {
                filename = strdup(ubcllibs[i]);
            }
            if (NULL == filename) {
                opal_show_help("help-mpi-common-ubcl.txt", "No memory",
                               true, OPAL_PROC_MY_HOSTNAME);
                return OPAL_ERR_NOT_AVAILABLE;
            }

            retval = opal_dl_open(filename, false, false,
                                  &libubcl_handle, &str);
            if (OPAL_SUCCESS != retval || NULL == libubcl_handle) {
                if (NULL != str) {
                    opal_argv_append(&errsize, errmsgs, str);
                } else {
                    opal_argv_append(&errsize, errmsgs,
                                     "opal_dl_open() returned NULL.");
                }
                common_ubcl_log_verbose(10, "UBCL: Library open error: %s",
                                    (*errmsgs)[errsize-1]);
            } else {
                if (mca_opal_common_ubcl_component.force_ld_lib_dlopen) {
                    /* Force retval to fake a good version check */
                    retval = OPAL_SUCCESS;
                } else {
                    /* We opened an UBCL library, now we need to check the version */
                    retval = mca_common_ubcl_test_lib_version(filename);
                }

                if (OPAL_SUCCESS != retval) {
                    asprintf(&str, "%s opened but version check failed. Skipping", filename);
                    opal_argv_append(&errsize, errmsgs, str);
                    opal_dl_close(libubcl_handle);
                    libubcl_handle = NULL;
                } else {
                    common_ubcl_log_verbose(10, "UBCL: Library successfully "
                                                "opened %s", filename);
                    dlopen_success = true;
                    free(filename);
                    break;
                }
            }
            i++;

            free(filename);
        }
        if (true == dlopen_success) {
            break; /* Break out of outer loop */
        }
        j++;
    }
    return dlopen_success;
}

static int mca_common_ubcl_dlopen_ubcl(void)
{
    char *ubcllibs[] = { "libubcl.so", "libubcl.so.0", NULL };
    char *searchpaths[] = { "", NULL };
    char **opt_searchpaths = NULL;
    char **errmsgs = NULL;
    char *errmsg = NULL;
    bool dlopen_success = false;
    int nb_dir = 0;

    if (1 < opal_atomic_add_fetch_32(&mca_opal_common_ubcl_component.is_dlopen, 1)) {
        return OPAL_SUCCESS;
    }

    if (!OPAL_HAVE_DL_SUPPORT) {
        opal_show_help("help-mpi-common-ubcl.txt", "dlopen disabled", true);
        return OPAL_ERR_NOT_AVAILABLE;
    }

    common_ubcl_log_verbose(10, "COMMMON_UBCL: Starting to look for UBCL"
                                " library");


    /* Now walk through all the potential names libubcl and find one that
     * works. If it does, all is good. If not, print out all the messages about
     * why things failed. This code was careful to try and save away all error
     * messages if the loading ultimately failed to help with debugging. */


    /* On the first try we just utilize the default loading paths from
     * the system. This is so that LD_LIBRARY_PATH is looked at in priority */
    dlopen_success = mca_common_ubcl_try_dlopen(searchpaths, ubcllibs, &errmsgs);

    if (true == dlopen_success) {
        goto success;
    }

    if (mca_opal_common_ubcl_component.ld_library_path_fail_warn) {
        common_ubcl_warning("We did not find a compatible UBCL in LD_LIBRARY_PATH\n");
    }

    if (mca_opal_common_ubcl_component.force_ld_lib_dlopen) {
        common_ubcl_error("No UBCL found in LD_LIBRARY_PATH and 'force_ld_lib_dlopen'"
                          " set to 'true'. We cannot load UBCL for the PML/UBCL to use\n");
        goto failed;
    }

    if (!mca_opal_common_ubcl_component.search_opt_ubcl) {
        common_ubcl_error("No UBCL found in LD_LIBRARY_PATH and 'search_opt_ubcl'"
                          " set to 'false'. We cannot load UBCL for the PML/UBCL to use\n");
        goto failed;
    }

    nb_dir = mca_common_ubcl_find_ubcl_install(&opt_searchpaths);

    if (-1 == nb_dir) {
        goto failed;
    }

    /* Now look into paths found by 'find_ubcl_install' */
    dlopen_success = mca_common_ubcl_try_dlopen(opt_searchpaths, ubcllibs, &errmsgs);
    mca_common_ubcl_free_found_searchpaths(&opt_searchpaths, nb_dir);

    if (true == dlopen_success) {
        goto success;
    }

failed:
    errmsg = opal_argv_join(errmsgs, '\n');
    opal_show_help("help-mpi-common-ubcl.txt", "dlopen failed", true,
            errmsg);
    opal_argv_free(errmsgs);
    free(errmsg);
    return OPAL_ERR_NOT_AVAILABLE;

success:
    opal_argv_free(errmsgs);
    free(errmsg);
    return OPAL_SUCCESS;
}

void mca_common_ubcl_register_mca(void)
{
    if (1 < opal_atomic_add_fetch_32(&mca_opal_common_ubcl_component.is_registered, 1)) {
        return;
    }
    MCA_REGISTER_COMMON_UBCL("verbose", "Verbosity level of component common/ubcl",
                             MCA_BASE_VAR_TYPE_INT, &mca_opal_common_ubcl_component.verbose);
    MCA_REGISTER_COMMON_UBCL("ld_lib_path_fail_warn",
                             "Warn the user when no fitting libraries were found"
                             " in the default system loading path (LD_LIBRARY_PATH)",
                             MCA_BASE_VAR_TYPE_BOOL, &mca_opal_common_ubcl_component.ld_library_path_fail_warn);
    MCA_REGISTER_COMMON_UBCL("force_ld_lib_dlopen",
                             "Force comon/ubcl to dlopen and use an UBCL library"
                             " found in LD_LIBRARY_PATH, regardless of API version",
                             MCA_BASE_VAR_TYPE_BOOL, &mca_opal_common_ubcl_component.force_ld_lib_dlopen);
    MCA_REGISTER_COMMON_UBCL("search_opt_ubcl",
                             "In case we don't find a suitable UBCL library in "
                             "LD_LIBRARY_PATH, automatically search /opt/ubcl for compatible UBCL",
                             MCA_BASE_VAR_TYPE_BOOL, &mca_opal_common_ubcl_component.search_opt_ubcl);

    // Extra level of string indirection needed to make ompi_info
    // happy since it will unload this library before the MCA base
    // cleans up the MCA vars. This will cause the string to go
    // out of scope unless we place the pointer to it on the heap.
    mca_opal_common_ubcl_component.ubcl_search_path = malloc(sizeof(char*));
    *mca_opal_common_ubcl_component.ubcl_search_path = default_search_path;
    MCA_REGISTER_COMMON_UBCL("ubcl_search_path",
                             "When 'search_opt_ubcl' is true, search for UBCL"
                             " version directories at this path",
                             MCA_BASE_VAR_TYPE_STRING, mca_opal_common_ubcl_component.ubcl_search_path);
}

int mca_common_ubcl_init(void)
{
    int ret;

    /* Safe guard for multiple init/fini */
    if (1 < opal_atomic_add_fetch_32(&mca_opal_common_ubcl_component.is_init, 1)) {
        /* UBCL already init */
        return OPAL_SUCCESS;
    }

    /* Open output stream */
    if (0 <= mca_opal_common_ubcl_component.verbose) {
        mca_opal_common_ubcl_component.output = opal_output_open(NULL);
        opal_output_set_verbosity(mca_opal_common_ubcl_component.output,
                mca_opal_common_ubcl_component.verbose);
        common_ubcl_log_verbose(10, "Opening common/ubcl component\n");
    } else {
        mca_opal_common_ubcl_component.output = -1;
    }

    /* Initializing modules */
    ret = mca_common_ubcl_dlopen_ubcl();

    if (ret != OPAL_SUCCESS) {
        common_ubcl_error("Could not dlopen UBCL");
    }

    return ret;
}

int mca_common_ubcl_fini(void)
{
    int ret;
    uint32_t refcount;

    /* Safe guard for multiple init/fini */
    refcount = opal_atomic_fetch_sub_32(&mca_opal_common_ubcl_component.is_init, 1);
    assert (0 < refcount);

    if (1 < refcount) {
        /* Not the last 'fini' */
        return OPAL_SUCCESS;
    }

    common_ubcl_log_verbose(10, "Closing common/ubcl component\n");

    /* Closing output */
    if (0 < mca_opal_common_ubcl_component.verbose) {
        opal_output_close(mca_opal_common_ubcl_component.output);
    }

    ret = opal_dl_close(libubcl_handle);

    return ret;
}

int mca_common_ubcl_is_init(void) {
    return (int) opal_atomic_add_fetch_32(&mca_opal_common_ubcl_component.is_init, 0);
}
