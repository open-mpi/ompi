/*
 * Copyright (c) 2019-2020 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#ifndef _WIN32
#include <dlfcn.h>
#include <pthread.h>
#else
#include <windows.h>
#endif

#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_vari.h"
#include "opal/util/parse_entry.h"

static char *
my_get_mca_string(char *name) {
    char *rv = NULL;
    int loc_id;

    if (mca_base_var_initialized) {
        loc_id = mca_base_var_find("ompi", "tools", NULL, name);
        if (loc_id >= 0) {
            char **tmp;
            mca_base_var_get_value(loc_id, &tmp, NULL, NULL);
            if (tmp && tmp[0]) {
                rv = strdup(tmp[0]); /* return a thing that's freeable, or null */
            }
        }
    }

    /* The above fails at the rank-level when this call happens very early,
     * which it will since the wrapper system intercepts MPI_Init.  I
     * expect in that case the OMPI_MCA_ompi_tools_* env is set for the rank
     * to see.
     */
    if (!rv) {
        char *env, *p;
        env = malloc(64 + strlen(name));
        sprintf(env, "OMPI_MCA_ompi_tools_%s", name);
        p = getenv(env);
        if (p) { rv = strdup(p); }
        free(env);
    }

    /* Supply a default for "entry_base" */
    if (!rv && 0 == strcmp(name, "entry_base")) {
        /* note: I really want to code the below strings as
         * something like "lib" OMPI_LIBMPI_NAME "." OPAL_DYN_LIB_SUFFIX
         * etc, but those don't seem to be visible from C. If they are
         * visible and I've just missed it, the below could be updated.
         */
        rv = strdup("libmpi.so,libmpi_mpifh.so");
        /*rv = strdup("libmpi_ibm.so,libmpi_ibm_mpifh.so");*/
    }
    return rv;
}

/*
 * Reads OMPI_MCA_tools_entry for
 *   lib,lib,lib
 *   v,vv
 *   preload,nopreload
 *   fort,fortran
 * Reads OMPI_MCA_tools_entry_base for
 *   lib,lib,lib
 *
 * All the strings-related arguments need to be freed by the caller.
 * But not each entry in an array of strings, just the top level pointer.
 * Eg if the caller does
 *     char **libs;
 *     ompi_entry_parse_mca(,,&libs,,,)
 * they would need to free(libs);
 */
void
ompi_entry_parse_mca(int *entry_is_active,
    int *verbose,
    char **preload_string,
    char ***libs, int *nlibs, /* leave "fortran" items in this list, remove v/preload */
    char ***baselibs, int *nbaselibs)
{
    char *list, *p;
    char *strtok_state;
    int i, max;
    int myverbose, myusepreload;
    char *mypreload_string;
    char **mylibs, **mybaselibs;
    int mynlibs, mynbaselibs;

    list = my_get_mca_string("entry");
    if (!list) {
        if (entry_is_active) { *entry_is_active = 0; }
        return;
    }
    if (entry_is_active) { *entry_is_active = 1; }

/* Initial walk to get a max size, then allocate */
    p = NULL;
    if (list) {
        list = strdup(list);
        p = strtok_r(list, ",", &strtok_state);
    }
    max = 2*sizeof(char*);
    for (i=0; p; ++i) {
        max += (strlen(p) + 1 + sizeof(char*));
        p = strtok_r(NULL, ",", &strtok_state);
    }
    mylibs = malloc(max);
    mylibs[0] = (char*)mylibs + i*sizeof(char*);
    mynlibs = 0;
    mypreload_string = malloc(max + 64); /* ex libmpiprofilesupport.so:libfoo.so */
    if (list) { free(list); }

    list = my_get_mca_string("entry_base");
    p = NULL;
    if (list) {
        list = strdup(list);
        p = strtok_r(list, ",", &strtok_state);
    }
    max = 2*sizeof(char*);
    for (i=0; p; ++i) {
        max += (strlen(p) + 1 + sizeof(char*));
        p = strtok_r(NULL, ",", &strtok_state);
    }
    mybaselibs = malloc(max);
    mybaselibs[0] = (char*)mybaselibs + i*sizeof(char*);
    mynbaselibs = 0;
    if (list) { free(list); }

/* Maintain the arrays of strings so the next one is ready to write into */
    myverbose = 0;
    myusepreload = 0;

/* Parse the entries from OMPI_MCA_tools_entry */
    list = my_get_mca_string("entry"); p = NULL;
    if (list) {
        p = strtok_r(list, ",", &strtok_state);
    }
    for (i=0; p; ++i) {
        char *lib_to_add = NULL;

        if (0==strcmp(p, "v")) {
            myverbose = 1;
        }
        else if (0==strcmp(p, "vv")) {
            myverbose = 2;
        }
        else if (0==strcmp(p, "preload")) {
            myusepreload = 1;
        }
        else if (0==strcmp(p, "nopreload")) {
            myusepreload = 0;
        }
        else if (0==strcmp(p, "fort")) {
            lib_to_add = strdup(p); /* fort and fortran stay as entries in the libs[] output list */
        }
        else if (0==strcmp(p, "fortran")) {
            lib_to_add = strdup(p);
        }
        else {
            /* if the string contains exclusively chars [a-zA-Z0-9_-], morph it into lib<string>.so */
            if (strspn(p, "abcdefghijklmnopqrstuvwxyz"
                          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                          "0123456789_-")
                == strlen(p))
            {
                lib_to_add = malloc(strlen(p) + 16);;
                sprintf(lib_to_add, "lib%s.so", p);
            } else {
                lib_to_add = strdup(p); /* so it can be freed */
            }
        }

        if (lib_to_add) {
            if (mynlibs>0) {
                mylibs[mynlibs] = mylibs[mynlibs-1] + strlen(mylibs[mynlibs-1]) + 1;
            }
            strcpy(mylibs[mynlibs++], lib_to_add);
            free(lib_to_add);
        }

        p = strtok_r(NULL, ",", &strtok_state);
    }
    if (list) { free(list); }

/* Parse the entries from OMPI_MCA_tools_entry_base */
    list = my_get_mca_string("entry_base"); p = NULL;
    p = strtok_r(list, ",", &strtok_state);
    for (i=0; p; ++i) {
        if (mynbaselibs>0) {
            mybaselibs[mynbaselibs] = mybaselibs[mynbaselibs-1] + strlen(mybaselibs[mynbaselibs-1]) + 1;
        }
        strcpy(mybaselibs[mynbaselibs++], p);

        p = strtok_r(NULL, ",", &strtok_state);
    }
    if (list) { free(list); }

/* Put results into preload too
 * skipping over the fort/fortran entries
 */
    if (myusepreload) {
        strcpy(mypreload_string, "libmpiprofilesupport.so");
        for (i=0; i<mynlibs; ++i) {
            p = mylibs[i];
            if (0!=strcmp(p, "fort") && 0!=strcmp(p, "fortran")) {
                strcat(mypreload_string, " ");
                strcat(mypreload_string, p);
            }
        }
    }

/* Put final results in the function's output args
 * Free items that aren't going into the output args
 */
    if (verbose) { *verbose = myverbose; }
    if (preload_string) {
        if (myusepreload && preload_string) {
            *preload_string = mypreload_string;
        } else {
            if (preload_string) { *preload_string = NULL; }
            free(mypreload_string);
        }
    }

    if (libs) {
        *libs = mylibs;
        *nlibs = mynlibs;
    } else {
        free(mylibs);
    }

    if (baselibs) {
        *baselibs = mybaselibs;
        *nbaselibs = mynbaselibs;
    } else {
        free(mybaselibs);
    }
}
