/*
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "opal_config.h"

#include <string.h>

#include "opal/util/os_path.h"
#include "opal/mca/installdirs/base/base.h"
#include "opal/mca/installdirs/installdirs.h"

#define EXPAND_STRING(field)                                            \
    do {                                                                \
        if (NULL != (start_pos = strstr(retval, "${" #field "}"))) {    \
            tmp = retval;                                               \
            *start_pos = '\0';                                          \
            end_pos = start_pos + strlen("${" #field "}");              \
            asprintf(&retval, "%s%s%s", tmp,                            \
                     opal_install_dirs.field + destdir_offset,          \
                     end_pos);                                          \
            free(tmp);                                                  \
            changed = true;                                             \
        }                                                               \
    } while (0)



char *
opal_install_dirs_expand(const char* input)
{
    size_t len, i;
    bool needs_expand = false;
    char *retval = strdup(input);
    char *destdir = getenv("OPAL_DESTDIR");
    size_t destdir_offset = 0;

    if (NULL != destdir && strlen(destdir) > 0) {
        destdir_offset = strlen(destdir);
    }

    len = strlen(input);
    for (i = 0 ; i < len ; ++i) {
        if (input[i] == '$') {
            needs_expand = true;
            break;
        }
    }

    retval = strdup(input);
    if (NULL == retval) return NULL;

    if (needs_expand) {
        bool changed = false;
        char *start_pos, *end_pos, *tmp;

        do {
            changed = false;
            EXPAND_STRING(prefix);
            EXPAND_STRING(exec_prefix);
            EXPAND_STRING(bindir);
            EXPAND_STRING(sbindir);
            EXPAND_STRING(libexecdir);
            EXPAND_STRING(datarootdir);
            EXPAND_STRING(datadir);
            EXPAND_STRING(sysconfdir);
            EXPAND_STRING(sharedstatedir);
            EXPAND_STRING(localstatedir);
            EXPAND_STRING(libdir);
            EXPAND_STRING(includedir);
            EXPAND_STRING(infodir);
            EXPAND_STRING(mandir);
            EXPAND_STRING(pkgdatadir);
            EXPAND_STRING(pkglibdir);
            EXPAND_STRING(pkgincludedir);
        } while (changed);
    }

    if (NULL != destdir) {
        char *tmp = retval;
        retval = opal_os_path(false, destdir, tmp, NULL);
        free(tmp);
    }

    return retval;
}
