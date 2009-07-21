/*
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystem, Inc.  All rights reserved.
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

/*
 * Read a field from an opal_install_dirs_t structure.
 *
 * The field name is passed as a string + a length, and need not be
 * null terminated.
 *
 * The implementation is reasonaly efficient, but perhaps less
 * readable than a bunch of strncmp calls.
 */

#define CHKTRAIL(s,n) if (strncmp(field, #s, field_length - n) != 0) return NULL

static const char *
field_lookup(const char *field,
             int field_length,
             opal_install_dirs_t *install_dirs)
{
    switch (*field++) {
    case 'b':
        CHKTRAIL(indir, 1);
        return install_dirs->bindir;
    case 'd':
        CHKTRAIL(atarootdir, 1);
        return install_dirs->datarootdir;
    case 'e':
        CHKTRAIL(xec_prefix, 1);
        return install_dirs->exec_prefix;
    case 'i':
        if ('n' != *field++) return NULL;
        switch (*field++) {
        case 'c':
            CHKTRAIL(ludedir, 3);
            return install_dirs->includedir;
        case 'f':
            CHKTRAIL(odir, 3);
            return install_dirs->infodir;
        }
        return NULL;
    case 'l':
        switch (*field++) {
        case 'o':
            CHKTRAIL(calstatedir, 2);
            return install_dirs->localstatedir;
        case 'i':
            switch (*field++) {
            case 'b':
                switch (*field++) {
                case 'd':
                    CHKTRAIL(ir, 4);
                    return install_dirs->libdir;
                case 'e':
                    CHKTRAIL(xecdir, 4);
                    return install_dirs->libexecdir;
                }
            }
        }
        return NULL;

    case 'm':
        CHKTRAIL(andir, 1);
        return install_dirs->mandir;

    case 'p':
        switch (*field++) {
        case 'r':
            CHKTRAIL(efix, 2);
            return install_dirs->prefix;
        case 'k':
            if ('g' == *field++) {
                switch (*field++) {
                case 'd':
                    CHKTRAIL(atadir, 4);
                    return install_dirs->pkgdatadir;
                case 'l':
                    CHKTRAIL(ibdir, 4);
                    return install_dirs->pkglibdir;
                case 'i':
                    CHKTRAIL(ncludedir, 4);
                    return install_dirs->pkgincludedir;
                }
            }
        }
        return NULL;
    case 's':
        switch (*field++) {
        case 'b':
            CHKTRAIL(indir, 2);
            return install_dirs->sbindir;
        case 'h':
            CHKTRAIL(aredstatedir, 2);
            return install_dirs->sharedstatedir;
        case 'y':
            CHKTRAIL(sconfdir, 2);
            return install_dirs->sysconfdir;
        }
    }
    return NULL;
}

/*
 * Sets *output to the input string with any "${foo}" references expanded.
 * The expansion values come from the fields of install_dirs.
 *
 * If dont_expand is non-NULL, references to the field it names are
 * not expanded.  (E.g., "${libdir}" will be copied as is to *output if
 * dont_expand is "libdir".)
 */

static void
install_dirs_expand(const char *input,
                    char **output,
		    size_t *output_len,
                    size_t *j,
                    opal_install_dirs_t *install_dirs,
                    const char *dont_expand)
{
    size_t len, i, m, n;
    const char *expansion;

    if (0 == *output_len) {
	*output_len = 100;
	*output = malloc(*output_len);
	if (NULL == *output) {
	    return;
	}
    }
    len = strlen(input);
    for (i = 0 ; i < len ; ++i) {
        expansion = NULL;
        if ('$' == input[i] && '{' == input[i+1]) {
            for (n = 0; i + n + 2 < len; n++) {
                if ('}' == input[i+n+2]) {
                    break;
                }
            }
            if (NULL == dont_expand ||
                strncmp(&input[i+2], dont_expand, n) != 0 ) {
                expansion = field_lookup(&input[i+2], n, install_dirs);
            } else {
                expansion = NULL;
            }
        }
        if (NULL != expansion) {
            install_dirs_expand(expansion, output, output_len, j, install_dirs,
                                dont_expand);
            i += n + 2;
        } else {
	    if (*j + 1 >= *output_len) {
		*output_len += 100;
		*output = realloc(*output, *output_len);
		if (NULL == *output) {
		    return;
		}
	    }
            (*output)[(*j)++] = input[i];
        }
    }

    (*output)[*j] = '\0';
}

char *
opal_install_dirs_expand(const char* input)
{
    char *retval = NULL;
    char *destdir = getenv("OPAL_DESTDIR");
    size_t j = 0, retval_len = 0;

    if (NULL != destdir) {
	j = strlen(destdir) + sizeof(OPAL_PATH_SEP) - 1;
	retval_len = j + 100;
	retval = malloc(retval_len);
	if (NULL == retval) {
	    return NULL;
	}
	sprintf(retval, "%s%s", destdir, OPAL_PATH_SEP);
    }

    install_dirs_expand(input, &retval, &retval_len, &j, &opal_install_dirs, NULL);
    if (NULL != retval) {
       retval = realloc(retval, j + 1);
    }
    return retval;
}

char *
opal_install_dirs_infer(const char *inferred_field,
                        const char *infer_from_field,
                        size_t infer_from_field_len,
                        opal_install_dirs_t *component_installdirs)
{
    const char *infer_from_path;
    char *component_field = NULL;
    size_t component_field_len = 0;
    const char *installed_field = NULL;
    char *p, *q;
    size_t j = 0, inferred_field_len = strlen(inferred_field);
    size_t leading, trailing, installed_field_len, retval_len;
    char *retval;

    infer_from_path = field_lookup(infer_from_field, infer_from_field_len,
                                   component_installdirs);
    install_dirs_expand(infer_from_path,
			&component_field,
			&component_field_len,
			&j,
                        component_installdirs,
                        inferred_field);

    installed_field = field_lookup(infer_from_field, infer_from_field_len,
                                   &opal_install_dirs);

    /*
     * Let's say component_field is, "/path/${prefix}/bin", and
     * infer_from_field is "bindir".  Then we want to:
     *
     *  1. Make sure that opal_install_dirs.bindir starts with "/path/"
     *
     *  2. Make sure that opal_install_dirs.bindir ends with "/bin"
     *
     *  3. Return the string that appears between those
     */

    for (p = component_field; *p; p++) {
        if (*p == '$' && p[1] == '{' &&
            strncmp(p+2, inferred_field, inferred_field_len) == 0) {
            break;
        }
    }
    if (*p == '\0') {
        free(component_field);
        return NULL;
    }
    leading = p - component_field;
    trailing = component_field + strlen(component_field) - p -
        inferred_field_len - 3;

    installed_field_len = strlen(installed_field);
    if (installed_field_len < trailing + leading) {
        free(component_field);
        return NULL;
    }

    if (strncmp(component_field, installed_field, leading) != 0) {
        free(component_field);
        return NULL;
    }

    if (strcmp(p + inferred_field_len + 3,
               installed_field + installed_field_len - trailing) != 0) {
        free(component_field);
        return NULL;
    }

    free(component_field);
    retval_len = installed_field_len - leading - trailing;
    retval = malloc(retval_len + 1);
    if (NULL == retval) {
        return NULL;
    }
    memcpy(retval, installed_field + leading, retval_len);
    retval[retval_len] = '\0';
    return retval;
}


