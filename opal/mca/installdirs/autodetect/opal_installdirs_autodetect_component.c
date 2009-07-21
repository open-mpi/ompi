/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009 Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * This module automatically detects the path to itself, and
 * instructs the base install_dirs module to infer the prefix
 * from that.
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/basename.h"

#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int opal_installdirs_autodetect_open(void);

opal_installdirs_base_component_t mca_installdirs_autodetect_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_INSTALLDIRS_BASE_VERSION_2_0_0,

        /* Component name and version */
        "autodetect",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        opal_installdirs_autodetect_open,
        NULL
    },
    {
        /* This component is Checkpointable */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

/*
 * Determine whether the load object is an executable or a shared library.
 * The code only works for ELF files.  The values and offsets are cribbed
 * from the "file" command's magic number file.
*/

typedef enum { executable, shared_object, unknown_object } load_obj_t;

static load_obj_t
whatis(const char *path)
{
    int fd;
    char buf[18];
    uint16_t *kind;

    fd = open(path, O_RDONLY);
    if (fd < 0) {
        return unknown_object;
    }

    if (read(fd, buf, sizeof(buf)) < sizeof(buf)) {
        close(fd);
        return unknown_object;
    }
    close(fd);

    if (strncmp(buf, "\177ELF", 4) != 0) {
        return unknown_object;
    }

    kind = (uint16_t*)&buf[16];
    if (2 == *kind) {
        return executable;
    } else if (3 == *kind) {
        return shared_object;
    } else {
        return unknown_object;
    }
}

/*
 * OS-dependent function to get the address of some instruction in
 * this module.  We cannot in general just do &func, because if the
 * module is in a shared library that can return some other address.
 * (On Solaris, for example, it will return the address of an entry in
 * the PLT of the program, rather than the shared library.)
 */

uintptr_t opal_installdirs_autodetect_pc(void);

/*
 * OS-dependent function to find the path to the executable or shared
 * library that contains the given address in the process' address
 * space.
 */

const char *opal_installdirs_autodetect_path(uintptr_t);

static int
opal_installdirs_autodetect_open(void)
{
    uintptr_t my_addr;
    const char *path;
    const char *my_dir;
    const char *infer_from;

    if (getenv("OPAL_DESTDIR") != NULL) {
	/*
	 * OPAL_DESTDIR does not play well with autodetect.  We
	 * certainly don't want it to be used as a prefix for the actual
	 * installed path.  We could try to inhibit it only for those
	 * paths that cannot be inferred from the actual installed
	 * path, but it is simpler just to assume the user wants no
	 * auto detection if OPAL_DESTDIR is set.
	 */
	return OPAL_ERR_NOT_FOUND;
    }

    my_addr = opal_installdirs_autodetect_pc();
    if (0 == my_addr) {
        return OPAL_ERR_NOT_FOUND;
    }
    path = opal_installdirs_autodetect_path(my_addr);
    if (NULL == path) {
        return OPAL_ERR_NOT_FOUND;
    }
    my_dir = opal_dirname(path);
    if (NULL == my_dir) {
        free(path);
        return OPAL_ERR_NOT_FOUND;
    }
    switch(whatis(path)) {
    case executable:
        mca_installdirs_autodetect_component.install_dirs_data.bindir = my_dir;
	infer_from = "${infer-bindir}";
        mca_installdirs_autodetect_component.install_dirs_data.libdir = infer_from;
        break;
    case shared_object:
        mca_installdirs_autodetect_component.install_dirs_data.libdir = my_dir;
        infer_from = "${infer-libdir}";
        mca_installdirs_autodetect_component.install_dirs_data.bindir = infer_from;
        break;
    default:
        free(my_dir);
        free(path);
        return OPAL_ERR_NOT_FOUND;
        break;
    }
    free(path);

    mca_installdirs_autodetect_component.install_dirs_data.prefix = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.exec_prefix = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.sbindir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.libexecdir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.datarootdir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.datadir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.sysconfdir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.sharedstatedir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.localstatedir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.includedir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.infodir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.mandir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.pkgdatadir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.pkglibdir = infer_from;
    mca_installdirs_autodetect_component.install_dirs_data.pkgincludedir = infer_from;

    return OPAL_SUCCESS;
}
    
