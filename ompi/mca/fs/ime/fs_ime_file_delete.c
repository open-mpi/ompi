/*
 * Copyright (c) 2018      DataDirect Networks. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ime_native.h"

#include "ompi_config.h"
#include "fs_ime.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fs/fs.h"

/*
 *	file_delete_ime
 *
 *	Function:	- deletes a file
 *	Accepts:	- file name & info
 *	Returns:	- Success if file closed
 */
int mca_fs_ime_file_delete (char* file_name,
                            struct opal_info_t *info)
{
    int ret;

    /* reset errno */
    errno = 0;

    ret = ime_native_unlink(file_name);
    if (ret != 0) {
        return mca_fs_base_get_mpi_err(errno);
    }

    return OMPI_SUCCESS;
}
