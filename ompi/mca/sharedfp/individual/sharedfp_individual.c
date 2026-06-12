/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013-2015 University of Houston. All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics. Since linkers generally pull in symbols by object fules,
 * keeping these symbols as the only symbols in this file prevents
 * utility programs such as "ompi_info" from having to import entire
 * modules just to query their version and parameters
 */

#include "ompi_config.h"
#include "mpi.h"
#include "ompi/file/file.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"
#include "ompi/mca/sharedfp/individual/sharedfp_individual.h"

/*
 * *******************************************************************
 * ************************ actions structure ************************
 * *******************************************************************
 */
 /* IMPORTANT: Update here when adding sharedfp component interface functions*/
static mca_sharedfp_base_module_2_0_0_t individual =  {
    mca_sharedfp_individual_module_init, /* initialise after being selected */
    mca_sharedfp_individual_module_finalize, /* close a module on a communicator */
    mca_sharedfp_individual_seek,
    mca_sharedfp_individual_get_position,
    mca_sharedfp_individual_read,
    mca_sharedfp_individual_read_ordered,
    mca_sharedfp_individual_read_ordered_begin,
    mca_sharedfp_individual_read_ordered_end,
    mca_sharedfp_individual_iread,
    mca_sharedfp_individual_write,
    mca_sharedfp_individual_write_ordered,
    mca_sharedfp_individual_write_ordered_begin,
    mca_sharedfp_individual_write_ordered_end,
    mca_sharedfp_individual_iwrite,
    mca_sharedfp_individual_file_open,
    mca_sharedfp_individual_file_close
};
/*
 * *******************************************************************
 * ************************* structure ends **************************
 * *******************************************************************
 */

static const char *mca_sharedfp_individual_relaxed_ordering_cb(opal_infosubscriber_t *object,
                                                               const char *key,
                                                               const char *value)
{
    ompi_file_t *file;
    mca_common_ompio_data_t *data;
    ompio_file_t *fh;

    if (NULL == object || NULL == key || NULL == value || '\0' == value[0]) {
        return NULL;
    }

    file = (ompi_file_t *) object;
    data = (mca_common_ompio_data_t *) file->f_io_selected_data;
    if (NULL == data) {
        return NULL;
    }
    fh = &data->ompio_fh;

    /*
     * sharedfp selection queries every candidate component.  This callback
     * must accept the hint while selection is still in progress because the
     * hint raises individual's priority, but once a sharedfp module is known
     * the hint is public only if this component actually won selection.
     */
    if (NULL != fh->f_sharedfp && &individual != fh->f_sharedfp) {
        return NULL;
    }

    return value;
}

int mca_sharedfp_individual_component_init_query(bool enable_progress_threads,
                                            bool enable_mpi_threads)
{
    /* Nothing to do */

   return OMPI_SUCCESS;
}

struct mca_sharedfp_base_module_2_0_0_t * mca_sharedfp_individual_component_file_query (ompio_file_t *fh, int *priority) {

    int amode;
    bool wronly_flag=false;
    bool relaxed_order_flag=false;
    opal_info_t *info;
    int flag;
    int ret;
    opal_cstring_t *info_str;
    *priority = 0;

    /*test, and update priority*/
    /*---------------------------------------------------------*/
    /* 1. Is the file write only? check amode for MPI_MODE_WRONLY */
    amode = fh->f_amode;
    if ( amode & MPI_MODE_WRONLY || amode & MPI_MODE_RDWR ) {
        wronly_flag=true;
	if ( mca_sharedfp_individual_verbose ) {
            opal_output(ompi_sharedfp_base_framework.framework_output,
                        "mca_sharedfp_individual_component_file_query: "
                        "MPI_MODE_WRONLY[true=%d,false=%d]=%d\n",true,false,wronly_flag);
	}
    } else {
        wronly_flag=false;
	if ( mca_sharedfp_individual_verbose ) {
            opal_output(ompi_sharedfp_base_framework.framework_output,
			"mca_sharedfp_individual_component_file_query: Can not run!, "
			"MPI_MODE_WRONLY[true=%d,false=%d]=%d\n",true,false,wronly_flag);
	}
    }

    /*---------------------------------------------------------*/
    /* 2. Did the user specify MPI_INFO relaxed ordering flag? */
    if (wronly_flag) {
        /*
         * This hint is meaningful only when this component can be selected.
         * Registering it conditionally avoids reporting a sharedfp hint as
         * accepted on read-only opens where individual sharedfp will not run.
         */
        ret = mca_common_ompio_info_subscribe(fh, "OMPIO_SHAREDFP_RELAXED_ORDERING",
                                              NULL,
                                              mca_sharedfp_individual_relaxed_ordering_cb);
        if (OMPI_SUCCESS != ret) {
            return NULL;
        }
    }

    info = fh->f_info;
    if ( wronly_flag && info != &(MPI_INFO_NULL->super) ){
        opal_info_get ( info,"OMPIO_SHAREDFP_RELAXED_ORDERING", &info_str, &flag);
        if ( flag ) {
           if ( mca_sharedfp_individual_verbose ) {
                opal_output(ompi_sharedfp_base_framework.framework_output,
                        "mca_sharedfp_individual_component_file_query: "
                        "OMPIO_SHAREDFP_RELAXED_ORDERING=%s\n", info_str->string);
	    }
            /* flag - Returns true if key defined, false if not (boolean). */
            relaxed_order_flag=true;
            OBJ_RELEASE(info_str);
        }
        else {
            if ( mca_sharedfp_individual_verbose ) {
               opal_output(ompi_sharedfp_base_framework.framework_output,
                        "mca_sharedfp_individual_component_file_query: "
                        "OMPIO_SHAREDFP_RELAXED_ORDERING MPI_Info key not set. "
                        "Set this key in order to increase this component's priority value.\n");
	    }
	}
    }
    else {
	if ( mca_sharedfp_individual_verbose ) {
            opal_output(ompi_sharedfp_base_framework.framework_output,
                 "mca_sharedfp_individual_component_file_query: "
                 "OMPIO_SHAREDFP_RELAXED_ORDERING MPI_Info key not set, "
                 "got MPI_INFO_NULL. Set this key in order to increase "
                 "this component's priority value.\n");
	}
    }

    /*For now, this algorithm will not run if the file is not opened write only.
     *Setting the OMPIO_SHAREDFP_RELAXED_ORDERING gives this module a higher priority
     *otherwise it gets a priority of zero. This means that this module will
     *run only if no other module can run
     */
    if ( wronly_flag && relaxed_order_flag){
        *priority=mca_sharedfp_individual_priority;
    }
    else {
        *priority=1;
    }

    if ( wronly_flag ){
        return &individual;
    }

    return NULL;
}

int mca_sharedfp_individual_component_file_unquery (ompio_file_t *file)
{
    /*
     * The query path may have subscribed and accepted the relaxed-ordering
     * hint so it could participate in priority selection.  If another
     * sharedfp component wins, remove the public value; otherwise
     * MPI_File_get_info would claim that a hint owned by the losing
     * individual component is part of the active file stack.
     */
    if (NULL != file && NULL != file->f_info) {
        (void) opal_info_delete(file->f_info, "OMPIO_SHAREDFP_RELAXED_ORDERING");
    }

    return OMPI_SUCCESS;
}

int mca_sharedfp_individual_module_init (ompio_file_t *file)
{
    return OMPI_SUCCESS;
}


int mca_sharedfp_individual_module_finalize (ompio_file_t *file)
{
    return OMPI_SUCCESS;
}
