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
 * Copyright (c) 2008-2021 University of Houston. All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021      Cisco Systems, Inc.  All rights reserved.
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
#include "opal/util/printf.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"
#include "ompi/mca/sharedfp/sm/sharedfp_sm.h"

#include "opal/util/basename.h"

/*
 * *******************************************************************
 * ************************ actions structure ************************
 * *******************************************************************
 */
 /* IMPORTANT: Update here when adding sharedfp component interface functions*/
static mca_sharedfp_base_module_1_0_0_t sm =  {
    mca_sharedfp_sm_module_init, /* initalise after being selected */
    mca_sharedfp_sm_module_finalize, /* close a module on a communicator */
    mca_sharedfp_sm_seek,
    mca_sharedfp_sm_get_position,
    mca_sharedfp_sm_read,
    mca_sharedfp_sm_read_ordered,
    mca_sharedfp_sm_read_ordered_begin,
    mca_sharedfp_sm_read_ordered_end,
    mca_sharedfp_sm_iread,
    mca_sharedfp_sm_write,
    mca_sharedfp_sm_write_ordered,
    mca_sharedfp_sm_write_ordered_begin,
    mca_sharedfp_sm_write_ordered_end,
    mca_sharedfp_sm_iwrite,
    mca_sharedfp_sm_file_open,
    mca_sharedfp_sm_file_close
};
/*
 * *******************************************************************
 * ************************* structure ends **************************
 * *******************************************************************
 */

int mca_sharedfp_sm_component_init_query(bool enable_progress_threads,
                                            bool enable_mpi_threads)
{
    /* Nothing to do */

   return OMPI_SUCCESS;
}

struct mca_sharedfp_base_module_1_0_0_t * mca_sharedfp_sm_component_file_query(ompio_file_t *fh, int *priority)
{
    int i;
    ompi_proc_t *proc;
    ompi_communicator_t * comm = fh->f_comm;
    int size = ompi_comm_size(comm);

    *priority = 0;

    /* test, and update priority. All processes have to be
    ** on a single node.
    ** original test copied from mca/coll/sm/coll_sm_module.c:
    */
    ompi_group_t *group = comm->c_local_group;

    for (i = 0; i < size; ++i) {
        proc = ompi_group_peer_lookup(group,i);
        if (!OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)){
            opal_output(ompi_sharedfp_base_framework.framework_output,
                        "mca_sharedfp_sm_component_file_query: Disqualifying myself: (%s/%s) "
                        "not all processes are on the same node.",
                        ompi_comm_print_cid (comm), comm->c_name);
            return NULL;
        }
    }


    /* Check that we can actually open the required file */
    char *filename_basename = opal_basename((char*)fh->f_filename);
    char *sm_filename;
    int comm_cid = -1;
    int pid = ompi_comm_rank (comm);
    
    opal_asprintf(&sm_filename, "%s/%s_cid-%d-%d.sm", ompi_process_info.job_session_dir,
             filename_basename, comm_cid, pid);
    free(filename_basename);

    int sm_fd = open(sm_filename, O_RDWR | O_CREAT,
                     S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if ( sm_fd == -1){
        /*error opening file*/
        opal_output(0,"mca_sharedfp_sm_component_file_query: Error, unable to open file for mmap: %s\n",sm_filename);
        free(sm_filename);
        return NULL;
    }
    close (sm_fd);
    unlink(sm_filename);
    free (sm_filename);
    
    /* This module can run */
    *priority = mca_sharedfp_sm_priority;
    return &sm;
}

int mca_sharedfp_sm_component_file_unquery (ompio_file_t *file)
{
   /* This function might be needed for some purposes later. for now it
    * does not have anything to do since there are no steps which need
    * to be undone if this module is not selected */

   return OMPI_SUCCESS;
}

int mca_sharedfp_sm_module_init (ompio_file_t *file)
{
    return OMPI_SUCCESS;
}


int mca_sharedfp_sm_module_finalize (ompio_file_t *file)
{
    return OMPI_SUCCESS;
}
