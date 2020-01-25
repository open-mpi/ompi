/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2019 University of Houston. All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "ompi/file/file.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fcoll/base/base.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"

#include "io_ompio.h"
#include "ompi/mca/common/ompio/common_ompio_request.h"
#include "math.h"
#include <unistd.h>

/* Read and write routines are split into two interfaces.
**   The
**   mca_io_ompio_file_read/write[_at]
**
** routines are the ones registered with the ompio modules.
** The
**
** mca_common_ompio_file_read/write[_at]
**
** routesin are used e.g. from the shared file pointer modules.
** The main difference is, that the first one takes an ompi_file_t
** as a file pointer argument, while the second uses the ompio internal
** ompio_file_t structure.
*/


int mca_io_ompio_file_write (ompi_file_t *fp,
			     const void *buf,
			     int count,
			     struct ompi_datatype_t *datatype,
			     ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;
    ompio_file_t *fh;

    data = (mca_common_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;
    OPAL_THREAD_LOCK(&fp->f_lock);
    ret = mca_common_ompio_file_write(fh,buf,count,datatype,status);
    OPAL_THREAD_UNLOCK(&fp->f_lock);
    return ret;
}


int mca_io_ompio_file_write_at (ompi_file_t *fh,
				OMPI_MPI_OFFSET_TYPE offset,
				const void *buf,
				int count,
				struct ompi_datatype_t *datatype,
				ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK(&fh->f_lock);
    ret = mca_common_ompio_file_write_at (&data->ompio_fh, offset,buf,count,datatype,status);
    OPAL_THREAD_UNLOCK(&fh->f_lock);

    return ret;
}

int mca_io_ompio_file_iwrite (ompi_file_t *fp,
			      const void *buf,
			      int count,
			      struct ompi_datatype_t *datatype,
			      ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;

    data = (mca_common_ompio_data_t *) fp->f_io_selected_data;
    OPAL_THREAD_LOCK(&fp->f_lock);
    ret = mca_common_ompio_file_iwrite(&data->ompio_fh,buf,count,datatype,request);
    OPAL_THREAD_UNLOCK(&fp->f_lock);

    return ret;
}


int mca_io_ompio_file_iwrite_at (ompi_file_t *fh,
				 OMPI_MPI_OFFSET_TYPE offset,
				 const void *buf,
				 int count,
				 struct ompi_datatype_t *datatype,
				 ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK(&fh->f_lock);
    ret = mca_common_ompio_file_iwrite_at(&data->ompio_fh,offset,buf,count,datatype,request);
    OPAL_THREAD_UNLOCK(&fh->f_lock);

    return ret;
}


/* Collective operations                                          */
/******************************************************************/

int mca_io_ompio_file_write_all (ompi_file_t *fh,
				 const void *buf,
				 int count,
				 struct ompi_datatype_t *datatype,
				 ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;

    OPAL_THREAD_LOCK(&fh->f_lock);
    ret = mca_common_ompio_file_write_all (&data->ompio_fh,
                                           buf,
                                           count,
                                           datatype,
                                           status);
    OPAL_THREAD_UNLOCK(&fh->f_lock);
    if ( MPI_STATUS_IGNORE != status ) {
	size_t size;

	opal_datatype_type_size (&datatype->super, &size);
	status->_ucount = count * size;
    }

    return ret;
}

int mca_io_ompio_file_write_at_all (ompi_file_t *fh,
				    OMPI_MPI_OFFSET_TYPE offset,
				    const void *buf,
				    int count,
				    struct ompi_datatype_t *datatype,
				    ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK(&fh->f_lock);
    ret = mca_common_ompio_file_write_at_all(&data->ompio_fh,offset,buf,count,datatype,status);
    OPAL_THREAD_UNLOCK(&fh->f_lock);

    return ret;
}

int mca_io_ompio_file_iwrite_all (ompi_file_t *fh,
				  const void *buf,
				  int count,
				  struct ompi_datatype_t *datatype,
				  ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data=NULL;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;

    OPAL_THREAD_LOCK(&fh->f_lock);
    ret = mca_common_ompio_file_iwrite_all (&data->ompio_fh,
                                            buf,
                                            count,
                                            datatype,
                                            request);
    OPAL_THREAD_UNLOCK(&fh->f_lock);

    return ret;
}


int mca_io_ompio_file_iwrite_at_all (ompi_file_t *fh,
				     OMPI_MPI_OFFSET_TYPE offset,
				     const void *buf,
				     int count,
				     struct ompi_datatype_t *datatype,
				     ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK(&fh->f_lock);
    ret = mca_common_ompio_file_iwrite_at_all ( &data->ompio_fh, offset, buf, count, datatype, request );
    OPAL_THREAD_UNLOCK(&fh->f_lock);
    return ret;
}


/* Infrastructure for shared file pointer operations */
/* (Individual and collective */
/******************************************************/

int mca_io_ompio_file_write_shared (ompi_file_t *fp,
				    const void *buf,
				    int count,
				    struct ompi_datatype_t *datatype,
				    ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;
    ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_common_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    if ( NULL == shared_fp_base_module ){
        opal_output(0, "No shared file pointer component found for this communicator. Can not execute\n");
        return OMPI_ERROR;
    }
    OPAL_THREAD_LOCK(&fp->f_lock);
    ret = shared_fp_base_module->sharedfp_write(fh,buf,count,datatype,status);
    OPAL_THREAD_UNLOCK(&fp->f_lock);

    return ret;
}

int mca_io_ompio_file_iwrite_shared (ompi_file_t *fp,
				     const void *buf,
				     int count,
				     struct ompi_datatype_t *datatype,
				     ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;
    ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_common_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    if ( NULL == shared_fp_base_module ){
        opal_output(0, "No shared file pointer component found for this communicator. Can not execute\n");
        return OMPI_ERROR;
    }
    OPAL_THREAD_LOCK(&fp->f_lock);
    ret = shared_fp_base_module->sharedfp_iwrite(fh,buf,count,datatype,request);
    OPAL_THREAD_UNLOCK(&fp->f_lock);

    return ret;
}

int mca_io_ompio_file_write_ordered (ompi_file_t *fp,
				     const void *buf,
				     int count,
				     struct ompi_datatype_t *datatype,
				     ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;
    ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_common_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    if ( NULL == shared_fp_base_module ){
        opal_output(0,"No shared file pointer component found for this communicator. Can not execute\n");
        return OMPI_ERROR;
    }
    OPAL_THREAD_LOCK(&fp->f_lock);
    ret = shared_fp_base_module->sharedfp_write_ordered(fh,buf,count,datatype,status);
    OPAL_THREAD_UNLOCK(&fp->f_lock);

    return ret;
}

int mca_io_ompio_file_write_ordered_begin (ompi_file_t *fp,
					   const void *buf,
					   int count,
					   struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;
    ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_common_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    if ( NULL == shared_fp_base_module ){
        opal_output(0, "No shared file pointer component found for this communicator. Can not execute\n");
	return OMPI_ERROR;
    }
    OPAL_THREAD_LOCK(&fp->f_lock);
    ret = shared_fp_base_module->sharedfp_write_ordered_begin(fh,buf,count,datatype);
    OPAL_THREAD_UNLOCK(&fp->f_lock);

    return ret;
}

int mca_io_ompio_file_write_ordered_end (ompi_file_t *fp,
					 const void *buf,
					 ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;
    ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_common_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    if ( NULL == shared_fp_base_module ){
        opal_output(0, "No shared file pointer component found for this communicator. Can not execute\n");
	return OMPI_ERROR;
    }
    OPAL_THREAD_LOCK(&fp->f_lock);
    ret = shared_fp_base_module->sharedfp_write_ordered_end(fh,buf,status);
    OPAL_THREAD_UNLOCK(&fp->f_lock);

    return ret;
}


/* Split collectives . Not really used but infrastructure is in place */
/**********************************************************************/
int mca_io_ompio_file_write_all_begin (ompi_file_t *fh,
				       const void *buf,
				       int count,
				       struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    ompio_file_t *fp;
    mca_common_ompio_data_t *data;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;
    fp = &data->ompio_fh;
    if ( true == fp->f_split_coll_in_use ) {
	printf("Only one split collective I/O operation allowed per file handle at any given point in time!\n");
	return MPI_ERR_OTHER;
    }
    /* No need for locking fh->f_lock, that is done in file_iwrite_all */
    ret = mca_io_ompio_file_iwrite_all ( fh, buf, count, datatype, &fp->f_split_coll_req );
    fp->f_split_coll_in_use = true;

    return ret;
}

int mca_io_ompio_file_write_all_end (ompi_file_t *fh,
				     const void *buf,
				     ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    ompio_file_t *fp;
    mca_common_ompio_data_t *data;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;
    fp = &data->ompio_fh;
    ret = ompi_request_wait ( &fp->f_split_coll_req, status );

    /* remove the flag again */
    fp->f_split_coll_in_use = false;
    return ret;
}


int mca_io_ompio_file_write_at_all_begin (ompi_file_t *fh,
					  OMPI_MPI_OFFSET_TYPE offset,
					  const void *buf,
					  int count,
					  struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data=NULL;
    ompio_file_t *fp=NULL;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;
    fp = &data->ompio_fh;

    if ( true == fp->f_split_coll_in_use ) {
	printf("Only one split collective I/O operation allowed per file handle at any given point in time!\n");
	return MPI_ERR_REQUEST;
    }
    OPAL_THREAD_LOCK(&fh->f_lock);
    ret = mca_common_ompio_file_iwrite_at_all ( fp, offset, buf, count, datatype, &fp->f_split_coll_req );
    OPAL_THREAD_UNLOCK(&fh->f_lock);
    fp->f_split_coll_in_use = true;

    return ret;
}


int mca_io_ompio_file_write_at_all_end (ompi_file_t *fh,
					const void *buf,
					ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;
    ompio_file_t *fp=NULL;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;
    fp = &data->ompio_fh;
    ret = ompi_request_wait ( &fp->f_split_coll_req, status );

    /* remove the flag again */
    fp->f_split_coll_in_use = false;

    return ret;
}
