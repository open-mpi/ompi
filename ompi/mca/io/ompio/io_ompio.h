/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2015 University of Houston. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_IO_OMPIO_H
#define MCA_IO_OMPIO_H

#include <fcntl.h>

#include "mpi.h"
#include "opal/class/opal_list.h"
#include "ompi/errhandler/errhandler.h"
#include "opal/threads/mutex.h"
#include "ompi/file/file.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/request/request.h"

extern int mca_io_ompio_cycle_buffer_size;
extern int mca_io_ompio_bytes_per_agg;
extern int mca_io_ompio_num_aggregators;
extern int mca_io_ompio_record_offset_info;
extern int mca_io_ompio_sharedfp_lazy_open;
extern int mca_io_ompio_grouping_option;
OMPI_DECLSPEC extern int mca_io_ompio_coll_timing_info;

/*
 * Flags
 */
#define OMPIO_CONTIGUOUS_MEMORY      0x00000001
#define OMPIO_UNIFORM_FVIEW          0x00000002
#define OMPIO_FILE_IS_OPEN           0x00000004
#define OMPIO_FILE_VIEW_IS_SET       0x00000008
#define OMPIO_CONTIGUOUS_FVIEW       0x00000010
#define OMPIO_AGGREGATOR_IS_SET      0x00000020
#define OMPIO_SHAREDFP_IS_SET        0x00000040

#define QUEUESIZE 2048
#define MCA_IO_DEFAULT_FILE_VIEW_SIZE 4*1024*1024
#define OMPIO_FCOLL_WANT_TIME_BREAKDOWN 0

#define OMPIO_MIN(a, b) (((a) < (b)) ? (a) : (b))
#define OMPIO_MAX(a, b) (((a) < (b)) ? (b) : (a))

/*
 * General values
 */
#define OMPIO_PREALLOC_MAX_BUF_SIZE   33554432
#define OMPIO_PERM_NULL               -1
#define OMPIO_IOVEC_INITIAL_SIZE      100
#define OMPIO_ROOT                    0
#define OMPIO_MAX_NAME                100
#define OMPIO_TAG_GATHER              -100
#define OMPIO_TAG_GATHERV             -101
#define OMPIO_TAG_BCAST               -102
#define OMPIO_TAG_SCATTERV            -103

/* ACCESS MODES --- not needed.. just use MPI_MODE_... */
#define OMPIO_MODE_CREATE              1
#define OMPIO_MODE_RDONLY              2
#define OMPIO_MODE_WRONLY              4
#define OMPIO_MODE_RDWR                8
#define OMPIO_MODE_DELETE_ON_CLOSE    16
#define OMPIO_MODE_UNIQUE_OPEN        32
#define OMPIO_MODE_EXCL               64
#define OMPIO_MODE_APPEND            128
#define OMPIO_MODE_SEQUENTIAL        256

/* PRINT QUEUES*/
#define WRITE_PRINT_QUEUE 1809
#define READ_PRINT_QUEUE 2178
/*---------------------------*/


/*AGGREGATOR GROUPING DECISIONS*/
#define OMPIO_MERGE                     1
#define OMPIO_SPLIT                     2
#define OMPIO_RETAIN                    3

#define DATA_VOLUME                     1
#define UNIFORM_DISTRIBUTION            2
#define CONTIGUITY                      3
#define OPTIMIZE_GROUPING               4
#define SIMPLE                          5
#define NO_REFINEMENT                   6


#define OMPIO_UNIFORM_DIST_THRESHOLD     0.5
#define OMPIO_CONTG_THRESHOLD        1048576
#define OMPIO_CONTG_FACTOR                 8
#define OMPIO_DEFAULT_STRIPE_SIZE    1048576
#define OMPIO_PROCS_PER_GROUP_TAG          0
#define OMPIO_PROCS_IN_GROUP_TAG           1
#define OMPIO_MERGE_THRESHOLD            0.5

/*---------------------------*/

BEGIN_C_DECLS

enum ompio_fs_type
{
    NONE = 0,
    UFS = 1,
    PVFS2 = 2,
    LUSTRE = 3,
    PLFS = 4
};

OMPI_DECLSPEC extern mca_io_base_component_2_0_0_t mca_io_ompio_component;
/*
 * global variables, instantiated in module.c
 */
extern opal_mutex_t mca_io_ompio_mutex;
extern mca_io_base_module_2_0_0_t mca_io_ompio_module;
OMPI_DECLSPEC extern mca_io_base_component_2_0_0_t mca_io_ompio_component;

typedef struct mca_io_ompio_io_array_t {
    void                 *memory_address;
    /* we need that of type OMPI_MPI_OFFSET_TYPE */
    void                 *offset;
    size_t               length;
    /*mca_io_ompio_server_t io_server;*/
} mca_io_ompio_io_array_t;


typedef struct mca_io_ompio_access_array_t{
    OMPI_MPI_OFFSET_TYPE *offsets;
    int *lens;
    MPI_Aint *mem_ptrs;
    int count;
} mca_io_ompio_access_array_t;

/*Used in extracting offset adj-matrix*/
typedef struct mca_io_ompio_offlen_array_t{
    OMPI_MPI_OFFSET_TYPE offset;
    MPI_Aint             length;
    int                  process_id;
}mca_io_ompio_offlen_array_t;

/*To extract time-information */
typedef struct {
    double time[3];
    int nprocs_for_coll;
    int aggregator;
}mca_io_ompio_print_entry;

typedef struct {
    mca_io_ompio_print_entry entry[QUEUESIZE + 1];
    int first;
    int last;
    int count;
} mca_io_ompio_print_queue;

typedef struct {
	int ndims;
	int *dims;
	int *periods;
	int *coords;
	int reorder;
} cart_topo_components;


typedef struct{
        OMPI_MPI_OFFSET_TYPE contg_chunk_size;
        int *procs_in_contg_group;
	int procs_per_contg_group;
} contg;



/*
 * Function that takes in a datatype and buffer, and decodes that datatype
 * into an iovec using the convertor_raw function
 */

/* forward declaration to keep the compiler happy. */
struct mca_io_ompio_file_t;
typedef int (*mca_io_ompio_decode_datatype_fn_t) (struct mca_io_ompio_file_t *fh,
						  struct ompi_datatype_t *datatype,
						  int count,
						  const void *buf,
						  size_t *max_data,
						  struct iovec **iov,
						  uint32_t *iov_count);
typedef int (*mca_io_ompio_generate_current_file_view_fn_t) (struct mca_io_ompio_file_t *fh,
							     size_t max_data,
							     struct iovec **f_iov,
							     int *iov_count);

/*
 * Function that sorts an io_array according to the offset by filling
 * up an array of the indices into the array (HEAP SORT)
 */
typedef int (*mca_io_ompio_sort_fn_t) (mca_io_ompio_io_array_t *io_array,
				       int num_entries,
				       int *sorted);

typedef int (*mca_io_ompio_sort_iovec_fn_t) (struct iovec *iov,
					     int num_entries,
					     int *sorted);

/* collective operations based on list of participating ranks instead of communicators*/
typedef int (*mca_io_ompio_allgather_array_fn_t) (void *sbuf,
						  int scount,
						  ompi_datatype_t *sdtype,
						  void *rbuf,
						  int rcount,
						  ompi_datatype_t *rdtype,
						  int root_index,
						  int *procs_in_group,
						  int procs_per_group,
						  ompi_communicator_t *comm);

typedef int (*mca_io_ompio_allgatherv_array_fn_t) (void *sbuf,
						   int scount,
						   ompi_datatype_t *sdtype,
						   void *rbuf,
						   int *rcounts,
						   int *disps,
						   ompi_datatype_t *rdtype,
						   int root_index,
						   int *procs_in_group,
						   int procs_per_group,
						   ompi_communicator_t *comm);

typedef int (*mca_io_ompio_gather_array_fn_t) (void *sbuf,
					       int scount,
					       ompi_datatype_t *sdtype,
					       void *rbuf,
					       int rcount,
					       ompi_datatype_t *rdtype,
					       int root_index,
					       int *procs_in_group,
					       int procs_per_group,
					       ompi_communicator_t *comm);
typedef int (*mca_io_ompio_gatherv_array_fn_t) (void *sbuf,
						int scount,
						ompi_datatype_t *sdtype,
						void *rbuf,
						int *rcounts,
						int *disps,
						ompi_datatype_t *rdtype,
						int root_index,
						int *procs_in_group,
						int procs_per_group,
						ompi_communicator_t *comm);

/* functions to retrieve the number of aggregators and the size of the
   temporary buffer on aggregators from the fcoll modules */
typedef void (*mca_io_ompio_get_num_aggregators_fn_t) ( int *num_aggregators);
typedef void (*mca_io_ompio_get_bytes_per_agg_fn_t) ( int *bytes_per_agg);
typedef int (*mca_io_ompio_set_aggregator_props_fn_t) (struct mca_io_ompio_file_t *fh,
							int num_aggregators,
							size_t bytes_per_proc);


typedef int (*mca_io_ompio_full_print_queue_fn_t) (int queue_type);
typedef int (*mca_io_ompio_register_print_entry_fn_t) (int queue_type,
							mca_io_ompio_print_entry x);


/**
 * Back-end structure for MPI_File
 */
struct mca_io_ompio_file_t {
    /* General parameters */
    int                    fd;
    struct ompi_file_t    *f_fh;     /* pointer back to the file_t structure */
    OMPI_MPI_OFFSET_TYPE   f_offset; /* byte offset of current position */
    OMPI_MPI_OFFSET_TYPE   f_disp;   /* file_view displacement */
    int                    f_rank;
    int                    f_size;
    int                    f_amode;
    int                    f_perm;
    ompi_communicator_t   *f_comm;
    const char            *f_filename;
    char                  *f_datarep;
    opal_convertor_t      *f_convertor;
    ompi_info_t           *f_info;
    int32_t                f_flags;
    void                  *f_fs_ptr;
    int                    f_atomicity;
    size_t                 f_stripe_size;
    int                    f_stripe_count;
    size_t                 f_cc_size;
    int                    f_bytes_per_agg;
    enum ompio_fs_type     f_fstype;
    ompi_request_t        *f_split_coll_req;
    bool                   f_split_coll_in_use;
    /* Place for selected sharedfp module to hang it's data.
       Note: Neither f_sharedfp nor f_sharedfp_component seemed appropriate for this.
    */
    void                  *f_sharedfp_data;

    /* process grouping parameters */
    int *f_procs_in_group;
    int  f_procs_per_group;
    int  f_aggregator_index;

    /* File View parameters */
    struct iovec     *f_decoded_iov;
    uint32_t          f_iov_count;
    ompi_datatype_t  *f_iov_type;
    size_t            f_position_in_file_view; /* in bytes */
    size_t            f_total_bytes; /* total bytes read/written within 1 Fview*/
    int               f_index_in_file_view;
    OPAL_PTRDIFF_TYPE f_view_extent;
    size_t            f_view_size;
    ompi_datatype_t  *f_etype;
    ompi_datatype_t  *f_filetype;
    ompi_datatype_t  *f_orig_filetype; /* the fileview passed by the user to us */
    size_t            f_etype_size;

    /* contains IO requests that needs to be read/written */
    mca_io_ompio_io_array_t *f_io_array;
    int                     f_num_of_io_entries;

    /* Hooks for modules to hang things */
    mca_base_component_t *f_fs_component;
    mca_base_component_t *f_fcoll_component;
    mca_base_component_t *f_fbtl_component;
    mca_base_component_t *f_sharedfp_component;

    /* structure of function pointers */
    mca_fs_base_module_t       *f_fs;
    mca_fcoll_base_module_t    *f_fcoll;
    mca_fbtl_base_module_t     *f_fbtl;
    mca_sharedfp_base_module_t *f_sharedfp;

    /* No Error handling done yet
    struct ompi_errhandler_t *error_handler;
    ompi_errhandler_type_t errhandler_type;
    */

    /*initial list of aggregators and groups*/
    int *f_init_aggr_list;
    int  f_init_num_aggrs;

    int f_init_procs_per_group;
    int *f_init_procs_in_group;

    int f_final_num_aggrs;

    /* internal ompio functions required by fbtl and fcoll */
    mca_io_ompio_decode_datatype_fn_t                       f_decode_datatype;
    mca_io_ompio_generate_current_file_view_fn_t f_generate_current_file_view;

    mca_io_ompio_sort_fn_t                                             f_sort;
    mca_io_ompio_sort_iovec_fn_t                                 f_sort_iovec;

    mca_io_ompio_allgather_array_fn_t                       f_allgather_array;
    mca_io_ompio_allgatherv_array_fn_t                     f_allgatherv_array;
    mca_io_ompio_gather_array_fn_t                             f_gather_array;
    mca_io_ompio_gatherv_array_fn_t                           f_gatherv_array;

    mca_io_ompio_get_num_aggregators_fn_t               f_get_num_aggregators;
    mca_io_ompio_get_bytes_per_agg_fn_t                   f_get_bytes_per_agg;
    mca_io_ompio_set_aggregator_props_fn_t             f_set_aggregator_props;

    mca_io_ompio_full_print_queue_fn_t                     f_full_print_queue;
    mca_io_ompio_register_print_entry_fn_t             f_register_print_entry;
};
typedef struct mca_io_ompio_file_t mca_io_ompio_file_t;

struct mca_io_ompio_data_t {
    mca_io_ompio_file_t ompio_fh;
};
typedef struct mca_io_ompio_data_t mca_io_ompio_data_t;

OMPI_DECLSPEC extern mca_io_ompio_print_queue *coll_write_time;
OMPI_DECLSPEC extern mca_io_ompio_print_queue *coll_read_time;

/* functions to retrieve the number of aggregators and the size of the
   temporary buffer on aggregators from the fcoll modules */
OMPI_DECLSPEC void mca_io_ompio_get_num_aggregators ( int *num_aggregators);
OMPI_DECLSPEC void mca_io_ompio_get_bytes_per_agg ( int *bytes_per_agg);


OMPI_DECLSPEC int mca_io_ompio_build_io_array ( mca_io_ompio_file_t *fh, int index, int cycles,
						size_t bpc, int max_data, uint32_t iov_count,
						struct iovec *decoded_iov, int *ii, int *jj,
						size_t *tbw );

OMPI_DECLSPEC int ompi_io_ompio_set_file_defaults (mca_io_ompio_file_t *fh);

OMPI_DECLSPEC int ompio_io_ompio_file_open (ompi_communicator_t *comm,
                                            const char *filename,
                                            int amode,
                                            ompi_info_t *info,
                                            mca_io_ompio_file_t *fh,bool use_sharedfp);

OMPI_DECLSPEC int ompio_io_ompio_file_write_at (mca_io_ompio_file_t *fh,
                                                OMPI_MPI_OFFSET_TYPE offset,
                                                const void *buf,
                                                int count,
                                                struct ompi_datatype_t *datatype,
                                                ompi_status_public_t *status);

OMPI_DECLSPEC int ompio_io_ompio_file_write (mca_io_ompio_file_t *fh,
                                             const void *buf,
                                             int count,
                                             struct ompi_datatype_t *datatype,
                                             ompi_status_public_t *status);

OMPI_DECLSPEC int ompio_io_ompio_file_close (mca_io_ompio_file_t *fh);

OMPI_DECLSPEC int ompio_io_ompio_file_write_at_all (mca_io_ompio_file_t *fh,
                                                    OMPI_MPI_OFFSET_TYPE offset,
                                                    const void *buf,
                                                    int count,
                                                    struct ompi_datatype_t *datatype,
                                                    ompi_status_public_t *status);

OMPI_DECLSPEC int ompio_io_ompio_file_iwrite_at (mca_io_ompio_file_t *fh,
                                                 OMPI_MPI_OFFSET_TYPE offset,
                                                 const void *buf,
                                                 int count,
                                                 struct ompi_datatype_t *datatype,
                                                 ompi_request_t **request);

OMPI_DECLSPEC int ompio_io_ompio_file_iwrite (mca_io_ompio_file_t *fh,
                                              const void *buf,
                                              int count,
                                              struct ompi_datatype_t *datatype,
                                              ompi_request_t **request);

OMPI_DECLSPEC int ompio_io_ompio_file_iwrite_at_all (mca_io_ompio_file_t *fh,
						     OMPI_MPI_OFFSET_TYPE offset,
						     const void *buf,
						     int count,
						     struct ompi_datatype_t *datatype,
						     ompi_request_t **request);
OMPI_DECLSPEC int ompio_io_ompio_file_iread (mca_io_ompio_file_t *fh,
					     void *buf,
					     int count,
					     struct ompi_datatype_t *datatype,
					     ompi_request_t **request);

OMPI_DECLSPEC int ompio_io_ompio_file_read (mca_io_ompio_file_t *fh,
                                            void *buf,
                                            int count,
                                            struct ompi_datatype_t *datatype,
                                            ompi_status_public_t *status);
OMPI_DECLSPEC int ompio_io_ompio_file_iread_at (mca_io_ompio_file_t *fh,
                                                OMPI_MPI_OFFSET_TYPE offset,
                                                void *buf,
                                                int count,
                                                struct ompi_datatype_t *datatype,
                                                ompi_request_t **request);
OMPI_DECLSPEC int ompio_io_ompio_file_iread_at_all (mca_io_ompio_file_t *fh,
						    OMPI_MPI_OFFSET_TYPE offset,
						    void *buf,
						    int count,
						    struct ompi_datatype_t *datatype,
						    ompi_request_t **request);
OMPI_DECLSPEC int ompio_io_ompio_file_read_at (mca_io_ompio_file_t *fh,
                                               OMPI_MPI_OFFSET_TYPE offset,
                                               void *buf,
                                               int count,
                                               struct ompi_datatype_t *datatype,
                                               ompi_status_public_t * status);
OMPI_DECLSPEC int ompio_io_ompio_file_read_at_all (mca_io_ompio_file_t *fh,
                                                   OMPI_MPI_OFFSET_TYPE offset,
                                                   void *buf,
                                                   int count,
                                                   struct ompi_datatype_t *datatype,
                                                   ompi_status_public_t * status);
OMPI_DECLSPEC int ompio_io_ompio_file_get_size (mca_io_ompio_file_t *fh,
                                                OMPI_MPI_OFFSET_TYPE *size);

OMPI_DECLSPEC int ompio_io_ompio_file_get_position (mca_io_ompio_file_t *fh,
						    OMPI_MPI_OFFSET_TYPE *offset);

/*
 * Function that takes in a datatype and buffer, and decodes that datatype
 * into an iovec using the convertor_raw function
 */
OMPI_DECLSPEC int ompi_io_ompio_decode_datatype (struct mca_io_ompio_file_t *fh,
                                                 struct ompi_datatype_t *datatype,
                                                 int count,
                                                 const void *buf,
                                                 size_t *max_data,
                                                 struct iovec **iov,
                                                 uint32_t *iov_count);

/*
 * Function that sorts an io_array according to the offset by filling
 * up an array of the indices into the array (HEAP SORT)
 */
OMPI_DECLSPEC int ompi_io_ompio_sort (mca_io_ompio_io_array_t *io_array,
                                      int num_entries,
                                      int *sorted);

OMPI_DECLSPEC int ompi_io_ompio_sort_iovec (struct iovec *iov,
					    int num_entries,
					    int *sorted);

OMPI_DECLSPEC int ompi_io_ompio_sort_offlen (mca_io_ompio_offlen_array_t *io_array,
                                             int num_entries,
                                             int *sorted);



OMPI_DECLSPEC int ompi_io_ompio_set_explicit_offset (mca_io_ompio_file_t *fh,
						     OMPI_MPI_OFFSET_TYPE offset);

OMPI_DECLSPEC int ompi_io_ompio_generate_current_file_view (struct mca_io_ompio_file_t *fh,
                                                            size_t max_data,
                                                            struct iovec **f_iov,
                                                            int *iov_count);

OMPI_DECLSPEC int ompi_io_ompio_generate_groups (mca_io_ompio_file_t *fh,
                                                 int num_aggregators,
                                                 int *root,
                                                 int *procs_per_group,
						 int **ranks);

/*Aggregator selection methods*/
OMPI_DECLSPEC int ompi_io_ompio_set_aggregator_props (struct mca_io_ompio_file_t *fh,
                                                      int num_aggregators,
                                                      size_t bytes_per_proc);


int mca_io_ompio_cart_based_grouping(mca_io_ompio_file_t *ompio_fh);

int mca_io_ompio_fview_based_grouping(mca_io_ompio_file_t *fh,
		                                    int *num_groups,
						    contg *contg_groups);
int mca_io_ompio_simple_grouping(mca_io_ompio_file_t *fh,
                                 int *num_groups,
                                 contg *contg_groups);

int mca_io_ompio_finalize_initial_grouping(mca_io_ompio_file_t *fh,
                                           int num_groups,
                                           contg *contg_groups);


OMPI_DECLSPEC int ompi_io_ompio_break_file_view (mca_io_ompio_file_t *fh,
                                                 struct iovec *iov,
                                                 int count,
                                                 int num_aggregators,
                                                 size_t stripe_size,
                                                 struct iovec **broken_iov,
                                                 int *broken_count);

OMPI_DECLSPEC int ompi_io_ompio_distribute_file_view (mca_io_ompio_file_t *fh,
                                                      struct iovec *broken_iov,
                                                      int broken_count,
                                                      int num_aggregators,
                                                      size_t stripe_size,
                                                      int **fview_count,
                                                      struct iovec **iov,
                                                      int *count);

OMPI_DECLSPEC int ompi_io_ompio_gather_data (mca_io_ompio_file_t *fh,
                                             void *send_buf,
                                             size_t total_bytes_sent,
                                             int *bytes_sent,
                                             struct iovec *broken_iovec,
                                             int broken_index,
                                             size_t partial,
                                             void *global_buf,
                                             int *bytes_per_process,
                                             int *displs,
                                             int num_aggregators,
                                             size_t stripe_size);

OMPI_DECLSPEC int ompi_io_ompio_scatter_data (mca_io_ompio_file_t *fh,
                                              void *receive_buf,
                                              size_t total_bytes_recv,
                                              int *bytes_received,
                                              struct iovec *broken_iovec,
                                              int broken_index,
                                              size_t partial,
                                              void *global_buf,
                                              int *bytes_per_process,
                                              int *displs,
                                              int num_aggregators,
                                              size_t stripe_size);


/*
 * Modified versions of Collective operations
 * Based on an array of procs in group
 */
OMPI_DECLSPEC int ompi_io_ompio_gatherv_array (void *sbuf,
                                               int scount,
                                               ompi_datatype_t *sdtype,
                                               void *rbuf,
                                               int *rcounts,
                                               int *disps,
                                               ompi_datatype_t *rdtype,
                                               int root_index,
                                               int *procs_in_group,
                                               int procs_per_group,
                                               ompi_communicator_t *comm);
OMPI_DECLSPEC int ompi_io_ompio_scatterv_array (void *sbuf,
                                                int *scounts,
                                                int *disps,
                                                ompi_datatype_t *sdtype,
                                                void *rbuf,
                                                int rcount,
                                                ompi_datatype_t *rdtype,
                                                int root_index,
                                                int *procs_in_group,
                                                int procs_per_group,
                                                ompi_communicator_t *comm);
OMPI_DECLSPEC int ompi_io_ompio_allgather_array (void *sbuf,
                                                 int scount,
                                                 ompi_datatype_t *sdtype,
                                                 void *rbuf,
                                                 int rcount,
                                                 ompi_datatype_t *rdtype,
                                                 int root_index,
                                                 int *procs_in_group,
                                                 int procs_per_group,
                                                 ompi_communicator_t *comm);

OMPI_DECLSPEC int ompi_io_ompio_allgatherv_array (void *sbuf,
                                                  int scount,
                                                  ompi_datatype_t *sdtype,
                                                  void *rbuf,
                                                  int *rcounts,
                                                  int *disps,
                                                  ompi_datatype_t *rdtype,
                                                  int root_index,
                                                  int *procs_in_group,
                                                  int procs_per_group,
                                                  ompi_communicator_t *comm);
OMPI_DECLSPEC int ompi_io_ompio_gather_array (void *sbuf,
                                              int scount,
                                              ompi_datatype_t *sdtype,
                                              void *rbuf,
                                              int rcount,
                                              ompi_datatype_t *rdtype,
                                              int root_index,
                                              int *procs_in_group,
                                              int procs_per_group,
                                              ompi_communicator_t *comm);
OMPI_DECLSPEC int ompi_io_ompio_bcast_array (void *buff,
                                             int count,
                                             ompi_datatype_t *datatype,
                                             int root_index,
                                             int *procs_in_group,
                                             int procs_per_group,
                                             ompi_communicator_t *comm);

OMPI_DECLSPEC int ompi_io_ompio_register_print_entry (int queue_type,
						      mca_io_ompio_print_entry x);

OMPI_DECLSPEC int ompi_io_ompio_unregister_print_entry (int queue_type, mca_io_ompio_print_entry *x);

OMPI_DECLSPEC int ompi_io_ompio_empty_print_queue(int queue_type);

OMPI_DECLSPEC int ompi_io_ompio_full_print_queue(int queue_type);

OMPI_DECLSPEC int ompi_io_ompio_initialize_print_queue(mca_io_ompio_print_queue *q);

OMPI_DECLSPEC int ompi_io_ompio_print_time_info(int queue_type,
						char *name_operation,
						mca_io_ompio_file_t *fh);
int ompi_io_ompio_set_print_queue (mca_io_ompio_print_queue **q,
				   int queue_type);


/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */

int mca_io_ompio_file_set_view (struct ompi_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE disp,
                                struct ompi_datatype_t *etype,
                                struct ompi_datatype_t *filetype,
                                const char *datarep,
                                struct ompi_info_t *info);

int mca_io_ompio_set_view_internal (struct mca_io_ompio_file_t *fh,
				    OMPI_MPI_OFFSET_TYPE disp,
				    struct ompi_datatype_t *etype,
				    struct ompi_datatype_t *filetype,
				    const char *datarep,
				    struct ompi_info_t *info);

int mca_io_ompio_file_get_view (struct ompi_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE *disp,
                                struct ompi_datatype_t **etype,
                                struct ompi_datatype_t **filetype,
                                char *datarep);
int mca_io_ompio_file_open (struct ompi_communicator_t *comm,
                            const char *filename,
                            int amode,
                            struct ompi_info_t *info,
                            struct ompi_file_t *fh);
int mca_io_ompio_file_close (struct ompi_file_t *fh);
int mca_io_ompio_file_delete (const char *filename,
                              struct ompi_info_t *info);
int mca_io_ompio_file_set_size (struct ompi_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE size);
int mca_io_ompio_file_preallocate (struct ompi_file_t *fh,
                                   OMPI_MPI_OFFSET_TYPE size);
int mca_io_ompio_file_get_size (struct ompi_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE * size);
int mca_io_ompio_file_get_amode (struct ompi_file_t *fh,
                                 int *amode);
int mca_io_ompio_file_set_info (struct ompi_file_t *fh,
                                struct ompi_info_t *info);
int mca_io_ompio_file_get_info (struct ompi_file_t *fh,
                                struct ompi_info_t ** info_used);
int mca_io_ompio_file_sync (struct ompi_file_t *fh);
int mca_io_ompio_file_seek (struct ompi_file_t *fh,
                            OMPI_MPI_OFFSET_TYPE offet,
                            int whence);
/* Section 9.3 */
int mca_io_ompio_file_set_view (struct ompi_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE disp,
                                struct ompi_datatype_t *etype,
                                struct ompi_datatype_t *filetype,
                                const char *datarep,
                                struct ompi_info_t *info);
int mca_io_ompio_file_get_view (struct ompi_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE *disp,
                                struct ompi_datatype_t **etype,
                                struct ompi_datatype_t **filetype,
                                char *datarep);

/* Section 9.4.2 */
int mca_io_ompio_file_read_at (struct ompi_file_t *fh,
                               OMPI_MPI_OFFSET_TYPE offset,
                               void *buf,
                               int count,
                               struct ompi_datatype_t *datatype,
                               ompi_status_public_t *status);
int mca_io_ompio_file_read_at_all (struct ompi_file_t *fh,
                                   OMPI_MPI_OFFSET_TYPE offset,
                                   void *buf,
                                   int count,
                                   struct ompi_datatype_t *datatype,
                                   ompi_status_public_t *status);
int mca_io_ompio_file_write_at (struct ompi_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE offset,
                                const void *buf,
                                int count,
                                struct ompi_datatype_t *datatype,
                                ompi_status_public_t *status);
int mca_io_ompio_file_write_at_all (struct ompi_file_t *fh,
                                    OMPI_MPI_OFFSET_TYPE offset,
                                    const void *buf,
                                    int count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_status_public_t *status);
int mca_io_ompio_file_iread_at (struct ompi_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE offset,
                                void *buf,
                                int count,
                                struct ompi_datatype_t *datatype,
                                ompi_request_t **request);
int mca_io_ompio_file_iwrite_at (struct ompi_file_t *fh,
                                 OMPI_MPI_OFFSET_TYPE offset,
                                 const void *buf,
                                 int count,
                                 struct ompi_datatype_t *datatype,
                                 ompi_request_t **request);

/* Section 9.4.3 */
int mca_io_ompio_file_read (struct ompi_file_t *fh,
                            void *buf,
                            int count,
                            struct ompi_datatype_t *datatype,
                            ompi_status_public_t *status);
int mca_io_ompio_file_read_all (struct ompi_file_t *fh,
                                void *buf,
                                int count,
                                struct ompi_datatype_t *datatype,
                                ompi_status_public_t *status);
int mca_io_ompio_file_iread_all (ompi_file_t *fh,
				void *buf,
				int count,
				struct ompi_datatype_t *datatype,
				 ompi_request_t **request);
int mca_io_ompio_file_iread_at_all (ompi_file_t *fh,
				    OMPI_MPI_OFFSET_TYPE offset,
				    void *buf,
				    int count,
				    struct ompi_datatype_t *datatype,
				    ompi_request_t **request);

int mca_io_ompio_file_write (struct ompi_file_t *fh,
                             const void *buf,
                             int count,
                             struct ompi_datatype_t *datatype,
                             ompi_status_public_t *status);
int mca_io_ompio_file_write_all (struct ompi_file_t *fh,
                                 const void *buf,
                                 int count,
                                 struct ompi_datatype_t *datatype,
                                 ompi_status_public_t *status);
int mca_io_ompio_file_iwrite_all (ompi_file_t *fh,
				  const void *buf,
				  int count,
				  struct ompi_datatype_t *datatype,
				  ompi_request_t **request);
int mca_io_ompio_file_iwrite_at_all (ompi_file_t *fh,
				     OMPI_MPI_OFFSET_TYPE offset,
				     const void *buf,
				     int count,
				     struct ompi_datatype_t *datatype,
				     ompi_request_t **request);
int mca_io_ompio_file_iread (struct ompi_file_t *fh,
                             void *buf,
                             int count,
                             struct ompi_datatype_t *datatype,
                             ompi_request_t **request);
int mca_io_ompio_file_iwrite (struct ompi_file_t *fh,
                              const void *buf,
                              int count,
                              struct ompi_datatype_t *datatype,
                              ompi_request_t **request);
int mca_io_ompio_file_seek (struct ompi_file_t *fh,
                            OMPI_MPI_OFFSET_TYPE offset,
                            int whence);
int mca_io_ompio_file_get_position (struct ompi_file_t *fh,
                                    OMPI_MPI_OFFSET_TYPE *offset);
int mca_io_ompio_file_get_byte_offset (struct ompi_file_t *fh,
                                       OMPI_MPI_OFFSET_TYPE offset,
                                       OMPI_MPI_OFFSET_TYPE *disp);

/* Section 9.4.4 */
int mca_io_ompio_file_read_shared (struct ompi_file_t *fh,
                                   void *buf,
                                   int count,
                                   struct ompi_datatype_t *datatype,
                                   ompi_status_public_t *status);
int mca_io_ompio_file_write_shared (struct ompi_file_t *fh,
                                    const void *buf,
                                    int count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_status_public_t *status);
int mca_io_ompio_file_iread_shared (struct ompi_file_t *fh,
                                    void *buf,
                                    int count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_request_t **request);
int mca_io_ompio_file_iwrite_shared (struct ompi_file_t *fh,
                                     const void *buf,
                                     int count,
                                     struct ompi_datatype_t *datatype,
                                     ompi_request_t **request);
int mca_io_ompio_file_read_ordered (struct ompi_file_t *fh,
                                    void *buf,
                                    int count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_status_public_t *status);
int mca_io_ompio_file_write_ordered (struct ompi_file_t *fh,
                                     const void *buf,
                                     int count,
                                     struct ompi_datatype_t *datatype,
                                     ompi_status_public_t *status);
int mca_io_ompio_file_seek_shared (struct ompi_file_t *fh,
                                   OMPI_MPI_OFFSET_TYPE offset,
                                   int whence);
int mca_io_ompio_file_get_position_shared (struct ompi_file_t *fh,
                                           OMPI_MPI_OFFSET_TYPE *offset);

/* Section 9.4.5 */
int mca_io_ompio_file_read_at_all_begin (struct ompi_file_t *fh,
                                         OMPI_MPI_OFFSET_TYPE offset,
                                         void *buf,
                                         int count,
                                         struct ompi_datatype_t *datatype);
int mca_io_ompio_file_read_at_all_end (struct ompi_file_t *fh,
                                       void *buf,
                                       ompi_status_public_t *status);
int mca_io_ompio_file_write_at_all_begin (struct ompi_file_t *fh,
                                          OMPI_MPI_OFFSET_TYPE offset,
                                          const void *buf,
                                          int count,
                                          struct ompi_datatype_t *datatype);
int mca_io_ompio_file_write_at_all_end (struct ompi_file_t *fh,
                                        const void *buf,
                                        ompi_status_public_t *status);
int mca_io_ompio_file_read_all_begin (struct ompi_file_t *fh,
                                      void *buf,
                                      int count,
                                      struct ompi_datatype_t *datatype);
int mca_io_ompio_file_read_all_end (struct ompi_file_t *fh,
                                    void *buf,
                                    ompi_status_public_t *status);
int mca_io_ompio_file_write_all_begin (struct ompi_file_t *fh,
                                       const void *buf,
                                       int count,
                                       struct ompi_datatype_t *datatype);
int mca_io_ompio_file_write_all_end (struct ompi_file_t *fh,
                                     const void *buf,
                                     ompi_status_public_t *status);
int mca_io_ompio_file_read_ordered_begin (struct ompi_file_t *fh,
                                          void *buf,
                                          int count,
                                          struct ompi_datatype_t *datatype);
int mca_io_ompio_file_read_ordered_end (struct ompi_file_t *fh,
                                        void *buf,
                                        ompi_status_public_t *status);
int mca_io_ompio_file_write_ordered_begin (struct ompi_file_t *fh,
                                           const void *buf,
                                           int count,
                                           struct ompi_datatype_t *datatype);
int mca_io_ompio_file_write_ordered_end (struct ompi_file_t *fh,
                                         const void *buf,
                                         struct ompi_status_public_t *status);

/* Section 9.5.1 */
int mca_io_ompio_file_get_type_extent (struct ompi_file_t *fh,
                                       struct ompi_datatype_t *datatype,
                                       MPI_Aint *extent);

/* Section 9.6.1 */
int mca_io_ompio_file_set_atomicity (struct ompi_file_t *fh,
                                     int flag);
int mca_io_ompio_file_get_atomicity (struct ompi_file_t *fh,
                                     int *flag);
int mca_io_ompio_file_sync (struct ompi_file_t *fh);
/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */


END_C_DECLS

#endif /* MCA_IO_OMPIO_H */
