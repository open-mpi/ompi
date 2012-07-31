#ifndef MCA_IO_OMPIO_NBC_H
#define MCA_IO_OMPIO_NBC_H

#include "io_ompio.h"

/* Function declaration for get and utility method to use with libNBC 
   implementation in io_ompio_nbc.c */
OMPI_DECLSPEC int mca_io_ompio_get_fcoll_dynamic_num_io_procs (int *num_procs);
OMPI_DECLSPEC int mca_io_ompio_get_fcoll_dynamic_cycle_buffer_size (int *cycle_buffer_size);
OMPI_DECLSPEC int mca_io_ompio_get_fcoll_dynamic_constant_cbs (int *constant_cbs);
OMPI_DECLSPEC int mca_io_ompio_get_f_aggregator_index (ompi_file_t *fh);
OMPI_DECLSPEC int mca_io_ompio_get_f_procs_in_group (ompi_file_t *fh, 
						     int **value);
OMPI_DECLSPEC int mca_io_ompio_get_f_procs_per_group (ompi_file_t *fh);
OMPI_DECLSPEC int mca_io_ompio_get_f_comm (ompi_file_t *fh, 
					   ompi_communicator_t **value);
OMPI_DECLSPEC int mca_io_ompio_get_iov_type (ompi_file_t *fh, 
					     ompi_datatype_t **value);
OMPI_DECLSPEC signed int mca_io_ompio_get_f_flags (ompi_file_t *fh);
OMPI_DECLSPEC int mca_io_ompio_get_fd (ompi_file_t *fh);
OMPI_DECLSPEC int mca_io_ompio_get_f_num_of_io_entries (ompi_file_t *fh);
OMPI_DECLSPEC int mca_io_ompio_get_f_io_array (ompi_file_t *fh, 
					       mca_io_ompio_io_array_t **f_io_array);
OMPI_DECLSPEC int mca_io_ompio_free_f_io_array (ompi_file_t *fh);

OMPI_DECLSPEC int mca_io_ompio_get_datatype_size (ompi_datatype_t *datatype);
OMPI_DECLSPEC int mca_io_ompio_decode_datatype_external(ompi_file_t *fh, 
							struct ompi_datatype_t *datatype,
							int count,
							void *buf,
							size_t *max_data,
							struct iovec **iov,
							uint32_t *iov_count);
OMPI_DECLSPEC int mca_io_ompio_generate_current_file_view (ompi_file_t *fp,
							  size_t max_data,
							  struct iovec **f_iov,
							  int *iov_count);   
OMPI_DECLSPEC int mca_io_ompio_set_aggregator_props (ompi_file_t *fh,
						     int num_aggregators,
						     size_t bytes_per_proc);
OMPI_DECLSPEC int mca_io_ompio_generate_io_array (ompi_file_t *file,
						  struct iovec *global_view,
						  int *tglobal_count, 
						  int *fview_count,
						  int *bytes_per_process, 
						  char *global_buf,
						  int *tblocks,
						  int *sorted, 
						  int *nvalue,
						  int *bytes_left,
						  int *sorted_index);
OMPI_DECLSPEC int mca_io_ompio_datatype_is_contiguous (ompi_datatype_t *datatype, 
						       ompi_file_t *fp);
OMPI_DECLSPEC int mca_io_ompio_non_contiguous_create_send_buf (int *bytes_sent, 
							       struct iovec *decoded_iov, 
							       char *send_buf);
OMPI_DECLSPEC int mca_io_ompio_non_contiguous_create_receive_buf(int *bytes_received,
								 struct iovec *decoded_iov,
								 char *receive_buf);

/* libNBC utility methods declarations ends here */

#endif /*MCA_IO_OMPIO_NBC_H*/
