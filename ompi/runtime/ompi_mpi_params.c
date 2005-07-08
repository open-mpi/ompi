/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"
#include "opal/util/output.h"
#include "mca/base/mca_base_param.h"
#include <time.h>

/*
 * Global variables
 *
 * As a deviation from the norm, ompi_mpi_param_check is also
 * extern'ed in src/mpi/interface/c/bindings.h because it is already
 * included in all MPI function imlementation files
 *
 * The values below are the default values.
 */
bool ompi_mpi_param_check = true;
bool ompi_debug_show_handle_leaks = false;
bool ompi_debug_no_free_handles = false;
bool ompi_mpi_show_mca_params = false;
char *ompi_mpi_show_mca_params_file = NULL;

int ompi_mpi_register_params(void)
{
    int param_check_param;
    int show_leaks_param;
    int no_free_param;
    int show_mca_params;
    int show_mca_params_file;
    int value;

    /* Whether we want MPI API function parameter checking or not */
    
    param_check_param = 
        mca_base_param_register_int("mpi", NULL, "param_check", NULL,
                                    MPI_PARAM_CHECK);
    mca_base_param_lookup_int(param_check_param, &value);
    ompi_mpi_param_check = (bool) value;
    if (ompi_mpi_param_check) {
        value = 0;
        if (MPI_PARAM_CHECK) {
            value = 1;
        }
        if (0 == value) {
            opal_output(0, "WARNING: MCA parameter mpi_param_check set to true, but parameter checking");
            opal_output(0, "WARNING: has been compiled out of Open MPI.  mpi_param_check value ignored.");
            ompi_mpi_param_check = false;
        }
    }

    /*
     * This string is going to be used in src/util/showstackframe.c
     */
    mca_base_param_register_string("mpi", NULL, "signal", NULL, NULL);
    

    /*
     * opal_progress: decide whether to yield and the event library
     * tick rate
     */
    mca_base_param_register_int("mpi", NULL, "yield_when_idle", NULL, -1);
    mca_base_param_register_int("mpi", NULL, "event_tick_rate", NULL, -1);


    /* Whether or not to show MPI handle leaks */
    
    show_leaks_param = 
        mca_base_param_register_int("mpi", NULL, "show_handle_leaks", NULL,
                                    (int) ompi_debug_show_handle_leaks);
    mca_base_param_lookup_int(show_leaks_param, &value);
    ompi_debug_show_handle_leaks = (bool) value;
    
    /* Whether or not to free MPI handles.  Useless without run-time
       param checking, so implicitly set that to true if we don't want
       to free the handles. */
    
    no_free_param =
        mca_base_param_register_int("mpi", NULL, "no_free_handles", NULL,
                                    (int) ompi_debug_no_free_handles);
    mca_base_param_lookup_int(no_free_param, &value);
    ompi_debug_no_free_handles = (bool) value;
    if (ompi_debug_no_free_handles) {
        ompi_mpi_param_check = true;
        value = 0;
        if (MPI_PARAM_CHECK) {
            value = 1;
        }
        if (0 == value) {
            opal_output(0, "WARNING: MCA parameter mpi_no_free_handles set to true, but MPI");
            opal_output(0, "WARNING: parameter checking has been compiled out of Open MPI.");
            opal_output(0, "WARNING: mpi_no_free_handles is therefore only partially effective!");
        }
    }

    /* Whether or not to print all MCA parameters in MPI_INIT */
    show_mca_params =
       mca_base_param_register_int("mpi", NULL, 
                                   "show_mca_params", NULL,
                                   (int) ompi_mpi_show_mca_params);
    mca_base_param_lookup_int(show_mca_params, &value);
    ompi_mpi_show_mca_params = (bool) value;
    mca_base_param_set_internal(show_mca_params, false);

    /* File to use when dumping the parameters */
    ompi_mpi_show_mca_params_file = strdup("");
    show_mca_params_file =
       mca_base_param_register_string("mpi", NULL, "show_mca_params_file", NULL,
                                      ompi_mpi_show_mca_params_file);
    mca_base_param_lookup_string(show_mca_params_file, &ompi_mpi_show_mca_params_file);
    mca_base_param_set_internal(show_mca_params_file, false);
    
    /* All done */

    return OMPI_SUCCESS;
}

int ompi_show_all_mca_params(int32_t rank, int requested, char *nodename) {
   opal_list_t *info;
   opal_list_item_t *i;
   mca_base_param_info_t *item;
   char *value_string;
   int value_int;
   FILE *fp;
   time_t timestamp;

   if (rank != 0) {
      return OMPI_SUCCESS;
   }

   timestamp = time(NULL);

   /* Open the file if one is specified */
   if (0 != strlen(ompi_mpi_show_mca_params_file)) {
      if ( NULL == (fp = fopen(ompi_mpi_show_mca_params_file, "w")) ) {
         opal_output(0, "Unable to open file <%s> to write MCA parameters", ompi_mpi_show_mca_params_file);
         return OMPI_ERR_FILE_OPEN_FAILURE;
      }
      fprintf(fp, "#\n");
      fprintf(fp, "# This file was automatically generated on %s", ctime(&timestamp));
      fprintf(fp, "# by MPI_COMM_WORLD rank %d (out of a total of %d) on %s\n", rank, requested, nodename );
      fprintf(fp, "#\n");
   }

   mca_base_param_dump(&info, false);
   for (i =  opal_list_get_first(info); 
        i != opal_list_get_last(info);
        i =  opal_list_get_next(i)) {
      item = (mca_base_param_info_t*) i;

      /* Get the parameter name, and convert it to a printable string */
      if (MCA_BASE_PARAM_TYPE_STRING == item->mbpp_type) {
         mca_base_param_lookup_string(item->mbpp_index, &value_string);
         if (NULL == value_string) {
            value_string = strdup("");
         }
      } else {
         mca_base_param_lookup_int(item->mbpp_index, &value_int);
         asprintf(&value_string, "%d", value_int);
      }

      /* Print the parameter */
      if (0 != strlen(ompi_mpi_show_mca_params_file)) {
         fprintf(fp, "%s=%s\n", item->mbpp_full_name, value_string);
      } else {
         opal_output(0, "%s=%s", item->mbpp_full_name, value_string);
      }

      free(value_string);
   }

   /* Close file, cleanup allocated memory*/
   if (0 != strlen(ompi_mpi_show_mca_params_file)) {
      fclose(fp);
   }
   mca_base_param_dump_release(info);

   return OMPI_SUCCESS;
}
