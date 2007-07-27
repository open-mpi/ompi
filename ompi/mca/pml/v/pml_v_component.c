/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
 
#include "ompi_config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD
# include <unistd.h>
#endif

#include "opal/event/event.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "ompi/mca/pml/base/base.h"
#include "pml_v.h"
#include "pml_v_protocol_base.h"

static int mca_pml_v_component_open(void);
static int mca_pml_v_component_close(void);
static int mca_pml_v_component_parasite_close(void);

static mca_pml_base_module_t *mca_pml_v_component_init(int* priority, bool enable_threads, bool enable_progress_threads);
static int mca_pml_v_component_finalize(void);
static int mca_pml_v_component_parasite_finalize(void);

static inline int mca_pml_v_param_register_int( const char* param_name, int default_value);
static inline char *mca_pml_v_param_register_string( const char* param_name, char *default_value);

mca_pml_base_component_1_0_0_t mca_pml_v_component = 
{
  /* First, the mca_base_component_t struct containing meta
   * information about the component itself */
  {
    MCA_PML_BASE_VERSION_1_0_0, /* Indicate that we are a pml v1.0.0 component (which also implies a specific MCA version) */
    "v", /* MCA component name */
    1,   /* MCA component major version */
    0,   /* MCA component minor version */
    0,   /* MCA component release version */
    mca_pml_v_component_open,
    mca_pml_v_component_close
  },

  /* Next the MCA v1.0.0 component meta data */
  {
    false /* Whether the component is checkpointable or not */
  },

  mca_pml_v_component_init,  /* component init */
  mca_pml_v_component_finalize   /* component finalize */
};

/******************************************************************************/
/* MCA level functions */

/* Register MCA parameters and setup verbose output system
 */
static int mca_pml_v_component_open(void)
{
    char *output;
    int verbose;
  
    output = mca_pml_v_param_register_string("output", "stderr");
    verbose = mca_pml_v_param_register_int("verbose", 0);

    pml_v_output_init(output, verbose);

    V_OUTPUT_VERBOSE(1, "loading pml_v");
    return mca_pml_v_protocol_base_load_all();
}
 
/* As any parasit, I dont want to die, so I will grab the mca_pml interface 
 * and put those from the selected vprotocol instead. Once this is done, 
 * I will do my stuff and call the real func and die only when my host pml 
 * will. 
 */
static int mca_pml_v_component_close(void)
{  
  /* choose one protocol (ignoring errors as we will be discarded soon if
   some occured) */
  /* TODO: dirty trick until pml_base_select gets fixed - have to be moved back to finalize */
  /* TODO: check for bozo case: no selected PML */
  /*  mca_pml_v_protocol_base_select(enable_progress_threads, enable_mpi_threads); */


    /* Gather some informations about the environment and selects the best protocol
 * considering environment and priority */
  mca_pml_v_protocol_base_select(0, 0);

  if(!strcmp(mca_pml_v.protocol_component.pmlm_version.mca_type_name, "vprotocol"))
  {
    /* ok, we have loaded a fault tolerant protocol, lets go */
    V_OUTPUT_VERBOSE(10, "pml_v.close: I don't want to die, I will parasite %s host component %s", 
                        mca_pml_base_selected_component.pmlm_version.mca_type_name,
                        mca_pml_base_selected_component.pmlm_version.mca_component_name);
      
    /* setting selected vprotocol mpi functions instead of host's one */
    mca_pml_v.host_pml = mca_pml; /* saving */
    /* protocol stuff */
    if(mca_pml_v.protocol.add_procs) mca_pml.pml_add_procs = mca_pml_v.protocol.add_procs;
    if(mca_pml_v.protocol.del_procs) mca_pml.pml_del_procs = mca_pml_v.protocol.del_procs;
    if(mca_pml_v.protocol.enable) mca_pml.pml_enable = mca_pml_v.protocol.enable;
    if(mca_pml_v.protocol.progress) mca_pml.pml_progress = mca_pml_v.protocol.progress;
    if(mca_pml_v.protocol.add_comm) mca_pml.pml_add_comm = mca_pml_v.protocol.add_comm;
    if(mca_pml_v.protocol.del_comm) mca_pml.pml_del_comm = mca_pml_v.protocol.del_comm;
    if(mca_pml_v.protocol.irecv_init) mca_pml.pml_irecv_init = mca_pml_v.protocol.irecv_init;
    if(mca_pml_v.protocol.irecv) mca_pml.pml_irecv = mca_pml_v.protocol.irecv;
    if(mca_pml_v.protocol.recv) mca_pml.pml_recv = mca_pml_v.protocol.recv;
    if(mca_pml_v.protocol.isend_init) mca_pml.pml_isend_init = mca_pml_v.protocol.isend_init;
    if(mca_pml_v.protocol.isend) mca_pml.pml_isend = mca_pml_v.protocol.isend;
    if(mca_pml_v.protocol.send) mca_pml.pml_send = mca_pml_v.protocol.send;
    if(mca_pml_v.protocol.iprobe) mca_pml.pml_iprobe = mca_pml_v.protocol.iprobe;
    if(mca_pml_v.protocol.probe) mca_pml.pml_probe = mca_pml_v.protocol.probe;
    if(mca_pml_v.protocol.start) mca_pml.pml_start = mca_pml_v.protocol.start;
    if(mca_pml_v.protocol.dump) mca_pml.pml_dump = mca_pml_v.protocol.dump;

    /* Add some extra space for Vprotocol at the end of each PML request
     * build a custom request obj class from the original PML request class
     * with enough trailing space and rebuild the request pool
     */
    if(mca_pml_v.protocol.req_recv_class)
    {
      ompi_free_list_t pml_fl_save = mca_pml_base_recv_requests;
      mca_pml_v.host_pml_req_recv_size = pml_fl_save.fl_elem_class->cls_sizeof;
      V_OUTPUT_VERBOSE(300, "req_rebuild: recv\tsize %ld+%ld\talignment=%ld", (long) mca_pml_v.host_pml_req_recv_size, (long) mca_pml_v.protocol.req_recv_class->cls_sizeof, (long) pml_fl_save.fl_alignment);
      mca_pml_v.protocol.req_recv_class->cls_parent = pml_fl_save.fl_elem_class; 
      mca_pml_v.protocol.req_recv_class->cls_sizeof += pml_fl_save.fl_elem_class->cls_sizeof;
      /* rebuild the requests free list with the right size */
      OBJ_DESTRUCT(&mca_pml_base_recv_requests);
      OBJ_CONSTRUCT(&mca_pml_base_recv_requests, ompi_free_list_t);
      ompi_free_list_init_ex(&mca_pml_base_recv_requests,
                              mca_pml_v.protocol.req_recv_class->cls_sizeof,
                              pml_fl_save.fl_alignment,
                              mca_pml_v.protocol.req_recv_class,
                              pml_fl_save.fl_num_allocated,
                              pml_fl_save.fl_max_to_alloc,
                              pml_fl_save.fl_num_per_alloc,
                              pml_fl_save.fl_mpool,
                              pml_fl_save.item_init,
                              pml_fl_save.ctx);
    }
    if(mca_pml_v.protocol.req_send_class)
    {
      ompi_free_list_t pml_fl_save = mca_pml_base_send_requests;
      mca_pml_v.host_pml_req_send_size = pml_fl_save.fl_elem_class->cls_sizeof;
      V_OUTPUT_VERBOSE(300, "req_rebuild: send\tsize %ld+%ld\talignment=%ld", (long) mca_pml_v.host_pml_req_send_size, (long) mca_pml_v.protocol.req_send_class->cls_sizeof, (long) pml_fl_save.fl_alignment);
      mca_pml_v.protocol.req_send_class->cls_parent = pml_fl_save.fl_elem_class; 
      mca_pml_v.protocol.req_send_class->cls_sizeof += pml_fl_save.fl_elem_class->cls_sizeof;
      /* rebuild the requests free list with the right size */
      OBJ_DESTRUCT(&mca_pml_base_send_requests);
      OBJ_CONSTRUCT(&mca_pml_base_send_requests, ompi_free_list_t);
      ompi_free_list_init_ex(&mca_pml_base_send_requests,
                              mca_pml_v.protocol.req_send_class->cls_sizeof,
                              pml_fl_save.fl_alignment,
                              mca_pml_v.protocol.req_send_class,
                              pml_fl_save.fl_num_allocated,
                              pml_fl_save.fl_max_to_alloc,
                              pml_fl_save.fl_num_per_alloc,
                              pml_fl_save.fl_mpool,
                              pml_fl_save.item_init,
                              pml_fl_save.ctx);                             
    }

    /* setting own close and finalize instead of host's one */
    mca_pml_v.host_pml_component = mca_pml_base_selected_component;
    snprintf(mca_pml_base_selected_component.pmlm_version.mca_component_name, 
              MCA_BASE_MAX_TYPE_NAME_LEN, "%s]v%s", 
              mca_pml_v.host_pml_component.pmlm_version.mca_component_name,
              mca_pml_v.protocol_component.pmlm_version.mca_component_name);
    mca_pml_base_selected_component.pmlm_version.mca_close_component = mca_pml_v_component_parasite_close;
    mca_pml_base_selected_component.pmlm_finalize = mca_pml_v_component_parasite_finalize;
#if 0
    /* may be useless ? If not have to parse the entire PML available list to find currently selected */ 
        component->pmlm_version.mca_close_component = mca_pml_v_component_parasite_close;
        component->pmlm_finalize = mca_pml_v_component_parasite_finalize;
#endif 

    V_OUTPUT_VERBOSE(10, "pml_v.close: I don't want to be unloaded. Referencing myself as using myself");
    if(OPAL_SUCCESS != mca_base_component_repository_retain_component("pml", "v"))
    {
      opal_output(0, "pml_v.close: can't retain myself !");
      return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
  }
  V_OUTPUT_VERBOSE(10, "pml_v.close: no fault tolerant protocol selected, ok, I let them kill me");
  return OMPI_SUCCESS;
}

/* MCA replacement close for host parasited pml component */
static int mca_pml_v_component_parasite_close(void)
{
  V_OUTPUT_VERBOSE(10, "pml_v.parasite_close: Ok, host component %s is closing, so I accept to die", 
                      mca_pml_v.host_pml_component.pmlm_version.mca_component_name);
  mca_pml = mca_pml_v.host_pml;
  mca_pml_base_selected_component = mca_pml_v.host_pml_component;

/* TODO: close the vprotocol component opened in open */
  
/* TODO: I have to dlclose myself, but if I do it from my own code it crashes. 
         waiting for somebody else to give me tools to do a cleaner stuff
         for now, the closed component stays in memory
  if(dlclose(myself_dlhandler)) opal_output(mca_pml_v_output, "pml_v.parasite_close: dlclose failed %s", dlerror());
*/

  opal_output_close(mca_pml_v.output);
  return mca_pml_v.host_pml_component.pmlm_version.mca_close_component();
}

/******************************************************************************/
/* MCA_PML level functions */

static mca_pml_base_module_t *mca_pml_v_component_init(int *priority,
                                                      bool enable_progress_threads,
                                                      bool enable_mpi_threads)
{
  V_OUTPUT_VERBOSE(10, "pml_v.init: I'm not supposed to be here until BTL loading stuff gets fixed!? That's bad...");
  /* I NEVER want to be the selected PML, so I report less than possible 
   * priority and a NULL module */
  *priority = -1;
  return NULL;
}

static int mca_pml_v_component_finalize(void)
{
  V_OUTPUT_VERBOSE(10, "pml_v.finalize: I'm not supposed to be here until BTL loading stuff gets fixed!? That's bad...");
  return OMPI_SUCCESS;
}

static int mca_pml_v_component_parasite_finalize(void)
{
  V_OUTPUT_VERBOSE(10, "pml_v.parasite_finalize");

  /* finalize vprotocol component */
  mca_pml_v.protocol_component.pmlm_finalize();
  
  if(mca_pml_v.host_pml_component.pmlm_finalize != NULL)
    return mca_pml_v.host_pml_component.pmlm_finalize();
  else 
    return OMPI_SUCCESS;
}

/******************************************************************************/
/* utilities */

static inline int mca_pml_v_param_register_int( const char* param_name,
                                                  int default_value )
{
    int id = mca_base_param_register_int("pml", "v", param_name, NULL, default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id, &param_value);
    return param_value;
}

static inline char *mca_pml_v_param_register_string( const char* param_name,
                                                  char *default_value )
{
    int id = mca_base_param_register_string("pml", "v", param_name, NULL, default_value);
    char *param_value = default_value;
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
