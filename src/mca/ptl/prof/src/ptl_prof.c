#include "ptl_prof.h"

static int ptl_prof_add_procs_fn( struct mca_ptl_base_module_t* ptl,
                                  size_t nprocs,
                                  struct ompi_proc_t** procs,
                                  struct mca_ptl_base_peer_t** peer,
                                  ompi_bitmap_t* reachable )
{
   return 0;
}

static int ptl_prof_del_procs_fn( struct mca_ptl_base_module_t* ptl,
                                  size_t nprocs,
                                  struct ompi_proc_t** procs,
                                  struct mca_ptl_base_peer_t** peer )
{
   return 0;
}

static int ptl_prof_finalize_fn( struct mca_ptl_base_module_t* ptl )
{
   return 0;
}

static int ptl_prof_send_fn( struct mca_ptl_base_module_t* ptl,
                             struct mca_ptl_base_peer_t* ptl_base_peer,
                             struct mca_pml_base_send_request_t* request,
                             size_t offset,
                             size_t size,
                             int flags )
{
   return 0;
}

static int ptl_prof_put_fn( struct mca_ptl_base_module_t* ptl,
                            struct mca_ptl_base_peer_t* ptl_base_peer,
                            struct mca_pml_base_send_request_t* request,
                            size_t offset,
                            size_t size,
                            int flags )
{
   return 0;
}

static int ptl_prof_get_fn( struct mca_ptl_base_module_t* ptl,
                            struct mca_ptl_base_peer_t* ptl_base_peer,
                            struct mca_pml_base_recv_request_t* request,
                            size_t offset,
                            size_t size,
                            int flags )
{
   return 0;
}

static void ptl_prof_matched_fn( struct mca_ptl_base_module_t* ptl,
                                 struct mca_ptl_base_recv_frag_t* request )
{
}

static int ptl_prof_request_init_fn( struct mca_ptl_base_module_t* ptl,
                                          struct mca_pml_base_send_request_t* request )
{
   return 0;
}

static void ptl_prof_request_fini_fn( struct mca_ptl_base_module_t* ptl, struct mca_pml_base_send_request_t* request )
{
}

static bool ptl_prof_match_fn( struct mca_ptl_base_module_t* ptl,
                               struct mca_ptl_base_recv_frag_t* recv_frag,
                               struct mca_ptl_base_match_header_t* header )
{
   return true;
}

static void ptl_prof_send_progress_fn( struct mca_ptl_base_module_t* ptl,
                                       struct mca_pml_base_send_request_t* send_request,
                                       size_t bytes_sent )
{
}

static void ptl_prof_recv_progress_fn( struct mca_ptl_base_module_t* ptl,
                                       struct mca_pml_base_recv_request_t* recv_request,
                                       size_t bytes_received,
                                       size_t bytes_delivered )
{
}

/* The default profiling PTL. We will canibalize all others PTL
 * except this one. It's just a simple way to have the control function
 * called.
 */
mca_ptl_prof_t mca_ptl_prof = {
    { NULL,
      0, /* maximum size of request cache for this PTL */
      0, /* number of bytes required by PTL for request cache */
      0, /* ptl_frag_first_size */
      0, /* ptl_frag_min_size */
      0, /* ptl_frag_max_size */
      0, /* ptl_exclusivity */
      0, /* ptl_latency */
      0, /* ptl_bandwidth */
      MCA_PTL_PUT | MCA_PTL_GET, /* ptl flags */
      ptl_prof_add_procs_fn,
      ptl_prof_del_procs_fn,
      ptl_prof_finalize_fn,
      ptl_prof_send_fn,
      ptl_prof_put_fn,
      ptl_prof_get_fn,
      ptl_prof_matched_fn,
      ptl_prof_request_init_fn,
      ptl_prof_request_fini_fn,
      ptl_prof_match_fn,
      ptl_prof_send_progress_fn,
      ptl_prof_recv_progress_fn,
      NULL,  /* the stack :) */
      NULL, 
    }
};

static void ptl_prof_construct(mca_ptl_prof_t* ptl)
{
   ptl->super.ptl_add_procs = ptl_prof_add_procs_fn;
   ptl->super.ptl_del_procs = ptl_prof_del_procs_fn;
   ptl->super.ptl_finalize = ptl_prof_finalize_fn;
   ptl->super.ptl_send = ptl_prof_send_fn;
   ptl->super.ptl_put = ptl_prof_put_fn;
   ptl->super.ptl_get = ptl_prof_get_fn;
   ptl->super.ptl_matched = ptl_prof_matched_fn;
   ptl->super.ptl_request_init = ptl_prof_request_init_fn;
   ptl->super.ptl_request_fini = ptl_prof_request_fini_fn;
   ptl->super.ptl_match = ptl_prof_match_fn;
   ptl->super.ptl_send_progress = ptl_prof_send_progress_fn;
   ptl->super.ptl_recv_progress = ptl_prof_recv_progress_fn;
   ptl->super.ptl_stack = NULL;
}

static void ptl_prof_destruct(mca_ptl_prof_t* ptl)
{
   /* deregistering the profiling ids from the profiling layer */
}

OBJ_CLASS_INSTANCE( mca_ptl_prof_t, ompi_object_t,
                    ptl_prof_construct,
                    ptl_prof_destruct );

