#include "svc_sched.h"
#include "util/proc_info.h"


mca_svc_sched_component_t mca_svc_sched_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a pml v1.0.0 module (which also
           implies a specific MCA version) */
                                                                                                                  
        MCA_SVC_BASE_VERSION_1_0_0,
                                                                                                                  
        "sched", /* MCA module name */
        1,  /* MCA module major version */
        0,  /* MCA module minor version */
        0,  /* MCA module release version */
        mca_svc_sched_component_open,  /* component open */
        mca_svc_sched_component_close  /* component close */
      },
                                                                                                                  
      /* Next the MCA v1.0.0 module meta data */
                                                                                                                  
      {
        /* Whether the module is checkpointable or not */
                                                                                                                  
        false
      },
                                                                                                                  
      mca_svc_sched_component_init
    },
};


/**
 * Utility function to register parameters
 */

static inline int mca_svc_sched_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("svc","sched",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/**
 * Sched component open - Initialize global data and register
 * any MCA parameters.
 */

int mca_svc_sched_component_open(void)
{
    OBJ_CONSTRUCT(&mca_svc_sched_component.sched_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_svc_sched_component.sched_node_list, ompi_list_t);
    OBJ_CONSTRUCT(&mca_svc_sched_component.sched_node_tree, ompi_rb_tree_t);

    mca_svc_sched_component.sched_debug =
        mca_svc_sched_param_register_int("debug", 0);
    return OMPI_SUCCESS;
}


/**
 * compare function for tree insert
 */

static int mca_svc_sched_name_compare(const ompi_process_name_t* n1, const ompi_process_name_t* n2)
{
   if(n1->cellid < n2->cellid)
       return -1;
   else if(n1->cellid > n2->cellid)
       return 1;
   else if(n1->jobid < n2->jobid)
       return -1;
   else if(n1->jobid > n2->jobid)
       return 1;
   else if(n1->vpid < n2->vpid)
       return -1;
   else if(n1->vpid > n2->vpid)
       return 1;
   return(0);
}
                                                                                                                                               
/**
 * Sched component initialization.
 */

mca_svc_base_module_t* mca_svc_sched_component_init(void)
{
    /* only run in the seed daemon */
    if(ompi_process_info.seed == false)
        return NULL;

    /* initialize data structures */
    ompi_rb_tree_init(&mca_svc_sched_component.sched_node_tree, 
        (ompi_rb_tree_comp_fn_t)mca_svc_sched_name_compare);
    return &mca_svc_sched_module;
}


/**
 *
 */

int mca_svc_sched_component_close(void)
{
    OBJ_DESTRUCT(&mca_svc_sched_component.sched_node_list);
    OBJ_DESTRUCT(&mca_svc_sched_component.sched_node_tree);
    OBJ_DESTRUCT(&mca_svc_sched_component.sched_lock);
    return OMPI_SUCCESS;
}


