#include "mca/allocator/allocator.h"
#include "include/constants.h"
#include "mca/base/mca_base_param.h"
#include "mca/allocator/bucket/allocator_bucket_alloc.h"

struct mca_allocator_base_module_t* mca_allocator_bucket_module_init(
    bool *allow_multi_user_threads,
    mca_allocator_base_component_segment_alloc_fn_t segment_alloc,
    mca_allocator_base_component_segment_free_fn_t segment_free);

int mca_allocator_bucket_module_open(void);

int mca_allocator_bucket_module_close(void);

void * mca_allocator_bucket_alloc_wrapper(struct mca_allocator_base_module_t* allocator,
                                          size_t size, size_t align);
static int mca_allocator_num_buckets;



int mca_allocator_bucket_finalize(struct mca_allocator_base_module_t* allocator)
{
    mca_allocator_bucket_cleanup(allocator);
    free(allocator);
    return(OMPI_SUCCESS);
}

struct mca_allocator_base_module_t* mca_allocator_bucket_module_init(
    bool *allow_multi_user_threads,
    mca_allocator_base_component_segment_alloc_fn_t segment_alloc,
    mca_allocator_base_component_segment_free_fn_t segment_free)
{
    size_t alloc_size = sizeof(mca_allocator_bucket_t);
    mca_allocator_bucket_t * retval;
    mca_allocator_bucket_t * allocator = malloc(alloc_size);
    if(NULL == allocator) {
        return(NULL);
    }
    retval = mca_allocator_bucket_init((mca_allocator_base_module_t *) allocator, mca_allocator_num_buckets, 
                                        segment_alloc, segment_free);
    if(NULL == retval) {
        free(allocator);
        return(NULL);
    }
    allocator->super.alc_alloc =  mca_allocator_bucket_alloc_wrapper;
    allocator->super.alc_realloc = mca_allocator_bucket_realloc;
    allocator->super.alc_free =  mca_allocator_bucket_free;
    allocator->super.alc_compact = mca_allocator_bucket_cleanup;
    allocator->super.alc_finalize = mca_allocator_bucket_finalize;

    return((mca_allocator_base_module_t *) allocator);
}

int mca_allocator_bucket_module_open(void) {

    int id = mca_base_param_register_int("allocator","bucket","num_buckets", NULL,30);
    mca_base_param_lookup_int(id,&mca_allocator_num_buckets);
    return(OMPI_SUCCESS);
}

int mca_allocator_bucket_module_close(void) {
    return(OMPI_SUCCESS);
}

void * mca_allocator_bucket_alloc_wrapper(struct mca_allocator_base_module_t* allocator,
                                          size_t size, size_t align)
{
    if(0 == align){
        return(mca_allocator_bucket_alloc(allocator, size));
    }
    return(mca_allocator_bucket_alloc_align(allocator, size, align));
}    


mca_allocator_base_component_t mca_allocator_bucket_component = { 

  /* First, the mca_base_module_t struct containing meta information
     about the module itself */

  {
    /* Indicate that we are a allocator v1.0.0 module (which also implies a
       specific MCA version) */

    MCA_ALLOCATOR_BASE_VERSION_1_0_0,

    "bucket", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_allocator_bucket_module_open,  /* module open */
    mca_allocator_bucket_module_close  /* module close */
  },
  
  /* Next the MCA v1.0.0 module meta data */

  {
    /* Whether the module is checkpointable or not */
    false
  },
  mca_allocator_bucket_module_init
};

