#define HAVE_MPI_GREQUEST_EXTENSIONS 1

extern void opal_progress(void);

typedef int (MPIX_Grequest_poll_function)(void *, MPI_Status *);
typedef int (MPIX_Grequest_wait_function)(int, void **, double, MPI_Status *);
typedef int MPIX_Grequest_class;

extern int ompi_grequestx_start(
    MPI_Grequest_query_function *gquery,
    MPI_Grequest_free_function *gfree,
    MPI_Grequest_cancel_function *gcancel,
    MPIX_Grequest_poll_function *gpoll,
    void* gstate,
    MPI_Request* request);

extern int ompi_grequestx_class_create(
    MPI_Grequest_query_function *gquery,
    MPI_Grequest_free_function *gfree,
    MPI_Grequest_cancel_function *gcancel,
    MPIX_Grequest_poll_function *gpoll,
    MPIX_Grequest_wait_function *gwait,
    MPIX_Grequest_class *greq_class);

extern int ompi_grequestx_class_allocate(
    MPIX_Grequest_class greq_class,
    void *extra_state,
    MPI_Request* request);

#define MPIR_Ext_cs_yield opal_progress
#define PMPIX_Grequest_class_allocate(greq_class,extra_state,request) ompi_grequestx_class_allocate(greq_class,extra_state,request)
#define PMPIX_Grequest_class_create(query_fn,free_fn,cancel_fn,poll_fn,wait_fn,greq_class) ompi_grequestx_class_create(query_fn,free_fn,cancel_fn,poll_fn,wait_fn,greq_class)
#define PMPIX_Grequest_start(query_fn,free_fn,cancel_fn,poll_fn,extra_state,request) ompi_grequestx_start(query_fn,free_fn,cancel_fn,poll_fn,extra_state,request)

