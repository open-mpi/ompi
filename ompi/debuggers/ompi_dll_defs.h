/**********************************************************************
 * Copyright (C) 2000-2004 by Etnus, LLC.
 * Copyright (C) 1999 by Etnus, Inc.
 * Copyright (C) 1997-1998 Dolphin Interconnect Solutions Inc.
 *
 * Permission is hereby granted to use, reproduce, prepare derivative
 * works, and to redistribute to others.
 *
 *				  DISCLAIMER
 *
 * Neither Dolphin Interconnect Solutions, Etnus LLC, nor any of their
 * employees, makes any warranty express or implied, or assumes any
 * legal liability or responsibility for the accuracy, completeness,
 * or usefulness of any information, apparatus, product, or process
 * disclosed, or represents that its use would not infringe privately
 * owned rights.
 *
 * This code was written by
 * James Cownie: Dolphin Interconnect Solutions. <jcownie@dolphinics.com>
 *               Etnus LLC <jcownie@etnus.com>
 **********************************************************************/

/* Update log
 *
 * May 19 1998 JHC: Changed the names of the structs now that we don't
 *              include this directly in mpi_interface.h
 * Oct 27 1997 JHC: Structure definitions for structures used to hold MPICH
 *              info required by the DLL for dumping message queues.
 */

/***********************************************************************
 * Information associated with a specific executable image
 */
typedef struct 
{
    const struct mqs_image_callbacks * image_callbacks;	/* Functions needed here */
    /* basic structures */
    struct {
        int size;
        struct {
            int opal_list_next;
        } offset;
    } opal_list_item_t;
    struct {
        int size;
        struct {
            int opal_list_sentinel;
        } offset;
    } opal_list_t;
    struct {
        int size;
    } ompi_free_list_item_t;
    struct {
        int size;
    } ompi_free_list_memory_t;
    struct {
        int size;
        struct {
            int fl_elem_class;    /* opal_class_t* */
            int fl_mpool;         /* struct mca_mpool_base_module_t* */
            int fl_elem_size;     /* size_t */
            int fl_alignment;     /* size_t */
            int fl_allocations;   /* opal_list_t */
            int fl_max_to_alloc;  /* size_t */
            int fl_num_per_alloc; /* size_t */
            int fl_num_allocated; /* size_t */
        } offset;
    } ompi_free_list_t;
    /* requests structures */
    struct {
        int size;
        struct {
            int req_type;
            int req_status;
            int req_complete;
            int req_state;
            int req_f_to_c_index;
        } offset;
    } ompi_request_t;
    struct {
        int size;
        struct {
            int req_addr;
            int req_count;
            int req_peer;
            int req_tag;
            int req_comm;
            int req_datatype;
            int req_proc;
            int req_sequence;
            int req_type;
            int req_pml_complete;
        } offset;
    } mca_pml_base_request_t;
    struct {
        int size;
        struct {
            int req_addr;
            int req_bytes_packed;
            int req_send_mode;
        } offset;
    } mca_pml_base_send_request_t;
    struct {
        int size;
        struct {
            int req_bytes_packed;
        } offset;
    } mca_pml_base_recv_request_t;
#if 0
    /* fragments for unexpected messages (as well as theirs headers) */
    struct {
        int size;
        struct {
            int hdr;
            int request;
        } offset;
    } mca_pml_ob1_recv_frag_t;
    struct {
        int size;
        struct {
            int hdr_type;
            int hdr_flags;
        } offset;
    } mca_pml_ob1_common_hdr_t;
    struct {
        int size;
        struct {
            int hdr_common;
            int hdr_ctx;
            int hdr_src;
            int hdr_tag;
            int hdr_seq;
        } offset;
    } mca_pml_ob1_match_hdr_t;
#endif
    /* communicator structures */
    struct {
        int size;
        struct {
            int lowest_free;
            int number_free;
            int size;
            int addr;
        } offset;
    } ompi_pointer_array_t;
    struct {
        int size;
        struct {
            int grp_proc_count;
            int grp_my_rank;
            int grp_flags;
        } offset;
    } ompi_group_t;
    struct {
        int size;
        struct {
            int c_name;
            int c_contextid;
            int c_my_rank;
            int c_local_group;
        } offset;
    } ompi_communicator_t;
    struct {
        int size;
        struct {
            int MPI_SOURCE;
            int MPI_TAG;
            int MPI_ERROR;
            int _count;
            int _cancelled;
        } offset;
    } ompi_status_public_t;
    struct {
        int size;
        struct {
            int size;
            int name;
        } offset;
    } ompi_datatype_t;
} mpi_image_info; 

/***********************************************************************
 * Information associated with a specific process
 */

typedef struct group_t
{
    mqs_taddr_t group_base;          /* Where was it in the process  */
    int         ref_count;           /* How many references to us */
    int         entries;             /* How many entries */
    int*        local_to_global;     /* The translation table */
} group_t;

/* Internal structure we hold for each communicator */
typedef struct communicator_t
{
    struct communicator_t * next;
    group_t *               group;		/* Translations */
    int                     recv_context;	/* Unique ID for the communicator */
    mqs_taddr_t             comm_ptr;
    int                     present;
    mqs_communicator        comm_info;		/* Info needed at the higher level */
} communicator_t;

typedef struct mqs_ompi_opal_list_t_pos {
    mqs_taddr_t current_item;
    mqs_taddr_t list;
    mqs_taddr_t sentinel;
} mqs_opal_list_t_pos;

typedef struct {
    mqs_opal_list_t_pos opal_list_t_pos;
    mqs_taddr_t current_item;
    mqs_taddr_t upper_bound;
    mqs_tword_t header_space;
    mqs_taddr_t free_list;
    mqs_tword_t fl_elem_class;         /* opal_class_t* */
    mqs_tword_t fl_mpool;              /* struct mca_mpool_base_module_t* */
    mqs_tword_t fl_elem_size;          /* size_t */
    mqs_tword_t fl_alignment;          /* size_t */
    mqs_tword_t fl_num_per_alloc;      /* size_t */
    mqs_tword_t fl_num_allocated;      /* size_t */
    mqs_tword_t fl_num_initial_alloc;  /* size_t */
} mqs_ompi_free_list_t_pos;


/* Information for a single process, a list of communicators, some
 * useful addresses, and the state of the iterators.
 */
typedef struct 
{
  const struct mqs_process_callbacks * process_callbacks; /* Functions needed here */

  struct communicator_t *communicator_list;	/* List of communicators in the process */
  mqs_target_type_sizes sizes;			/* Process architecture information */

  /* Addresses in the target process */
  mqs_taddr_t send_queue_base;			/* Where to find the send message queues */
  mqs_taddr_t recv_queue_base;			/* Where to find the recv message queues */
  mqs_taddr_t sendq_base;			/* Where to find the send queue */
  mqs_taddr_t commlist_base;			/* Where to find the list of communicators */
  /* Other info we need to remember about it */
  mqs_tword_t comm_number_free;         /* the number of available positions in
                                         * the communicator array. */
  mqs_tword_t comm_lowest_free;         /* the lowest free communicator */
  mqs_tword_t show_internal_requests;   /* show or not the Open MPI internal requests */
  /* State for the iterators */
  struct communicator_t *current_communicator;	/* Easy, we're walking a simple list */

  int world_proc_array_entries;
  mqs_taddr_t* world_proc_array;
    
  mqs_ompi_free_list_t_pos next_msg;            /* And state for the message iterator */
  mqs_op_class  what;				/* What queue are we looking on */
} mpi_process_info;





