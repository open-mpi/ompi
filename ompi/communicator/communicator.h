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

#ifndef OMPI_COMMUNICATOR_H
#define OMPI_COMMUNICATOR_H

#include "opal/class/opal_object.h"
#include "class/opal_hash_table.h"
#include "errhandler/errhandler.h"
#include "threads/mutex.h"

#include "util/output.h"
#include "mpi.h"
#include "group/group.h"
#include "mca/coll/coll.h"
#include "mca/topo/topo.h"
#include "mca/gpr/gpr_types.h"
#include "mca/oob/oob_types.h"
#include "request/request.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC extern opal_class_t ompi_communicator_t_class;

#define OMPI_COMM_INTER      0x00000001
#define OMPI_COMM_CART       0x00000002
#define OMPI_COMM_GRAPH      0x00000004
#define OMPI_COMM_NAMEISSET  0x00000008
#define OMPI_COMM_ISFREED    0x00000010
#define OMPI_COMM_INTRINSIC  0x00000020
#define OMPI_COMM_HIDDEN     0x00000040
#define OMPI_COMM_DYNAMIC    0x00000080
#define OMPI_COMM_INVALID    0x00000100
#define OMPI_COMM_PML_ADDED  0x00000200

/* some utility #defines */
#define OMPI_COMM_IS_INTER(comm) ((comm)->c_flags & OMPI_COMM_INTER)
#define OMPI_COMM_IS_INTRA(comm) (!((comm)->c_flags & OMPI_COMM_INTER))
#define OMPI_COMM_IS_CART(comm) ((comm)->c_flags & OMPI_COMM_CART)
#define OMPI_COMM_IS_GRAPH(comm) ((comm)->c_flags & OMPI_COMM_GRAPH)
#define OMPI_COMM_IS_INTRINSIC(comm) ((comm)->c_flags & OMPI_COMM_INTRINSIC)
#define OMPI_COMM_IS_HIDDEN(comm) ((comm)->c_flags & OMPI_COMM_HIDDEN)
#define OMPI_COMM_IS_FREED(comm) ((comm)->c_flags & OMPI_COMM_ISFREED)
#define OMPI_COMM_IS_DYNAMIC(comm) ((comm)->c_flags &OMPI_COMM_DYNAMIC)
#define OMPI_COMM_IS_INVALID(comm) ((comm)->c_flags &OMPI_COMM_INVALID)
#define OMPI_COMM_IS_PML_ADDED(comm) ((comm)->c_flags &OMPI_COMM_PML_ADDED)

#define OMPI_COMM_SET_HIDDEN(comm) ((comm)->c_flags |= OMPI_COMM_HIDDEN)
#define OMPI_COMM_SET_DYNAMIC(comm) ((comm)->c_flags |= OMPI_COMM_DYNAMIC)
#define OMPI_COMM_SET_INVALID(comm) ((comm)->c_flags |= OMPI_COMM_INVALID)

#define OMPI_COMM_SET_PML_ADDED(comm) ((comm)->c_flags |= OMPI_COMM_PML_ADDED)

/* a set of special tags: */

/*  to recognize an MPI_Comm_join in the comm_connect_accept routine. */
#define OMPI_COMM_JOIN_TAG      -32000

#define OMPI_COMM_ALLGATHER_TAG -31078
#define OMPI_COMM_BARRIER_TAG   -31079
#define OMPI_COMM_ALLREDUCE_TAG -31080

/** 
 * Modes reqquired for accquiring the new comm-id.
 * The first (INTER/INTRA) indicates whether the
 * input comm was an inter/intra-comm, the second
 * whether the new communicator will be an inter/intra 
 *comm
 */
#define OMPI_COMM_CID_INTRA        0x00000020
#define OMPI_COMM_CID_INTER        0x00000040
#define OMPI_COMM_CID_INTRA_BRIDGE 0x00000080
#define OMPI_COMM_CID_INTRA_OOB    0x00000100

OMPI_DECLSPEC extern ompi_pointer_array_t ompi_mpi_communicators; 

struct ompi_communicator_t {
    opal_object_t              c_base; 
    ompi_mutex_t               c_lock; /* mutex for name and potentially 
                                          attributes */
    char  c_name[MPI_MAX_OBJECT_NAME];
    uint32_t              c_contextid;
    int                     c_my_rank;
    uint32_t                  c_flags; /* flags, e.g. intercomm, 
                                          topology, etc. */

    ompi_group_t        *c_local_group;
    ompi_group_t       *c_remote_group;
  
    /* Attributes */
    opal_hash_table_t       *c_keyhash;

    /**< inscribing cube dimension */
    int c_cube_dim; 

    /* Hooks for topo module to hang things */
    mca_base_component_t *c_topo_component;

    const mca_topo_base_module_1_0_0_t *c_topo; 
    /**< structure of function pointers */

    mca_topo_base_comm_t *c_topo_comm; 
    /**< structure containing basic information about the topology */

    struct mca_topo_base_module_comm_t *c_topo_module; 
    /**< module specific data */

    /* index in Fortran <-> C translation array */

    int c_f_to_c_index;

    /* Error handling.  This field does not have the "c_" prefix so
       that the OMPI_ERRHDL_* macros can find it, regardless of whether
       it's a comm, window, or file. */

    ompi_errhandler_t                  *error_handler;
    ompi_errhandler_type_t             errhandler_type;

    /* Hooks for PML to hang things */
    struct mca_pml_comm_t  *c_pml_comm;
    struct mca_pml_proc_t **c_pml_procs;

    mca_coll_base_module_1_0_0_t c_coll;
    /**< Selected collective module, saved by value for speed (instead
         of by reference) */

    const mca_coll_base_component_1_0_0_t *c_coll_selected_component;
    /**< Selected coll component */
    const mca_coll_base_module_1_0_0_t *c_coll_selected_module;
    /**< The selected module, but only when the selected module
         is not* the basic module.  Used during comm_unselect(). */
    struct mca_coll_base_comm_t *c_coll_selected_data;
    /**< Allow the selected module to cache data on the communicator */

    const mca_coll_base_module_1_0_0_t *c_coll_basic_module;
    /**< Save the basic module; only necessary when the selected
         module is *not* the basic module, but was supplemented
         with methods from the basic module. */
    struct mca_coll_base_comm_t *c_coll_basic_data;
    /**< Allow the basic module to cache data on the communicator */
};
    typedef struct ompi_communicator_t ompi_communicator_t;
    OMPI_DECLSPEC extern ompi_communicator_t *ompi_mpi_comm_parent;
    OMPI_DECLSPEC extern ompi_communicator_t ompi_mpi_comm_null;


    /**
     * is this a valid communicator
     */
    static inline int ompi_comm_invalid(ompi_communicator_t* comm)
    {
	if ((NULL == comm) || (MPI_COMM_NULL == comm) || 
	    (comm->c_flags & OMPI_COMM_ISFREED ) ||
	    (OMPI_COMM_IS_INVALID(comm)) )
	    return true;
	else
	    return false;
    }

    /**
     * rank w/in the communicator
     */
    static inline int ompi_comm_rank(ompi_communicator_t* comm)
    {
	return comm->c_my_rank;
    }

    /**
     * size of the communicator
     */
    static inline int ompi_comm_size(ompi_communicator_t* comm)
    {
	return comm->c_local_group->grp_proc_count;
    }
    
    /**
     * size of the remote group for inter-communicators.
     * returns zero for an intra-communicator
     */
    static inline int ompi_comm_remote_size(ompi_communicator_t* comm)
    {
	if ( comm->c_flags & OMPI_COMM_INTER ) 
	    return comm->c_remote_group->grp_proc_count;
	else 
	    return 0;
    }
    
    /* return pointer to communicator associated with context id cid,
     * No error checking is done*/
    static inline ompi_communicator_t *ompi_comm_lookup(uint32_t cid) 
    { 
	/* array of pointers to communicators, indexed by context ID */
	OMPI_DECLSPEC extern ompi_pointer_array_t ompi_mpi_communicators;
	return (ompi_communicator_t*)ompi_pointer_array_get_item(&ompi_mpi_communicators, cid);
    }
    
    static inline ompi_proc_t* ompi_comm_peer_lookup(ompi_communicator_t* comm, int peer_id)
    {
#if OMPI_ENABLE_DEBUG
	if(peer_id >= comm->c_remote_group->grp_proc_count) {
	    ompi_output(0, "ompi_comm_lookup_peer: invalid peer index (%d)", peer_id);
	    return (ompi_proc_t *) NULL;
	}
#endif
	return comm->c_remote_group->grp_proc_pointers[peer_id];
    }
    
    static inline bool ompi_comm_peer_invalid(ompi_communicator_t* comm, int peer_id)
    {
	if(peer_id < 0 || peer_id >= comm->c_remote_group->grp_proc_count) {
	    return true;
	}
	return false;
    }
    
    
    /** 
     * Initialise MPI_COMM_WORLD and MPI_COMM_SELF 
     */
    int ompi_comm_init(void);
    OMPI_DECLSPEC int ompi_comm_link_function(void);
    
    /** 
     * extract the local group from a communicator 
     */
    int ompi_comm_group ( ompi_communicator_t *comm, ompi_group_t **group );
    
    /**
     * create a communicator based on a group 
     */
    int ompi_comm_create ( ompi_communicator_t* comm, ompi_group_t *group, 
			   ompi_communicator_t** newcomm );
    
    
    /**
     * create a cartesian communicator
     */
    int ompi_topo_create (ompi_communicator_t *old_comm, 
                          int ndims_or_nnodes,
                          int *dims_or_index,
                          int *periods_or_edges,
                          bool reorder,
                          ompi_communicator_t **comm_cart,
                          int cart_or_graph);
    
    /**
     * split a communicator based on color and key. Parameters
     * are identical to the MPI-counterpart of the function.
     * 
     * @param comm: input communicator
     * @param color
     * @param key
     *
     * @
     */
    int ompi_comm_split ( ompi_communicator_t *comm, int color, int key, 
                         ompi_communicator_t** newcomm, bool pass_on_topo);
    
    /** 
     * dup a communicator. Parameter are identical to the MPI-counterpart 
     * of the function. It has been extracted, since we need to be able 
     * to dup a communicator internally as well.
     *
     * @param comm: input communicator
     *
     */
    int ompi_comm_dup ( ompi_communicator_t *comm, ompi_communicator_t **newcomm);


    /**
     * free a communicator 
     */
    int ompi_comm_free ( ompi_communicator_t **comm );

    /**
     * allocate a new communicator structure 
     * @param local_group_size
     * @param remote_group_size
     *
     * this routine allocates the structure, the according local and
     * remote groups, the proc-arrays in the local and remote group.
     * It furthermore sets the fortran index correctly, 
     * and sets all other elements to zero.
     */
    ompi_communicator_t* ompi_comm_allocate ( int local_group_size, 
                                              int remote_group_size );

    /**
     * allocate new communicator ID
     * @param newcomm:    pointer to the new communicator
     * @param oldcomm:    original comm
     * @param bridgecomm: bridge comm for intercomm_create
     * @param mode: combination of input 
     *              OMPI_COMM_CID_INTRA:        intra-comm
     *              OMPI_COMM_CID_INTER:        inter-comm 
     *              OMPI_COMM_CID_INTRA_BRIDGE: 2 intracomms connected by 
     *                                          a bridge comm. local_leader
     *                                          and remote leader are in this
     *                                          case an int (rank in bridge-comm).
     *              OMPI_COMM_CID_INTRA_OOB:    2 intracomms, leaders talk
     *                                          through OOB. lleader and rleader
     *                                          are the required contact information.
     * @param send_first: to avoid a potential deadlock for 
     *                    the OOB version.
     * This routine has to be thread safe in the final version.
     */
    int ompi_comm_nextcid ( ompi_communicator_t* newcomm, 
                            ompi_communicator_t* oldcomm, 
                            ompi_communicator_t* bridgecomm, 
                            void* local_leader, 
                            void* remote_leader, 
                            int mode, 
                            int send_first);
    

    /**
     * shut down the communicator infrastructure.
     */
    int ompi_comm_finalize (void);

    /**
     * This is THE routine, where all the communicator stuff
     * is really set.
     */
    int ompi_comm_set ( ompi_communicator_t* newcomm,
                        ompi_communicator_t* oldcomm,
                        int local_size, 
                        ompi_proc_t **local_procs,
                        int remote_size,
                        ompi_proc_t **remote_procs,
                        opal_hash_table_t *attr,
                        ompi_errhandler_t *errh, 
                        mca_base_component_t *topocomponent );
    /**
     * This is a short-hand routine used in intercomm_create.
     * The routine makes sure, that all processes have afterwards
     * a list of ompi_proc_t pointers for the remote group.
     */
    ompi_proc_t **ompi_comm_get_rprocs ( ompi_communicator_t *local_comm, 
                                       ompi_communicator_t *bridge_comm, 
                                       int local_leader,
                                       int remote_leader,
                                       orte_rml_tag_t tag,
                                       int rsize);


    /**
     * This routine verifies, whether local_group and remote group are overlapping
     * in intercomm_create
     */
    int ompi_comm_overlapping_groups (int size, ompi_proc_t **lprocs,
				      int rsize, ompi_proc_t ** rprocs);


    /**
     * This is a routine determining whether the local or the
     * remote group will be first in the new intra-comm.
     * Just used from within MPI_Intercomm_merge.
     */
    int ompi_comm_determine_first ( ompi_communicator_t *intercomm,
                                   int high );


    int ompi_comm_activate ( ompi_communicator_t* newcomm, 
                             ompi_communicator_t* oldcomm, 
                             ompi_communicator_t* bridgecomm, 
                             void* local_leader, 
                             void* remote_leader, 
                             int mode, 
                             int send_first,
                             mca_base_component_t *collcomponent );
    

    /**
     * a simple function to dump the structure
     */
    int ompi_comm_dump ( ompi_communicator_t *comm );

    /** 
     * a simple function to determint a port number
     */
    int ompi_open_port (char *port_name);

    /**
     * takes a port_name and returns the oob-contact information
     * and the tag
     */
    char * ompi_parse_port (char *port_name, orte_rml_tag_t *tag) ;

    /** 
     * routines handling name publishing, lookup and unpublishing
     */
    int ompi_comm_namepublish ( char *service_name, char *port_name );
    char* ompi_comm_namelookup ( char *service_name );
    int ompi_comm_nameunpublish ( char *service_name ); 


    /* setting name */
    int ompi_comm_set_name (ompi_communicator_t *comm, char *name );

    /* THE routine for dynamic process management. This routine
       sets the connection up between two independent applications.
    */
    int ompi_comm_connect_accept ( ompi_communicator_t *comm, int root,
                                   orte_process_name_t *port, int send_first,
                                   ompi_communicator_t **newcomm, orte_rml_tag_t tag);

    /* A helper routine for ompi_comm_connect_accept.
     * This routine is necessary, since in the connect/accept case, the processes
     * executing the connect operation have the OOB contact information of the
     * leader of the remote group, however, the processes executing the 
     * accept get their own port_name = OOB contact information passed in as 
     * an argument. This is however useless.
     * 
     * Therefore, the two root processes exchange this information at this point.
     *
     */
    orte_process_name_t *ompi_comm_get_rport (orte_process_name_t *port,
                                              int send_first, ompi_proc_t *proc,
					                         orte_rml_tag_t tag);
    


    /*
     * these are the init and finalize functions for the comm_reg
     * stuff. These routines are necessary for handling multi-threading
     * scenarious in the communicator_cid allocation
     */
    void ompi_comm_reg_init(void);
    void ompi_comm_reg_finalize(void);

    /* start the new processes from MPI_Comm_spawn_multiple.  Initial
     * version, very rough 
     */
    int ompi_comm_start_processes(int count, char **array_of_commands,
                                  char ***array_of_argv, 
                                  int *array_of_maxprocs, 
                                  MPI_Info *array_of_info, 
                                  char *port_name);

    /*
     * This routine checks, whether an application has been spawned
     * by another MPI application, or has been independently started.
     * If it has been spawned, it establishes the parent communicator.
     * Since the routine has to communicate, it should be among the last
     * steps in MPI_Init, to be sure that everything is already set up.
     */
    int ompi_comm_dyn_init(void);

    /** 
     * Executes internally a disconnect on all dynamic communicators
     * in case the user did not disconnect them.
     */
    int ompi_comm_dyn_finalize(void);

    /* this routine counts the number of different jobids of the processes
       given in a certain communicator. If there is more than one jobid,
       we mark the communicator as 'dynamic'. This is especially relevant
       for the MPI_Comm_disconnect *and* for MPI_Finalize, where we have
       to wait for all still connected processes. */
    extern int ompi_comm_num_dyncomm;
    void ompi_comm_mark_dyncomm (ompi_communicator_t *comm);

    /* the next two routines implement a kind of non-blocking barrier.
       the only difference is, that you can wait for the completion
       of more than one initiated ibarrier. This is required for waiting
       for all still connected processes in MPI_Finalize.

       ompi_comm_disconnect_init returns a handle, which has to be passed in 
       to ompi_comm_disconnect_waitall. The second routine blocks, until
       all non-blocking barriers described by the handles are finished.
       The communicators can than be released.
    */

    struct ompi_comm_disconnect_obj {
	ompi_communicator_t *comm;
	int                  size;
	ompi_request_t     **reqs;
	int                   buf;
    };
    typedef struct ompi_comm_disconnect_obj ompi_comm_disconnect_obj;

    ompi_comm_disconnect_obj *ompi_comm_disconnect_init (ompi_communicator_t *comm);
    void ompi_comm_disconnect_waitall (int count, ompi_comm_disconnect_obj **objs );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_COMMUNICATOR_H */
