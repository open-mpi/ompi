/*
 * $HEADER$
 */

#ifndef LAM_MPI_H
#define LAM_MPI_H

#include "lam_config.h"

/*
 * Just in case you need it.  :-)
 */
#define LAM_MPI 1

/*
 * MPI version
 */
#define MPI_VERSION 2
#define MPI_SUBVERSION 0

/*
 * Typedefs
 */
typedef long MPI_Aint;
typedef struct lam_communicator_t *MPI_Comm;
typedef struct lam_datatype_t *MPI_Datatype;
typedef struct lam_errhandler_t *MPI_Errhandler;
typedef struct lam_file_t *MPI_File;
typedef struct lam_group_t *MPI_Group;
typedef struct lam_info_t *MPI_Info;
typedef struct lam_op_t *MPI_Op;
typedef struct lam_request_t *MPI_Request;
typedef struct lam_status_public_t MPI_Status;
typedef struct lam_win_t *MPI_Win;

/*
 * MPI_Status
 */
struct lam_status_public_t { 
  int MPI_SOURCE;
  int MPI_TAG;
  int MPI_ERROR;
};
typedef struct lam_status_public_t lam_status_public_t;

/*
 * User typedefs
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
#if __STDC__ || defined(c_plusplus) || defined(__cplusplus)
  typedef int (MPI_Copy_function)(MPI_Comm, int, void *,
                                void *, void *, int *);
  typedef int (MPI_Delete_function)(MPI_Comm, int, void *, void *);
  typedef void (MPI_Comm_errhandler_fn)(MPI_Comm *, int *, ...);
  typedef void (MPI_Win_errhandler_fn)(MPI_Win *, int *, ...);
  typedef void (MPI_Handler_function)(MPI_Comm *, int *, ...);
  typedef void (MPI_User_function)(void *, void *, int *, MPI_Datatype *);
  typedef int (MPI_Comm_copy_attr_function)(MPI_Comm, int, void *,
                                            void *, void *, int *);
  typedef int (MPI_Comm_delete_attr_function)(MPI_Comm, int, void *, void *);
  typedef int (MPI_Type_copy_attr_function)(MPI_Datatype, int, void *,
                                            void *, void *, int *);
  typedef int (MPI_Type_delete_attr_function)(MPI_Datatype, int,
                                              void *, void *);
  typedef int (MPI_Win_copy_attr_function)(MPI_Win, int, void *,
                                           void *, void *, int *);
  typedef int (MPI_Win_delete_attr_function)(MPI_Win, int, void *, void *);
#endif
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * Miscellaneous constants
 * JMS: Some of these may be L7-specific and should be removed...
 */
#define MPI_SUCCESS     0       /* no errors */
#define MPI_ANY_SOURCE      -1      /* match any source rank */
#define MPI_PROC_NULL       -2      /* rank of null process */
#define MPI_CANCEL_SOURCE   -3      /* successful cancel */
#define MPI_ANY_TAG     -1      /* match any message tag */
#define MPI_GER_TAG     -2      /* used for GER protocol */
#define MPI_MERGE_TAG       -3      /* used to merge inter-comm */
#define MPI_MAX_PROCESSOR_NAME  256     /* max proc. name length */
#define MPI_MAX_ERROR_STRING    256     /* max error message length */
#define MPI_MAX_OBJECT_NAME 64      /* max object name length */
#define MPI_UNDEFINED       -32766      /* undefined stuff */
#define MPI_CART        1       /* cartesian topology */
#define MPI_GRAPH       2       /* graph topology */
#define MPI_KEYVAL_INVALID  -1      /* invalid key value */

/*
 * More constants
 * JMS: Copied straight from L7 -- feel free to change
 * JMS: Some of these are probably L7-specific and should be deleted
 */
#define MPI_BOTTOM      ((void *) 0)    /* base reference address */
#define MPI_BSEND_OVERHEAD  40      /* size of bsend header + ptr */
#define MPI_MAX_INFO_KEY    36      /* max info key length */
#define MPI_MAX_INFO_VAL    256     /* max info value length */
#define MPI_ARGV_NULL       ((char **) 0)   /* NULL argument vector */
#define MPI_ARGVS_NULL      ((char ***) 0)  /* NULL argument vectors */
#define MPI_ERRCODES_IGNORE ((void *) 0)    /* don't return error codes */
#define MPI_MAX_PORT_NAME       36              /* max port name length */
#define MPI_MAX_NAME_LEN    MPI_MAX_PORT_NAME /* max port name length */
#define MPI_ORDER_C     0       /* C row major order */
#define MPI_ORDER_FORTRAN   1       /* Fortran column major order */
#define MPI_DISTRIBUTE_BLOCK    0       /* block distribution */
#define MPI_DISTRIBUTE_CYCLIC   1       /* cyclic distribution */
#define MPI_DISTRIBUTE_NONE     2       /* not distributed */
#define MPI_DISTRIBUTE_DFLT_DARG (-1)       /* default distribution arg */

/*
 * Predefined attribute keyvals
 */
enum {
  MPI_TAG_UB,
  MPI_HOST,
  MPI_IO,

  MPI_WTIME_IS_GLOBAL,
  MPI_UNIVERSE_SIZE,
  MPI_APPNUM,

  MPI_WIN_BASE,
  MPI_WIN_SIZE,
  MPI_WIN_DISP_UNIT,

  /* 
   * Even though these four are IMPI attributes, they need to be there
   * for all MPI jobs
   */
  IMPI_CLIENT_SIZE,
  IMPI_CLIENT_COLOR,
  IMPI_HOST_SIZE,
  IMPI_HOST_COLOR,

  /*
   * Predefined attribute keyvals, but LAM-specific
   */
  LAM_UNIVERSE_NCPUS,
  LAM_UNIVERSE_NNODES,

  /*
   * Predefined attribute keyvals, but LAM/MPI-specific
   */
  LAM_MPI_SSI_COLL,
  LAM_MPI_SSI_COLL_CROSSOVER,
  LAM_MPI_SSI_COLL_ASSOCIATIVE,
  LAM_MPI_SSI_COLL_REDUCE_CROSSOVER
};

/*
 * Error classes
 */
enum {
  MPI_ERR_BUFFER = 1, /* invalid buffer pointer */
  MPI_ERR_COUNT, /* invalid count argument */
  MPI_ERR_TYPE, /* invalid datatype argument */
  MPI_ERR_TAG, /* invalid tag argument */
  MPI_ERR_COMM, /* invalid communicator */
  MPI_ERR_RANK, /* invalid rank */
  MPI_ERR_REQUEST, /* invalid request handle */
  MPI_ERR_ROOT, /* invalid root */
  MPI_ERR_GROUP, /* invalid group */
  MPI_ERR_OP, /* invalid operation */
  MPI_ERR_TOPOLOGY, /* invalid topology */
  MPI_ERR_DIMS, /* invalid dimension argument */
  MPI_ERR_ARG, /* invalid argument */
  MPI_ERR_UNKNOWN, /* unknown error */
  MPI_ERR_TRUNCATE, /* message truncated on receive */
  MPI_ERR_OTHER, /* LAM error */
  MPI_ERR_INTERN, /* internal MPI error */
  MPI_ERR_IN_STATUS, /* error code is in status */
  MPI_ERR_PENDING, /* pending request */
  MPI_ERR_SYSRESOURCE, /* out of system resources */
  MPI_ERR_LOCALDEAD, /* process in local group is dead */
  MPI_ERR_REMOTEDEAD, /* process in remote group is dead */
  MPI_ERR_VALUE, /* truncated info value */
  MPI_ERR_FLAGS, /* mismatched run-time flags */
  MPI_ERR_SERVICE, /* name publishing service error */
  MPI_ERR_NAME, /* name not published */
  MPI_ERR_SPAWN, /* error while spawning processes */
  MPI_ERR_KEYVAL, /* invalid key value */
  MPI_ERR_INFO_NOKEY, /* no such info key */
  MPI_ERR_WIN, /* invalid window */
  MPI_ERR_EPOCH, /* mismatched one-sided synch. */
  MPI_ERR_TYPENOTSUP, /* operation not supported on type */
  MPI_ERR_INFO_KEY, /* invalid info key */
  MPI_ERR_INFO_VALUE, /* invalid info value */
  MPI_ERR_NO_MEM, /* no memory left */
  MPI_ERR_BASE, /* invalid base pointer value */

  MPI_ERR_LASTCODE /* last error code */
};

/*
 * Comparison results.  Don't change the order of these, the group
 * comparison functions rely on it.
 */
enum {
  MPI_IDENT,
  MPI_CONGRUENT,
  MPI_SIMILAR,
  MPI_UNEQUAL
};

/*
 * MPI_Init_thread constants
 */
enum {
  MPI_THREAD_SINGLE,
  MPI_THREAD_FUNNELED,
  MPI_THREAD_SERIALIZED,
  MPI_THREAD_MULTIPLE
};

/*
 * Datatype combiners.
 */
enum {
  MPI_COMBINER_NAMED,
  MPI_COMBINER_DUP,
  MPI_COMBINER_CONTIGUOUS,
  MPI_COMBINER_VECTOR,
  MPI_COMBINER_HVECTOR_INTEGER,
  MPI_COMBINER_HVECTOR,
  MPI_COMBINER_INDEXED,
  MPI_COMBINER_HINDEXED_INTEGER,
  MPI_COMBINER_HINDEXED,
  MPI_COMBINER_INDEXED_BLOCK,
  MPI_COMBINER_STRUCT_INTEGER,
  MPI_COMBINER_STRUCT,
  MPI_COMBINER_SUBARRAY,
  MPI_COMBINER_DARRAY,
  MPI_COMBINER_F90_REAL,
  MPI_COMBINER_F90_COMPLEX,
  MPI_COMBINER_F90_INTEGER,
  MPI_COMBINER_RESIZED
};

/*
 * NULL handles
 */
#define MPI_GROUP_NULL ((MPI_Group) 0)
#define MPI_COMM_NULL ((MPI_Comm) 0)
#define MPI_DATATYPE_NULL ((MPI_Datatype) 0)
#define MPI_REQUEST_NULL ((MPI_Request) 0)
#define MPI_OP_NULL ((MPI_Op) 0)
#define MPI_ERRHANDLER_NULL ((MPI_Errhandler) 0)
#define MPI_INFO_NULL ((MPI_Info) 0)
#define MPI_WIN_NULL ((MPI_Win) 0)

#define MPI_STATUS_IGNORE ((MPI_Status *) 0)
#define MPI_STATUSES_IGNORE ((MPI_Status *) 0)

#define MPI_NULL_COPY_FN ((MPI_Copy_function *) 0)
#define MPI_NULL_DELETE_FN ((MPI_Delete_function *) 0)
#define MPI_COMM_NULL_COPY_FN ((MPI_Comm_copy_attr_function *) 0)
#define MPI_COMM_NULL_DELETE_FN ((MPI_Comm_delete_attr_function *) 0)
#define MPI_TYPE_NULL_COPY_FN ((MPI_Type_copy_attr_function *) 0)
#define MPI_TYPE_NULL_DELETE_FN ((MPI_Type_delete_attr_function *) 0)
#define MPI_WIN_NULL_COPY_FN ((MPI_Win_copy_attr_function *) 0)
#define MPI_WIN_NULL_DELETE_FN ((MPI_Win_delete_attr_function *) 0)

/*
 * External variables
 */
extern struct lam_communicator lam_mpi_comm_world;
extern struct lam_communicator lam_mpi_comm_self;

extern struct lam_group lam_mpi_group_empty;

extern struct lam_op lam_mpi_max, lam_mpi_min;
extern struct lam_op lam_mpi_sum, lam_mpi_prod;
extern struct lam_op lam_mpi_land, lam_mpi_band;
extern struct lam_op lam_mpi_lor, lam_mpi_bor;
extern struct lam_op lam_mpi_lxor, lam_mpi_bxor;
extern struct lam_op lam_mpi_maxloc, lam_mpi_minloc;
extern struct lam_op lam_mpi_replace;

extern struct lam_datatype lam_mpi_char, lam_mpi_byte;
extern struct lam_datatype lam_mpi_int, lam_mpi_logic;
extern struct lam_datatype lam_mpi_short, lam_mpi_long;
extern struct lam_datatype lam_mpi_float, lam_mpi_double;
extern struct lam_datatype lam_mpi_long_double;
extern struct lam_datatype lam_mpi_cplex, lam_mpi_packed;
extern struct lam_datatype lam_mpi_unsigned_char;
extern struct lam_datatype lam_mpi_unsigned_short;
extern struct lam_datatype lam_mpi_unsigned;
extern struct lam_datatype lam_mpi_unsigned_long;
extern struct lam_datatype lam_mpi_ub, lam_mpi_lb;
extern struct lam_datatype lam_mpi_float_int, lam_mpi_double_int;
extern struct lam_datatype lam_mpi_long_int, lam_mpi_2int;
extern struct lam_datatype lam_mpi_short_int, lam_mpi_dblcplex;
extern struct lam_datatype lam_mpi_integer, lam_mpi_real;
extern struct lam_datatype lam_mpi_dblprec, lam_mpi_character;
extern struct lam_datatype lam_mpi_2real, lam_mpi_2dblprec;
extern struct lam_datatype lam_mpi_2integer, lam_mpi_longdbl_int;
extern struct lam_datatype lam_mpi_wchar, lam_mpi_long_long_int;
extern struct lam_datatype lam_mpi_unsigned_long_long;
extern struct lam_datatype lam_mpi_cxx_cplex, lam_mpi_cxx_dblcplex;
extern struct lam_datatype lam_mpi_cxx_ldblcplex;
extern struct lam_datatype lam_mpi_cxx_bool;

extern struct lam_errorhandler lam_mpi_errors_are_fatal;
extern struct lam_errorhandler lam_mpi_errors_return;

extern MPI_Fint *MPI_F_STATUS_IGNORE;
extern MPI_Fint *MPI_F_STATUSES_IGNORE;

/*
 * MPI predefined handles
 */
#define MPI_COMM_WORLD ((MPI_Comm) &lam_mpi_comm_world)
#define MPI_COMM_SELF ((MPI_Comm) &lam_mpi_comm_self)

#define MPI_GROUP_EMPTY ((MPI_Group) &lam_mpi_group_empty)

#define MPI_MAX ((MPI_Op) &lam_mpi_max)
#define MPI_MIN ((MPI_Op) &lam_mpi_min)
#define MPI_SUM ((MPI_Op) &lam_mpi_sum)
#define MPI_PROD ((MPI_Op) &lam_mpi_prod)
#define MPI_LAND ((MPI_Op) &lam_mpi_land)
#define MPI_BAND ((MPI_Op) &lam_mpi_band)
#define MPI_LOR ((MPI_Op) &lam_mpi_lor)
#define MPI_BOR ((MPI_Op) &lam_mpi_bor)
#define MPI_LXOR ((MPI_Op) &lam_mpi_lxor)
#define MPI_BXOR ((MPI_Op) &lam_mpi_bxor)
#define MPI_MAXLOC ((MPI_Op) &lam_mpi_maxloc)
#define MPI_MINLOC ((MPI_Op) &lam_mpi_minloc)
#define MPI_REPLACE ((MPI_Op) &lam_mpi_replace)

#define MPI_BYTE ((MPI_Datatype) &lam_mpi_byte)
#define MPI_PACKED ((MPI_Datatype) &lam_mpi_packed)
#define MPI_CHAR ((MPI_Datatype) &lam_mpi_char)
#define MPI_SHORT ((MPI_Datatype) &lam_mpi_short)
#define MPI_INT ((MPI_Datatype) &lam_mpi_int)
#define MPI_LONG ((MPI_Datatype) &lam_mpi_long)
#define MPI_FLOAT ((MPI_Datatype) &lam_mpi_float)
#define MPI_DOUBLE ((MPI_Datatype) &lam_mpi_double)
#define MPI_LONG_DOUBLE ((MPI_Datatype) &lam_mpi_long_double)
#define MPI_UNSIGNED_CHAR ((MPI_Datatype) &lam_mpi_unsigned_char)
#define MPI_UNSIGNED_SHORT ((MPI_Datatype) &lam_mpi_unsigned_short)
#define MPI_UNSIGNED_LONG ((MPI_Datatype) &lam_mpi_unsigned_long)
#define MPI_UNSIGNED ((MPI_Datatype) &lam_mpi_unsigned)
#define MPI_FLOAT_INT ((MPI_Datatype) &lam_mpi_float_int)
#define MPI_DOUBLE_INT ((MPI_Datatype) &lam_mpi_double_int)
#define MPI_LONG_DOUBLE_INT ((MPI_Datatype) &lam_mpi_longdbl_int)
#define MPI_LONG_INT ((MPI_Datatype) &lam_mpi_long_int)
#define MPI_SHORT_INT ((MPI_Datatype) &lam_mpi_short_int)
#define MPI_2INT ((MPI_Datatype) &lam_mpi_2int)
#define MPI_UB ((MPI_Datatype) &lam_mpi_ub)
#define MPI_LB ((MPI_Datatype) &lam_mpi_lb)
#define MPI_WCHAR ((MPI_Datatype) &lam_mpi_wchar)
#define MPI_LONG_LONG_INT ((MPI_Datatype) &lam_mpi_long_long_int)
#define MPI_UNSIGNED_LONG_LONG ((MPI_Datatype) &lam_mpi_unsigned_long_long)

#define MPI_ERRORS_ARE_FATAL ((MPI_Errhandler) &lam_mpi_errors_are_fatal)
#define MPI_ERRORS_RETURN ((MPI_Errhandler) &lam_mpi_errors_return)


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  /*
   * MPI API
   */
  /* JMS: Look for missing functions (e.g., MPI_File_*) */
  int MPI_DUP_FN(MPI_Comm, int, void *, void *, void *, int *);
  int MPI_COMM_DUP_FN(MPI_Comm, int, void *, void *, void *, int *);
  int MPI_TYPE_DUP_FN(MPI_Datatype, int, void *, void *, void *, int *);
  int MPI_WIN_DUP_FN(MPI_Win, int, void *, void *, void *, int *);

  int MPI_Abort(MPI_Comm comm, int errorcode);
  int MPI_Accumulate(void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
                   int target_rank, MPI_Aint target_disp, int target_count,
                   MPI_Datatype target_datatype, MPI_Op op, MPI_Win win); 
  int MPI_Add_error_class(int *errorclass);
  int MPI_Add_error_code(int errorclass, int *errorcode);
  int MPI_Add_error_string(int errorcode, char *string);
  int MPI_Address(void *location, MPI_Aint *address);
  int MPI_Allgather(void *sendbuf, int sendcount, MPI_Datatype sendtype, 
                    void *recvbuf, int recvcount, 
                    MPI_Datatype recvtype, MPI_Comm comm);
  int MPI_Allgatherv(void *sendbuf, int sendcount, MPI_Datatype sendtype, 
                     void *recvbuf, int *recvcounts, 
                     int *displs, MPI_Datatype recvtype, MPI_Comm comm);
  int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, 
                    void *baseptr);
  int MPI_Allreduce(void *sendbuf, void *recvbuf, int count, 
                    MPI_Datatype datatype, MPI_Op op, MPI_Comm comm); 
  int MPI_Alltoall(void *sendbuf, int sendcount, MPI_Datatype sendtype, 
                   void *recvbuf, int recvcount, 
                   MPI_Datatype recvtype, MPI_Comm comm);
  int MPI_Alltoallv(void *sendbuf, int *sendcounts, int *sdispls, 
                    MPI_Datatype sendtype, void *recvbuf, int *recvcounts,
                    int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
  int MPI_Alltoallw(void *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype *sendtypes, 
                    void *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype *recvtypes,
                    MPI_Comm comm);
  int MPI_Attr_delete(MPI_Comm comm, int keyval);
  int MPI_Attr_get(MPI_Comm comm, int keyval, void *attribute_val, int *flag);
  int MPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val);
  int MPI_Barrier(MPI_Comm comm);
  int MPI_Bcast(void *buffer, int count, MPI_Datatype datatype, 
                int root, MPI_Comm comm);
  int MPI_Bsend(void *buf, int count, MPI_Datatype datatype, 
                int dest, int tag, MPI_Comm comm);
  int MPI_Bsend_init(void *buf, int count, MPI_Datatype datatype, 
                     int dest, int tag, MPI_Comm comm, MPI_Request *request); 
  int MPI_Buffer_attach(void *buffer, int size);
  int MPI_Buffer_detach(void *buffer, int *size);
  int MPI_Cancel(MPI_Request *request);
  int MPI_Cart_coords(MPI_Comm comm, int rank, int maxdims, int *coords);
  int MPI_Cart_create(MPI_Comm old_comm, int ndims, int *dims, 
                      int *periods, int redorder, MPI_Comm *comm_cart);
  int MPI_Cart_get(MPI_Comm comm, int maxdims, int *dims, 
                   int *periods, int *coords);
  int MPI_Cart_map(MPI_Comm comm, int ndims, int *dims, 
                   int *periods, int *newrank);
  int MPI_Cart_rank(MPI_Comm comm, int *coords, int *rank);
  int MPI_Cart_shift(MPI_Comm comm, int direction, int disp, 
                     int *rank_source, int *rank_dest);
  int MPI_Cart_sub(MPI_Comm comm, int *remain_dims, MPI_Comm *new_comm);
  int MPI_Cartdim_get(MPI_Comm comm, int *ndims);
  int MPI_Close_port(char *port_name);
  int MPI_Comm_accept(char *port_name, MPI_Info info, int root, 
                      MPI_Comm comm, MPI_Comm *newcomm);
  MPI_Fint MPI_Comm_c2f(MPI_Comm comm);
  int MPI_Comm_call_error_handler(MPI_Comm comm, int errorcode);
  int MPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result);
  int MPI_Comm_connect(char *port_name, MPI_Info info, int root, 
                       MPI_Comm comm, MPI_Comm *newcomm);
  int MPI_Comm_create_errhandler(MPI_Comm_errhandler_fn *function, 
                                 MPI_Errhandler *errhandler);
  int MPI_Comm_create_keyval(MPI_Comm_copy_attr_function *comm_copy_attr_fn, 
                             MPI_Comm_delete_attr_function *comm_delete_attr_fn, 
                             int *comm_keyval, void *extra_state);
  int MPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm);
  int MPI_Comm_delete_attr(MPI_Comm comm, int comm_keyval);
  int MPI_Comm_disconnect(MPI_Comm *comm);
  int MPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm);
  MPI_Comm MPI_Comm_f2c(MPI_Fint comm);
  int MPI_Comm_free_keyval(int *comm_keyval);
  int MPI_Comm_free(MPI_Comm *comm);
  int MPI_Comm_get_attr(MPI_Comm comm, int comm_keyval, 
                        void *attribute_val, int *flag);
  int MPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *erhandler);
  int MPI_Comm_get_name(MPI_Comm comm, char *comm_name, int *resultlen);
  int MPI_Comm_get_parent(MPI_Comm *parent);
  int MPI_Comm_group(MPI_Comm comm, MPI_Group *group);
  int MPI_Comm_join(int fd, MPI_Comm *intercomm);
  int MPI_Comm_rank(MPI_Comm comm, int *rank);
  int MPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group);
  int MPI_Comm_remote_size(MPI_Comm comm, int *size);
  int MPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val);
  int MPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler);
  int MPI_Comm_set_name(MPI_Comm comm, char *comm_name);
  int MPI_Comm_size(MPI_Comm comm, int *size);
  int MPI_Comm_spawn(char *command, char **argv, int maxprocs, MPI_Info info, 
                     int root, MPI_Comm comm, MPI_Comm *intercomm, 
                     int *array_of_errcodes);
  int MPI_Comm_spawn_multiple(int count, char **array_of_commands, char ***array_of_argv, 
                              int *array_of_maxprocs, MPI_Info *array_of_info, 
                              int root, MPI_Comm comm, MPI_Comm *intercomm, 
                              int *array_of_errcodes);
  int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm);
  int MPI_Comm_test_inter(MPI_Comm comm, int *flag);
  /* 
   * Anju:
   * Here ends the alphabet C
   */
  int MPI_Dims_create(int, int, int *);
  MPI_Fint MPI_Errhandler_c2f(MPI_Errhandler err);
  int MPI_Errhandler_create(MPI_Handler_function *, MPI_Errhandler *);
  MPI_Errhandler MPI_Errhandler_f2c(MPI_Fint f_handle);
  int MPI_Errhandler_free(MPI_Errhandler *);
  int MPI_Errhandler_get(MPI_Comm, MPI_Errhandler *);
  int MPI_Errhandler_set(MPI_Comm, MPI_Errhandler);
  int MPI_Error_class(int, int *);
  int MPI_Error_string(int, char *, int *);
  int MPI_Exscan(void *, void *, int, MPI_Datatype, MPI_Op, MPI_Comm);
  int MPI_Finalize(void);
  int MPI_Finalized(int *flag);
  int MPI_Free_mem(void *base);
  int MPI_Gather(void *, int, MPI_Datatype, void *, int, 
                 MPI_Datatype, int, MPI_Comm);
  int MPI_Gatherv(void *, int, MPI_Datatype, void *, int *, 
                  int *, MPI_Datatype, int, MPI_Comm);
  int MPI_Get_address(void *, MPI_Aint *);
  int MPI_Get(void *, int, MPI_Datatype, int, MPI_Aint, int, 
              MPI_Datatype, MPI_Win);
  int MPI_Get_count(MPI_Status *, MPI_Datatype, int *);
  int MPI_Get_elements(MPI_Status *, MPI_Datatype, int *);
  int MPI_Get_processor_name(char *, int *);
  int MPI_Get_version(int *, int *);
  int MPI_Graph_create(MPI_Comm, int, int *, int *, int, MPI_Comm *);
  int MPI_Graphdims_get(MPI_Comm, int *, int *);
  int MPI_Graph_get(MPI_Comm, int, int, int *, int *);
  int MPI_Graph_map(MPI_Comm, int, int *, int *, int *);
  int MPI_Graph_neighbors(MPI_Comm, int, int, int *);
  int MPI_Graph_neighbors_count(MPI_Comm, int, int *);
  MPI_Fint MPI_Group_c2f(MPI_Group);
  int MPI_Group_compare(MPI_Group, MPI_Group, int *);
  int MPI_Group_difference(MPI_Group, MPI_Group, MPI_Group *);
  int MPI_Group_excl(MPI_Group, int, int *, MPI_Group *);
  MPI_Group MPI_Group_f2c(MPI_Fint);
  int MPI_Group_free(MPI_Group *);
  int MPI_Group_incl(MPI_Group, int, int *, MPI_Group *);
  int MPI_Group_intersection(MPI_Group, MPI_Group, MPI_Group *);
  int MPI_Group_range_excl(MPI_Group, int, int [][3], MPI_Group *);
  int MPI_Group_range_incl(MPI_Group, int, int [][3], MPI_Group *);
  int MPI_Group_rank(MPI_Group, int *);
  int MPI_Group_size(MPI_Group, int *);
  int MPI_Group_translate_ranks(MPI_Group, int, int *, MPI_Group, int *);
  int MPI_Group_union(MPI_Group, MPI_Group, MPI_Group *);
  int MPI_Ibsend(void *, int, MPI_Datatype, int, int, MPI_Comm, 
                 MPI_Request *);
  MPI_Fint MPI_Info_c2f(MPI_Info);
  int MPI_Info_create(MPI_Info *);
  int MPI_Info_delete(MPI_Info, char *);
  int MPI_Info_dup(MPI_Info, MPI_Info *);
  MPI_Info MPI_Info_f2c(MPI_Fint);
  int MPI_Info_free(MPI_Info *);
  int MPI_Info_get(MPI_Info, char *, int, char *, int *);
  int MPI_Info_get_nkeys(MPI_Info, int *);
  int MPI_Info_get_nthkey(MPI_Info, int, char *);
  int MPI_Info_get_valuelen(MPI_Info, char *, int *, int *);
  int MPI_Info_set(MPI_Info, char *, char *);
  int MPI_Init(int *, char ***);
  int MPI_Initialized(int *);
  int MPI_Init_thread(int *, char ***, int, int *);
  int MPI_Intercomm_create(MPI_Comm, int, MPI_Comm, int, int, MPI_Comm *);
  int MPI_Intercomm_merge(MPI_Comm, int, MPI_Comm *);
  int MPI_Iprobe(int, int, MPI_Comm, int *, MPI_Status *);
  int MPI_Irecv(void *, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
  int MPI_Irsend(void *, int, MPI_Datatype, int, int, MPI_Comm,
                 MPI_Request *);
  int MPI_Isend(void *, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
  int MPI_Issend(void *, int, MPI_Datatype, int, int, MPI_Comm, 
                 MPI_Request *);
  int MPI_Is_thread_main(int *);
  int MPI_Keyval_create(MPI_Copy_function *, MPI_Delete_function *, 
                        int *, void *);
  int MPI_Keyval_free(int *);
  int MPI_Lookup_name(char *, MPI_Info, char *);
  MPI_Fint MPI_Op_c2f(MPI_Op op); 
  int MPI_Op_create(MPI_User_function *, int, MPI_Op *);
  int MPI_Open_port(MPI_Info, char *);
  MPI_Op MPI_Op_f2c(MPI_Fint f_handle);
  int MPI_Op_free(MPI_Op *);
  int MPI_Pack(void *, int, MPI_Datatype, void *, int, int *, MPI_Comm);
  int MPI_Pack_size(int, MPI_Datatype, MPI_Comm, int *);
  int MPI_Pcontrol(int level, ...);
  int MPI_Probe(int, int, MPI_Comm, MPI_Status *);
  int MPI_Publish_name(char *, MPI_Info, char *);
  int MPI_Put(void *, int, MPI_Datatype, int, MPI_Aint, int, 
              MPI_Datatype, MPI_Win);
  int MPI_Query_thread(int *);
  int MPI_Recv(void *, int, MPI_Datatype, int, int, MPI_Comm, MPI_Status *);
  int MPI_Recv_init(void *, int, MPI_Datatype, int, int, 
                    MPI_Comm, MPI_Request *);
  int MPI_Reduce(void *, void *, int, MPI_Datatype, MPI_Op, int, MPI_Comm);
  int MPI_Reduce_scatter(void *, void *, int *, MPI_Datatype, 
                         MPI_Op, MPI_Comm);
  MPI_Fint MPI_Request_c2f(MPI_Request);
  MPI_Request MPI_Request_f2c(MPI_Fint);
  int MPI_Request_free(MPI_Request *);
  int MPI_Rsend(void *, int, MPI_Datatype, int, int, MPI_Comm);
  int MPI_Rsend_init(void *, int, MPI_Datatype, int, int, 
                     MPI_Comm, MPI_Request *);
  int MPI_Scan(void *, void *, int, MPI_Datatype, MPI_Op, MPI_Comm);
  int MPI_Scatter(void *, int, MPI_Datatype, void *, int, 
                  MPI_Datatype, int, MPI_Comm);
  int MPI_Scatterv(void *, int *, int *, MPI_Datatype, 
                   void *, int, MPI_Datatype, int, MPI_Comm);
  int MPI_Send(void *, int, MPI_Datatype, int, int, MPI_Comm);
  int MPI_Send_init(void *, int, MPI_Datatype, int, int, 
                    MPI_Comm, MPI_Request *);
  int MPI_Sendrecv(void *, int, MPI_Datatype, int, int, void *, 
                   int, MPI_Datatype, int, int, MPI_Comm, 
                   MPI_Status *);
  int MPI_Sendrecv_replace(void *, int, MPI_Datatype, int, int, 
                           int, int, MPI_Comm, MPI_Status *);
  int MPI_Ssend(void *, int, MPI_Datatype, int, int, MPI_Comm);
  int MPI_Ssend_init(void *, int, MPI_Datatype, int, int, 
                     MPI_Comm, MPI_Request *);
  int MPI_Startall(int, MPI_Request *);
  int MPI_Start(MPI_Request *);
  int MPI_Status_c2f(MPI_Status *, MPI_Fint *);
  int MPI_Status_f2c(MPI_Fint *, MPI_Status *);
  int MPI_Testall(int count, MPI_Request array_of_requests[], int *flag, 
                  MPI_Status array_of_statuses[]);
  int MPI_Testany(int count, MPI_Request array_of_requests[], int *index, 
                  MPI_Status *status);
  int MPI_Test(MPI_Request *request, int *flag, MPI_Status *status);
  int MPI_Test_cancelled(MPI_Status *status, int *flag);
  int MPI_Testsome(int incount, MPI_Request array_of_requests[], 
                   int *outcount, int array_of_indices, 
                   MPI_Status array_of_statuses);
  int MPI_Topo_test(MPI_Comm comm, int *status);
  MPI_Fint MPI_Type_c2f(MPI_Datatype);
  int MPI_Type_commit(MPI_Datatype *type);
  int MPI_Type_contiguous(int count, MPI_Datatype oldtype, 
                          MPI_Datatype *newtype);
  int MPI_Type_create_darray(int size, int rank, int ndims, 
                             int gsize_array[], int distrib_array[], 
                             int darg_array[], int psize_array[],
                             int order, MPI_Datatype oldtype, 
                             MPI_Datatype *newtype);
  int MPI_Type_create_f90_complex(int p, int r, MPI_Datatype *newtype);
  int MPI_Type_create_f90_integer(int r, MPI_Datatype *newtype);
  int MPI_Type_create_f90_real(int p, int r, MPI_Datatype *newtype);
  int MPI_Type_create_hindexed(int count, int array_of_blocklengths[], 
                               MPI_Aint array_of_displacements[], 
                               MPI_Datatype oldtype, 
                               MPI_Datatype *newtype);
  int MPI_Type_create_hvector(int count, int blocklength, MPI_Aint stride, 
                              MPI_Datatype oldtype, 
                              MPI_Datatype *newtype);
  int MPI_Type_create_keyval(MPI_Type_copy_attr_function *type_copy_attr_fn, 
                             MPI_Type_delete_attr_function *type_delete_attr_fn, 
                             int *type_keyval, void *extra_state);
  int MPI_Type_create_indexed_block(int count, int blocklength,
                                  int array_of_displacements[],
                                  MPI_Datatype oldtype,
                                  MPI_Datatype *newtype);
  int MPI_Type_create_struct(int count, int array_of_block_lengths[], 
                             MPI_Aint array_of_displacements[], 
                             MPI_Datatype array_of_types[], 
                             MPI_Datatype *newtype);
  int MPI_Type_create_subarray(int ndims, int size_array[], int subsize_array[], 
                               int start_array[], int order, 
                               MPI_Datatype oldtype, MPI_Datatype *newtype);
  int MPI_Type_create_resized(MPI_Datatype oldtype, MPI_Aint lb, 
                               MPI_Aint extent, MPI_Datatype *newtype); 
  int MPI_Type_delete_attr(MPI_Datatype type, int type_keyval);
  int MPI_Type_dup(MPI_Datatype type, MPI_Datatype *newtype);
  int MPI_Type_extent(MPI_Datatype type, MPI_Aint *extent);
  int MPI_Type_free(MPI_Datatype *type);
  int MPI_Type_free_keyval(int *type_keyval);
  MPI_Datatype MPI_Type_f2c(MPI_Fint);
  int MPI_Type_get_attr(MPI_Datatype type, int type_keyval, 
                        void *attribute_val, int *flag);
  int MPI_Type_get_contents(MPI_Datatype mtype, int max_integers, 
                            int max_addresses, int max_datatypes, 
                            int array_of_integers[], 
                            MPI_Aint array_of_addresses[], 
                            MPI_Datatype array_of_datatypes[]);
  int MPI_Type_get_envelope(MPI_Datatype type, int *num_integers, 
                            int *num_addresses, int *num_datatypes, 
                            int *combiner);
  int MPI_Type_get_extent(MPI_Datatype type, MPI_Aint *lb, 
                          MPI_Aint *extent);
  int MPI_Type_get_name(MPI_Datatype type, char *type_name, 
                        int *resultlen);
  int MPI_Type_get_true_extent(MPI_Datatype datatype, MPI_Aint *true_lb, 
                               MPI_Aint *true_extent);
  int MPI_Type_hindexed(int count, int array_of_blocklengths[], 
                        MPI_Aint array_of_displacements[], 
                        MPI_Datatype oldtype, MPI_Datatype *newtype);
  int MPI_Type_hvector(int count, int blocklength, MPI_Aint stride, 
                       MPI_Datatype oldtype, MPI_Datatype *newtype);
  int MPI_Type_indexed(int count, int array_of_blocklengths[], 
                       int array_of_displacements[], 
                       MPI_Datatype oldtype, MPI_Datatype *newtype);
  int MPI_Type_lb(MPI_Datatype type, MPI_Aint *lb);
  int MPI_Type_match_size(int typeclass, int size, MPI_Datatype *type);
  int MPI_Type_set_attr(MPI_Datatype type, int type_keyval, 
                        void *attr_val);
  int MPI_Type_set_name(MPI_Datatype type, char *type_name);
  int MPI_Type_size(MPI_Datatype type, int *size);
  int MPI_Type_struct(int count, int array_of_blocklengths[], 
                      MPI_Aint array_of_displacements[], 
                      MPI_Datatype array_of_types[], 
                      MPI_Datatype *newtype);
  int MPI_Type_ub(MPI_Datatype mtype, MPI_Aint *ub);
  int MPI_Type_vector(int count, int blocklength, int stride, 
                      MPI_Datatype oldtype, MPI_Datatype *newtype);
  int MPI_Unpack(void *, int, int *, void *, int, MPI_Datatype, MPI_Comm);
  int MPI_Unpublish_name(char *, MPI_Info, char *);
  int MPI_Waitall(int, MPI_Request *, MPI_Status *);
  int MPI_Waitany(int, MPI_Request *, int *, MPI_Status *);
  int MPI_Wait(MPI_Request *, MPI_Status *);
  int MPI_Waitsome(int, MPI_Request *, int *, int *, MPI_Status *);
  MPI_Fint MPI_Win_c2f(MPI_Win);
  int MPI_Win_call_errhandler(MPI_Win, int);
  int MPI_Win_complete(MPI_Win);
  int MPI_Win_create(void *, MPI_Aint, int, MPI_Info, MPI_Comm, MPI_Win *);
  int MPI_Win_create_errhandler(MPI_Win_errhandler_fn *, 
                                MPI_Errhandler *);
  int MPI_Win_create_keyval(MPI_Win_copy_attr_function *, 
                            MPI_Win_delete_attr_function *, 
                            int *, void *);
  int MPI_Win_delete_attr(MPI_Win, int);
  MPI_Win MPI_Win_f2c(MPI_Fint);
  int MPI_Win_fence(int, MPI_Win);
  int MPI_Win_free(MPI_Win *);
  int MPI_Win_free_keyval(int *);
  int MPI_Win_get_attr(MPI_Win, int, void *, int *);
  int MPI_Win_get_errhandler(MPI_Win, MPI_Errhandler *);
  int MPI_Win_get_group(MPI_Win, MPI_Group *);
  int MPI_Win_get_name(MPI_Win, char *, int *);
  int MPI_Win_lock(int, int, int, MPI_Win);
  int MPI_Win_post(MPI_Group, int, MPI_Win);
  int MPI_Win_set_attr(MPI_Win, int, void *);
  int MPI_Win_set_errhandler(MPI_Win, MPI_Errhandler);
  int MPI_Win_set_name(MPI_Win, char *);
  int MPI_Win_start(MPI_Group, int, MPI_Win);
  int MPI_Win_test(MPI_Win, int *);
  int MPI_Win_unlock(int, MPI_Win);
  int MPI_Win_wait(MPI_Win);
  double MPI_Wtick(void);
  double MPI_Wtime(void);

  /*
   * Profiling MPI API
   */
  int PMPI_Abort(MPI_Comm comm, int errorcode);
  int PMPI_Accumulate(void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
                   int target_rank, MPI_Aint target_disp, int target_count,
                   MPI_Datatype target_datatype, MPI_Op op, MPI_Win win); 
  int PMPI_Add_error_class(int *errorclass);
  int PMPI_Add_error_code(int errorclass, int *errorcode);
  int PMPI_Add_error_string(int errorcode, char *string);
  int PMPI_Address(void *location, MPI_Aint *address);
  int PMPI_Allgather(void *sendbuf, int sendcount, MPI_Datatype sendtype, 
                    void *recvbuf, int recvcount, 
                    MPI_Datatype recvtype, MPI_Comm comm);
  int PMPI_Allgatherv(void *sendbuf, int sendcount, MPI_Datatype sendtype, 
                     void *recvbuf, int *recvcounts, 
                     int *displs, MPI_Datatype recvtype, MPI_Comm comm);
  int PMPI_Alloc_mem(MPI_Aint size, MPI_Info info, 
                    void *baseptr);
  int PMPI_Allreduce(void *sendbuf, void *recvbuf, int count, 
                    MPI_Datatype datatype, MPI_Op op, MPI_Comm comm); 
  int PMPI_Alltoall(void *sendbuf, int sendcount, MPI_Datatype sendtype, 
                   void *recvbuf, int recvcount, 
                   MPI_Datatype recvtype, MPI_Comm comm);
  int PMPI_Alltoallv(void *sendbuf, int *sendcounts, int *sdispls, 
                    MPI_Datatype sendtype, void *recvbuf, int *recvcounts,
                    int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
  int PMPI_Alltoallw(void *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype *sendtypes, 
                    void *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype *recvtypes,
                    MPI_Comm comm);
  int PMPI_Attr_delete(MPI_Comm comm, int keyval);
  int PMPI_Attr_get(MPI_Comm comm, int keyval, void *attribute_val, int *flag);
  int PMPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val);
  int PMPI_Barrier(MPI_Comm comm);
  int PMPI_Bcast(void *buffer, int count, MPI_Datatype datatype, 
                int root, MPI_Comm comm);
  int PMPI_Bsend(void *buf, int count, MPI_Datatype datatype, 
                int dest, int tag, MPI_Comm comm);
  int PMPI_Bsend_init(void *buf, int count, MPI_Datatype datatype, 
                     int dest, int tag, MPI_Comm comm, MPI_Request *request); 
  int PMPI_Buffer_attach(void *buffer, int size);
  int PMPI_Buffer_detach(void *buffer, int *size);
  int PMPI_Cancel(MPI_Request *request);
  int PMPI_Cart_coords(MPI_Comm comm, int rank, int maxdims, int *coords);
  int PMPI_Cart_create(MPI_Comm old_comm, int ndims, int *dims, 
                      int *periods, int redorder, MPI_Comm *comm_cart);
  int PMPI_Cart_get(MPI_Comm comm, int maxdims, int *dims, 
                   int *periods, int *coords);
  int PMPI_Cart_map(MPI_Comm comm, int ndims, int *dims, 
                   int *periods, int *newrank);
  int PMPI_Cart_rank(MPI_Comm comm, int *coords, int *rank);
  int PMPI_Cart_shift(MPI_Comm comm, int direction, int disp, 
                     int *rank_source, int *rank_dest);
  int PMPI_Cart_sub(MPI_Comm comm, int *remain_dims, MPI_Comm *new_comm);
  int PMPI_Cartdim_get(MPI_Comm comm, int *ndims);
  int PMPI_Close_port(char *port_name);
  int PMPI_Comm_accept(char *port_name, MPI_Info info, int root, 
                      MPI_Comm comm, MPI_Comm *newcomm);
  MPI_Fint PMPI_Comm_c2f(MPI_Comm comm);
  int PMPI_Comm_call_error_handler(MPI_Comm comm, int errorcode);
  int PMPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result);
  int PMPI_Comm_connect(char *port_name, MPI_Info info, int root, 
                       MPI_Comm comm, MPI_Comm *newcomm);
  int PMPI_Comm_create_errhandler(MPI_Comm_errhandler_fn *function, 
                                 MPI_Errhandler *errhandler);
  int PMPI_Comm_create_keyval(MPI_Comm_copy_attr_function *comm_copy_attr_fn, 
                             MPI_Comm_delete_attr_function *comm_delete_attr_fn, 
                             int *comm_keyval, void *extra_state);
  int PMPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm);
  int PMPI_Comm_delete_attr(MPI_Comm comm, int comm_keyval);
  int PMPI_Comm_disconnect(MPI_Comm *comm);
  int PMPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm);
  MPI_Comm PMPI_Comm_f2c(MPI_Fint comm);
  int PMPI_Comm_free_keyval(int *comm_keyval);
  int PMPI_Comm_free(MPI_Comm *comm);
  int PMPI_Comm_get_attr(MPI_Comm comm, int comm_keyval, 
                        void *attribute_val, int *flag);
  int PMPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *erhandler);
  int PMPI_Comm_get_name(MPI_Comm comm, char *comm_name, int *resultlen);
  int PMPI_Comm_get_parent(MPI_Comm *parent);
  int PMPI_Comm_group(MPI_Comm comm, MPI_Group *group);
  int PMPI_Comm_join(int fd, MPI_Comm *intercomm);
  int PMPI_Comm_rank(MPI_Comm comm, int *rank);
  int PMPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group);
  int PMPI_Comm_remote_size(MPI_Comm comm, int *size);
  int PMPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val);
  int PMPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler);
  int PMPI_Comm_set_name(MPI_Comm comm, char *comm_name);
  int PMPI_Comm_size(MPI_Comm comm, int *size);
  int PMPI_Comm_spawn(char *command, char **argv, int maxprocs, MPI_Info info, 
                     int root, MPI_Comm comm, MPI_Comm *intercomm, 
                     int *array_of_errcodes);
  int PMPI_Comm_spawn_multiple(int count, char **array_of_commands, char ***array_of_argv, 
                              int *array_of_maxprocs, MPI_Info *array_of_info, 
                              int root, MPI_Comm comm, MPI_Comm *intercomm, 
                              int *array_of_errcodes);
  int PMPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm);
  int PMPI_Comm_test_inter(MPI_Comm comm, int *flag);
  /*
   * Here ends Aplphabet C
   */
  int PMPI_Dims_create(int, int, int *);
  MPI_Fint PMPI_Errhandler_c2f(MPI_Errhandler err);
  int PMPI_Errhandler_create(MPI_Handler_function *, MPI_Errhandler *);
  MPI_Errhandler PMPI_Errhandler_f2c(MPI_Fint f_handle);
  int PMPI_Errhandler_free(MPI_Errhandler *);
  int PMPI_Errhandler_get(MPI_Comm, MPI_Errhandler *);
  int PMPI_Errhandler_set(MPI_Comm, MPI_Errhandler);
  int PMPI_Error_class(int, int *);
  int PMPI_Error_string(int, char *, int *);
  int PMPI_Exscan(void *, void *, int, MPI_Datatype, MPI_Op, MPI_Comm);
  int PMPI_Finalize(void);
  int PMPI_Finalized(int *flag);
  int PMPI_Free_mem(void *base);
  int PMPI_Gather(void *, int, MPI_Datatype, void *, int, 
                  MPI_Datatype, int, MPI_Comm);
  int PMPI_Gatherv(void *, int, MPI_Datatype, void *, int *, 
                   int *, MPI_Datatype, int, MPI_Comm);
  int PMPI_Get_address(void *, MPI_Aint *);
  int PMPI_Get(void *, int, MPI_Datatype, int, MPI_Aint, int, 
               MPI_Datatype, MPI_Win);
  int PMPI_Get_count(MPI_Status *, MPI_Datatype, int *);
  int PMPI_Get_elements(MPI_Status *, MPI_Datatype, int *);
  int PMPI_Get_processor_name(char *, int *);
  int PMPI_Get_version(int *, int *);
  int PMPI_Graph_create(MPI_Comm, int, int *, int *, int, MPI_Comm *);
  int PMPI_Graphdims_get(MPI_Comm, int *, int *);
  int PMPI_Graph_get(MPI_Comm, int, int, int *, int *);
  int PMPI_Graph_map(MPI_Comm, int, int *, int *, int *);
  int PMPI_Graph_neighbors(MPI_Comm, int, int, int *);
  int PMPI_Graph_neighbors_count(MPI_Comm, int, int *);
  MPI_Fint PMPI_Group_c2f(MPI_Group);
  int PMPI_Group_compare(MPI_Group, MPI_Group, int *);
  int PMPI_Group_difference(MPI_Group, MPI_Group, MPI_Group *);
  int PMPI_Group_excl(MPI_Group, int, int *, MPI_Group *);
  MPI_Group PMPI_Group_f2c(MPI_Fint);
  int PMPI_Group_free(MPI_Group *);
  int PMPI_Group_incl(MPI_Group, int, int *, MPI_Group *);
  int PMPI_Group_intersection(MPI_Group, MPI_Group, MPI_Group *);
  int PMPI_Group_range_excl(MPI_Group, int, int [][3], MPI_Group *);
  int PMPI_Group_range_incl(MPI_Group, int, int [][3], MPI_Group *);
  int PMPI_Group_rank(MPI_Group, int *);
  int PMPI_Group_size(MPI_Group, int *);
  int PMPI_Group_translate_ranks(MPI_Group, int, int *, MPI_Group, int *);
  int PMPI_Group_union(MPI_Group, MPI_Group, MPI_Group *);
  int PMPI_Ibsend(void *, int, MPI_Datatype, int, int, MPI_Comm, 
                  MPI_Request *);
  MPI_Fint PMPI_Info_c2f(MPI_Info);
  int PMPI_Info_create(MPI_Info *);
  int PMPI_Info_delete(MPI_Info, char *);
  int PMPI_Info_dup(MPI_Info, MPI_Info *);
  MPI_Info PMPI_Info_f2c(MPI_Fint);
  int PMPI_Info_free(MPI_Info *);
  int PMPI_Info_get(MPI_Info, char *, int, char *, int *);
  int PMPI_Info_get_nkeys(MPI_Info, int *);
  int PMPI_Info_get_nthkey(MPI_Info, int, char *);
  int PMPI_Info_get_valuelen(MPI_Info, char *, int *, int *);
  int PMPI_Info_set(MPI_Info, char *, char *);
  int PMPI_Init(int *, char ***);
  int PMPI_Initialized(int *);
  int PMPI_Init_thread(int *, char ***, int, int *);
  int PMPI_Intercomm_create(MPI_Comm, int, MPI_Comm, int, int, MPI_Comm *);
  int PMPI_Intercomm_merge(MPI_Comm, int, MPI_Comm *);
  int PMPI_Iprobe(int, int, MPI_Comm, int *, MPI_Status *);
  int PMPI_Irecv(void *, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
  int PMPI_Irsend(void *, int, MPI_Datatype, int, int, MPI_Comm,
                  MPI_Request *);
  int PMPI_Isend(void *, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
  int PMPI_Issend(void *, int, MPI_Datatype, int, int, MPI_Comm, 
                  MPI_Request *);
  int PMPI_Is_thread_main(int *);
  int PMPI_Keyval_create(MPI_Copy_function *, MPI_Delete_function *, 
                         int *, void *);
  int PMPI_Keyval_free(int *);
  int PMPI_Lookup_name(char *, MPI_Info, char *);
  MPI_Fint PMPI_Op_c2f(MPI_Op op); 
  int PMPI_Op_create(MPI_User_function *, int, MPI_Op *);
  int PMPI_Open_port(MPI_Info, char *);
  MPI_Op PMPI_Op_f2c(MPI_Fint f_handle);
  int PMPI_Op_free(MPI_Op *);
  int PMPI_Pack(void *, int, MPI_Datatype, void *, int, int *, MPI_Comm);
  int PMPI_Pack_size(int, MPI_Datatype, MPI_Comm, int *);
  int PMPI_Pcontrol(int level, ...);
  int PMPI_Probe(int, int, MPI_Comm, MPI_Status *);
  int PMPI_Publish_name(char *, MPI_Info, char *);
  int PMPI_Put(void *, int, MPI_Datatype, int, MPI_Aint, int, 
               MPI_Datatype, MPI_Win);
  int PMPI_Query_thread(int *);
  int PMPI_Recv(void *, int, MPI_Datatype, int, int, MPI_Comm, MPI_Status *);
  int PMPI_Recv_init(void *, int, MPI_Datatype, int, int, 
                     MPI_Comm, MPI_Request *);
  int PMPI_Reduce(void *, void *, int, MPI_Datatype, MPI_Op, int, MPI_Comm);
  int PMPI_Reduce_scatter(void *, void *, int *, MPI_Datatype, 
                          MPI_Op, MPI_Comm);
  MPI_Fint PMPI_Request_c2f(MPI_Request);
  MPI_Request PMPI_Request_f2c(MPI_Fint);
  int PMPI_Request_free(MPI_Request *);
  int PMPI_Rsend(void *, int, MPI_Datatype, int, int, MPI_Comm);
  int PMPI_Rsend_init(void *, int, MPI_Datatype, int, int, 
                      MPI_Comm, MPI_Request *);
  int PMPI_Scan(void *, void *, int, MPI_Datatype, MPI_Op, MPI_Comm);
  int PMPI_Scatter(void *, int, MPI_Datatype, void *, int, 
                   MPI_Datatype, int, MPI_Comm);
  int PMPI_Scatterv(void *, int *, int *, MPI_Datatype, 
                    void *, int, MPI_Datatype, int, MPI_Comm);
  int PMPI_Send(void *, int, MPI_Datatype, int, int, MPI_Comm);
  int PMPI_Send_init(void *, int, MPI_Datatype, int, int, 
                     MPI_Comm, MPI_Request *);
  int PMPI_Sendrecv(void *, int, MPI_Datatype, int, int, void *, 
                    int, MPI_Datatype, int, int, MPI_Comm, 
                    MPI_Status *);
  int PMPI_Sendrecv_replace(void *, int, MPI_Datatype, int, int, 
                            int, int, MPI_Comm, MPI_Status *);
  int PMPI_Ssend(void *, int, MPI_Datatype, int, int, MPI_Comm);
  int PMPI_Ssend_init(void *, int, MPI_Datatype, int, int, 
                      MPI_Comm, MPI_Request *);
  int PMPI_Startall(int, MPI_Request *);
  int PMPI_Start(MPI_Request *);
  int PMPI_Status_c2f(MPI_Status *, MPI_Fint *);
  int PMPI_Status_f2c(MPI_Fint *, MPI_Status *);
  int PMPI_Testall(int count, MPI_Request array_of_requests[], int *flag, 
                  MPI_Status array_of_statuses[]);
  int PMPI_Testany(int count, MPI_Request array_of_requests[], int *index, 
                  MPI_Status *status);
  int PMPI_Test(MPI_Request *request, int *flag, MPI_Status *status);
  int PMPI_Test_cancelled(MPI_Status *status, int *flag);
  int PMPI_Testsome(int incount, MPI_Request array_of_requests[], 
                   int *outcount, int array_of_indices, 
                   MPI_Status array_of_statuses);
  int PMPI_Topo_test(MPI_Comm comm, int *status);
  MPI_Fint PMPI_Type_c2f(MPI_Datatype);
  int PMPI_Type_commit(MPI_Datatype *type);
  int PMPI_Type_contiguous(int count, MPI_Datatype oldtype, 
                          MPI_Datatype *newtype);
  int PMPI_Type_create_darray(int size, int rank, int ndims, 
                             int gsize_array[], int distrib_array[], 
                             int darg_array[], int psize_array[],
                             int order, MPI_Datatype oldtype, 
                             MPI_Datatype *newtype);
  int PMPI_Type_create_f90_complex(int p, int r, MPI_Datatype *newtype);
  int PMPI_Type_create_f90_integer(int r, MPI_Datatype *newtype);
  int PMPI_Type_create_f90_real(int p, int r, MPI_Datatype *newtype);
  int PMPI_Type_create_hindexed(int count, int array_of_blocklengths[], 
                               MPI_Aint array_of_displacements[], 
                               MPI_Datatype oldtype, 
                               MPI_Datatype *newtype);
  int PMPI_Type_create_hvector(int count, int blocklength, MPI_Aint stride, 
                              MPI_Datatype oldtype, 
                              MPI_Datatype *newtype);
  int PMPI_Type_create_keyval(MPI_Type_copy_attr_function *type_copy_attr_fn, 
                             MPI_Type_delete_attr_function *type_delete_attr_fn, 
                             int *type_keyval, void *extra_state);
  int PMPI_Type_create_indexed_block(int count, int blocklength,
                                  int array_of_displacements[],
                                  MPI_Datatype oldtype,
                                  MPI_Datatype *newtype);
  int PMPI_Type_create_struct(int count, int array_of_block_lengths[], 
                             MPI_Aint array_of_displacements[], 
                             MPI_Datatype array_of_types[], 
                             MPI_Datatype *newtype);
  int PMPI_Type_create_subarray(int ndims, int size_array[], int subsize_array[], 
                               int start_array[], int order, 
                               MPI_Datatype oldtype, MPI_Datatype *newtype);
  int PMPI_Type_create_resized(MPI_Datatype oldtype, MPI_Aint lb, 
                               MPI_Aint extent, MPI_Datatype *newtype); 
  int PMPI_Type_delete_attr(MPI_Datatype type, int type_keyval);
  int PMPI_Type_dup(MPI_Datatype type, MPI_Datatype *newtype);
  int PMPI_Type_extent(MPI_Datatype type, MPI_Aint *extent);
  int PMPI_Type_free(MPI_Datatype *type);
  int PMPI_Type_free_keyval(int *type_keyval);
  MPI_Datatype PMPI_Type_f2c(MPI_Fint);
  int PMPI_Type_get_attr(MPI_Datatype type, int type_keyval, 
                        void *attribute_val, int *flag);
  int PMPI_Type_get_contents(MPI_Datatype mtype, int max_integers, 
                            int max_addresses, int max_datatypes, 
                            int array_of_integers[], 
                            MPI_Aint array_of_addresses[], 
                            MPI_Datatype array_of_datatypes[]);
  int PMPI_Type_get_envelope(MPI_Datatype type, int *num_integers, 
                            int *num_addresses, int *num_datatypes, 
                            int *combiner);
  int PMPI_Type_get_extent(MPI_Datatype type, MPI_Aint *lb, 
                          MPI_Aint *extent);
  int PMPI_Type_get_name(MPI_Datatype type, char *type_name, 
                        int *resultlen);
  int PMPI_Type_get_true_extent(MPI_Datatype datatype, MPI_Aint *true_lb, 
                               MPI_Aint *true_extent);
  int PMPI_Type_hindexed(int count, int array_of_blocklengths[], 
                        MPI_Aint array_of_displacements[], 
                        MPI_Datatype oldtype, MPI_Datatype *newtype);
  int PMPI_Type_hvector(int count, int blocklength, MPI_Aint stride, 
                       MPI_Datatype oldtype, MPI_Datatype *newtype);
  int PMPI_Type_indexed(int count, int array_of_blocklengths[], 
                       int array_of_displacements[], 
                       MPI_Datatype oldtype, MPI_Datatype *newtype);
  int PMPI_Type_lb(MPI_Datatype type, MPI_Aint *lb);
  int PMPI_Type_match_size(int typeclass, int size, MPI_Datatype *type);
  int PMPI_Type_set_attr(MPI_Datatype type, int type_keyval, 
                        void *attr_val);
  int PMPI_Type_set_name(MPI_Datatype type, char *type_name);
  int PMPI_Type_size(MPI_Datatype type, int *size);
  int PMPI_Type_struct(int count, int array_of_blocklengths[], 
                      MPI_Aint array_of_displacements[], 
                      MPI_Datatype array_of_types[], 
                      MPI_Datatype *newtype);
  int PMPI_Type_ub(MPI_Datatype mtype, MPI_Aint *ub);
  int PMPI_Type_vector(int count, int blocklength, int stride, 
                      MPI_Datatype oldtype, MPI_Datatype *newtype);
  int PMPI_Unpack(void *, int, int *, void *, int, MPI_Datatype, MPI_Comm);
  int PMPI_Unpublish_name(char *, MPI_Info, char *);
  int PMPI_Waitall(int, MPI_Request *, MPI_Status *);
  int PMPI_Waitany(int, MPI_Request *, int *, MPI_Status *);
  int PMPI_Wait(MPI_Request *, MPI_Status *);
  int PMPI_Waitsome(int, MPI_Request *, int *, int *, MPI_Status *);
  MPI_Fint PMPI_Win_c2f(MPI_Win);
  int PMPI_Win_call_errhandler(MPI_Win, int);
  int PMPI_Win_complete(MPI_Win);
  int PMPI_Win_create(void *, MPI_Aint, int, MPI_Info, MPI_Comm, MPI_Win *);
  int PMPI_Win_create_errhandler(MPI_Win_errhandler_fn *, 
                                 MPI_Errhandler *);
  int PMPI_Win_create_keyval(MPI_Win_copy_attr_function *, 
                             MPI_Win_delete_attr_function *, 
                             int *, void *);
  int PMPI_Win_delete_attr(MPI_Win, int);
  MPI_Win PMPI_Win_f2c(MPI_Fint);
  int PMPI_Win_fence(int, MPI_Win);
  int PMPI_Win_free(MPI_Win *);
  int PMPI_Win_free_keyval(int *);
  int PMPI_Win_get_attr(MPI_Win, int, void *, int *);
  int PMPI_Win_get_errhandler(MPI_Win, MPI_Errhandler *);
  int PMPI_Win_get_group(MPI_Win, MPI_Group *);
  int PMPI_Win_get_name(MPI_Win, char *, int *);
  int PMPI_Win_lock(int, int, int, MPI_Win);
  int PMPI_Win_post(MPI_Group, int, MPI_Win);
  int PMPI_Win_set_attr(MPI_Win, int, void *);
  int PMPI_Win_set_errhandler(MPI_Win, MPI_Errhandler);
  int PMPI_Win_set_name(MPI_Win, char *);
  int PMPI_Win_start(MPI_Group, int, MPI_Win);
  int PMPI_Win_test(MPI_Win, int *);
  int PMPI_Win_unlock(int, MPI_Win);
  int PMPI_Win_wait(MPI_Win);
  double PMPI_Wtick(void);
  double PMPI_Wtime(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_MPI_H */
