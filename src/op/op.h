/*
 * $HEADER$
 */
/**
 * @file
 *
 * Public interface for the MPI_Op handle.
 */

#ifndef OMPI_OP_H
#define OMPI_OP_H

#include "ompi_config.h"

#include "mpi.h"
#include "datatype/datatype.h"
#include "class/ompi_object.h"
#include "class/ompi_pointer_array.h"


/**
 * Fortran handles; must be [manually set to be] equivalent to the
 * values in mpif.h.
 */
enum {
  OMPI_OP_FORTRAN_NULL = 0,
  /**< Corresponds to Fortran MPI_OP_NULL */
  OMPI_OP_FORTRAN_MAX,
  /**< Corresponds to Fortran MPI_MAX */
  OMPI_OP_FORTRAN_MIN,
  /**< Corresponds to Fortran MPI_MIN */
  OMPI_OP_FORTRAN_SUM,
  /**< Corresponds to Fortran MPI_SUM */
  OMPI_OP_FORTRAN_PROD,
  /**< Corresponds to Fortran MPI_PROD */
  OMPI_OP_FORTRAN_LAND,
  /**< Corresponds to Fortran MPI_LAND */
  OMPI_OP_FORTRAN_BAND,
  /**< Corresponds to Fortran MPI_BAND */
  OMPI_OP_FORTRAN_LOR,
  /**< Corresponds to Fortran MPI_LOR */
  OMPI_OP_FORTRAN_BOR,
  /**< Corresponds to Fortran MPI_BOR */
  OMPI_OP_FORTRAN_LXOR,
  /**< Corresponds to Fortran MPI_LXOR */
  OMPI_OP_FORTRAN_BXOR,
  /**< Corresponds to Fortran MPI_BXOR */
  OMPI_OP_FORTRAN_MAXLOC,
  /**< Corresponds to Fortran MPI_MAXLOC */
  OMPI_OP_FORTRAN_MINLOC,
  /**< Corresponds to Fortran MPI_MINLOC */
  OMPI_OP_FORTRAN_REPLACE,
  /**< Corresponds to Fortran MPI_REPLACE */

  OMPI_OP_FORTRAN_MAX_TYPE
  /**< Maximum value */
};

/**
 * Corresponding to the types that we can reduce over.  See
 * MPI-1:4.9.2, p114-115.
 */
enum {
  OMPI_OP_TYPE_INT,
  /**< C integer: int */
  OMPI_OP_TYPE_LONG,
  /**< C integer: long */
  OMPI_OP_TYPE_SHORT,
  /**< C integer: short */
  OMPI_OP_TYPE_UNSIGNED_SHORT,
  /**< C integer: unsigned short */
  OMPI_OP_TYPE_UNSIGNED,
  /**< C integer: unsigned */
  OMPI_OP_TYPE_UNSIGNED_LONG,
  /**< C integer: unsigned long */

  OMPI_OP_TYPE_INTEGER,
  /**< Fortran integer */

  OMPI_OP_TYPE_FLOAT,
  /**< Floating point: float */
  OMPI_OP_TYPE_DOUBLE,
  /**< Floating point: double */
  OMPI_OP_TYPE_REAL,
  /**< Floating point: real */
  OMPI_OP_TYPE_DOUBLE_PRECISION,
  /**< Floating point: double precision */
  OMPI_OP_TYPE_LONG_DOUBLE,
  /**< Floating point: long double */

  OMPI_OP_TYPE_LOGICAL,
  /**< Logical */

  OMPI_OP_TYPE_COMPLEX,
  /**< Complex */

  OMPI_OP_TYPE_BYTE,
  /**< Byte */

  OMPI_OP_TYPE_2REAL,
  /**< 2 location Fortran: 2 real */
  OMPI_OP_TYPE_2DOUBLE_PRECISION,
  /**< 2 location Fortran: 2 double precision */
  OMPI_OP_TYPE_2INTEGER,
  /**< 2 location Fortran: 2 integer */

  OMPI_OP_TYPE_FLOAT_INT,
  /**< 2 location C: float int */
  OMPI_OP_TYPE_DOUBLE_INT,
  /**< 2 location C: double int */
  OMPI_OP_TYPE_LONG_INT,
  /**< 2 location C: long int */
  OMPI_OP_TYPE_2INT,
  /**< 2 location C: int int */
  OMPI_OP_TYPE_SHORT_INT,
  /**< 2 location C: short int */
  OMPI_OP_TYPE_LONG_DOUBLE_INT,
  /**< 2 location C: long double int */

  OMPI_OP_TYPE_WCHAR,
  /**< 2 location C: wchar_t */

  OMPI_OP_TYPE_MAX
  /**< Maximum type */
};


/**
 * Typedef for C op functions.  
 *
 * We don't use MPI_User_function because this would create a
 * confusing dependency loop between this file and mpi.h.  So this is
 * repeated code, but it's better this way (and this typedef will
 * never change, so there's not much of a maintenance worry).
 */
typedef void (ompi_op_c_handler_fn_t)(void *, void *, int *, MPI_Datatype *);


/**
 * Typedef for fortran op functions.
 */
typedef void (ompi_op_fortran_handler_fn_t)(void *, void *, int *, MPI_Fint *);


/*
 * Flags for MPI_Op
 */
#define OMPI_OP_FLAGS_INTRINSIC    0x0001
  /**< Set if the MPI_Op is a built-in operation */
#define OMPI_OP_FLAGS_FORTRAN_FUNC 0x0002
  /**< Set if the callback function is in Fortran */
#define OMPI_OP_FLAGS_ASSOC        0x0004
  /**< Set if the callback function is associative */
#define OMPI_OP_FLAGS_COMMUTE      0x0008
  /**< Set if the callback function is communative */


/**
 * Back-end type of MPI_Op
 */
struct ompi_op_t {
  ompi_object_t super;
  /**< Parent class, for reference counting */

  char o_name[MPI_MAX_OBJECT_NAME];
  /**< Name, for debugging purposes */

  uint32_t o_flags;
  /**< Flags about the op */

  union {
    ompi_op_c_handler_fn_t *c_fn;
    /**< C handler function pointer */
    ompi_op_fortran_handler_fn_t *fort_fn;
    /**< Fortran handler function pointer */
  } o_func[OMPI_OP_TYPE_MAX];
  /**< Array of function pointers, indexed on the operation type.  For
       non-intrinsice MPI_Op's, only the 0th element will be
       meaningful. */

  int o_f_to_c_index;
  /**< Index in Fortran <-> C translation array */
};
/**
 * Convenience typedef 
 */
typedef struct ompi_op_t ompi_op_t;


/**
 * Array to map ddt->id values to the corresponding position in the op
 * function array.
 *
 * NOTE: It is possible to have an implementation without this map.
 * There are basically 3 choices for implementing "how to find the
 * right position in the op array based on the datatype":
 *
 * 1. Use the exact same ordering as ddt->id in the op map.  This is
 * nice in that it's always a direct lookup via one memory
 * de-reference.  But it makes a sparse op array, and it's at least
 * somewhat wasteful.  It also chains the ddt and op implementations
 * together.  If the ddt ever changes its ordering, op is screwed.  It
 * seemed safer from a maintenance point of view not to do it that
 * way.
 *
 * 2. Re-arrange the ddt ID values so that all the reducable types are
 * at the beginning.  This means that we can have a dense array here
 * in op, but then we have the same problem as number one -- and so
 * this didn't seem like a good idea from a maintenance point of view.
 *
 * 3. Create a mapping between the ddt->id values and the position in
 * the op array.  This allows a nice dense op array, and if we make
 * the map based on symbolic values, then if ddt ever changes its
 * ordering, it won't matter to op.  This seemed like the safest thing
 * to do from a maintenance perspective, and since it only costs one
 * extra lookup, and that lookup is way cheaper than the function call
 * to invoke the reduction operation, it seemed like the best idea.
 */
OMPI_DECLSPEC extern int ompi_op_ddt_map[DT_MAX_PREDEFINED];

/**
 * Global variable for MPI_OP_NULL
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_null;

/**
 * Global variable for MPI_MAX
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_max;

/**
 * Global variable for MPI_MIN
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_min;

/**
 * Global variable for MPI_SUM
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_sum;

/**
 * Global variable for MPI_PROD
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_prod;

/**
 * Global variable for MPI_LAND
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_land;

/**
 * Global variable for MPI_BAND
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_band;

/**
 * Global variable for MPI_LOR
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_lor;

/**
 * Global variable for MPI_BOR
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_bor;

/**
 * Global variable for MPI_LXOR
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_lxor;

/**
 * Global variable for MPI_BXOR
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_bxor;

/**
 * Global variable for MPI_MAXLOC
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_maxloc;

/**
 * Global variable for MPI_MINLOC
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_minloc;

/**
 * Global variable for MPI_REPLACE
 */
OMPI_DECLSPEC extern ompi_op_t ompi_mpi_op_replace;


/**
 * Table for Fortran <-> C op handle conversion
 */
OMPI_DECLSPEC extern ompi_pointer_array_t *ompi_op_f_to_c_table;


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  /**
   * Initialize the op interface.
   *
   * @returns OMPI_SUCCESS Upon success
   * @returns OMPI_ERROR Otherwise
   *
   * Invoked from ompi_mpi_init(); sets up the op interface, creates
   * the predefined MPI operations, and creates the corresopnding F2C
   * translation table.
   */
  int ompi_op_init(void);

  /**
   * Finalize the op interface.
   *
   * @returns OMPI_SUCCESS Always
   *
   * Invokes from ompi_mpi_finalize(); tears down the op interface, and
   * destroys the F2C translation table.
   */
  int ompi_op_finalize(void);

  /**
   * Create a ompi_op_t
   *
   * @param commute Boolean indicating whether the operation is
   *        communative or not
   * @param func Function pointer of the error handler
   *
   * @returns op Pointer to the ompi_op_t that will be
   *   created and returned
   *
   * This function is called as the back-end of all the MPI_OP_CREATE
   * functions.  It creates a new ompi_op_t object, initializes it to
   * the correct object type, and sets the callback function on it.
   *
   * The type of the function pointer is (arbitrarily) the fortran
   * function handler type.  Since this function has to accept 2
   * different function pointer types (lest we have 2 different
   * functions to create errhandlers), the fortran one was picked
   * arbitrarily.  Note that (void*) is not sufficient because at
   * least theoretically, a sizeof(void*) may not necessarily be the
   * same as sizeof(void(*)).
   *
   * NOTE: It *always* sets the "fortran" flag to false.  The Fortran
   * wrapper for MPI_OP_CREATE is expected to reset this flag to true
   * manually.
   */
  ompi_op_t *ompi_op_create(bool commute, ompi_op_fortran_handler_fn_t *func);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/**
 * Check to see if an op is intrinsic.
 *
 * @param op The op to check
 *
 * @returns true If the op is intrinsic
 * @returns false If the op is not intrinsic
 *
 * Self-explanitory.  This is needed in a few top-level MPI functions;
 * this function is provided to hide the internal structure field
 * names.
 */
static inline bool ompi_op_is_intrinsic(ompi_op_t *op)
{
  return (bool) (0 != (op->o_flags & OMPI_OP_FLAGS_INTRINSIC));
}


/**
 * Check to see if an op is communative or not
 *
 * @param op The op to check
 *
 * @returns true If the op is communative
 * @returns false If the op is not communative
 *
 * Self-explanitory.  This is needed in a few top-level MPI functions;
 * this function is provided to hide the internal structure field
 * names.
 */
static inline bool ompi_op_is_commute(ompi_op_t *op)
{
  return (bool) (0 != (op->o_flags & OMPI_OP_FLAGS_COMMUTE));
}


/**
 * Perform a reduction operation.
 *
 * @param op The operation (IN)
 * @param source Source (input) buffer (IN)
 * @param target Target (output) buffer (IN/OUT)
 * @param count Number of elements (IN)
 * @param dtype MPI datatype (IN)
 *
 * @returns void As with MPI user-defined reduction functions, there
 * is no return code from this function.
 *
 * Perform a reduction operation with count elements of type dtype in
 * the buffers source and target.  The target buffer obtains the
 * result (i.e., the original values in the target buffer are reduced
 * with the values in the source buffer and the result is stored in
 * the target buffer).
 *
 * This function figures out which reduction operation function to
 * invoke and wehther to invoke it with C- or Fortran-style invocation
 * methods.  If the op is intrinsic and has the operation defined for
 * dtype, the appropriate bacl-end function will be invoked.
 * Otherwise, the op is assumed to be a user op and the first function
 * pointer in the op array will be used.
 *
 * NOTE: This function assumes that a correct combination will be
 * given to it; it makes no provision for errors (in the name of
 * optimization).  If you give it an intrinsic op with a datatype that
 * is note defined to have that operation, it is likely to seg fault.
 */
static inline void ompi_op_reduce(ompi_op_t *op, void *source, void *target,
                                  int count, ompi_datatype_t *dtype)
{
  MPI_Fint fint = (MPI_Fint) dtype->d_f_to_c_index;

  /*
   * Call the reduction function.  Two dimensions: a) if both the op
   * and the datatype are intrinsic, we have a series of predefined
   * functions for each datatype, b) if the op has a fortran callback
   * function or not.
   *
   * NOTE: We assume here that we will get a valid result back from
   * the ompi_op_ddt_map[] (and not -1) -- if we do, then the
   * parameter check in the top-level MPI function should have caught
   * it.  If we get -1 because the top-level parameter check is off,
   * then it's an erroneous program and it's the user's fault.  :-)
   */

  if (0 != (op->o_flags & OMPI_OP_FLAGS_INTRINSIC) &&
      dtype->id < DT_MAX_PREDEFINED) {
    if (0 != (op->o_flags & OMPI_OP_FLAGS_FORTRAN_FUNC)) {
      op->o_func[ompi_op_ddt_map[dtype->id]].fort_fn(source, target,
                                                     &count, &fint);
    } else {
      op->o_func[ompi_op_ddt_map[dtype->id]].c_fn(source, target, &count,
                                                  &dtype);
    }
  } 

  /* User-defined function */

  else if (0 != (op->o_flags & OMPI_OP_FLAGS_FORTRAN_FUNC)) {
    op->o_func[0].fort_fn(source, target, &count, &fint);
  } else {
    op->o_func[0].c_fn(source, target, &count, &dtype);
  }
}

#endif /* OMPI_OP_H */
