// -*- c++ -*-
// 
// Copyright (c) 2004-2005 The Trustees of Indiana University.
//                         All rights reserved.
// Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
//                         All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$

//

#include "mpicxx.h"

namespace MPI {

#if ! OMPI_HAVE_CXX_EXCEPTION_SUPPORT
int mpi_errno = MPI_SUCCESS;
#endif


const void* BOTTOM = (void*) MPI_BOTTOM;

// return  codes
const int SUCCESS = MPI_SUCCESS;
const int ERR_BUFFER = MPI_ERR_COUNT;
const int ERR_TYPE = MPI_ERR_TYPE;
const int ERR_TAG = MPI_ERR_TAG;
const int ERR_COMM = MPI_ERR_COMM;
const int ERR_RANK = MPI_ERR_RANK;
const int ERR_REQUEST = MPI_ERR_REQUEST;
const int ERR_ROOT = MPI_ERR_ROOT;
const int ERR_GROUP = MPI_ERR_GROUP;
const int ERR_OP = MPI_ERR_OP;
const int ERR_TOPOLOGY = MPI_ERR_TOPOLOGY;
const int ERR_DIMS = MPI_ERR_DIMS;
const int ERR_ARG = MPI_ERR_ARG;
const int ERR_UNKNOWN = MPI_ERR_UNKNOWN;
const int ERR_TRUNCATE = MPI_ERR_TRUNCATE;
const int ERR_OTHER = MPI_ERR_OTHER;
const int ERR_INTERN = MPI_ERR_INTERN;

const int ERR_BASE = MPI_ERR_BASE;
const int ERR_INFO_VALUE = MPI_ERR_INFO_VALUE;
const int ERR_INFO_KEY = MPI_ERR_INFO_KEY;
const int ERR_INFO_NOKEY = MPI_ERR_INFO_NOKEY;
const int ERR_KEYVAL = MPI_ERR_KEYVAL;
const int ERR_NAME = MPI_ERR_NAME;
const int ERR_NO_MEM = MPI_ERR_NO_MEM;
const int ERR_SERVICE = MPI_ERR_SERVICE;
const int ERR_SPAWN = MPI_ERR_SPAWN;
const int ERR_WIN = MPI_ERR_WIN;

const int ERR_PENDING = MPI_ERR_PENDING;
const int ERR_IN_STATUS = MPI_ERR_IN_STATUS;
const int ERR_LASTCODE = MPI_ERR_LASTCODE;

// assorted constants
const int PROC_NULL = MPI_PROC_NULL;
const int ANY_SOURCE = MPI_ANY_SOURCE;
const int ANY_TAG = MPI_ANY_TAG;
const int UNDEFINED = MPI_UNDEFINED;
const int BSEND_OVERHEAD = MPI_BSEND_OVERHEAD;
const int KEYVAL_INVALID = MPI_KEYVAL_INVALID;

// error-handling specifiers
const Errhandler  ERRORS_ARE_FATAL(MPI_ERRORS_ARE_FATAL);
const Errhandler  ERRORS_RETURN(MPI_ERRORS_RETURN);
const Errhandler  ERRORS_THROW_EXCEPTIONS(MPI_ERRORS_RETURN);
//JGS: the MPI_ERRORS_RETURN function in ERRORS_THROW_EXCEPTIONS gets replaced
//by the throw_exptn_fctn in Init)

// maximum sizes for strings
const int MAX_PROCESSOR_NAME = MPI_MAX_PROCESSOR_NAME;
const int MAX_ERROR_STRING = MPI_MAX_ERROR_STRING;
const int MAX_INFO_KEY = MPI_MAX_INFO_KEY;
const int MAX_INFO_VAL = MPI_MAX_INFO_VAL;
const int MAX_PORT_NAME = MPI_MAX_PORT_NAME;
const int MAX_OBJECT_NAME = MPI_MAX_OBJECT_NAME;

// elementary datatypes
const Datatype CHAR(MPI_CHAR);
const Datatype SHORT(MPI_SHORT);
const Datatype INT(MPI_INT);
const Datatype LONG(MPI_LONG);
const Datatype UNSIGNED_CHAR(MPI_UNSIGNED_CHAR);
const Datatype UNSIGNED_SHORT(MPI_UNSIGNED_SHORT);
const Datatype UNSIGNED(MPI_UNSIGNED);
const Datatype UNSIGNED_LONG(MPI_UNSIGNED_LONG);
const Datatype FLOAT(MPI_FLOAT);
const Datatype DOUBLE(MPI_DOUBLE);
const Datatype LONG_DOUBLE(MPI_LONG_DOUBLE);
const Datatype BYTE(MPI_BYTE);
const Datatype PACKED(MPI_PACKED);
const Datatype WCHAR(MPI_WCHAR);

// datatypes for reductions functions (C / C++)
const Datatype FLOAT_INT(MPI_FLOAT_INT);
const Datatype DOUBLE_INT(MPI_DOUBLE_INT);
const Datatype LONG_INT(MPI_LONG_INT);
const Datatype TWOINT(MPI_2INT);
const Datatype SHORT_INT(MPI_SHORT_INT);
const Datatype LONG_DOUBLE_INT(MPI_LONG_DOUBLE);

#if OMPI_WANT_F77_BINDINGS
// elementary datatype (Fortran)
const Datatype REAL(&ompi_mpi_real);
const Datatype INTEGER(&ompi_mpi_integer);
const Datatype DOUBLE_PRECISION(&ompi_mpi_dblprec);
const Datatype F_COMPLEX(&ompi_mpi_cplex);
const Datatype LOGICAL(&ompi_mpi_logic);
const Datatype CHARACTER(&ompi_mpi_character);

// datatype for reduction functions (Fortran)
const Datatype TWOREAL(&ompi_mpi_2real);
const Datatype TWODOUBLE_PRECISION(&ompi_mpi_2dblprec);
const Datatype TWOINTEGER(&ompi_mpi_2integer);

// optional datatypes (Fortran)
const Datatype INTEGER2(&ompi_mpi_integer);
const Datatype REAL2(&ompi_mpi_real);
const Datatype INTEGER1(&ompi_mpi_char);
const Datatype INTEGER4(&ompi_mpi_short);
const Datatype REAL4(&ompi_mpi_real);
const Datatype REAL8(&ompi_mpi_double);

#endif // OMPI_WANT_f77_BINDINGS

// optional datatype (C / C++)
const Datatype UNSIGNED_LONG_LONG(MPI_UNSIGNED_LONG_LONG);
const Datatype LONG_LONG(MPI_LONG_LONG);

// c++ types
const Datatype BOOL(&ompi_mpi_cxx_bool);
const Datatype COMPLEX(&ompi_mpi_cxx_cplex);
const Datatype DOUBLE_COMPLEX(&ompi_mpi_cxx_dblcplex);
const Datatype LONG_DOUBLE_COMPLEX(&ompi_mpi_cxx_ldblcplex);

// datatype decoding constants
const int COMBINER_NAMED = MPI_COMBINER_NAMED;
const int COMBINER_DUP = MPI_COMBINER_DUP;
const int COMBINER_CONTIGUOUS = MPI_COMBINER_CONTIGUOUS;
const int COMBINER_VECTOR = MPI_COMBINER_VECTOR;
const int COMBINER_HVECTOR_INTEGER =  MPI_COMBINER_HVECTOR_INTEGER;
const int COMBINER_HVECTOR = MPI_COMBINER_HVECTOR;
const int COMBINER_INDEXED = MPI_COMBINER_INDEXED;	
const int COMBINER_HINDEXED_INTEGER = MPI_COMBINER_HINDEXED_INTEGER;
const int COMBINER_HINDEXED =  MPI_COMBINER_HINDEXED;	
const int COMBINER_INDEXED_BLOCK =  MPI_COMBINER_INDEXED_BLOCK;
const int COMBINER_STRUCT_INTEGER =  MPI_COMBINER_STRUCT_INTEGER;
const int COMBINER_STRUCT = MPI_COMBINER_STRUCT;
const int COMBINER_SUBARRAY = MPI_COMBINER_SUBARRAY;
const int COMBINER_DARRAY = MPI_COMBINER_DARRAY;
const int COMBINER_F90_REAL = MPI_COMBINER_F90_REAL;
const int COMBINER_F90_COMPLEX = MPI_COMBINER_F90_COMPLEX;
const int COMBINER_F90_INTEGER = MPI_COMBINER_F90_INTEGER;
const int COMBINER_RESIZED = MPI_COMBINER_RESIZED;

// thread constants
const int THREAD_SINGLE = MPI_THREAD_SINGLE;
const int THREAD_FUNNELED = MPI_THREAD_FUNNELED;
const int THREAD_SERIALIZED = MPI_THREAD_SERIALIZED;
const int THREAD_MULTIPLE = MPI_THREAD_MULTIPLE;

// reserved communicators
Intracomm COMM_WORLD(MPI_COMM_WORLD);
Intracomm COMM_SELF(MPI_COMM_SELF);

// results of communicator and group comparisons
const int IDENT = MPI_IDENT;
const int CONGRUENT = MPI_CONGRUENT;
const int SIMILAR = MPI_SIMILAR;
const int UNEQUAL = MPI_UNEQUAL;

// environmental inquiry keys
const int TAG_UB = MPI_TAG_UB;
const int IO = MPI_IO;
const int HOST = MPI_HOST;
const int WTIME_IS_GLOBAL = MPI_WTIME_IS_GLOBAL;
const int UNIVERSE_SIZE = MPI_UNIVERSE_SIZE;
const int APPNUM = MPI_APPNUM;
const int WIN_BASE = MPI_WIN_BASE;
const int WIN_SIZE = MPI_WIN_SIZE;
const int WIN_DISP_UNIT = MPI_WIN_DISP_UNIT;

// collective operations
const Op MAX(MPI_MAX);
const Op MIN(MPI_MIN);
const Op SUM(MPI_SUM);
const Op PROD(MPI_PROD);
const Op MAXLOC(MPI_MAXLOC);
const Op MINLOC(MPI_MINLOC);
const Op BAND(MPI_BAND);
const Op BOR(MPI_BOR);
const Op BXOR(MPI_BXOR);
const Op LAND(MPI_LAND);
const Op LOR(MPI_LOR);
const Op LXOR(MPI_LXOR);
const Op REPLACE(MPI_REPLACE);

// null handles
const Group        GROUP_NULL = MPI_GROUP_NULL;
const Win          WIN_NULL = MPI_WIN_NULL;
const Info         INFO_NULL = MPI_INFO_NULL;
//const Comm         COMM_NULL = MPI_COMM_NULL;
//const MPI_Comm          COMM_NULL = MPI_COMM_NULL;
Comm_Null    COMM_NULL;
const Datatype     DATATYPE_NULL = MPI_DATATYPE_NULL;
Request      REQUEST_NULL = MPI_REQUEST_NULL;
const Op           OP_NULL = MPI_OP_NULL;
const Errhandler   ERRHANDLER_NULL;  

// constants specifying empty or ignored input
const char**       ARGV_NULL = (const char**) MPI_ARGV_NULL;
const char***      ARGVS_NULL = (const char***) MPI_ARGVS_NULL;

// empty group
const Group GROUP_EMPTY(MPI_GROUP_EMPTY);

// topologies
const int GRAPH = MPI_GRAPH;
const int CART = MPI_CART;

// special datatypes for contstruction of derived datatypes
const Datatype UB(MPI_UB);
const Datatype LB(MPI_LB);

}; /* namespace MPI */
