// -*- c++ -*-
// 
// $HEADER$
//


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
// return  codes
extern const int SUCCESS;
extern const int ERR_BUFFER;
extern const int ERR_COUNT;
extern const int ERR_TYPE;
extern const int ERR_TAG ;
extern const int ERR_COMM;
extern const int ERR_RANK;
extern const int ERR_REQUEST;
extern const int ERR_ROOT;
extern const int ERR_GROUP;
extern const int ERR_OP;
extern const int ERR_TOPOLOGY;
extern const int ERR_DIMS;
extern const int ERR_ARG;
extern const int ERR_UNKNOWN;
extern const int ERR_TRUNCATE;
extern const int ERR_OTHER;
extern const int ERR_INTERN;
extern const int ERR_PENDING;
extern const int ERR_IN_STATUS;
extern const int ERR_LASTCODE;

extern const int ERR_BASE;
extern const int ERR_INFO_VALUE;
extern const int ERR_INFO_KEY;
extern const int ERR_INFO_NOKEY;
extern const int ERR_KEYVAL;
extern const int ERR_NAME;
extern const int ERR_NO_MEM;
extern const int ERR_SERVICE;
extern const int ERR_SPAWN;
extern const int ERR_WIN;


// assorted constants
extern const void* BOTTOM;
extern const int PROC_NULL;
extern const int ANY_SOURCE;
extern const int ANY_TAG;
extern const int UNDEFINED;
extern const int BSEND_OVERHEAD;
extern const int KEYVAL_INVALID;

// error-handling specifiers
extern const Errhandler  ERRORS_ARE_FATAL;
extern const Errhandler  ERRORS_RETURN;
extern const Errhandler  ERRORS_THROW_EXCEPTIONS;

// maximum sizes for strings
extern const int MAX_PROCESSOR_NAME;
extern const int MAX_ERROR_STRING;
extern const int MAX_INFO_KEY;
extern const int MAX_INFO_VAL;
extern const int MAX_PORT_NAME;
extern const int MAX_OBJECT_NAME;

// elementary datatypes (C / C++)
extern const Datatype CHAR;
extern const Datatype SHORT;          
extern const Datatype INT;            
extern const Datatype LONG;
extern const Datatype SIGNED_CHAR;
extern const Datatype UNSIGNED_CHAR;
extern const Datatype UNSIGNED_SHORT; 
extern const Datatype UNSIGNED;       
extern const Datatype UNSIGNED_LONG;  
extern const Datatype FLOAT;
extern const Datatype DOUBLE;
extern const Datatype LONG_DOUBLE;
extern const Datatype BYTE;
extern const Datatype PACKED;
extern const Datatype WCHAR;

// datatypes for reductions functions (C / C++)
extern const Datatype FLOAT_INT;
extern const Datatype DOUBLE_INT;
extern const Datatype LONG_INT;
extern const Datatype TWOINT;
extern const Datatype SHORT_INT;
extern const Datatype LONG_DOUBLE_INT;

// elementary datatype (Fortran)
extern const Datatype INTEGER;
extern const Datatype REAL;
extern const Datatype DOUBLE_PRECISION;
extern const Datatype F_COMPLEX;
extern const Datatype LOGICAL;
extern const Datatype CHARACTER;

// datatype for reduction functions (Fortran)
extern const Datatype TWOREAL;
extern const Datatype TWODOUBLE_PRECISION;
extern const Datatype TWOINTEGER;

// optional datatypes (Fortran)
extern const Datatype INTEGER1;
extern const Datatype INTEGER2;
extern const Datatype INTEGER4;
extern const Datatype REAL2;
extern const Datatype REAL4;
extern const Datatype REAL8;

// optional datatype (C / C++)
extern const Datatype LONG_LONG;
extern const Datatype UNSIGNED_LONG_LONG;

// c++ types
extern const Datatype BOOL;
extern const Datatype COMPLEX;
extern const Datatype DOUBLE_COMPLEX;
extern const Datatype LONG_DOUBLE_COMPLEX;

// special datatypes for contstruction of derived datatypes
extern const Datatype UB;
extern const Datatype LB;

// datatype decoding constants
extern const int COMBINER_NAMED;
extern const int COMBINER_DUP;
extern const int COMBINER_CONTIGUOUS;
extern const int COMBINER_VECTOR;
extern const int COMBINER_HVECTOR_INTEGER;
extern const int COMBINER_HVECTOR;
extern const int COMBINER_INDEXED;
extern const int COMBINER_HINDEXED_INTEGER;
extern const int COMBINER_HINDEXED;
extern const int COMBINER_INDEXED_BLOCK;
extern const int COMBINER_STRUCT_INTEGER;
extern const int COMBINER_STRUCT;
extern const int COMBINER_SUBARRAY;
extern const int COMBINER_DARRAY;
extern const int COMBINER_F90_REAL;
extern const int COMBINER_F90_COMPLEX;
extern const int COMBINER_F90_INTEGER;
extern const int COMBINER_RESIZED;

// thread constants
extern const int THREAD_SINGLE;
extern const int THREAD_FUNNELED;
extern const int THREAD_SERIALIZED;
extern const int THREAD_MULTIPLE;

// reserved communicators
// JGS these can not be const because Set_errhandler is not const
extern Intracomm COMM_WORLD;
extern Intracomm COMM_SELF;

// results of communicator and group comparisons
extern const int IDENT;
extern const int CONGRUENT;
extern const int SIMILAR;
extern const int UNEQUAL;

// environmental inquiry keys
extern const int TAG_UB;
extern const int IO;
extern const int HOST;
extern const int WTIME_IS_GLOBAL;
extern const int UNIVERSE_SIZE;
extern const int APPNUM;
extern const int WIN_BASE;
extern const int WIN_SIZE;
extern const int WIN_DISP_UNIT;

// collective operations
extern const Op MAX;
extern const Op MIN;
extern const Op SUM;
extern const Op PROD;
extern const Op MAXLOC;
extern const Op MINLOC;
extern const Op BAND;
extern const Op BOR;
extern const Op BXOR;
extern const Op LAND;
extern const Op LOR;
extern const Op LXOR;
extern const Op REPLACE;

// null handles
extern const Group        GROUP_NULL;
extern const Win          WIN_NULL;
extern const Info         INFO_NULL;
//extern const Comm         COMM_NULL;
//extern const MPI_Comm     COMM_NULL;
extern Comm_Null          COMM_NULL;
extern const Datatype     DATATYPE_NULL;
extern Request            REQUEST_NULL;
extern const Op           OP_NULL;
extern const Errhandler   ERRHANDLER_NULL;  

// constants specifying empty or ignored input
extern const char**       ARGV_NULL;
extern const char***      ARGVS_NULL;

// empty group
extern const Group  GROUP_EMPTY;

// topologies
extern const int GRAPH;
extern const int CART;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

