/*
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow.
 */

package mpi;

class Constant
{
    protected int THREAD_SINGLE, THREAD_FUNNELED, THREAD_SERIALIZED,
                  THREAD_MULTIPLE;

    protected int GRAPH, DIST_GRAPH, CART;
    protected int ANY_SOURCE, ANY_TAG;
    protected int PROC_NULL;
    protected int UNDEFINED;
    protected int IDENT, CONGRUENT, SIMILAR, UNEQUAL;
    protected int TAG_UB, HOST, IO, WTIME_IS_GLOBAL;

    protected int APPNUM, LASTUSEDCODE, UNIVERSE_SIZE, WIN_BASE, WIN_SIZE,
                  WIN_DISP_UNIT;

    protected int VERSION, SUBVERSION;
    protected int ROOT, KEYVAL_INVALID, BSEND_OVERHEAD;
    protected int MAX_OBJECT_NAME, MAX_PORT_NAME, MAX_DATAREP_STRING;
    protected int MAX_INFO_KEY, MAX_INFO_VAL;
    protected int ORDER_C, ORDER_FORTRAN;

    protected int DISTRIBUTE_BLOCK, DISTRIBUTE_CYCLIC, DISTRIBUTE_NONE, 
                  DISTRIBUTE_DFLT_DARG;

    protected int MODE_CREATE, MODE_RDONLY, MODE_WRONLY, MODE_RDWR,
                  MODE_DELETE_ON_CLOSE, MODE_UNIQUE_OPEN, MODE_EXCL,
                  MODE_APPEND, MODE_SEQUENTIAL;

    protected int DISPLACEMENT_CURRENT;
    protected int SEEK_SET, SEEK_CUR, SEEK_END;

    protected int MODE_NOCHECK, MODE_NOPRECEDE, MODE_NOPUT, MODE_NOSTORE, 
                  MODE_NOSUCCEED;

    protected int LOCK_EXCLUSIVE, LOCK_SHARED;

    // Error classes and codes
    protected int SUCCESS;
    protected int ERR_BUFFER;
    protected int ERR_COUNT;
    protected int ERR_TYPE;
    protected int ERR_TAG;
    protected int ERR_COMM;
    protected int ERR_RANK;
    protected int ERR_REQUEST;
    protected int ERR_ROOT;
    protected int ERR_GROUP;
    protected int ERR_OP;
    protected int ERR_TOPOLOGY;
    protected int ERR_DIMS;
    protected int ERR_ARG;
    protected int ERR_UNKNOWN;
    protected int ERR_TRUNCATE;
    protected int ERR_OTHER;
    protected int ERR_INTERN;
    protected int ERR_IN_STATUS;
    protected int ERR_PENDING;
    protected int ERR_ACCESS;
    protected int ERR_AMODE;
    protected int ERR_ASSERT;
    protected int ERR_BAD_FILE;
    protected int ERR_BASE;
    protected int ERR_CONVERSION;
    protected int ERR_DISP;
    protected int ERR_DUP_DATAREP;
    protected int ERR_FILE_EXISTS;
    protected int ERR_FILE_IN_USE;
    protected int ERR_FILE;
    protected int ERR_INFO_KEY;
    protected int ERR_INFO_NOKEY;
    protected int ERR_INFO_VALUE;
    protected int ERR_INFO;
    protected int ERR_IO;
    protected int ERR_KEYVAL;
    protected int ERR_LOCKTYPE;
    protected int ERR_NAME;
    protected int ERR_NO_MEM;
    protected int ERR_NOT_SAME;
    protected int ERR_NO_SPACE;
    protected int ERR_NO_SUCH_FILE;
    protected int ERR_PORT;
    protected int ERR_QUOTA;
    protected int ERR_READ_ONLY;
    protected int ERR_RMA_CONFLICT;
    protected int ERR_RMA_SYNC;
    protected int ERR_SERVICE;
    protected int ERR_SIZE;
    protected int ERR_SPAWN;
    protected int ERR_UNSUPPORTED_DATAREP;
    protected int ERR_UNSUPPORTED_OPERATION;
    protected int ERR_WIN;
    protected int ERR_LASTCODE;
    protected int ERR_SYSRESOURCE;

    protected Constant()
    {
	setConstant();
    }

    private native void setConstant();

} // Constant
