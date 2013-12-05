/*
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow.
 */

#include "ompi_config.h"

#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi_Constant.h"
#include "mpiJava.h"

void ompi_java_setStaticLongField(JNIEnv *env, jclass c,
                                  char *field, jlong value)
{
    jfieldID id = (*env)->GetStaticFieldID(env, c, field, "J");
    (*env)->SetStaticLongField(env, c, id, value);
}

void ompi_java_setIntField(JNIEnv *env, jclass c, jobject obj,
                           char *field, jint value)
{
    jfieldID id = (*env)->GetFieldID(env, c, field, "I");
    (*env)->SetIntField(env, obj, id, value);
}

/*
 * Class:     mpi_Constant
 * Method:    setConstant
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_Constant_setConstant(JNIEnv *env, jobject obj)
{
    jclass c = (*env)->GetObjectClass(env, obj);
    ompi_java_setIntField(env, c, obj, "THREAD_SINGLE",     MPI_THREAD_SINGLE);
    ompi_java_setIntField(env, c, obj, "THREAD_FUNNELED",   MPI_THREAD_FUNNELED);
    ompi_java_setIntField(env, c, obj, "THREAD_SERIALIZED", MPI_THREAD_SERIALIZED);
    ompi_java_setIntField(env, c, obj, "THREAD_MULTIPLE",   MPI_THREAD_MULTIPLE);

    ompi_java_setIntField(env, c, obj, "ANY_SOURCE", MPI_ANY_SOURCE);
    ompi_java_setIntField(env, c, obj, "ANY_TAG",    MPI_ANY_TAG);
    ompi_java_setIntField(env, c, obj, "PROC_NULL",  MPI_PROC_NULL);
    ompi_java_setIntField(env, c, obj, "GRAPH",      MPI_GRAPH);
    ompi_java_setIntField(env, c, obj, "DIST_GRAPH", MPI_DIST_GRAPH);
    ompi_java_setIntField(env, c, obj, "CART",       MPI_CART);

    ompi_java_setIntField(env, c, obj, "UNDEFINED",  MPI_UNDEFINED);
    ompi_java_setIntField(env, c, obj, "IDENT",      MPI_IDENT);
    ompi_java_setIntField(env, c, obj, "CONGRUENT",  MPI_CONGRUENT);
    ompi_java_setIntField(env, c, obj, "SIMILAR",    MPI_SIMILAR);
    ompi_java_setIntField(env, c, obj, "UNEQUAL",    MPI_UNEQUAL);

    ompi_java_setIntField(env, c, obj, "TAG_UB",          MPI_TAG_UB);
    ompi_java_setIntField(env, c, obj, "HOST",            MPI_HOST);
    ompi_java_setIntField(env, c, obj, "IO",              MPI_IO);
    ompi_java_setIntField(env, c, obj, "WTIME_IS_GLOBAL", MPI_WTIME_IS_GLOBAL);
    ompi_java_setIntField(env, c, obj, "APPNUM",          MPI_APPNUM);
    ompi_java_setIntField(env, c, obj, "LASTUSEDCODE",    MPI_LASTUSEDCODE);
    ompi_java_setIntField(env, c, obj, "UNIVERSE_SIZE",   MPI_UNIVERSE_SIZE);
    ompi_java_setIntField(env, c, obj, "WIN_BASE",        MPI_WIN_BASE);
    ompi_java_setIntField(env, c, obj, "WIN_SIZE",        MPI_WIN_SIZE);
    ompi_java_setIntField(env, c, obj, "WIN_DISP_UNIT",   MPI_WIN_DISP_UNIT);

    ompi_java_setIntField(env, c, obj, "VERSION", MPI_VERSION);
    ompi_java_setIntField(env, c, obj, "SUBVERSION", MPI_SUBVERSION);
    ompi_java_setIntField(env, c, obj, "ROOT", MPI_ROOT);
    ompi_java_setIntField(env, c, obj, "KEYVAL_INVALID", MPI_KEYVAL_INVALID);
    ompi_java_setIntField(env, c, obj, "BSEND_OVERHEAD", MPI_BSEND_OVERHEAD);
    ompi_java_setIntField(env, c, obj, "MAX_OBJECT_NAME", MPI_MAX_OBJECT_NAME);
    ompi_java_setIntField(env, c, obj, "MAX_PORT_NAME", MPI_MAX_PORT_NAME);
    ompi_java_setIntField(env, c, obj, "MAX_DATAREP_STRING", MPI_MAX_DATAREP_STRING);
    ompi_java_setIntField(env, c, obj, "MAX_INFO_KEY", MPI_MAX_INFO_KEY);
    ompi_java_setIntField(env, c, obj, "MAX_INFO_VAL", MPI_MAX_INFO_VAL);
    ompi_java_setIntField(env, c, obj, "ORDER_C", MPI_ORDER_C);
    ompi_java_setIntField(env, c, obj, "ORDER_FORTRAN", MPI_ORDER_FORTRAN);
    ompi_java_setIntField(env, c, obj, "DISTRIBUTE_BLOCK", MPI_DISTRIBUTE_BLOCK);
    ompi_java_setIntField(env, c, obj, "DISTRIBUTE_CYCLIC", MPI_DISTRIBUTE_CYCLIC);
    ompi_java_setIntField(env, c, obj, "DISTRIBUTE_NONE", MPI_DISTRIBUTE_NONE);
    ompi_java_setIntField(env, c, obj, "DISTRIBUTE_DFLT_DARG", MPI_DISTRIBUTE_DFLT_DARG);

    ompi_java_setIntField(env, c, obj, "MODE_CREATE", MPI_MODE_CREATE);
    ompi_java_setIntField(env, c, obj, "MODE_RDONLY", MPI_MODE_RDONLY);
    ompi_java_setIntField(env, c, obj, "MODE_WRONLY", MPI_MODE_WRONLY);
    ompi_java_setIntField(env, c, obj, "MODE_RDWR", MPI_MODE_RDWR);
    ompi_java_setIntField(env, c, obj, "MODE_DELETE_ON_CLOSE", MPI_MODE_DELETE_ON_CLOSE);
    ompi_java_setIntField(env, c, obj, "MODE_UNIQUE_OPEN", MPI_MODE_UNIQUE_OPEN);
    ompi_java_setIntField(env, c, obj, "MODE_EXCL", MPI_MODE_EXCL);
    ompi_java_setIntField(env, c, obj, "MODE_APPEND", MPI_MODE_APPEND);
    ompi_java_setIntField(env, c, obj, "MODE_SEQUENTIAL", MPI_MODE_SEQUENTIAL);
    ompi_java_setIntField(env, c, obj, "DISPLACEMENT_CURRENT", MPI_DISPLACEMENT_CURRENT);
    ompi_java_setIntField(env, c, obj, "SEEK_SET", MPI_SEEK_SET);
    ompi_java_setIntField(env, c, obj, "SEEK_CUR", MPI_SEEK_CUR);
    ompi_java_setIntField(env, c, obj, "SEEK_END", MPI_SEEK_END);

    ompi_java_setIntField(env, c, obj, "MODE_NOCHECK", MPI_MODE_NOCHECK);
    ompi_java_setIntField(env, c, obj, "MODE_NOPRECEDE", MPI_MODE_NOPRECEDE);
    ompi_java_setIntField(env, c, obj, "MODE_NOPUT", MPI_MODE_NOPUT);
    ompi_java_setIntField(env, c, obj, "MODE_NOSTORE", MPI_MODE_NOSTORE);
    ompi_java_setIntField(env, c, obj, "MODE_NOSUCCEED", MPI_MODE_NOSUCCEED);
    ompi_java_setIntField(env, c, obj, "LOCK_EXCLUSIVE", MPI_LOCK_EXCLUSIVE);
    ompi_java_setIntField(env, c, obj, "LOCK_SHARED", MPI_LOCK_SHARED);

    // Error classes and codes
    ompi_java_setIntField(env, c, obj, "SUCCESS",          MPI_SUCCESS);
    ompi_java_setIntField(env, c, obj, "ERR_BUFFER",       MPI_ERR_BUFFER);
    ompi_java_setIntField(env, c, obj, "ERR_COUNT",        MPI_ERR_COUNT);
    ompi_java_setIntField(env, c, obj, "ERR_TYPE",         MPI_ERR_TYPE);
    ompi_java_setIntField(env, c, obj, "ERR_TAG",          MPI_ERR_TAG);
    ompi_java_setIntField(env, c, obj, "ERR_COMM",         MPI_ERR_COMM);
    ompi_java_setIntField(env, c, obj, "ERR_RANK",         MPI_ERR_RANK);
    ompi_java_setIntField(env, c, obj, "ERR_REQUEST",      MPI_ERR_REQUEST);
    ompi_java_setIntField(env, c, obj, "ERR_ROOT",         MPI_ERR_ROOT);
    ompi_java_setIntField(env, c, obj, "ERR_GROUP",        MPI_ERR_GROUP);
    ompi_java_setIntField(env, c, obj, "ERR_OP",           MPI_ERR_OP);
    ompi_java_setIntField(env, c, obj, "ERR_TOPOLOGY",     MPI_ERR_TOPOLOGY);
    ompi_java_setIntField(env, c, obj, "ERR_DIMS",         MPI_ERR_DIMS);
    ompi_java_setIntField(env, c, obj, "ERR_ARG",          MPI_ERR_ARG);
    ompi_java_setIntField(env, c, obj, "ERR_UNKNOWN",      MPI_ERR_UNKNOWN);
    ompi_java_setIntField(env, c, obj, "ERR_TRUNCATE",     MPI_ERR_TRUNCATE);
    ompi_java_setIntField(env, c, obj, "ERR_OTHER",        MPI_ERR_OTHER);
    ompi_java_setIntField(env, c, obj, "ERR_INTERN",       MPI_ERR_INTERN);
    ompi_java_setIntField(env, c, obj, "ERR_IN_STATUS",    MPI_ERR_IN_STATUS);
    ompi_java_setIntField(env, c, obj, "ERR_PENDING",      MPI_ERR_PENDING);
    ompi_java_setIntField(env, c, obj, "ERR_ACCESS",       MPI_ERR_ACCESS);
    ompi_java_setIntField(env, c, obj, "ERR_AMODE",        MPI_ERR_AMODE);
    ompi_java_setIntField(env, c, obj, "ERR_ASSERT",       MPI_ERR_ASSERT);
    ompi_java_setIntField(env, c, obj, "ERR_BAD_FILE",     MPI_ERR_BAD_FILE);
    ompi_java_setIntField(env, c, obj, "ERR_BASE",         MPI_ERR_BASE);
    ompi_java_setIntField(env, c, obj, "ERR_CONVERSION",   MPI_ERR_CONVERSION);
    ompi_java_setIntField(env, c, obj, "ERR_DISP",         MPI_ERR_DISP);
    ompi_java_setIntField(env, c, obj, "ERR_DUP_DATAREP",  MPI_ERR_DUP_DATAREP);
    ompi_java_setIntField(env, c, obj, "ERR_FILE_EXISTS",  MPI_ERR_FILE_EXISTS);
    ompi_java_setIntField(env, c, obj, "ERR_FILE_IN_USE",  MPI_ERR_FILE_IN_USE);
    ompi_java_setIntField(env, c, obj, "ERR_FILE",         MPI_ERR_FILE);
    ompi_java_setIntField(env, c, obj, "ERR_INFO_KEY",     MPI_ERR_INFO_KEY);
    ompi_java_setIntField(env, c, obj, "ERR_INFO_NOKEY",   MPI_ERR_INFO_NOKEY);
    ompi_java_setIntField(env, c, obj, "ERR_INFO_VALUE",   MPI_ERR_INFO_VALUE);
    ompi_java_setIntField(env, c, obj, "ERR_INFO",         MPI_ERR_INFO);
    ompi_java_setIntField(env, c, obj, "ERR_IO",           MPI_ERR_IO);
    ompi_java_setIntField(env, c, obj, "ERR_KEYVAL",       MPI_ERR_KEYVAL);
    ompi_java_setIntField(env, c, obj, "ERR_LOCKTYPE",     MPI_ERR_LOCKTYPE);
    ompi_java_setIntField(env, c, obj, "ERR_NAME",         MPI_ERR_NAME);
    ompi_java_setIntField(env, c, obj, "ERR_NO_MEM",       MPI_ERR_NO_MEM);
    ompi_java_setIntField(env, c, obj, "ERR_NOT_SAME",     MPI_ERR_NOT_SAME);
    ompi_java_setIntField(env, c, obj, "ERR_NO_SPACE",     MPI_ERR_NO_SPACE);
    ompi_java_setIntField(env, c, obj, "ERR_NO_SUCH_FILE", MPI_ERR_NO_SUCH_FILE);
    ompi_java_setIntField(env, c, obj, "ERR_PORT",         MPI_ERR_PORT);
    ompi_java_setIntField(env, c, obj, "ERR_QUOTA",        MPI_ERR_QUOTA);
    ompi_java_setIntField(env, c, obj, "ERR_READ_ONLY",    MPI_ERR_READ_ONLY);
    ompi_java_setIntField(env, c, obj, "ERR_RMA_CONFLICT", MPI_ERR_RMA_CONFLICT);
    ompi_java_setIntField(env, c, obj, "ERR_RMA_SYNC",     MPI_ERR_RMA_SYNC);
    ompi_java_setIntField(env, c, obj, "ERR_SERVICE",      MPI_ERR_SERVICE);
    ompi_java_setIntField(env, c, obj, "ERR_SIZE",         MPI_ERR_SIZE);
    ompi_java_setIntField(env, c, obj, "ERR_SPAWN",        MPI_ERR_SPAWN);

    ompi_java_setIntField(env, c, obj, "ERR_UNSUPPORTED_DATAREP",
                          MPI_ERR_UNSUPPORTED_DATAREP);

    ompi_java_setIntField(env, c, obj, "ERR_UNSUPPORTED_OPERATION",
                          MPI_ERR_UNSUPPORTED_OPERATION);

    ompi_java_setIntField(env, c, obj, "ERR_WIN",          MPI_ERR_WIN);
    ompi_java_setIntField(env, c, obj, "ERR_LASTCODE",     MPI_ERR_LASTCODE);
    ompi_java_setIntField(env, c, obj, "ERR_SYSRESOURCE",  MPI_ERR_SYSRESOURCE);
}
