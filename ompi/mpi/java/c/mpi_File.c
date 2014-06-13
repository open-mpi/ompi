#include "ompi_config.h"

#include <stdlib.h>
#include <assert.h>
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_File.h"
#include "mpiJava.h"

JNIEXPORT jlong JNICALL Java_mpi_File_open(
        JNIEnv *env, jobject jthis, jlong comm,
        jstring jfilename, jint amode, jlong info)
{
    const char* filename = (*env)->GetStringUTFChars(env, jfilename, NULL);
    MPI_File fh;

    int rc = MPI_File_open((MPI_Comm)comm, (char*)filename,
                           amode, (MPI_Info)info, &fh);

    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleaseStringUTFChars(env, jfilename, filename);
    return (jlong)fh;
}

JNIEXPORT jlong JNICALL Java_mpi_File_close(
        JNIEnv *env, jobject jthis, jlong fh)
{
    MPI_File file = (MPI_File)fh;
    int rc = MPI_File_close(&file);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)file;
}

JNIEXPORT void JNICALL Java_mpi_File_delete(
        JNIEnv *env, jclass clazz, jstring jfilename, jlong info)
{
    const char* filename = (*env)->GetStringUTFChars(env, jfilename, NULL);
    int rc = MPI_File_delete((char*)filename, (MPI_Info)info);
    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleaseStringUTFChars(env, jfilename, filename);
}

JNIEXPORT void JNICALL Java_mpi_File_setSize(
        JNIEnv *env, jobject jthis, jlong fh, jlong size)
{
    int rc = MPI_File_set_size((MPI_File)fh, (MPI_Offset)size);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_File_preallocate(
        JNIEnv *env, jobject jthis, jlong fh, jlong size)
{
    int rc = MPI_File_preallocate((MPI_File)fh, (MPI_Offset)size);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jlong JNICALL Java_mpi_File_getSize(
        JNIEnv *env, jobject jthis, jlong fh)
{
    MPI_Offset size;
    int rc = MPI_File_get_size((MPI_File)fh, &size);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)size;
}

JNIEXPORT jlong JNICALL Java_mpi_File_getGroup(
        JNIEnv *env, jobject jthis, jlong fh)
{
    MPI_Group group;
    int rc = MPI_File_get_group((MPI_File)fh, &group);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)group;
}

JNIEXPORT jint JNICALL Java_mpi_File_getAMode(
        JNIEnv *env, jobject jthis, jlong fh)
{
    int amode;
    int rc = MPI_File_get_amode((MPI_File)fh, &amode);
    ompi_java_exceptionCheck(env, rc);
    return amode;
}

JNIEXPORT void JNICALL Java_mpi_File_setInfo(
        JNIEnv *env, jobject jthis, jlong fh, jlong info)
{
    int rc = MPI_File_set_info((MPI_File)fh, (MPI_Info)info);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jlong JNICALL Java_mpi_File_getInfo(
        JNIEnv *env, jobject jthis, jlong fh)
{
    MPI_Info info;
    int rc = MPI_File_get_info((MPI_File)fh, &info);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)info;
}

JNIEXPORT void JNICALL Java_mpi_File_setView(
        JNIEnv *env, jobject jthis, jlong fh, jlong disp,
        jlong etype, jlong filetype, jstring jdatarep, jlong info)
{
    const char* datarep = (*env)->GetStringUTFChars(env, jdatarep, NULL);

    int rc = MPI_File_set_view(
            (MPI_File)fh, (MPI_Offset)disp, (MPI_Datatype)etype,
            (MPI_Datatype)filetype, (char*)datarep, (MPI_Info)info);

    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleaseStringUTFChars(env, jdatarep, datarep);
}

JNIEXPORT void JNICALL Java_mpi_File_readAt(
        JNIEnv *env, jobject jthis, jlong fh, jlong fileOffset,
        jobject buf, jboolean db, jint off, jint count,
        jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getWritePtr(&ptr, &item, env, buf, db, count, type);
    MPI_Status status;

    int rc = MPI_File_read_at((MPI_File)fh, (MPI_Offset)fileOffset,
                              ptr, count, type, &status);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseWritePtr(ptr, item, env, buf, db, off, count, type, bType);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_readAtAll(
        JNIEnv *env, jobject jthis, jlong fh, jlong fileOffset,
        jobject buf, jboolean db, jint off, jint count,
        jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getWritePtr(&ptr, &item, env, buf, db, count, type);
    MPI_Status status;

    int rc = MPI_File_read_at_all((MPI_File)fh, (MPI_Offset)fileOffset,
                                  ptr, count, type, &status);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseWritePtr(ptr, item, env, buf, db, off, count, type, bType);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_writeAt(
        JNIEnv *env, jobject jthis, jlong fh, jlong fileOffset,
        jobject buf, jboolean db, jint off, jint count,
        jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, off, count, type, bType);
    MPI_Status status;

    int rc = MPI_File_write_at((MPI_File)fh, (MPI_Offset)fileOffset,
                               ptr, count, type, &status);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(ptr, item, buf, db);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_writeAtAll(
        JNIEnv *env, jobject jthis, jlong fh, jlong fileOffset,
        jobject buf, jboolean db, jint off, jint count,
        jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, off, count, type, bType);
    MPI_Status status;

    int rc = MPI_File_write_at_all((MPI_File)fh, (MPI_Offset)fileOffset,
                                   ptr, count, (MPI_Datatype)type, &status);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(ptr, item, buf, db);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT jlong JNICALL Java_mpi_File_iReadAt(
        JNIEnv *env, jobject jthis, jlong fh, jlong offset,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_File_iread_at((MPI_File)fh, (MPI_Offset)offset,
                               ptr, count, (MPI_Datatype)type, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_File_iWriteAt(
        JNIEnv *env, jobject jthis, jlong fh, jlong offset,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_File_iwrite_at((MPI_File)fh, (MPI_Offset)offset,
                                ptr, count, (MPI_Datatype)type, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_File_read(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jboolean db,
        jint off, jint count, jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getWritePtr(&ptr, &item, env, buf, db, count, type);
    MPI_Status status;
    int rc = MPI_File_read((MPI_File)fh, ptr, count, type, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseWritePtr(ptr, item, env, buf, db, off, count, type, bType);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_readAll(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jboolean db,
        jint off, jint count, jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getWritePtr(&ptr, &item, env, buf, db, count, type);
    MPI_Status status;
    int rc = MPI_File_read_all((MPI_File)fh, ptr, count, type, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseWritePtr(ptr, item, env, buf, db, off, count, type, bType);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_write(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jboolean db,
        jint off, jint count, jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, off, count, type, bType);
    MPI_Status status;
    int rc = MPI_File_write((MPI_File)fh, ptr, count, type, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(ptr, item, buf, db);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_writeAll(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jboolean db,
        jint off, jint count, jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, off, count, type, bType);
    MPI_Status status;
    int rc = MPI_File_write_all((MPI_File)fh, ptr, count, type, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(ptr, item, buf, db);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT jlong JNICALL Java_mpi_File_iRead(
        JNIEnv *env, jobject jthis, jlong fh,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_File_iread((MPI_File)fh, ptr, count,
                            (MPI_Datatype)type, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_File_iWrite(
        JNIEnv *env, jobject jthis, jlong fh,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_File_iwrite((MPI_File)fh, ptr, count,
                             (MPI_Datatype)type, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_File_seek(
        JNIEnv *env, jobject jthis, jlong fh, jlong offset, jint whence)
{
    int rc = MPI_File_seek((MPI_File)fh, (MPI_Offset)offset, whence);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jlong JNICALL Java_mpi_File_getPosition(
        JNIEnv *env, jobject jthis, jlong fh)
{
    MPI_Offset offset;
    int rc = MPI_File_get_position((MPI_File)fh, &offset);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)offset;
}

JNIEXPORT jlong JNICALL Java_mpi_File_getByteOffset(
        JNIEnv *env, jobject jthis, jlong fh, jlong offset)
{
    MPI_Offset disp;
    int rc = MPI_File_get_byte_offset((MPI_File)fh, (MPI_Offset)offset, &disp);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)disp;
}

JNIEXPORT void JNICALL Java_mpi_File_readShared(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jboolean db,
        jint off, jint count, jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getWritePtr(&ptr, &item, env, buf, db, count, type);
    MPI_Status status;
    int rc = MPI_File_read_shared((MPI_File)fh, ptr, count, type, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseWritePtr(ptr, item, env, buf, db, off, count, type, bType);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_writeShared(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jboolean db,
        jint off, jint count, jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, off, count, type, bType);
    MPI_Status status;
    int rc = MPI_File_write_shared((MPI_File)fh, ptr, count, type, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(ptr, item, buf, db);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT jlong JNICALL Java_mpi_File_iReadShared(
        JNIEnv *env, jobject jthis, jlong fh,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_File_iread_shared((MPI_File)fh, ptr, count,
                                   (MPI_Datatype)type, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_File_iWriteShared(
        JNIEnv *env, jobject jthis, jlong fh,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_File_iwrite_shared((MPI_File)fh, ptr, count,
                                    (MPI_Datatype)type, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_File_readOrdered(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jboolean db,
        jint off, jint count, jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getWritePtr(&ptr, &item, env, buf, db, count, type);
    MPI_Status status;
    int rc = MPI_File_read_ordered((MPI_File)fh, ptr, count, type, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseWritePtr(ptr, item, env, buf, db, off, count, type, bType);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_writeOrdered(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jboolean db,
        jint off, jint count, jlong jType, jint bType, jlongArray stat)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, off, count, type, bType);
    MPI_Status status;
    int rc = MPI_File_write_ordered((MPI_File)fh, ptr, count, type, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(ptr, item, buf, db);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_seekShared(
        JNIEnv *env, jobject jthis, jlong fh, jlong offset, jint whence)
{
    int rc = MPI_File_seek_shared((MPI_File)fh, (MPI_Offset)offset, whence);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jlong JNICALL Java_mpi_File_getPositionShared(
        JNIEnv *env, jobject jthis, jlong fh)
{
    MPI_Offset offset;
    int rc = MPI_File_get_position_shared((MPI_File)fh, &offset);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)offset;
}

JNIEXPORT void JNICALL Java_mpi_File_readAtAllBegin(
        JNIEnv *env, jobject jthis, jlong fh, jlong offset,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);

    int rc = MPI_File_read_at_all_begin((MPI_File)fh, (MPI_Offset)offset,
                                        ptr, count, (MPI_Datatype)type);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_File_readAtAllEnd(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jlongArray stat)
{
    MPI_Status status;
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    int rc = MPI_File_read_at_all_end((MPI_File)fh, ptr, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_writeAtAllBegin(
        JNIEnv *env, jobject jthis, jlong fh, jlong fileOffset,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);

    int rc = MPI_File_write_at_all_begin((MPI_File)fh, (MPI_Offset)fileOffset,
                                         ptr, count, (MPI_Datatype)type);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_File_writeAtAllEnd(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jlongArray stat)
{
    MPI_Status status;
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    int rc = MPI_File_write_at_all_end((MPI_File)fh, ptr, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_readAllBegin(
        JNIEnv *env, jobject jthis, jlong fh,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);

    int rc = MPI_File_read_all_begin(
             (MPI_File)fh, ptr, count, (MPI_Datatype)type);

    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_File_readAllEnd(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jlongArray stat)
{
    MPI_Status status;
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    int rc = MPI_File_read_all_end((MPI_File)fh, ptr, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_writeAllBegin(
        JNIEnv *env, jobject jthis, jlong fh,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);

    int rc = MPI_File_write_all_begin(
             (MPI_File)fh, ptr, count, (MPI_Datatype)type);

    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_File_writeAllEnd(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jlongArray stat)
{
    MPI_Status status;
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    int rc = MPI_File_write_all_end((MPI_File)fh, ptr, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_readOrderedBegin(
        JNIEnv *env, jobject jthis, jlong fh,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);

    int rc = MPI_File_read_ordered_begin(
             (MPI_File)fh, ptr, count, (MPI_Datatype)type);

    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_File_readOrderedEnd(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jlongArray stat)
{
    MPI_Status status;
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    int rc = MPI_File_read_ordered_end((MPI_File)fh, ptr, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT void JNICALL Java_mpi_File_writeOrderedBegin(
        JNIEnv *env, jobject jthis, jlong fh,
        jobject buf, jint count, jlong type)
{
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);

    int rc = MPI_File_write_ordered_begin(
             (MPI_File)fh, ptr, count, (MPI_Datatype)type);

    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_File_writeOrderedEnd(
        JNIEnv *env, jobject jthis, jlong fh, jobject buf, jlongArray stat)
{
    MPI_Status status;
    void *ptr = (*env)->GetDirectBufferAddress(env, buf);
    int rc = MPI_File_write_ordered_end((MPI_File)fh, ptr, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(env, stat, &status);
}

JNIEXPORT jint JNICALL Java_mpi_File_getTypeExtent(
        JNIEnv *env, jobject jthis, jlong fh, jlong type)
{
    MPI_Aint extent;

    int rc = MPI_File_get_type_extent(
             (MPI_File)fh, (MPI_Datatype)type, &extent);

    ompi_java_exceptionCheck(env, rc);
    return (int)extent;
}

JNIEXPORT void JNICALL Java_mpi_File_setAtomicity(
        JNIEnv *env, jobject jthis, jlong fh, jboolean atomicity)
{
    int rc = MPI_File_set_atomicity((MPI_File)fh, atomicity);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_File_sync(
        JNIEnv *env, jobject jthis, jlong fh)
{
    int rc = MPI_File_sync((MPI_File)fh);
    ompi_java_exceptionCheck(env, rc);
}
