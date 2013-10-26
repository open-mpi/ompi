package mpi;

import java.nio.*;
import static mpi.MPI.isHeapBuffer;
import static mpi.MPI.isDirectBuffer;
import static mpi.MPI.assertDirectBuffer;

/**
 * This class represents {@code MPI_File}.
 */
public final class File
{
private long handle;
private FileView view = new FileView(0, MPI.BYTE, MPI.BYTE, "native");
private Status beginStatus;

/**
 * Java binding of {@code MPI_FILE_OPEN} using {@code MPI_INFO_NULL}.
 * @param comm     communicator
 * @param filename name of the file to open
 * @param amode    file access mode
 * @throws MPIException 
 */
public File(Comm comm, String filename, int amode) throws MPIException
{
    MPI.check();
    handle = open(comm.handle, filename, amode, Info.NULL);
}

/**
 * Java binding of {@code MPI_FILE_OPEN}.
 * @param comm     communicator
 * @param filename name of the file to open
 * @param amode    file access mode
 * @param info     info object
 * @throws MPIException 
 */
public File(Comm comm, String filename, int amode, Info info)
    throws MPIException
{
    MPI.check();
    handle = open(comm.handle, filename, amode, info.handle);
}

private native long open(long comm, String filename, int amode, long info)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_CLOSE}.
 * @throws MPIException 
 */
public void close() throws MPIException
{
    MPI.check();
    handle = close(handle);
}

private native long close(long fh) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_DELETE} using {@code MPI_INFO_NULL}.
 * @param filename name of the file to delete
 * @throws MPIException 
 */
public static void delete(String filename) throws MPIException
{
    MPI.check();
    delete_jni(filename, Info.NULL);
}

/**
 * Java binding of {@code MPI_FILE_DELETE}.
 * @param filename name of the file to delete
 * @param info     info object
 * @throws MPIException 
 */
public static void delete(String filename, Info info) throws MPIException
{
    MPI.check();
    delete_jni(filename, info.handle);
}

private static native void delete_jni(String filename, long info)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_SET_SIZE}.
 * @param size size to truncate or expand file
 * @throws MPIException 
 */
public void setSize(long size) throws MPIException
{
    MPI.check();
    setSize(handle, size);
}

private native void setSize(long fh, long size) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_PREALLOCATE}.
 * @param size size to preallocate file
 * @throws MPIException 
 */
public void preallocate(long size) throws MPIException
{
    MPI.check();
    preallocate(handle, size);
}

private native void preallocate(long fh, long size) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_GET_SIZE}.
 * @return size of file in bytes
 * @throws MPIException 
 */
public long getSize() throws MPIException
{
    MPI.check();
    return getSize(handle);
}

private native long getSize(long fh) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_GET_GROUP}.
 * @return group wich opened the file
 * @throws MPIException 
 */
public Group getGroup() throws MPIException
{
    MPI.check();
    return new Group(getGroup(handle));
}

private native long getGroup(long fh) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_GET_AMODE}.
 * @return file access mode to open the file
 * @throws MPIException 
 */
public int getAMode() throws MPIException
{
    MPI.check();
    return getAMode(handle);
}

private native int getAMode(long fh) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_SET_INFO}.
 * @param info info object
 * @throws MPIException 
 */
public void setInfo(Info info) throws MPIException
{
    MPI.check();
    setInfo(handle, info.handle);
}

private native void setInfo(long fh, long info) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_GET_INFO}.
 * @return new info object
 * @throws MPIException 
 */
public Info getInfo() throws MPIException
{
    MPI.check();
    return new Info(getInfo(handle));
}

private native long getInfo(long fh) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_SET_VIEW} using {@code MPI_INFO_NULL}.
 * @param disp     displacement
 * @param etype    elementary datatype
 * @param filetype filetype
 * @param datarep  data representation
 * @throws MPIException 
 */
public void setView(long disp, Datatype etype,
                    Datatype filetype, String datarep)
    throws MPIException
{
    MPI.check();
    setView(handle, disp, etype.handle, filetype.handle, datarep, Info.NULL);
    view = new FileView(disp, etype, filetype, datarep);
}

/**
 * Java binding of {@code MPI_FILE_SET_VIEW}.
 * @param disp     displacement
 * @param etype    elementary datatype
 * @param filetype filetype
 * @param datarep  data representation
 * @param info     info object
 * @throws MPIException 
 */
public void setView(long disp, Datatype etype,
                    Datatype filetype, String datarep, Info info)
    throws MPIException
{
    MPI.check();
    setView(handle, disp, etype.handle, filetype.handle, datarep, info.handle);
    view = new FileView(disp, etype, filetype, datarep);
}

private native void setView(
        long fh, long disp, long etype,
        long filetype, String datarep, long info) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_GET_VIEW}.
 * @return file view
 */
public FileView getView()
{
    return view;
}

/**
 * Java binding of {@code MPI_FILE_READ_AT}.
 * @param offset file offset
 * @param buf    buffer
 * @param count  number of items in buffer
 * @param type   datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status readAt(long offset, Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    readAt(handle, offset, buf, off, count, type, stat);
    return stat;
}

private native void readAt(
        long fh, long fileOffset, Object buf, int offset,
        int count, Datatype type, Status stat) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ_AT_ALL}.
 * @param offset file offset
 * @param buf    buffer
 * @param count  number of items in buffer
 * @param type   datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status readAtAll(long offset, Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    readAtAll(handle, offset, buf, off, count, type, stat);
    return stat;
}

private native void readAtAll(
        long fh, long fileOffset, Object buf, int offset,
        int count, Datatype type, Status stat) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_AT}.
 * @param offset file offset
 * @param buf    buffer
 * @param count  number of items in buffer
 * @param type   datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status writeAt(long offset, Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    writeAt(handle, offset, buf, off, count, type, stat);
    return stat;
}

private native void writeAt(
        long fh, long fileOffset, Object buf, int offset,
        int count, Datatype type, Status stat) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_AT_ALL}.
 * @param offset file offset
 * @param buf    buffer
 * @param count  number of items in buffer
 * @param type   datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status writeAtAll(long offset, Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    writeAtAll(handle, offset, buf, off, count, type, stat);
    return stat;
}

private native void writeAtAll(
        long fh, long fileOffset, Object buf, int offset,
        int count, Datatype type, Status stat) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_IREAD_AT}.
 * @param offset file offset
 * @param buf    buffer
 * @param count  number of items in buffer
 * @param type   datatype of each buffer element
 * @return request object
 * @throws MPIException 
 */
public Request iReadAt(long offset, Buffer buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    assertDirectBuffer(buf);
    return new Request(iReadAt(handle, offset, buf, count, type.handle));
}

private native long iReadAt(
        long fh, long offset, Buffer buf, int count, long type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_IWRITE_AT}.
 * @param offset file offset
 * @param buf    buffer
 * @param count  number of items in buffer
 * @param type   datatype of each buffer element
 * @return request object
 * @throws MPIException 
 */
public Request iWriteAt(long offset, Buffer buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    assertDirectBuffer(buf);
    return new Request(iWriteAt(handle, offset, buf, count, type.handle));
}

private native long iWriteAt(
        long fh, long offset, Buffer buf, int count, long type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status read(Object buf, int count, Datatype type) throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    read(handle, buf, off, count, type, stat);
    return stat;
}

private native void read(
        long fh, Object buf, int offset,
        int count, Datatype type, Status stat) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ_ALL}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status readAll(Object buf, int count, Datatype type) throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    readAll(handle, buf, off, count, type, stat);
    return stat;
}

private native void readAll(
        long fh, Object buf, int offset,
        int count, Datatype type, Status stat) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status write(Object buf, int count, Datatype type) throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    write(handle, buf, off, count, type, stat);
    return stat;
}

private native void write(
        long fh, Object buf, int offset,
        int count, Datatype type, Status stat) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_ALL}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status writeAll(Object buf, int count, Datatype type) throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    writeAll(handle, buf, off, count, type, stat);
    return stat;
}

private native void writeAll(
        long fh, Object buf, int offset,
        int count, Datatype type, Status stat) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_IREAD}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return request object
 * @throws MPIException 
 */
public Request iRead(Buffer buf, int count, Datatype type) throws MPIException
{
    MPI.check();
    assertDirectBuffer(buf);
    return new Request(iRead(handle, buf, count, type.handle));
}

private native long iRead(long fh, Buffer buf, int count, long type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_IWRITE}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return request object
 * @throws MPIException 
 */
public Request iWrite(Buffer buf, int count, Datatype type) throws MPIException
{
    MPI.check();
    assertDirectBuffer(buf);
    return new Request(iWrite(handle, buf, count, type.handle));
}

private native long iWrite(long fh, Buffer buf, int count, long type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_SEEK}.
 * @param offset file offset
 * @param whence update mode
 * @throws MPIException 
 */
public void seek(long offset, int whence) throws MPIException
{
    MPI.check();
    seek(handle, offset, whence);
}

private native void seek(long fh, long offset, int whence) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_GET_POSITION}.
 * @return offset of individual pointer
 * @throws MPIException 
 */
public long getPosition() throws MPIException
{
    MPI.check();
    return getPosition(handle);
}

private native long getPosition(long fh) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_GET_BYTE_OFFSET}.
 * @param offset offset
 * @return absolute byte position of offset
 * @throws MPIException 
 */
public long getByteOffset(long offset) throws MPIException
{
    MPI.check();
    return getByteOffset(handle, offset);
}

private native long getByteOffset(long fh, long offset) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ_SHARED}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status readShared(Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    readShared(handle, buf, off, count, type, stat);
    return stat;
}

private native void readShared(
        long fh, Object buf, int offset, int count, Datatype type, Status stat)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_SHARED}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status writeShared(Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    writeShared(handle, buf, off, count, type, stat);
    return stat;
}

private native void writeShared(
        long fh, Object buf, int offset, int count, Datatype type, Status stat)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_IREAD_SHARED}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return request object
 * @throws MPIException 
 */
public Request iReadShared(Buffer buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    assertDirectBuffer(buf);
    return new Request(iReadShared(handle, buf, count, type.handle));
}

private native long iReadShared(long fh, Buffer buf, int count, long type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_IWRITE_SHARED}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return request object
 * @throws MPIException 
 */
public Request iWriteShared(Buffer buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    assertDirectBuffer(buf);
    return new Request(iWriteShared(handle, buf, count, type.handle));
}

private native long iWriteShared(long fh, Buffer buf, int count, long type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ_ORDERED}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status readOrdered(Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    readOrdered(handle, buf, off, count, type, stat);
    return stat;
}

private native void readOrdered(
        long fh, Object buf, int offset, int count, Datatype type, Status stat)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_ORDERED}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @return status object
 * @throws MPIException 
 */
public Status writeOrdered(Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();
    int off = 0;

    if(isHeapBuffer(buf))
    {
        off = ((Buffer)buf).arrayOffset();
        buf = ((Buffer)buf).array();
    }

    Status stat = new Status();
    writeOrdered(handle, buf, off, count, type, stat);
    return stat;
}

private native void writeOrdered(
        long fh, Object buf, int offset, int count, Datatype type, Status stat)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_SEEK_SHARED}.
 * @param offset file offset
 * @param whence update mode
 * @throws MPIException 
 */
public void seekShared(long offset, int whence) throws MPIException
{
    MPI.check();
    seekShared(handle, offset, whence);
}

private native void seekShared(long fh, long offset, int whence)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_GET_POSITION_SHARED}.
 * @return offset of individual pointer
 * @throws MPIException 
 */
public long getPositionShared() throws MPIException
{
    MPI.check();
    return getPositionShared(handle);
}

private native long getPositionShared(long fh) throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ_AT_ALL_BEGIN}.
 * @param offset file offset
 * @param buf    buffer
 * @param count  number of items in buffer
 * @param type   datatype of each buffer element
 * @throws MPIException 
 */
public void readAtAllBegin(long offset, Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        readAtAllBegin(handle, offset, buf, count, type);
    }
    else
    {
        int off = 0;

        if(isHeapBuffer(buf))
        {
            off = ((Buffer)buf).arrayOffset();
            buf = ((Buffer)buf).array();
        }

        Status stat = new Status();
        readAtAll(handle, offset, buf, off, count, type, stat);
        beginStatus = stat;
    }
}

private native void readAtAllBegin(
        long fh, long offset, Object buf, int count, Datatype type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ_AT_ALL_END}.
 * @param buf buffer
 * @return status object
 * @throws MPIException 
 */
public Status readAtAllEnd(Object buf) throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        Status stat = new Status();
        readAtAllEnd(handle, buf, stat);
        return stat;
    }
    else
    {
        return getBeginStatus();
    }
}

private native void readAtAllEnd(long fh, Object buf, Status stat)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_AT_ALL_BEGIN}.
 * @param offset file offset
 * @param buf    buffer
 * @param count  number of items in buffer
 * @param type   datatype of each buffer element
 * @throws MPIException 
 */
public void writeAtAllBegin(long offset, Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        writeAtAllBegin(handle, offset, buf, count, type);
    }
    else
    {
        int off = 0;

        if(isHeapBuffer(buf))
        {
            off = ((Buffer)buf).arrayOffset();
            buf = ((Buffer)buf).array();
        }

        Status stat = new Status();
        writeAtAll(handle, offset, buf, off, count, type, stat);
        beginStatus = stat;
    }
}

private native void writeAtAllBegin(
        long fh, long fileOffset, Object buf, int count, Datatype type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_AT_ALL_END}.
 * @param buf buffer
 * @return status object
 * @throws MPIException 
 */
public Status writeAtAllEnd(Object buf) throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        Status stat = new Status();
        writeAtAllEnd(handle, buf, stat);
        return stat;
    }
    else
    {
        return getBeginStatus();
    }
}

private native void writeAtAllEnd(long fh, Object buf, Status stat)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ_ALL_BEGIN}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @throws MPIException 
 */
public void readAllBegin(Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        readAllBegin(handle, buf, count, type);
    }
    else
    {
        int off = 0;

        if(isHeapBuffer(buf))
        {
            off = ((Buffer)buf).arrayOffset();
            buf = ((Buffer)buf).array();
        }

        Status stat = new Status();
        readAll(handle, buf, off, count, type, stat);
        beginStatus = stat;
    }
}

private native void readAllBegin(long fh, Object buf, int count, Datatype type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ_ALL_END}.
 * @param buf buffer
 * @return status object
 * @throws MPIException 
 */
public Status readAllEnd(Object buf) throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        Status stat = new Status();
        readAllEnd(handle, buf, stat);
        return stat;
    }
    else
    {
        return getBeginStatus();
    }
}

private native void readAllEnd(long fh, Object buf, Status stat)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_ALL_BEGIN}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @throws MPIException 
 */
public void writeAllBegin(Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        writeAllBegin(handle, buf, count, type);
    }
    else
    {
        int off = 0;

        if(isHeapBuffer(buf))
        {
            off = ((Buffer)buf).arrayOffset();
            buf = ((Buffer)buf).array();
        }

        Status stat = new Status();
        writeAll(handle, buf, off, count, type, stat);
        beginStatus = stat;
    }
}

private native void writeAllBegin(long fh, Object buf, int count, Datatype type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_ALL_END}.
 * @param buf buffer
 * @return status object
 * @throws MPIException 
 */
public Status writeAllEnd(Object buf) throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        Status stat = new Status();
        writeAllEnd(handle, buf, stat);
        return stat;
    }
    else
    {
        return getBeginStatus();
    }
}

private native void writeAllEnd(long fh, Object buf, Status stat)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ_ORDERED_BEGIN}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @throws MPIException
 */
public void readOrderedBegin(Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        readOrderedBegin(handle, buf, count, type);
    }
    else
    {
        int off = 0;

        if(isHeapBuffer(buf))
        {
            off = ((Buffer)buf).arrayOffset();
            buf = ((Buffer)buf).array();
        }

        Status stat = new Status();
        readOrdered(handle, buf, off, count, type, stat);
        beginStatus = stat;
    }
}

private native void readOrderedBegin(
        long fh, Object buf, int count, Datatype type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_READ_ORDERED_END}.
 * @param buf buffer
 * @return status object
 * @throws MPIException 
 */
public Status readOrderedEnd(Object buf) throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        Status stat = new Status();
        readOrderedEnd(handle, buf, stat);
        return stat;
    }
    else
    {
        return getBeginStatus();
    }
}

private native void readOrderedEnd(long fh, Object buf, Status stat)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_ORDERED_BEGIN}.
 * @param buf   buffer
 * @param count number of items in buffer
 * @param type  datatype of each buffer element
 * @throws MPIException 
 */
public void writeOrderedBegin(Object buf, int count, Datatype type)
    throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        writeOrderedBegin(handle, buf, count, type);
    }
    else
    {
        int off = 0;

        if(isHeapBuffer(buf))
        {
            off = ((Buffer)buf).arrayOffset();
            buf = ((Buffer)buf).array();
        }

        Status stat = new Status();
        writeOrdered(handle, buf, off, count, type, stat);
        beginStatus = stat;
    }
}

private native void writeOrderedBegin(
        long fh, Object buf, int count, Datatype type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_WRITE_ORDERED_END}.
 * @param buf buffer
 * @return status object
 * @throws MPIException 
 */
public Status writeOrderedEnd(Object buf) throws MPIException
{
    MPI.check();

    if(isDirectBuffer(buf))
    {
        Status stat = new Status();
        writeOrderedEnd(handle, buf, stat);
        return stat;
    }
    else
    {
        return getBeginStatus();
    }
}

private native void writeOrderedEnd(long fh, Object buf, Status stat)
        throws MPIException;

private Status getBeginStatus()
{
    Status s = beginStatus;
    beginStatus = null;
    return s;
}

/**
 * Java binding of {@code MPI_FILE_GET_TYPE_EXTENT}.
 * @param type
 * @return datatype extent
 * @throws MPIException 
 */
public int getTypeExtent(Datatype type) throws MPIException
{
    MPI.check();
    return getTypeExtent(handle, type.handle) / type.baseSize;
}

private native int getTypeExtent(long fh, long type)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_SET_ATOMICITY}.
 * @param atomicity true to set atomic mode, false to set nonatomic mode
 * @throws MPIException 
 */
public void setAtomicity(boolean atomicity) throws MPIException
{
    MPI.check();
    setAtomicity(handle, atomicity);
}

private native void setAtomicity(long fh, boolean atomicity)
        throws MPIException;

/**
 * Java binding of {@code MPI_FILE_SYNC}.
 * @throws MPIException 
 */
public void sync() throws MPIException
{
    MPI.check();
    sync(handle);
}

private native void sync(long handle) throws MPIException;

} // File
