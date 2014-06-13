package mpi;

/**
 * This class represents {@code MPI_Info}.
 */
public final class Info implements Freeable
{
protected long handle;
protected static final long NULL = getNull();

/**
 * Java binding of the MPI operation {@code MPI_INFO_CREATE}.
 */
public Info() throws MPIException
{
    MPI.check();
    handle = create();
}

protected Info(long handle)
{
    this.handle = handle;
}

private native long create();

protected static Info newEnv()
{
    return new Info(getEnv());
}

private native static long getEnv();
private native static long getNull();

/**
 * Java binding of the MPI operation {@code MPI_INFO_SET}.
 * @param key   key
 * @param value value
 * @throws MPIException 
 */
public void set(String key, String value) throws MPIException
{
    MPI.check();
    set(handle, key, value);
}

private native void set(long handle, String key, String value)
        throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_INFO_SET}.
 * @param key key
 * @return value or {@code null} if key is not defined
 * @throws MPIException 
 */
public String get(String key) throws MPIException
{
    MPI.check();
    return get(handle, key);
}

private native String get(long handle, String key) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_INFO_SET}.
 * @param key key
 * @throws MPIException 
 */
public void delete(String key) throws MPIException
{
    MPI.check();
    delete(handle, key);
}

private native void delete(long handle, String key) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_INFO_GET_NKEYS}.
 * @return number of defined keys
 * @throws MPIException 
 */
public int size() throws MPIException
{
    MPI.check();
    return size(handle);
}

private native int size(long handle) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_INFO_GET_NTHKEY}.
 * @param i key number
 * @return key
 * @throws MPIException 
 */
public String getKey(int i) throws MPIException
{
    MPI.check();
    return getKey(handle, i);
}

private native String getKey(long handle, int i) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_INFO_DUP}.
 * <p>It is recommended to use {@link #dup} instead of {@link #clone}
 * because the last can't throw an {@link mpi.MPIException}.
 * @return info object
 */
@Override public Info clone()
{
    try
    {
        return dup();
    }
    catch(MPIException e)
    {
        throw new RuntimeException(e.getMessage());
    }
}

/**
 * Java binding of the MPI operation {@code MPI_INFO_DUP}.
 * @return info object
 * @throws MPIException
 */
public Info dup() throws MPIException
{
    MPI.check();
    return new Info(dup(handle));
}

private native long dup(long handle) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_INFO_FREE}.
 * @throws MPIException 
 */
@Override public void free() throws MPIException
{
    MPI.check();
    handle = free(handle);
}

private native long free(long handle) throws MPIException;

/**
 * Tests if the info object is {@code MPI_INFO_NULL} (has been freed).
 * @return true if the info object is {@code MPI_INFO_NULL}, false otherwise.
 */
public boolean isNull()
{
    return isNull(handle);
}

private native boolean isNull(long handle);

} // Info
