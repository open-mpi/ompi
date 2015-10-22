package shmem;

/**
 * Symmetric work array.
 */
public final class PSync
{
private long handle;

/**
 * Allocates a symmetric work array.
 * @param size Number of elements in the work array.
 * @throws ShMemException Allocation error.
 */
public PSync(int size) throws ShMemException
{
    handle = newPSync(size);

    if(handle == 0)
        throw new ShMemException("Allocation error.");
}

private native long newPSync(int size);

/**
 * Frees a work array.
 */
public void free()
{
    free(handle);
}

private native void free(long handle);

protected long addr()
{
    return handle;
}

} // PSync
