package shmem;

/**
 * Signals that an SHMEM exception of some sort has occurred.
 */
public final class ShMemException extends Exception
{
/**
 * Creates an exception.
 * @param message message associated to the exception
 */
public ShMemException(String message)
{
    super(message);
}

} // ShMemException
