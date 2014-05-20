import shmem.*;
import java.nio.*;

public class oshmem_strided_puts
{
    public static void main(String[] args) throws ShMemException
    {
        ShMem.startPEs(0);
        int me = ShMem.getMyPE();

        short[] source = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        Addr    target = new Addr(2 * 10); // short is 2 bytes.

        if(me == 0)
        {
            /* put 10 words into target on PE 1 */
            target.iPutShort(source, 1, 2, 5, 1);
        }

        ShMem.barrierAll(); // sync sender and receiver

        if(me == 1)
        {
            ShortBuffer buf = target.asShortBuffer();
            System.out.printf("target on PE %d is %d %d %d %d %d\n", me,
                              buf.get(0), buf.get(1), buf.get(2),
                              buf.get(3), buf.get(4));
        }

        ShMem.barrierAll(); // sync before exiting
        target.free();
    }
}
