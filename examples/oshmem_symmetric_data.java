import shmem.*;
import java.nio.*;

public class oshmem_symmetric_data
{
    private static final int SIZE = 16;

    public static void main(String[] args) throws ShMemException
    {
        ShMem.startPEs(0);

        int numPE = ShMem.getNumPEs(),
            myPE  = ShMem.getMyPE();

        int[] source = new int[SIZE];
        Addr  target = new Addr(4 * SIZE); // int is 4 bytes

        if(myPE == 0)
        {
            // initialize array
            for(int i = 0; i < SIZE; i++)
                source[i] = i;

            // local, not symmetric
            // static makes it symmetric
            // put "size" words into target on each PE
            for(int i = 1; i < numPE; i++)
                target.putInt(source, i);
        }

        ShMem.barrierAll(); // sync sender and receiver

        if(myPE != 0)
        {
            StringBuilder sb = new StringBuilder();
            sb.append("Target on PE "+ myPE +" is \t");
            IntBuffer buf = target.asIntBuffer();

            for(int i = 0; i < SIZE; i++)
                sb.append(buf.get(i) +" \t");

            sb.append('\n');
            System.out.print(sb);
        }

        ShMem.barrierAll(); // sync before exiting
        target.free();
    }
}
