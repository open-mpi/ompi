import shmem.*;
import java.nio.*;

public class oshmem_max_reduction
{
    private static final int N = 3;

    public static void main(String[] args) throws ShMemException
    {
        ShMem.startPEs(0);

        int numPEs = ShMem.getNumPEs(),
            myPE   = ShMem.getMyPE();

        Addr  src   = new Addr(8 * N), // long is 8 bytes.
              dst   = new Addr(8 * N),
              pWrk  = new Addr(8 * ShMem.REDUCE_SYNC_SIZE);
        PSync pSync = new PSync(ShMem.BCAST_SYNC_SIZE);

        LongBuffer srcBuf = src.asLongBuffer(),
                   dstBuf = dst.asLongBuffer();

        for(int i = 0; i < N; i++)
            srcBuf.put(i, myPE + i);

        ShMem.barrierAll();
        dst.maxToAllLong(src, N, 0, 0, numPEs, pWrk, pSync);

        StringBuilder sb = new StringBuilder();
        sb.append(myPE +"/"+ numPEs +" dst =");

        for(int i = 0; i < N; i++)
            sb.append(" "+ dstBuf.get(i));

        sb.append("\n");
        System.out.print(sb);

        src.free();
        dst.free();
        pWrk.free();
        pSync.free();
    }
}
