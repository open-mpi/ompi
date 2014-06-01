import shmem.*;

public class oshmem_circular_shift
{
    public static void main(String[] args) throws ShMemException
    {
        ShMem.startPEs(0);

        int numPEs = ShMem.getNumPEs(),
            myPE   = ShMem.getMyPE(),
            peer   = (myPE + 1) % numPEs;

        int[] aaa = new int[1];
        Addr  bbb = new Addr(4);

        System.out.println("Process "+ myPE +" gets message from "+
                           peer +" ("+ numPEs +" processes in ring)");

        bbb.getInt(aaa, peer);
        ShMem.barrierAll();
        bbb.free();
        System.out.println("Process "+ myPE +" exiting");
    }
}
