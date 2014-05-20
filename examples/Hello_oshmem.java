import shmem.*;

public class Hello_oshmem
{
    public static void main(String[] args)
    {
        ShMem.startPEs(0);
        int nproc = ShMem.getNumPEs();
        int proc  = ShMem.getMyPE();
        System.out.println("Hello, world, I am "+ proc +" of "+ nproc);
    }
}
