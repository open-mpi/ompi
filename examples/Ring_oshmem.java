import shmem.*;

public class Ring_oshmem
{
    public static void main(String[] args) throws ShMemException
    {
        ShMem.startPEs(0);
        int nproc = ShMem.getNumPEs();
        int proc  = ShMem.getMyPE();
        Addr rbuf = new Addr(4); // One integer value.
        rbuf.putInt(-1);
        int message = 10;

        // Calculate the PE number of the next process in the ring. Use the
        // modulus operator so that the last process "wraps around" to PE 0.
        int next = (proc + 1) % nproc;

        if(proc == 0)
        {
            System.out.println("Process 0 puts message "+ message +" to "+
                               next +" ("+ nproc +" processes in ring)");

            rbuf.putInt(message, next);
        }

        // Pass the message around the ring. The exit mechanism works as
        // follows: the message (a positive integer) is passed around the
        // ring. Each time it passes PE 0, it is decremented. When each
        // processes receives a message containing a 0 value, it passes the
        // message on to the next process and then quits. By passing the 0
        // message first, every process gets the 0 message and can quit
        // normally.

        while(message > 0)
        {
            rbuf.waitUntilInt(ShMem.CMP_EQ, message);

            if(proc == 0)
            {
                message--;
                System.out.println("Process 0 decremented value: "+ message);
            }

            rbuf.putInt(message, next);

            if(proc != 0)
                message--;
        }

        // All done
        rbuf.free();
        System.out.println("Process "+ proc +" exiting");
    }
}
