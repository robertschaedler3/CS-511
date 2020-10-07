import java.util.concurrent.locks.Condition;

public class Semaphore {

    private int permissions;
    private Condition nonZero;

    public Semaphore() {
        this.permissions = 0;
    }

    public Semaphore(int permissions) {
        this.permissions = permissions;
    }

    public synchronized void aquire() {
        while (permissions == 0)
            nonZero.wait();
        permissions--;
    }

    public synchronized void release() {
        permissions++;
        nonZero.notifyAll();
    }
}
