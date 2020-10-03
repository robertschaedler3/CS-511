import java.text.NumberFormat;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

public class Bakery implements Runnable {
    public Map<BreadType, Semaphore> accessToShelf;
    public Semaphore accessToShelves = new Semaphore(3);
    public Semaphore accessToRegisters = new Semaphore(4);
    public Semaphore cashierMutex = new Semaphore(1);

    private static final int TOTAL_CUSTOMERS = 200;
    private static final int ALLOWED_CUSTOMERS = 50;
    private static final int FULL_BREAD = 20;
    
    private Map<BreadType, Integer> availableBread;
    private ExecutorService executor;
    private float sales = 0;

    private Semaphore accessToRye = new Semaphore(1);
    private Semaphore accessToSourdough = new Semaphore(1);
    private Semaphore accessToWonder = new Semaphore(1);

    /**
     * Remove a loaf from the available breads and restock if necessary
     */
    public void takeBread(BreadType bread) {
        int breadLeft = availableBread.get(bread);
        if (breadLeft > 0) {
            availableBread.put(bread, breadLeft - 1);
        } else {
            System.out.println("No " + bread.toString() + " bread left! Restocking...");
            // restock by preventing access to the bread stand for some time
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            }
            availableBread.put(bread, FULL_BREAD - 1);
        }
    }

    /**
     * Add to the total sales
     */
    public void addSales(float value) {
        sales += value;
    }

    /**
     * Run all customers in a fixed thread pool
     */
    public void run() {
        availableBread = new ConcurrentHashMap<BreadType, Integer>();
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);

        accessToShelf = new ConcurrentHashMap<BreadType, Semaphore>();
        accessToShelf.put(BreadType.RYE, accessToRye);
        accessToShelf.put(BreadType.SOURDOUGH, accessToSourdough);
        accessToShelf.put(BreadType.WONDER, accessToWonder);

        executor = Executors.newFixedThreadPool(ALLOWED_CUSTOMERS);
        for (int i = 0; i < TOTAL_CUSTOMERS; i++) {
            executor.execute(new Customer(this));
        }
        executor.shutdown();

        try {
            executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
            NumberFormat numFmt = NumberFormat.getCurrencyInstance();
            System.out.printf("Total Sales: %s\n", numFmt.format(sales));
        } catch (InterruptedException ie) {
            ie.printStackTrace();
        }
    }
}