import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Semaphore;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {
        this.bakery = bakery;
        this.rnd = new Random();
        this.shoppingCart = new ArrayList<>();

        // Shop for 0-200 millis
        this.shopTime = rnd.nextInt(200);

        // Checkout for 0-200 millis
        this.checkoutTime = rnd.nextInt(200);
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        try {
            // Make a shopping list
            this.fillShoppingCart();

            // Customer is at the shelves
            this.bakery.accessToShelves.acquire();

            for (BreadType breadType : shoppingCart) {
                Semaphore accessToShelf = this.bakery.accessToShelf.get(breadType);

                accessToShelf.acquire();
                Thread.sleep(shopTime / shoppingCart.size()); // Customer is shopping at a shelf
                this.bakery.takeBread(breadType);
                accessToShelf.release();
            }

            // Customer leaves shelves and moves to registers
            this.bakery.accessToRegisters.acquire();
            this.bakery.accessToShelves.release();

            this.bakery.cashierMutex.acquire();
            Thread.sleep(checkoutTime); // Customer is checking out
            this.bakery.addSales(this.getItemsValue());
            this.bakery.cashierMutex.release();

            // Customer leaves the bakery
            this.bakery.accessToRegisters.release();

            System.out.println(this);

        } catch (InterruptedException ie) {
            ie.printStackTrace();
        }

    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return String.format("Customer %10s : Cart=%-33s  shopTime=%-3s  checkoutTime=%-3s", hashCode(), Arrays.toString(shoppingCart.toArray()), shopTime, checkoutTime);
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}