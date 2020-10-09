# Monitors

We've seen that `semaphores` are and efficient tool to solve synchronization problems. Semaphores are elegant and efficient for solving problems in concurrent programs however, they are low-level constructs since they are not structured. Monitors will provide synchronization by encapsulation.

A monitor containts a set of operations encapsulated in modules and unique lock that ensures mutual exclusion to all operations in the monitor. Additionally special variables called condition variables, that are used to program conditional synchronization.

## Counter Example

With Semaphores:
```java
class Counter {
    private cnt = 0;
    private Semaphore mutex = new Semaphore(1);

    public void inc() {
        mutex.aquire();
        cnt++;
        mutex.release();
    }

    public void dec() {
        mutex.aquire();
        cnt--;
        mutex.release();
    }
}
```

With Monitors:
```java
monitor Counter {
    private cnt = 0;

    public void inc() {
        cnt++;
    }

    public void dec() {
        cnt--;
    }
}
```

Each object has its own lock called `intrinsic` or `monitor` lock. It also has its own `wait-set` (more on this later). The class also has its own locak but it is not used in this example.

Apart from the lock, there are `conditional variables` associated with the monitor. 

- Operations:
  - `Cond.wait()`: **always** blocks the process and places it in the waiting queue of the variable `Cond`. When it blocks, it releases the mutex on the monitor.
  - `Cond.notify()`: unblocks the first process in the waiting queue of the variableCondand sets it to the *ready* state. If there are no processes in the waiting queue, it has no effect.
  - `Cond.empty()`: checks if waiting queue ofCondis empty or not
- A queue of blocked processes

> **Example: Buffer of Size 1**
> 
> ```java
> monitor Buffer {
>     
>     private Object buffer = null; // shared buffer
>     private Condition full;       // wait until space available
>     private Condition empty;      // wait until buffer available
>     
>     public Object consume() {
>         while (buffer == null)
>             full.wait();
>         Object aux = buffer;
>         buffer = null;
>         empty.notify();
>         return aux;
>     }
>     
>     public void produce(Object o) {
>         while (buffer != null)
>             empty.wait();
>         buffer = o;
>         full.notify();
>     }
>         
> }
> ```

## Wait

Blocks the process currently executing and associates it with a variable’s queue. Upon blocking, it frees the `lock` allowing the entry of other processes.

## Notify

Two strategies:
- Notify and Urgent Wait: `E < N < W` (classical monitors)
- Notify and Continue: `E = W < N` (Java ⇐ We focus on this one)

where the letters denote the precedence of:
- `N`: notifying processes
- `W`: waiting processes 
- `E`: processes blocked on entry

> **Example: Monitor that Defines a Semaphore**
> 
> ```java
> monitor Semaphore {
> 
>     private Condition nonZero;
>     private int permissions;
>     
>     public Semaphore(int n) {
>         this.permissions = n;
>     }
>     
>     public void acquire() {
>         while (permissions == 0)
>             nonZero.wait ();
>         permissions--;
>     }
>     
>     public void release() {
>         permissions++;
>         nonZero.notifyAll();
>     }
>     
> }
> ```

## Readers/Writers 

```java
monitor RW {

    int readers, writers = 0;
    Condition okToRead, okToWrite;

    public void StartRead() {
        while (writers != 0 || !okToWrite.empty()) {
            okToRead.wait();
        }
        readers++;
    }

    public void EndRead() {
        readers--;
        if (readers == 0) {
            okToWrite.notify();
        }
    }

    public void StartWrite() {
        while (writers != 0 || reader != 0) {
            okToWrite.wait();
        }
        writers = 1;
    }

    public void EndWrite() {
        writers = 0;
        okToWrite.signal();
        okToRead.signalAll();
    }
}
```

Upholds the readers-writers invariant, however it gives priority to readers over writers becasue:
- new readers can enter the moniotr without waiting as long as a reader is active
- waiting writers have to wait until the last reader calls `endRead` and signals `okToWrite`
- as long as readers keep arriving and queuing for entering the monitor, the waiting writers will never execute.

### Fair Solution for Writers:

The problems above can be resolved by checking if there are waiting writers. *Creating a solution without deadlock that is fair for both readers and writers is particularly difficult*.

```java
monitor RW {

    int readers, writers, writersWaiting = 0;
    Condition okToRead, okToWrite;

    public void StartRead() {
        // Check for waiting writers
        while (writers != 0 || writersWaiting != 0) {
            okToRead.wait();
        }
        readers++;
    }

    public void EndRead() {
        readers--;
        if (readers == 0) {
            okToWrite.notify();
        }
    }

    public void StartWrite() {
        while (writers != 0 || reader != 0) {
            writersWaiting++;
            okToWrite.wait();
            writersWaiting--;
        }
        writers = 1;
    }

    public void EndWrite() {
        writers = 0;
        okToWrite.signal();
        okToRead.signalAll();
    }
}
```

## Barrier Synchonization

Creates a *one-time-use* barrier for synchronizing threads such that *N* threads must reach the barrier before they can continue.

```java
monitor Barrier {

    final int N = 3;
    int count = 0;
    Condition readyToPass;

    public void pass() {
        if (c < N) {
            count++;
            while(count < N) {
                readyToPass.wait()
            }
            readyToPass.signal();
        }
    }

}
```