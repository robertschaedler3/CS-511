# Semaphores II

**Patterns based on semeaphores**

A **semaphore** is an abstract data type with two operations (*aquire and release*). It can be used to solve the mutual exclusion problem and to synchronize cooperative threads.

## Producers/consumers

This is a common pattern of interaction which must cater for a difference in speed between each party.

### Unbounded Buffer

The producer can work freely and the consumer must wait for the producer to produce. 

### Bounded Buffer

The producer must wait whan the buffer is full and the consuemr must wait for the producer to produce.

## Readers/writers

There are shared resources between two types of threads.
- **Readers**: access the resources without modifying it (can access simultaneously)
- **Writers**: access the resource and may modify it (at most one at any given time)

### Properties a Solution Should Possess

- Each read/write operation should occur inside the critical region
- Must guarantee mutual exclusion between the writers
- Must allow multiple readers to execute inside the critical region simultaneously

### First Solution: *Priority Readers*

- One semaphore for controlling write access 
- Before writing, the permission must be obtained and then released when done
- The first reader must “steal” the permission to write and the last one must return it
  - We must count the number of readers inside the critical section
  - This must be done inside its own critical section

### Second Solution: *Priority Writers*

- The readers can potentially lock out all the writers
  - We need to count the number of writers that are waiting
  - Also, this counter requires its own critical section
- Before reading the readers must obtain a permission to do so
