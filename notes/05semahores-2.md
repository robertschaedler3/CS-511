# Semaphores II

**Patterns based on semeaphores**

A **semaphore** is an abstract data type with two operations (*aquire and release*). It can be used to solve the mutual exclusion problem and to synchronize cooperative threads.

## Producers/consumers

This is a common pattern of interaction which must cater for a difference in speed between each party.

### Unbounded Buffer

The producer can work freely and the consumer must wait for the producer to produce. 

### Bounded Buffer

The producer must wait whan the buffer is full and the consuemr must wait for the producer to produce.

