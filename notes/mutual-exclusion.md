# Mutual Exclusion

## Atomicity Assumption on Statements

An instruction such as `x = x+1` is actually compiled into simpler sets of assembly instructions. This could cause undesired interleavings at the assembly level. 

> **Atomic Operation**: An operation is atomic if it cannot be interleaved at a lower level ofabstraction.

- Atomic operations are the smallest units in terms of which a path can be constituted
- In order to reason about concurrency at the `source` level, we need to know what is assumed atomic at the `source` level
  - We assume throughout this course that all (single-line) statements are atomic
  - In particular, assignments such as `counter = counter+1` are assumed to be atomic

We occasionally simulate lack of atomicity of assignment (atthe source level) by decomposing it into several simpleinstructions.

For example:
```java 
counter = counter +1;
```
can be decomposed into:
```java
temp = counter +1;
counter = temp;
```

> An occurrence of variable `v` in `P` is a critical reference if
> 1.  it is assigned in `P` and occurs in another process `Q`, or
> 2.  it is read in `P` and is assigned in another process `Q`.
> 
> A program satisfies the `Limited Critical Reference (LCR)` property if every statement contains **at most one** critical reference
> 
> Concurrent programs that satisfy the LCR restriction yield the same set of behaviors whether the statements are considered atomic or are compiled to a machine architecture with atomic load and store.
> 
> Attempting to present programs that satisfy the LCR restriction is thus convenient

## Race Condition

Once thread `T1` starts doing something, it needs to *“race”* to finish it because if thread `T2` looks at the shared variablebefore `T1` is done, it may see something inconsistent

> **Race Condition**: Arises  if  two  or  more  threads  access  the  same  variables  or  objects concurrently and at least one does updates

This is hard to detect and there can be race conditions in programs that satisfy LCR.

## Critical Section


> **Critical Section**: A  part  of  the  program  that  accesses  shared  memory  and  which  wewish to execute atomically

> **The Mutual Exclusion Problem**
> 
> Guarantee that:
> 1.  `Mutex`:  At any point in time, there is at most one thread inthe critical section
> 2.  `Absence of livelock`:  If various threads try to enter the criticalsection, at least one of them will succeed
> 3.  `Free from starvation`:  A thread trying to enter its criticalsection will eventually be able to do so

### Reasonable Assumptions of MEP


