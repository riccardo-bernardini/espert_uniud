# The algorithm

1. Read the event file and apply rectification (if needed)
2. Store the events in an ordered table, with the events ordered lexicographically as (x, y, time).  In this way when I will read the table in order, I will see the events of each pixel in order of increasing time.
3. Find out the minimum and maximum time and subtract from each event time t0.  Usually t0 is the minimum event time, but it can be imposed via the CLI
4. Scan the event table and, for every pixel p=(x,y), find the first and last index of the table relative to the segment of events at pixel p
5. For every pixel p
   a. Create signal x_p(nT) accumulating the events in [nT, (n+1)T)
   b. Filter x_p to get y_p
   c. Subsample y_p to get u_p
   d. Store p and u_p
6. For every time instant nNT
   a.  Initialize the n-th frame with the initial image
   a.  For every stored pixel p
       x. Read u_p(nNT)  
       x. Store it at pixel p
   b.  Save frame n
   
Step 5 and 6 can be done in parallel by many tasks.  6 can begin only after 5 ends

I can store the pixel p in an ordered list (or a vector), while the signal u_p(0) ... u_p(L NT) is stored in the L+1 consecutive entries of a vector with indices S(p), S(p)+1, ...  In this way, I can access u_p(n NT) at the address S(p)+n 

# Formalization

* An event file is a sequence of 4-tuples (time, x, y, sign), ordered in increasing time
* The user select T-min, T-max, T-frame and N.  Let T-c = T-frame / N
* If more than one file is specified,
   * T-min is (by default) the maximum among the smallest timestamp of each file
   * T-max is (by default) the minimum among the largest timestamp of each file
* All the events outside the interval T-min..T-max are removed
* For every pixel p and n=0, 1, ...
  * Let x_p(n) be the total sum of events (with sign) happening with 
  
```
T-min + n Tc <= timestamp < T-min + (n+1) Tc
```
that is
```
n <= (timestamp-Tmin)/Tc < n+1
```
Therefore, the event with timestamp T contribuites to `n=floor((T-Tmin)/Tc)` 

  * The maximum n (nmax) is such that 
```
T-min + n Tc <= T-max
T-min + (n+1) Tc > T-max
```
that is,
```
n+1 > (Tmax-Tmin)/Tc >= n
```
it follows that it must be `nmax=floor((Tmax-Tmin)/Tc)`

The total number of frames is `floor(nmax / N)`
