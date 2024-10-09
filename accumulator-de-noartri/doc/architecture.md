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
