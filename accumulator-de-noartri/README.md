# What is this?

This is complete rewriting of the accumulator software of DV.  I am also going to change the approach.

# The model

Suppose we want to produce one frame every NT microseconds, with N an oversampling factor.

We proceed as follows
1. Split the time axis in intervals of T microseconds and count the number of events (with sign) that happen in each interval.  Let x_p(nT) be the number of events (with sign) that happened in ((n-1)T, nT] at pixel p
2. Filter each x_p with a filter h to obtain y_p=x_p * h (where '*' is the convolution)
3. Subsample y_p by a factor N to obtain u_p(NT n)=y_p(NT n)
4. Scale every u_p(NT n) by S to obtain the value of pixel p at frame n (at time NTn)
5. Every signal u_p is normalized so that all u_p begin at the same time and end at the same time
