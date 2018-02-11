This code takes a matrix of nonnegative values and tries to quickly find
the symmetrical matrix most similar to it.  I wrote this in 2015. 

A practical application of this is in transit demand modeling.  If we
assume that demand to travel from one station on a route to another
particular station is a function of the distance between the stations
and the number of station entries at the other station, we can determine
the proportion of passengers traveling from each station to each
other station.

The problem with the above approach is that the results of such an approach
are not symmetrical, ie. more people will choose to travel from station
A to station B than from station B to station A.  In the real world,
this means that the function chosen to model transit demand is probably
not the best such function.  However, since a distance-and-boardings-based
model is easy to set up, we may choose to use it anyway if what we're
interested in is not any serious analysis, but rather a fun thought
experiment.

So, to talk about the expected number of passengers who might travel between
stations A and B, we need to find a matrix that is symmetric, but similar
to the demand function we choose.  Which is what this code does.
