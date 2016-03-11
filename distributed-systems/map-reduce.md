This is a summary of the [Google MapReduce][map-reduce-pdf].

[map-reduce-pdf]: http://static.googleusercontent.com/media/research.google.com/en/us/archive/mapreduce-osdi04.pdf

I will focus on the distributed aspect.

The MapReduce cluster has a master. The job is submitted to the
master. It splits the input file up into bits and assigns some mappers
to begin processing. They output to intermediate files. The master is
informed of these files.

When a mapper finishes, the master notifies the reducer. It reads the
intermediate files and sorts them. The reducers write out their
results.

If mappers or reducers fail, the cluster master will notice, and will
start other workers in their place. It can do this even if the worker
is only being slow.

Last, MapReduce plays nice with GFS because it does sequential reading
and append-only writing.
