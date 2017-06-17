## GeForce and Architecture

The GeForce GTX 1080Ti (March 2017) is currently Nvidia's
top-of-the-line GeForce card. GeForce is consumer cards. The GeForce
10 series (current series) all use the Pascal architecture. The retail
price is $699.

The Titan X (August 2016) and Titan Xp (April 2017) are ever so
slightly more powerful, but appear to be based on the same
architecture and process. They also have 1GB more memory. But the cost
is $1200 which is almost twice the price of the 1080Ti.

You have much better value buying two 1080Ti than one Titan Xp.

The Volta architecture comes next. It drops from 16nm to 12nm
process. It appears like it may have better FP16 support. But I doubt
it will be available for GeForce...

## Quadro

I believe that Quadro is effectively the same hardware as GeForce, but
with different drivers. It is typically used for CAD and CGI. It is a
market segmentation play. In fact, I think you can modify GeForce
cards to identify as Quadro.

The Quadro GP100 seems most like the 1080 Ti but does poorly in
benchmarks? The Quadrop P6000 looks to have 24Gb of memory, but is
otherwise similar to 1080 Ti. It does not quite as well as the 1080 Ti
and costs $5,000.

It looks like P6000 is basically the same as the Titan X but with
double the memory and a few more CUDA cores turned on. But it can't be
worth the money.

There appears to be no reason ever to buy a Quadro card.

## Tesla

Tesla is the name of a productline *and* an architecture. Lol. The
Tesla architecture is old and irrelevant now.

It appears that GeForce has almost equivalent single precision
performance, but that double and half precision is crippled
maybed. The Tesla has half precision throughput that is double the
single precision throughput.

This is their market segmentation play for GPGPU. If you want 16bit or
64bit FP throughput, you need to go with the Tesla line.

It looks like most recent tech is based on Pascal: this is P100 GPU
Accelerator. There are some versions which have 12 or 16GB memory, and
one is a mezzanine card that operates at a slightly higher clock
rate. Performance is all broadly similar.

The P40 GPU accelerator is based on the GP102 chip, which is newer
than the P100's GP100, but this has the crippled half/double precision
performance. It is unclear why one would want this, but it has 24GB of
memory, and slightly better single precision performance.

I believe the K80 is what AWS has. This is several generations
old. Note that the first letter indicates the architecture: K is Kepler.

The top of the line will be V100, which has the Volta architecture. It
will do 30 TFLOPS of half-precision work. This is about 50% more than
the P100 line.

It also has something called *tensor cores*. What the fuck is that?
They claim these can do 120 TFLOPS. The tensor cores do *GEMM*, which
is the name for generalized matrix multiply, which is `Result := aXY +
bZ`.

It looks like the tensor core is something called a *systolic
array*. How this can be done to perform matrix multiplication is
explained here:

    http://web.cecs.pdx.edu/~mperkows/temp/May22/0020.Matrix-multiplication-systolic.pdf

Availability for Tesla purchases is spotty. It may cost $10-15k! It
will be in their "desktop supercomputer" appliance, and also in a rack
mounted form.

## TPU

The first generation seems mostly to benefit from a systolic matrix
multiplication unit. It does INT8 math, and is basically only used for
inference, since the accuracy is too low to reliably follow a
gradient. It theoretically can do 92 TOPS/sec; we don't say TFLOPS/sec
because that's *floating point*. The K80 can do 2.8 TFLOPs/sec of
floating point, while a Haswell 18 core can do 2.6 INT8 ops.

BTW, that K80 perf number may be bullshit. That's the FP32 number I'm
almost sure; the FP16 is 8.73 TFLOPs. Wait, they turned off boost
mode, which dropped the perf. They also computed based on a single die
vs dual die, which halves the TFLOPs. Why did they do that?

It seems like Google doesn't really want you to do training; they're
fine selling you inference. This is maybe for rent in the GCE cloud,
possibly.

## Random

It appears that SLI is not an important feature. SLI is about
appearing as a single card. But TF can know about and use two
explicitly seperate cards. So SLI actually performs worse: it's meant
for games that don't want to do two cards, I think.

## Architecutres

* Volta (not yet?)
* Pascal (released 2016)
* Maxwell (released 2014)
* Kepler (released 2012)
* Fermi (released 2010)
* Tesla

Site for benchmarks: http://www.videocardbenchmark.net/directCompute.html
