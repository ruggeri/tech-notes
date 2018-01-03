I bought a P2.xlarge instance in Oregon.

## Mount EBS and Install Anaconda

Mount EBS volume with enough space.
Install anaconda
wget https://repo.continuum.io/archive/Anaconda3-4.3.0-Linux-x86_64.sh
bash Anaconda3-4.3.0-Linux-x86_64.sh
Install this in your EBS volume.

## Install CUDA Toolkit

Next install CUDA toolkit (I installed 8.0.61-1; believe this also
installs drivers)

https://developer.nvidia.com/cuda-downloads
http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/cuda-repo-ubuntu1604_8.0.61-1_amd64.deb
sudo dpkg -i cuda-repo-ubuntu1604_8.0.61-1_amd64.deb
sudo apt-get update
sudo apt-get install cuda=8.0.61-1

There's a pdf with post installation instructions (but I list them
below.)

http://developer.download.nvidia.com/compute/cuda/8.0/secure/Prod2/docs/sidebar/CUDA_Installation_Guide_Linux.pdf?autho=1488188889_1fa20c1d401a0769cb835f9e8da551a8&file=CUDA_Installation_Guide_Linux.pdf

Post CUDA installation:
export PATH=/usr/local/cuda-8.0/bin${PATH:+:${PATH}}
export LD_LIBRARY_PATH=/usr/local/cuda-8.0/lib64\ ${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}

**You should be able to run nvidia-smi and get a report on the card.**

## CUDNN

I believe this is just a library to accelerate some DNN functionality.

https://developer.nvidia.com/rdp/cudnn-download
https://developer.nvidia.com/compute/machine-learning/cudnn/secure/v5.1/prod_20161129/8.0/cudnn-8.0-linux-x64-v5.1-tgz

They won’t let you download this directly to AWS, so you need to
download from their website, then upload with scp.

export LD_LIBRARY_PATH="/ebs/ubuntu/cuda/lib64"

(Confusing name for a directory because this is actually a different
library)

## libcupti-dev

I guess this is a profiling tools interface for CUDA. You install with:

sudo apt-get install libcupti-dev

## TensorFlow

Finally, you should be able to install:

pip install tensorflow-gpu

Run a simple validation:
>>> import tensorflow as tf
>>> hello = tf.constant('Hello, TensorFlow!')
>>> sess = tf.Session()
>>> print(sess.run(hello))

## Jupyter

jupyter notebook --generate-config

In the config change:

c.NotebookApp.ip = '*'

May also need to add rule for security group on AWS so you can reach
port 8888.

## Persistence Mode

After each reboot, you must reset persistence mode to on:

    # AWS documents say this can take several minutes to run?
    sudo nvidia-persistenced
    sudo nvidia-smi -pm 1


You maybe have to turn off the power scaling feature too

    sudo nvidia-smi --auto-boost-default=0

And this tells the GPU to set its max speed to max:

    # For P2
    sudo nvidia-smi -ac "2505,875"
    # For P3
    sudo nvidia-smi -ac "877,1530"

This stuff never seems to be needed for me, actually. It seems like
the default is in persistence mode?

## P3 Instances

On P3 you need to use Cuda 9 and CUDNN 7. I've uploaded those to the
/ebs directory.

This was a helpful resource:
    https://github.com/mind/wheels/releases/tag/tf1.4-gpu-cuda9

Here's where I downloaded CUDA 9.0.176-1, with instructions
https://developer.nvidia.com/cuda-90-download-archive

    sudo dpkg -i cuda-repo-ubuntu1604_9.0.176-1_amd64.deb
    sudo apt-key adv --fetch-keys http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/7fa2af80.pub
    sudo apt-get update
    sudo apt-get install cuda=9.0.176-1

I also needed to compile the Intel MKL thing as described.

## P3 Instances: Build your own TF

I actually recently built my own TF from source. It wasn't actually
that bad.

I also had to clear out a *bunch* of other versions of CUDA, CUDNN,
whatever that seem like maybe they were preinstalled. That pissed me
off; I think Amazon installed maybe some packages that were screwing
things up? I think the use maybe of a different driver or something
maybe causes the instability I previously noticed.

P3 often hangs. I have the problem even if I just run `nvidia-smi -pm
1`, so it can't be TF.

But ever since I've cleared out a bunch of the NVIDIA garbage that
seemed pre-installed, things seem much better.

Building TF is actually quite easy. The only trick was:

    https://stackoverflow.com/questions/43113508/math-functions-hpp-not-found-when-using-cuda-with-eigen

All I had to do was create a symlink.

I'm not 100% convinced we're out of the woods, but I haven't had P3
instability since (not that long a sample size, tho).
