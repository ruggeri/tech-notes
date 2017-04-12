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
sudo apt-get install cuda

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
