**Compute**

* EC2
    * You get virtualized machines that you run an OS on.
* Elastic Beanstalk
    * This is a layer of abstraction above EC2.
    * You deploy an application, and it runs it on your EC2 instance
      for you.
    * Will autoscale for load. Will setup ELB.
* Lambda
    * You write a bunch of event handlers, which Lambda will execute
      when an event comes in. You don't manage the machine itself.

**Storage**

* S3
    * An object store. Not a block device, so you can't mount an FS on
      this.
* CloudFront
    * A CDN.
* Elastic Block Storage
    * A distributed block storage system. You need to format it.
    * Unlike EC2 instance storage, will persist across stop/start of
      EC2 instances.
    * Supposed to be very fast.
    * Sounds like it is attached to an EC2 instance though.
    * Is replicated for redundancy, BTW.
* Elastic File System
    * Network FS. Slow.
    * Available across datacenters; accessible from multiple machines.
    * Does have file-level locking.
    * Sounds like GFS.
* Glacier
    * Very cheap storage. Access time is hours, though. Used for
      retention.

**Database**

* RDS
    * They run a DB for you. You select how much IO, CPU, disk you
      need.
    * (You could do this with EBS, btw).
    * Not infinitely scalable; max size is 6GB.
* DynamoDB
    * NoSQL DB. Scalable. Not relational.
    * Looks like there is a library for transactions. Not sure how
      this affects perf.
* ElastiCache
    * In-memory caching. Can use Memcached or Redis.
* Redshift
    * Column-orriented DB.
    * Faster querying, worse transactional performance.
    * Very compressible.
    * Designed to be MPP.

**Networking**

* Route 53
    * Registrar and Name Server
* Elastic Load Balancing
    * Auto distributes load across EC2 instances.
    * High availability.

**Analytics**

* EMR
    * A Hadoop infrastructure.
    * Presumably easier than setting up EC2 yourself.
* Elasticsearch
    * Runs Elasticsearch for you.
    * Elasticsearch appears to be just about rebuilding indices for
      you.

**Other**

* Kinesis
    * It's a distributed message queue. Competes with Kafka.
    * Competes with SQS.
    * Advantages:
        * Records are ordered. Can be played back multiple times.
        * Partitioned so that all records for a given key can be
          processed by the same event processor.
* SQS
    * A regular message queue.
    * Advantages:
        * Because messages are ACKed, the application doesn't need to
          keep track of a cursor.
        * But that's kinda BS because failed messages may be
          redelivered...
    * Sounds like the scaling story is easier. Can add more readers at
      any time. Whereas with Kafka, you can't dynamically do that.
        * Why? I understand that Kafka can't reshard on the fly, but
          if I don't need all messages to be delivered to the same
          person, isn't it okay just to have multiple readers reading
          the same shard?
* SES
    * Sends email for you.
    * Also can receive email and execute a lambda function.
