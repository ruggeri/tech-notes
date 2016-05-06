## Most Popular

By StackOverflow job count.

* MongoDB (197)
* Redis (161)
    * http://www.amazon.com/Redis-Action-Josiah-L-Carlson/dp/1617290858
* ElasticSearch (113)
    * http://www.amazon.com/Elasticsearch-Action-Radu-Gheorghe/dp/1617291625
* Cassandra (102)
    * http://www.amazon.com/Learning-Apache-Cassandra-Tolerant-Real-Time/dp/1783989203
    * Based on a DHT
    * No transactions
    * Always writable, even in partition. Clearly not consistent.
    * Conflict resolution happens at readtime.
* Memcached (45)
* Hbase (43)
    * Based on BigTable, which is basically just a sharded DB on GFS.
    * Uses Chubby to elect a master. Master presumably 
* Solr (38)

## Data Stores

* Google
    * BigTable (== HBase)
    * Chubby (== Zookeeper)
    * GFS/Colossus/Megastore (== HDFS)
    * MapReduce (== Hadoop)
    * Spanner (semi-relational)/F1 (SQL on Spanner?)
* Amazon Dynamo
    * == Riak
    * == Cassandra
* Facebok TAO (some graph thing)
* MongoDB
* Datomic
* CouchDB (looks like a MongoDB competitor?)
    * http://guide.couchdb.org/draft/consistency.html
* Neo4j
* LinkedIn Voldemort (sounds like I don't care, it's an
  HBase/Cassandra competitor)

Other:

* Google LevelDB (open source), RocksDB (based on LevelDB)
    * I think this is just LSM stuff?
    * RocksDB is maybe just for SSD?

## Processing Frameworks

* Google
    * MapReduce
    * Pregel (graph processing)
    * FlumeJava (just a framework to wire up
    * Dremel
    * Sawzall (== Pig)
    * Hive
* Other
    * Storm

## Other

* Jeff Dean on ML: http://research.google.com/pubs/pub40565.html
* RethinkDB (document db that can do joins and group by)
* MemSQL (in memory and plays nice with flash maybe?)
* FoundationDB (An F1 clone? Acquired by Apple.)
* ElasticSearch
* Spark/Storm/Cascalog/Scalding
* This page had useful fast summaries of the various tech:
    * https://github.com/onurakpolat/awesome-bigdata
