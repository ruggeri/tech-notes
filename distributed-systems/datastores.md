## Most Popular

By StackOverflow job count.

* MongoDB (197)
* Redis (161)
* ElasticSearch (113)
* Cassandra (102)
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
    * Google File System
    * BigTable (== HBase)
    * Colossus
    * Megastore
    * Spanner
    * F1
* Dynamo
    * == Riak
    * == Cassandra
* MongoDB
* Datomic
* CouchDB
    * http://guide.couchdb.org/draft/consistency.html
* Neo4j
* Voldemort

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
