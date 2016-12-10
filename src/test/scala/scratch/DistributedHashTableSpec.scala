package dht

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

class DistributedHashTableSpec extends FeatureSpec with Matchers with GivenWhenThen {

  feature("A distributed, decentralized, fault tolerant, scalable hash table ") {

    scenario("a table with a single node assigns all keys to the node") {
      pending
    }

    scenario("an existing table assigns new nodes to a random key range") {
      pending
    }

    scenario("an existing table reassigns the key range for a node that leaves to the remaining nodes") {
      pending
    }

    scenario("each node is assigned it's predecessor's key range as a secondary range") {
      pending
    }
  }
}
