package example.structures

/**
  * Map trait
  *
  * @tparam K the key
  * @tparam V the value to store
  */
trait Map[K, V] {
  def +( kv: (K,V)*)
  def -(key: K)
  def get(key: K): Option[V]
}


/**
  * Hash map with an array using the divisional method
  *
  * @tparam K the key
  * @tparam V the value to store
  */
class HashMap[K, V] extends Map[K, V] {

  private val keys: Array[List[(K,V)]] = new Array(256)

  def apply(key: K) = get(key) match {
    case Some(x) => x
    case None => throw new NoSuchElementException("No such element found with that key.")
  }

  override def +(kv: (K, V)*) {
    val hashes = kv map{ kv => (divisionalMethod(kv._1), kv) }
    hashes foreach {
      case (hash, keyValue) =>
        keys(hash) match {
          case x: List[K] =>
            val y = x.filterNot( keyValue._1 == _._1)
            keys(hash) = keyValue :: y
          case _ =>
            keys(hash) = keyValue :: Nil
        }

    }
  }

  override def get(key: K): Option[V] = keys(divisionalMethod(key)) match {
    case null => throw new NoSuchElementException("No such element found with that key.")
    case x => x.find( key == _._1) map { _._2 }
  }

  override def -(key: K) {
    val hash = divisionalMethod(key)
    val space = keys(hash)
    if(space != null) {
      val list = keys(hash).filterNot(key == _._1)
      keys(hash) = list
    }
  }

  private def divisionalMethod(key: K): Int = key.## % keys.size
}


/**
  * A map with a List as the back store
  *
  * @tparam K the key
  * @tparam V the value to store
  */
class ListMap[K, V] extends Map[K, V]{

  private var store = List.empty[(K,V)]

  override def +(kv: (K, V)*) = {
    kv foreach {
      y =>
        val (l, r) = store span( x => x._1 != y._1)
        if( r != Nil) store = l ++ (y :: r.tail)
        else store = y :: l
    }
  }

  override def get(key: K): Option[V] = store.find(key == _._1) map {_._2}

  override def -(key: K) = {
    val (l, r) = store.span(_._1 != key)
    if( r != Nil) store = l ++ r.tail
  }
}