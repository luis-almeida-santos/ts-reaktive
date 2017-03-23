package com.tradeshift.reaktive.marshal

import scala.collection.JavaConverters._

package object scaladsl {
  implicit def readSeq2Scala[E,T](p: ReadProtocol[E,javaslang.collection.Seq[T]]): ReadProtocol[E,scala.collection.immutable.Seq[T]] =
    p.map(f1(seq => scala.collection.immutable.Seq.empty[T] ++ seq.asScala))
    
  implicit def readVector2Scala[E,T](p: ReadProtocol[E,javaslang.collection.Vector[T]]): ReadProtocol[E,scala.collection.immutable.Vector[T]] =
    p.map(f1(seq => scala.collection.immutable.Vector.empty[T] ++ seq.asScala))
    
  implicit def readHashMap2Scala[E,K,V](p: ReadProtocol[E,javaslang.collection.HashMap[K,V]]): ReadProtocol[E,scala.collection.immutable.HashMap[K,V]] =
    p.map(f1(seq => scala.collection.immutable.HashMap.empty[K,V] ++ seq.toJavaMap.asScala))
    
  implicit def readWriteSeq2Scala[E,T](p: Protocol[E,javaslang.collection.Seq[T]]): Protocol[E,scala.collection.Seq[T]] =
    p.map(f1(seq => scala.collection.immutable.Vector.empty[T] ++ seq.asScala),
          f1(seq => javaslang.collection.Vector.ofAll(seq.asJava)))
    
  implicit def readWriteMap2Scala[E,K,V](p: Protocol[E,javaslang.collection.Map[K,V]]): Protocol[E,scala.collection.immutable.Map[K,V]] =
    p.map(f1(m => scala.collection.immutable.HashMap.empty[K,V] ++ m.toJavaMap.asScala),
          f1(m => javaslang.collection.HashMap.ofAll(m.asJava)))
          
  implicit def readWriteOption2Scala[E,T](p: Protocol[E,javaslang.control.Option[T]]): Protocol[E,Option[T]] =
    p.map(f1(m => if (m.isDefined()) Some(m.get()) else None),
          f1(m => if (m.isDefined) javaslang.control.Option.some(m.get) else javaslang.control.Option.none()))
          
  implicit def readOption2Scala[E,T](p: ReadProtocol[E,javaslang.control.Option[T]]): ReadProtocol[E,Option[T]] =
    p.map(f1(m => if (m.isDefined()) Some(m.get()) else None))
          
  implicit def writeOption2Scala[E,T](p: WriteProtocol[E,javaslang.control.Option[T]]): WriteProtocol[E,Option[T]] =
    p.compose(f1(m => if (m.isDefined) javaslang.control.Option.some(m.get) else javaslang.control.Option.none()))
          
  implicit def writeSeqToScala[E,T](p: WriteProtocol[E,javaslang.collection.Seq[T]]): WriteProtocol[E,scala.collection.Seq[T]] =
    p.compose(f1(seq => javaslang.collection.Vector.ofAll(seq.asJava)))
    
  implicit def writeIterableToScala[E,T](p: WriteProtocol[E,java.lang.Iterable[T]]): WriteProtocol[E,scala.collection.Iterable[T]] =
    p.compose(f1(it => it.asJava))
    
  implicit def writeMapToScala[E,K,V](p: WriteProtocol[E,javaslang.collection.Map[K,V]]): WriteProtocol[E,scala.collection.immutable.Map[K,V]] =
    p.compose(f1(map => javaslang.collection.HashMap.ofAll(map.asJava)))
    
  def f1[T,U](f: T => U) = new javaslang.Function1[T,U] {
    override def apply(t: T): U = f(t)
  }
  
  def f2[T1,T2,U](f: (T1,T2) => U) = new javaslang.Function2[T1,T2,U] {
    override def apply(t1: T1, t2: T2): U = f(t1, t2)
  }  

  def f3[T1,T2,T3,U](f: (T1,T2,T3) => U) = new javaslang.Function3[T1,T2,T3,U] {
    override def apply(t1: T1, t2: T2, t3: T3): U = f(t1, t2, t3)
  }  
}