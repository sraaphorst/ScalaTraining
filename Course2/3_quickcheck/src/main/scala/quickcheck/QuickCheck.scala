package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.Try

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(k, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // Insert two elements into a heap. They should appear in the proper order.
  property("twoElemsInProperOrder") = forAll { (i1: Int, i2: Int) =>
    findMin(insert(i1, insert(i2, empty))) == math.min(i1, i2)
  }

  // If we add an element to an empty heap and then delete the min element, the resultant heap should be empty.
  property("addDeleteIsEmpty") = forAll { i: Int =>
    isEmpty(deleteMin(insert(i, empty)))
  }

  // Convenience function to unroll a heap into a list.
  private def unroll(hp: H): List[Int] = if (isEmpty(hp)) Nil else findMin(hp) :: unroll(deleteMin(hp))

  // Elements of a heap should always be ordered in nondecreasing order: if we unroll the heap into a list and
  // sort the list, it should equal the original unrolled heap.
  property("heapIsOrdered") = forAll { h: H =>
    val lst = unroll(h)
    lst.sorted == lst
  }

  // If we meld two heaps, the minimum element of the melded heap should be the minimum of the minimum elements
  // of the two original heaps, accounting for possibly empty heaps.
  property("meldRespectsMin") = forAll { (h1: H, h2: H) =>
    val minMeldOpt = Try(findMin(meld(h1, h2))).toOption
    val minOpt = List(Try(findMin(h1)).toOption, Try(findMin(h2)).toOption).flatten.sorted.headOption
    minMeldOpt == minOpt
  }

  // For two NONEMPTY heaps only, melding them is the same as extracting the minimum from the first heap, inserting it
  // into the second heap, and then melding the two resulting heaps.
  property("meldTwoWaysGivesSameResult") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val meldH1 = meld(h1, h2)
      val meldH2 = meld(deleteMin(h1), insert(findMin(h1), h2))
      unroll(meldH1) == unroll(meldH2)
    }
  }
}
