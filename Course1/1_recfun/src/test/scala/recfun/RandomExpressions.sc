import java.util.Random

val rand = new Random()
rand.nextInt()

trait Generator[+T] { self =>
  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate = f(self.generate).generate
  }
}

implicit val integers = new Generator[Int] {
  val rand = new Random()
  override def generate = rand.nextInt()
}

val booleans = integers.map(_ > 0)
booleans.generate

def pairs[T, U](implicit t: Generator[T], u: Generator[U]) = for {
  x <- t
  y <- u
} yield (x,y)

val intPairs = pairs[Int, Int]
intPairs.generate

def single[T](x: T): Generator[T] = new Generator[T] {
  override def generate = x
}

val ones = single(1)
ones.generate

def choose(low: Int, high: Int): Generator[Int] =
  for (x <- integers) yield low + math.abs(x) % (high - low)

val oneToFive = choose(1, 5)
(1 to 10).map(_ => oneToFive.generate)

def oneOf[T](xs: T*): Generator[T] = for (idx <- choose(0, xs.length)) yield xs(idx)

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color
val colors = oneOf(Red, Green, Blue)
(1 to 10).map(_ => colors.generate)

def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)
def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

// A generator to generate random Tree objects
trait Tree[+T]
case class Inner[+T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[+T](x: T) extends Tree[T]

def trees[T](implicit generator: Generator[T]): Generator[Tree[T]] = for {
  isLeaf <- booleans
  node <- if (isLeaf) leaves(generator) else inners(generator)
} yield node
def leaves[T](generator: Generator[T]) = for {
  x <- generator
} yield Leaf(x)
def inners[T](generator: Generator[T]) = for {
  x <- trees(generator)
  y <- trees(generator)
} yield Inner(x, y)

trees[Int].generate

// Now we can do random testing:
def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
  for (i <- 0 until numTimes) {
    val value = g.generate
    assert(test(value), s"test failed for $value")
  }
  println(s"passed $numTimes tests")
}

test(integers)(x => math.abs(x) > 0)

test(pairs(lists, lists)) {
  case (xs, ys) => (xs ++ ys).length > xs.length
}
