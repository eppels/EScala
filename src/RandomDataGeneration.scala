/**
 * Created by Max on 4/10/2017.
 */
object SimpleSentences extends App {
  val subjects = Seq("Noel", "The cat", "The dog")
  val verbs = Seq("wrote", "chased", "slept on")
  val objects = Seq("the book", "the ball", "the bed")

  //all possible s/v/o combinations
  val allCombos = for {
    s <- subjects
    v <- verbs
    o <- objects
  } yield s"$s $v $o"

  allCombos.foreach(println)
}

object SensicalSentences extends App {
  val subjects = Seq("Noel", "The cat", "The dog")
  val possibleVerbs = Map(
    "Noel" -> Seq("wrote", "chased", "slept on"),
    "The cat" -> Seq("meowed at", "chased", "slept on"),
    "The dog" -> Seq("barked at", "chased", "slept on")
  )
  val possibleObjects = Map(
    "wrote" -> Seq("the book", "the letter", "the code"),
    "chased" -> Seq("the ball", "the dog", "the cat"),
    "slept on" -> Seq("the bed", "the mat", "the train"),
    "meowed at" -> Seq("Noel", "the door", "the food cupboard"),
    "barked at" -> Seq("the postman", "the car", "the cat")
  )

  val allCombos = for {
    s <- subjects
    v <- possibleVerbs(s)
    o <- possibleObjects(v)
  } yield s"$s $v $o"

  allCombos.foreach(println)
}

object Distribution {
  def uniform[T](elements: Seq[T]) = Distribution(elements.map(e => (e, 1.0 / elements.size)))
}
//I don't understand how this data model accounts for dependent probabilities well, need help
case class Distribution[T](possibilities: Seq[(T, Double)]) {
  def prob(el : T) = possibilities.filter(_._1 == el).map(_._2).sum

  def normalize = {
    val totalWeight = (possibilities map {case (a, p) =>p}).sum
    Distribution(possibilities map {case (a, p) => a -> (p / totalWeight)})
  }

  def compact = {
    val distinct = (possibilities map {case (a, p) => a}).distinct
    Distribution(distinct map {a => a-> prob(a)})
  }

}

object DistributionLogic extends App {
  //wasn't sure how to interpret the questions about adding methods to implement the models, this is what I came up with...
  val simpleDistribution = Distribution.uniform(SimpleSentences.allCombos)
  val sensicalDistribution = Distribution(SimpleSentences.allCombos.map(s => if (SensicalSentences.allCombos.contains(s)) (s, 1.0 / SensicalSentences.allCombos.size) else (s, 0)))

  //trying to skp to the complex problem...
  val cookGoodSmell = ("Good smell", .3)
  val cookNoSmell = ("No smell", .7)
  val catSmellsAndHarasses = ("harass", .8)
  val catSmellsNoHarassment = ("no harassment", .2)
  val catNoSmellAndHarasses = ("harass", .4)
  val catNoSmellNoHarassment = ("no harassment", .6)

}

