//> using scala 3.5.2
//> using dep "org.typelevel::cats-core:2.12.0"

// Zod / Valibot を自作する

// do parse and validate data
def parse0(s: String): Int = s.toInt
parse0("42")

// パースに失敗することもあるので失敗可能にする
case class ParseFailed(s: String)
def parse1(s: String): Either[ParseFailed, Int] =
  s.toIntOption.toRight(ParseFailed(s))
parse1("no")

// ある構造を受理するスキーマとして定義する
case class SchemaFailed[A](attempted: A)
trait Schema1[A, B] {
  def run(in: A): Either[SchemaFailed[A], B]
}

val PosInt = new Schema1[Int, Int] {
  def run(n: Int) = if n > 0 then Right(n) else Left(SchemaFailed(n))
}

PosInt.run(42)
PosInt.run(-42)

// ところで、Schemaはmapできそう:
// PosInt.map(_ * 2) // => runするとバリデーションされ、数字も2倍

// Schema1を"カリー化"する(今回は出力にしか興味がない)
trait Schema2[B] {
  def run[A](in: A): Either[SchemaFailed[A], B]
}

// こうするとSchema2はF[_]の形になるので色々と都合が良い
given cats.Functor[Schema2] with {
  def map[B, C](fa: Schema2[B])(f: B => C): Schema2[C] = new Schema2[C] {
    def run[A](in: A) = fa.run(in).map(f)
  }
}

val PosInt2: Schema2[Int] = new Schema2[Int]:
  def run[A](a: A) = a match
    case n: Int => if n > 0 then Right(n) else Left(SchemaFailed(n))
    case x      => Left(SchemaFailed(x))

PosInt2.run(42)
PosInt2.run(-42)
PosInt2.run("666")

// Functorのメソッドを呼び出せた
summon[cats.Functor[Schema2]]
  .map[Int, String](PosInt2)(n => s"The value is: $n")
  .run(42)

// これだけだと使いにくいのでextension methodを定義する
extension [B](s: Schema2[B])(using f: cats.Functor[Schema2]) {
  def map = f.map(s)
}

// めっちゃ便利になった
PosInt2.map(_ * 2).run(42)
PosInt2.map(_ * 2).run("666")

// ところで、出力だけでなく入力を変形することもできるはず。
// IntではなくDoubleを受け付け、切り捨ててIntとして読み取るスキーマを考えられる。
// これはContravariant Functorとして定式化されている。
// 今回は入力側に着目したいのでさらに小細工する

// 着目順序を入れ替えられるようにする
trait Schema4:
  type In
  type Out
  def run(in: In): Either[SchemaFailed[In], Out]

type ZoomIn = [I] =>> Schema4 {
  type In = I
}
type ZoomOut = [O] =>> Schema4 {
  type Out = O
}

given cats.Contravariant[ZoomIn] with {
  def contramap[A, AA](fa: ZoomIn[A])(f: AA => A): ZoomIn[AA] = new Schema4 {
    type In = AA
    type Out = fa.Out
    def run(in: In): Either[SchemaFailed[In], Out] =
      fa.run(f(in)).left.map(_ => SchemaFailed(in))
  }
}

val PosInt4 = new Schema4:
  type In = Int
  type Out = Int
  def run(n: In): Either[SchemaFailed[In], Out] =
    if n > 0 then Right(n) else Left(SchemaFailed(n))

summon[cats.Contravariant[ZoomIn]]
  .contramap(PosInt4)((d: Double) => d.toInt)
  .run(42.195)
summon[cats.Contravariant[ZoomIn]]
  .contramap(PosInt4)((d: Double) => d.toInt)
  .run(-42.195)

// map側も同様だが、mapとcontramapを同時に実装できる構造にはProfunctorという名前がついており、catsにもある
// けっきょくSchema1の形に戻ってくる
given cats.arrow.Profunctor[Schema1] with {
  def dimap[A, B, C, D](
      fab: Schema1[A, B]
  )(f: C => A)(g: B => D): Schema1[C, D] = new Schema1 {
    def run(in: C): Either[SchemaFailed[C], D] =
      fab.run(f(in)).map(g).left.map(_ => SchemaFailed(in))
  }
}

// さて、実はextension methodは勝手にcatsが導出してくれるので何もする必要はないのだ
import cats.syntax.all.{*, given}

// Profunctorでは、contramapはlmap、mapはrmapという名前になっている
PosInt.lmap((n: Int) => -n).run(-42)
val s = PosInt.rmap(_ * 2).run(42)

// この時点でも色々なことができる。具体的な型が分からなくても色々できる
def Positive[N](using num: Numeric[N]) = new Schema1[N, N]:
  def run(in: N): Either[SchemaFailed[N], N] =
    if num.gt(num.sign(in), num.zero) then Right(in) else Left(SchemaFailed(in))

Positive[Int].lmap((n: Int) => n + 1).run(42)
Positive[Double].run(-42.195)
Positive[Short].run(120.toShort)

// せっかくなのでフィルターのように書けるようにしたい。
// つまり IsInt |> Positive |> PrettyPrint のように書けたい。
// しかし今のところlmapとrmapがあるだけで、これは関数を直接受け取る。
// 今回はSchema1同士の合成なので直接は合成できない。
// そこで、composeが実装できる型クラスであるArrowを利用する
given cats.arrow.Arrow[Schema1] with {
  def compose[A, B, C](f: Schema1[B, C], g: Schema1[A, B]): Schema1[A, C] = new Schema1[A, C] {
    def run(in: A): Either[SchemaFailed[A], C] = g.run(in).flatMap(f.run).left.map(_ => SchemaFailed(in))
  }
  def first[A, B, C](fa: Schema1[A, B]): Schema1[(A, C), (B, C)] =
    new Schema1[(A, C), (B, C)] {
      def run(in: (A, C)): Either[SchemaFailed[(A, C)], (B, C)] =
        fa.run(in._1).map(_ -> in._2).left.map(_ => SchemaFailed(in))
    }
  def lift[A, B](f: A => B): Schema1[A, B] = new Schema1[A, B] {
    def run(in: A): Either[SchemaFailed[A], B] = Right(f(in))
  }
}

val IsInt = new Schema1[Any, Int] {
  def run(in: Any): Either[SchemaFailed[Any], Int] = in match
    case n: Int => Right(n)
    case _: Any => Left(SchemaFailed(in))
}

val PrettyPrint = new Schema1[Int, String] {
  def run(in: Int): Either[SchemaFailed[Int], String] = Right(s"The value is: $in")
}

IsInt.andThen(Positive).andThen(PrettyPrint).run(42)

// あとはextension methodでちょっとした構文糖を用意してあげよう
extension [A, B](s: Schema1[A, B]) {
  def |>[C](ss: Schema1[B, C]): Schema1[A, C] = s.andThen(ss)
}

val PrintPositiveInt = IsInt |> Positive |> PrettyPrint

PrintPositiveInt.run(42)
PrintPositiveInt.run(-42)
PrintPositiveInt.run("666")

// どちらかにマッチすればいい、ということもある。
// truthyな値を受け入れるスキーマを作ろう
val TruthyString = new Schema1[String, Boolean] {
  def run(in: String): Either[SchemaFailed[String], Boolean] = in match
    case "" => Right(false)
    case _ => Right(true) 
}

def TruthyNumber[N](using num: Numeric[N]) = new Schema1[N, Boolean] {
  def run(in: N): Either[SchemaFailed[N], Boolean] = in match
    case n if num.equiv(n, num.zero) => Right(false)
    case _ => Right(true)
}

def TruthyBoolean = new Schema1[Boolean, Boolean] {
  def run(in: Boolean): Either[SchemaFailed[Boolean], Boolean] = Right(in)
}

TruthyString.run("true")
TruthyNumber[Int].run(42)
TruthyNumber[Double].run(0.0)

// TruthyString | TruthyNumber | TruthyBoolean と書けるようにする
