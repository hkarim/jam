package jam.example.model

trait Enumerator[A] {
  def apply(name: String): A
  def to(value: A): String
  def enumerate: Vector[A]
}
