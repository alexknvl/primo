/*-enable-unique*/
import scala.annotation.unique
class C {
  var f: C = _
  def consume(x: C @unique) {}

  def m() {
    val c: C @unique = new C
    consume(c)
  }
}