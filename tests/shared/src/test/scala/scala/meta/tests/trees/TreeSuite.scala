package scala.meta.tests
package trees

import munit._
import scala.meta._

class TreeSuite extends FunSuite {
  test("Name.unapply") {
    assert(Name.unapply(Term.Name("a")).contains("a"))
    assert(Name.unapply(Type.Name("a")).contains("a"))
  }
}
