package scala.meta.tests.parsers.dotty

import scala.meta._

class InterleavedDeclSuite extends BaseDottySuite {

  import dialects.Scala3Future

  protected override val dialect: Dialect = Scala3Future

  test("def f: Unit") {
    checkTree(templStat("def f: Unit")) {
      Decl.Def(Nil, Term.Name("f"), Nil, Type.Name("Unit"))
    }
  }

  test("def f(x: Int): Unit") {
    checkTree(templStat("def f(x: Int): Unit")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(
          Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil
        ),
        Type.Name("Unit")
      )
    }
  }

  test("def f(x: Int*): Unit") {
    checkTree(templStat("def f(x: Int*): Unit")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(
          Term.Param(Nil, Term.Name("x"), Some(Type.Repeated(Type.Name("Int"))), None) :: Nil
        ),
        Type.Name("Unit")
      )
    }
  }

  test("def f(x: => Int): Unit") {
    checkTree(templStat("def f(x: => Int): Unit")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(
          Term.Param(Nil, Term.Name("x"), Some(Type.ByName(Type.Name("Int"))), None) :: Nil
        ),
        Type.Name("Unit")
      )
    }
  }

  test("def f(implicit x: Int): Unit") {
    checkTree(templStat("def f(implicit x: Int): Unit")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(
          Term.Param(Mod.Implicit() :: Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil
        ),
        Type.Name("Unit")
      )
    }
  }

  test("def f: X") {
    checkTree(templStat("def f: X")) {
      Decl.Def(Nil, Term.Name("f"), Nil, Type.Name("X"))
    }
  }

  test("def f[T]: T") {
    checkTree(templStat("def f[T]: T")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil,
        Nil,
        Type.Name("T")
      )
    }
  }

  test("def f[A][B]: A") {
    runTestError[Stat](
      "def f[A][B]: A",
      """|error: = expected but [ found
         |def f[A][B]: A
         |        ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B]: B") {
    runTestAssert[Stat]("def f[A](a: A, as: A*)[B]: B") {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Member.ParamClauseGroup(
          Type.ParamClause(
            Type.Param(Nil, Type.Name("A"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil
          ),
          Term.ParamClause(
            List(
              Term.Param(Nil, Term.Name("a"), Some(Type.Name("A")), None),
              Term.Param(Nil, Term.Name("as"), Some(Type.Repeated(Type.Name("A"))), None)
            ),
            None
          ) :: Nil
        ) :: Member.ParamClauseGroup(
          Type.ParamClause(
            Type.Param(Nil, Type.Name("B"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil
          ),
          Nil
        ) :: Nil,
        Type.Name("B")
      )
    }
  }

  test("def f[A](implicit a: A)[B](implicit b: B): B") {
    runTestError[Stat](
      "def f[A](implicit a: A)[B](implicit b: B): B",
      """|error: = expected but [ found
         |def f[A](implicit a: A)[B](implicit b: B): B
         |                       ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B") {
    runTestAssert[Stat]("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B") {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Member.ParamClauseGroup(
          Type.ParamClause(
            Type.Param(Nil, Type.Name("A"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil
          ),
          Term.ParamClause(
            List(
              Term.Param(Nil, Term.Name("a"), Some(Type.Name("A")), None),
              Term.Param(Nil, Term.Name("as"), Some(Type.Repeated(Type.Name("A"))), None)
            ),
            None
          ) :: Nil
        ) :: Member.ParamClauseGroup(
          Type.ParamClause(
            Type.Param(Nil, Type.Name("B"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil
          ),
          Term.ParamClause(
            List(
              Term.Param(Nil, Term.Name("b"), Some(Type.Name("B")), None),
              Term.Param(Nil, Term.Name("bs"), Some(Type.Repeated(Type.Name("B"))), None)
            ),
            None
          ) :: Nil
        ) :: Member.ParamClauseGroup(
          Type.ParamClause(
            Type.Param(Nil, Type.Name("C"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil
          ),
          Term.ParamClause(
            List(Term.Param(List(Mod.Implicit()), Term.Name("c"), Some(Type.Name("C")), None)),
            Some(Mod.Implicit())
          ) :: Nil
        ) :: Nil,
        Type.Name("B")
      )
    }
  }
}
