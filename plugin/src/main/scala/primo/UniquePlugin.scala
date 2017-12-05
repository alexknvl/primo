/* Capability Type Checker for Unique References
 * Copyright 2009-2010 LAMP/EPFL
 *
 * @author Philipp Haller
 */

package primo

import scala.tools.nsc
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.{Global, Phase}

class UniquePlugin(val global: Global) extends Plugin {
  import global._

  override val name = "unique"
  override val description = "uniqueness checker"
  override val components: List[PluginComponent] = List(UniqueComp, SwapComp)

  private object PrimoComp extends PrimoComponent {
    override val global = UniquePlugin.this.global
    override val runsAfter = List("refchecks")
    override val phaseName = "primo"
  }

  private object UniqueComp extends UniqueComponent {
    override val global = UniquePlugin.this.global
    override val runsAfter = List("refchecks")
    override val phaseName = "unique"
  }

  private object SwapComp extends SwapTransform {
    override val global = UniquePlugin.this.global
    override val runsAfter = List("unique")
    override val phaseName = "swap"
  }

  override def processOptions(options: List[String], error: String => Unit) {
    if (options contains "verbose")
      UniqueComp.verbose = true
  }

  private object ImmComponent extends PluginComponent {
    override val global = UniquePlugin.this.global
    override val runsAfter = List("refchecks")
    override val phaseName = "immchecker"
    override def newPhase(prev: Phase) = new ImmPhase(prev)
  }

  private object IsoComponent extends PluginComponent {
    override val global = UniquePlugin.this.global
    //val runsAfter = "refchecks"
    //val runsAfter = "explicitouter"
    override val runsAfter = List("cleanup")
    override val phaseName = "isochecker"
    override def newPhase(prev: Phase) = new IsoPhase(prev)
  }

  var immTypes: List[Symbol] = List(definitions.NothingClass,
                                    definitions.IntClass)

  private class ImmPhase(prev: Phase) extends Phase(prev) {
    override def name = "immchecker"

    val MarkerImm = rootMirror.getClassIfDefined("scala.annotation.immutable")
    val MarkerAss = rootMirror.getClassIfDefined("scala.annotation.assignable")

    def checkImmutable(unit: CompilationUnit, cd: ClassDef) {
      // check members under assumption that class
      // and its type params are immutable
      immTypes = cd.symbol.info.typeSymbol :: (cd.symbol.typeParams ::: immTypes)

      // 1. check that all vars are marked @assignable
      val prop1 = cd.symbol.info.members forall { m =>
        if (m.hasFlag(MUTABLE) && !m.hasAnnotation(MarkerAss)) {
          unit.error(m.pos, m+" not assignable in immutable "+cd.symbol.info.typeSymbol)
          false
        } else true
      }

      // 2. check that all vals refer to immutable types
      val valdefs = cd.symbol.info.members filter (m => m.isValue && m.isGetter)
      val prop2 = valdefs forall { vd =>
        if (!immTypes.contains(vd.info.resultType.typeSymbol)) {
          unit.error(vd.pos, "type of "+vd.name+" not known to be immutable")
          false
        } else true
      }
      if (prop1 && prop2)
        println("OK: @immutable "+cd.symbol.info.typeSymbol)
    }

    def check(unit: CompilationUnit, tree: Tree) {
      tree match {
        case cd @ ClassDef(mods, name, tparams, impl) if cd.symbol.hasAnnotation(MarkerImm) =>
          checkImmutable(unit, cd)

        case ClassDef(mods, name, tparams, impl) =>
          check(unit, impl)

        case PackageDef(name, stats) =>
          // package name { stats }
          stats foreach (t => check(unit, t))

        case ModuleDef(mods, name, impl) => // (eliminated by refcheck)
          // mods object name impl  where impl = extends parents { defs }
          check(unit, impl)

        case ValDef(mods, name, tpt, rhs) =>
          // mods val name: tpt = rhs
          // note missing type information is expressed by tpt = TypeTree()
          check(unit, rhs)

        case Template(parents, self, body) =>
          // extends parents { self => body }
          // if self is missing it is represented as emptyValDef
          body foreach (t => check(unit, t))

        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
          // note missing type information is expressed by tpt = TypeTree()
          check(unit, rhs)

        case Block(stats, expr) =>
          // { stats; expr }
          stats foreach (s => check(unit, s))
          check(unit, expr)

        case If(cond, thenp, elsep) =>
          // if (cond) thenp else elsep
          check(unit, thenp)
          check(unit, elsep)

        case Assign(lhs, rhs) =>
          // lhs = rhs
          check(unit, rhs)

        case EmptyTree =>

        case other =>
      }
    }

    override def run: Unit = {
      for (unit <- currentRun.units)
        check(unit, unit.body)
    }
  }

  private class IsoPhase(prev: Phase) extends Phase(prev) {
    override def name: String = "isochecker"
//    var safeTypes: List[Symbol] = List(definitions.getClass("scala.actors.Actor"),
//                                       definitions.getClass("scala.actors.OutputChannel"))
//    val ActorMod  = definitions.getModule("scala.actors.Actor")
//    val ActorSpawn = newTermName("actor")

    def checkActorSpawn(unit: CompilationUnit, app: Apply, body: Tree) {
      //println("found application of `actor`")
      //println("body is:")
      //println(body)

      body match {
        case Block(stats, expr) =>
          //println("stats: "+stats)
          //println("expr: "+expr)
          expr match {
            case Typed(Apply(tree3, args), _) =>
              //println("tree3: "+tree3)
              //println("args: "+args)

              // args is List($outer.this, env_1, ..., env_k)
              // where env_i are from environment
              // env_i has to be safe

              args foreach {
                case arg@Ident(name) => {
                  //println(arg+" is ident "+name)
                  //println("arg.symbol: "+arg.symbol)
                  //println("arg.symbol.info: "+arg.symbol.info)
                  //println("arg.symbol.info.typeSymbol: "+arg.symbol.info.typeSymbol)

                  val tsym = arg.symbol.info.typeSymbol
                  val isImm = immTypes contains tsym
                  val isSafe = false // safeTypes contains tsym
                  //println("isImm: "+isImm)
                  //println("isSafe: "+isSafe)
                  if (!isImm && !isSafe) {
                    unit.error(arg.pos, "type of " + name + " not known to be immutable or safe")
                    false
                  } else true
                }
                case arg@This(qual) =>
                  //println(arg+" is qualified this")

                  val tsym = arg.symbol.info.typeSymbol
                  ////println("arg.symbol.info.typeSymbol: "+tsym)
                  ////println("is anon fun: "+tsym.isAnonymousFunction)

                  val isImm = immTypes contains tsym
                  val isSafe = false // safeTypes contains tsym
                  ////println("isImm: "+isImm)
                  ////println("isSafe: "+isSafe)
                  if (!isImm && !isSafe && !tsym.isAnonymousFunction) {
                    unit.error(arg.pos, "type of " + arg + " not known to be immutable or safe")
                    false
                  } else true
                case other =>
                  ////println(arg+" is other "+other.getClass)
                  false
              }
              //if (localized)
                //println("OK: body of actor at "+app.pos.dbgString+" is localized")
          }

        case other =>
          ////println(other.getClass)
      }
    }

    def check(unit: CompilationUnit, tree: Tree) {
      tree match {
        case ClassDef(mods, name, tparams, impl) =>
          check(unit, impl)

//        case app @ Apply(Select(rcvr, ActorSpawn), List(body)) =>
//          checkActorSpawn(unit, app, body)

        case PackageDef(name, stats) =>
          // package name { stats }
          stats foreach (t => check(unit, t))

        case ModuleDef(mods, name, impl) => // (eliminated by refcheck)
          // mods object name impl  where impl = extends parents { defs }
          check(unit, impl)

        case ValDef(mods, name, tpt, rhs) =>
          // mods val name: tpt = rhs
          // note missing type information is expressed by tpt = TypeTree()
          check(unit, rhs)

        case Template(parents, self, body) =>
          // extends parents { self => body }
          // if self is missing it is represented as emptyValDef
          body foreach (t => check(unit, t))

        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
          // note missing type information is expressed by tpt = TypeTree()
          check(unit, rhs)

        case Block(stats, expr) =>
          // { stats; expr }
          stats foreach (s => check(unit, s))
          check(unit, expr)

        case If(cond, thenp, elsep) =>
          // if (cond) thenp else elsep
          check(unit, thenp)
          check(unit, elsep)

        case Assign(lhs, rhs) =>
          // lhs = rhs
          check(unit, rhs)

        case EmptyTree =>

        case other =>
/*
  case TypeDef(mods, name, tparams, rhs) =>                       (eliminated by erasure)
     // mods type name[tparams] = rhs
     // mods type name[tparams] >: lo <: hi,  where lo, hi are in a TypeBoundsTree,
                                              and DEFERRED is set in mods
  case LabelDef(name, params, rhs) =>
     // used for tailcalls and like
     // while/do are desugared to label defs as follows:
     // while (cond) body ==> LabelDef($L, List(), if (cond) { body; L$() } else ())
     // do body while (cond) ==> LabelDef($L, List(), body; if (cond) L$() else ())
  case Import(expr, selectors) =>                                 (eliminated by typecheck)
     // import expr.{selectors}
     // Selectors are a list of pairs of names (from, to).
     // The last (and maybe only name) may be a nme.WILDCARD
     // for instance
     //   import qual.{x, y => z, _}  would be represented as
     //   Import(qual, List(("x", "x"), ("y", "z"), (WILDCARD, null)))
  case Annotation(constr, elements) =>
     // @constr(elements) where constr = tp(args),   (an instance of Apply)
     //                         elements = { val x1 = c1, ..., val xn = cn }
  case DocDef(comment, definition) =>                             (eliminated by typecheck)
     // /** comment */ definition
  case CaseDef(pat, guard, body) =>                               (eliminated by transmatch/explicitouter)
    // case pat if guard => body
  case Sequence(trees) =>                                         (eliminated by transmatch/explicitouter)
    // pat1, ..., pat_n
  case Alternative(trees) =>                                      (eliminated by transmatch/explicitouter)
    // pat1 | ... | patn
  case Star(elem) =>                                              (eliminated by transmatch/explicitouter)
    // pat*
  case Bind(name, body) =>                                        (eliminated by transmatch/explicitouter)
    // name @ pat
  case UnApply(fun: Tree, args)                                   (introduced by typer, eliminated by transmatch/explicitouter)
    // used for unapply's
  case ArrayValue(elemtpt, trees) =>                              (introduced by uncurry)
    // used to pass arguments to vararg arguments
    // for instance, printf("%s%d", foo, 42) is translated to after uncurry to:
    // Apply(
    //   Ident("printf"),
    //   Literal("%s%d"),
    //   ArrayValue(<Any>, List(Ident("foo"), Literal(42))))
  case Function(vparams, body) =>                                 (eliminated by lambdaLift)
    // vparams => body  where vparams:List[ValDef]
  case Match(selector, cases) =>
    // selector match { cases }
  case Return(expr) =>
    // return expr
  case Try(block, catches, finalizer) =>
    // try block catch { catches } finally finalizer where catches: List[CaseDef]
  case Throw(expr) =>
    // throw expr
  case New(tpt) =>
    // new tpt   always in the context: (new tpt).<init>[targs](args)
  case Typed(expr, tpt) =>                                        (eliminated by erasure)
    // expr: tpt
  case TypeApply(fun, args) =>
    // fun[args]
  case Apply(fun, args) =>
    // fun(args)
    // for instance fun[targs](args)  is expressed as  Apply(TypeApply(fun, targs), args)
  case ApplyDynamic(qual, args)                                   (introduced by erasure, eliminated by cleanup)
    // fun(args)
  case Super(qual, mix) =>
    // qual.super[mix]     if qual and/or mix is empty, ther are nme.EMPTY.toTypeName
  case This(qual) =>
    // qual.this
  case Select(qualifier, selector) =>
    // qualifier.selector
  case Ident(name) =>
    // name
    // note: type checker converts idents that refer to enclosing fields or methods
    // to selects; name ==> this.name
  case Literal(value) =>
    // value
  case TypeTree() =>                                              (introduced by refcheck)
    // a type that's not written out, but given in the tpe attribute
  case Annotated(annot, arg) =>                                   (eliminated by typer)
    // arg @annot  for types,  arg: @annot for exprs
  case SingletonTypeTree(ref) =>                                  (eliminated by uncurry)
    // ref.type
  case SelectFromTypeTree(qualifier, selector) =>                 (eliminated by uncurry)
    // qualifier # selector, a path-dependent type p.T is expressed as p.type # T
  case CompoundTypeTree(templ: Template) =>                       (eliminated by uncurry)
    // parent1 with ... with parentN { refinement }
  case AppliedTypeTree(tpt, args) =>                              (eliminated by uncurry)
    // tpt[args]
  case TypeBoundsTree(lo, hi) =>                                  (eliminated by uncurry)
    // >: lo <: hi
  case ExistentialTypeTree(tpt, whereClauses) =>                  (eliminated by uncurry)
    // tpt forSome { whereClauses }
*/
      }
    }

    override def run: Unit = {
      /*for (unit <- currentRun.units;
           tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
           if rcvr.tpe <:< definitions.IntClass.tpe)
      {
        unit.error(tree.pos, "definitely division by zero")
      }*/

      for (unit <- currentRun.units)
        check(unit, unit.body)
    }
  }

}
