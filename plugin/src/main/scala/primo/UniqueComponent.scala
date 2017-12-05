/* Capability Type Checker for Uniqueness
 * Copyright 2009-2010 LAMP/EPFL
 *
 * @author Philipp Haller
 */

package primo

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.{Global, Phase}

abstract class UniqueComponent extends PluginComponent with Debug {

  val global: Global

  import global._

  def newPhase(prev: Phase) = new UniquePhase(prev)

  class UniquePhase(prev: Phase) extends Phase(prev) {
    def name = "unique"

    type Capability = Int
    //TODO: get rid of Boolean component
    type Guard = (Capability, Boolean)

    /* Method capability types.
     */
    case class MethodCapType(receiver: Option[Guard],
                             params: List[Option[Guard]],
                             result: Option[Guard],
                             provided: List[Capability]) {

      def requiredCaps: List[Int] = {
        val pcs = params flatMap { p =>
          if (p.isEmpty) List()
          else List(p.get._1)
        }
        if (receiver.isEmpty) pcs
        else if (pcs contains receiver.get._1) pcs
        else receiver.get._1 :: pcs
      }

      def isResultUnique: Boolean = result match {
        case None => false
        case Some((resCap, _)) => !requiredCaps.contains(resCap)
      }
    }

    var cap = 0

    // first non-shared capability given out is 1.
    // this means we can make 0 the shared capability.
    def freshCap: Int = {
      cap = cap + 1; cap
    }

    val gTree = collection.mutable.Map.empty[Tree, Guard]
    var gSym = collection.mutable.Map.empty[Symbol, Guard]
    val gThis = collection.mutable.Map.empty[Symbol, Guard]

    // for a nested template, collects the symbols that are captured
    val capturedSyms = collection.mutable.Map.empty[Symbol, List[Symbol]]

    // maps method and function symbols to their capability types
    var mtype = collection.immutable.Map.empty[Symbol, MethodCapType]

    // all class symbols that are annotated
    // class symbols that can carry caps other than shared
    val ctype = collection.mutable.Set.empty[Symbol]

    // symbols of transient classes
    // class symbols that can carry caps other than shared
    val ltype = collection.mutable.Set.empty[Symbol]

    val uniqAnnot: ClassSymbol = rootMirror.getRequiredClass("scala.annotation.unique")
    val transAnnot: ClassSymbol = rootMirror.getRequiredClass("scala.annotation.transient")
    val peerAnnot: ClassSymbol = rootMirror.getRequiredClass("scala.annotation.peer")
    val uncheckedUniqAnnot: ClassSymbol = rootMirror.getRequiredClass("scala.annotation.uncheckedUnique")

    val jlStringClass: ClassSymbol = rootMirror.getRequiredClass("java.lang.String")

    val uniqueMod = rootMirror.getRequiredModule("scala.annotation.UniqueOps")
    val captureFun = definitions.getMember(uniqueMod, "capture": nme.NameType)
    val mkCapturedFun = definitions.getMember(uniqueMod, "mkCaptured": nme.NameType)
    val capturedTrait: ClassSymbol = rootMirror.getRequiredClass("scala.annotation.Captured")
    val capturedByFun = definitions.getMember(capturedTrait, "capturedBy": nme.NameType)
    val swapFun = definitions.getMember(uniqueMod, "swap": nme.NameType)
    val shareFun = definitions.getMember(uniqueMod, "share": nme.NameType)
    val share2Fun = definitions.getMember(uniqueMod, "share2": nme.NameType)
    val funClass = definitions.FunctionClass

    val transientMethods = List(
      //      methSym("scala.collection.Seq", "indexOf"),
      //                                methSym("scala.collection.Iterator", "foreach"),
      //                                methSym("scala.collection.Iterator", "map"),
      //                                methSym("scala.collection.Iterator", "next")
    )

    //    val actorTrait = definitions.getClass("scala.actors.Actor")
    //    val actorReactMethod = definitions.getMember(actorTrait, "react" : nme.NameType)
    //    val actorReactWithinMethod = definitions.getMember(actorTrait, "reactWithin" : nme.NameType)

    var immutableSyms = collection.immutable.Set.empty[Symbol]

    //    def initMethodCapTypes() {
    ////      val sendMethod =         definitions.getMember(actorTrait, nme)
    ////      val outputChannelTrait = definitions.getClass("scala.actors.OutputChannel")
    ////      val outputChannelSend =  definitions.getMember(outputChannelTrait, nme.BANG)
    ////      val sendMethodCapType =
    ////        MethodCapType(None, List(Some((freshCap, true))), None, List())
    ////      mtype = mtype + (sendMethod -> sendMethodCapType)
    ////      mtype = mtype + (outputChannelSend -> sendMethodCapType)
    //    }

    def initImmutableSyms() {
      immutableSyms = immutableSyms ++
        List(
          // top types
          definitions.AnyClass,
          definitions.AnyValClass,
          definitions.AnyRefClass,
          definitions.ObjectClass,
          definitions.NothingClass,
          // the scala value classes
          definitions.UnitClass,
          definitions.ByteClass,
          definitions.ShortClass,
          definitions.CharClass,
          definitions.IntClass,
          definitions.LongClass,
          definitions.FloatClass,
          definitions.DoubleClass,
          definitions.BooleanClass,
          // some special scala classes
          definitions.StringClass,
          definitions.ProductRootClass,
          definitions.OptionClass,
          definitions.TupleClass(2),
          definitions.TupleClass(3),
          definitions.TupleClass(4),
          definitions.TupleClass(5),
          // some scala collection classes
          rootMirror.getRequiredModule("scala.collection.immutable.Nil"),
          rootMirror.getRequiredClass("scala.collection.immutable.List"),
          rootMirror.getRequiredModule("scala.collection.immutable.List"),
          rootMirror.getRequiredClass("scala.Array"),
          rootMirror.getRequiredClass("scala.PartialFunction"),
          // some java classes
          rootMirror.getRequiredClass("java.lang.Comparable"),
          rootMirror.getRequiredClass("java.io.Serializable"),
          rootMirror.getRequiredClass("java.io.File"),
          rootMirror.getRequiredClass("java.io.PrintWriter"),
          rootMirror.getRequiredClass("java.io.StringWriter")
        )
    }

    def initTransientSyms() {
      ltype ++= List(
        definitions.ArrayClass,
        definitions.RepeatedParamClass,
        definitions.ByNameParamClass,
        //TODO: improve multi-arity handling
        rootMirror.getRequiredClass("scala.Function0"),
        rootMirror.getRequiredClass("scala.Function1"),
        rootMirror.getRequiredClass("scala.Function2"),
        rootMirror.getRequiredClass("scala.collection.Seq"),
        rootMirror.getRequiredClass("scala.collection.Iterable"),
        rootMirror.getRequiredClass("scala.collection.Iterator"),
        rootMirror.getRequiredClass("scala.collection.Set"),
        rootMirror.getRequiredClass("scala.collection.mutable.ArrayBuffer"))
    }

    //    initMethodCapTypes()
    initImmutableSyms()
    initTransientSyms()

    // expects typeSymbols
    def isTransient(sym: Symbol): Boolean = {
      hasTransientAnnot(sym) ||
        (immutableSyms contains sym) || (ltype contains sym)
    }

    def isTransient(tpe: Type, typeVarsLocal: Boolean, verbose: Boolean): Boolean = isTransient(tpe) || (tpe match {
      case TypeRef(pre, sym, args) =>
        val argsLocal = args.forall(a => isTransient(a, typeVarsLocal, verbose))
        val symLocal = isTransient(sym.info.typeSymbol)
        if (verbose) {
          vinfo("args local? " + argsLocal)
          vinfoClass("sym.info", sym.info)
          vinfoClass("sym.info.typeSymbol", sym.info.typeSymbol)
          vinfo("typeSymbol is NoSymbol? " + (sym.info.typeSymbol == NoSymbol))
          vinfo("sym local? " + symLocal)
        }
        (symLocal && argsLocal) ||
          (sym.info.typeSymbol == NoSymbol)

      case _ => false
    })

    def isTransient(tpe: Type): Boolean = isImmutable(tpe) || (isTransient(tpe.typeSymbol) &&
      (tpe match {
        case TypeRef(pre, sym, args) =>
          val argsLocal = args.forall(a => isTransient(a))
          val symLocal = isTransient(sym.info.typeSymbol)
          symLocal && argsLocal
        case _ => false
      }))

    // expects typeSymbols
    def isImmutable(sym: Symbol): Boolean = {
      if (sym != null)
        definitions.isPrimitiveValueClass(sym) || (immutableSyms contains sym)
      else
        true
    }

    def isImmutable(tpe: Type): Boolean = isImmutable(tpe.typeSymbol) && {
      tpe match {
        case TypeRef(pre, sym, args) =>
          //vinfo("found TypeRef "+tpe)
          val argsImmut = args.forall(a => isImmutable(a))
          //vinfo("args immut? "+argsImmut)
          //vinfo("sym.info.typeSym "+sym.info.typeSymbol)
          //vinfo("sym immut? "+isImmutable(sym.info.typeSymbol))
          isImmutable(sym.info.typeSymbol) && argsImmut
        case _ => true
      }
    }

    def methSym(clazz: String, method: String): Symbol =
      definitions.getMember(rootMirror.getRequiredClass(clazz), method: nme.NameType)

    def mkGuard(cap: Capability): Guard =
      (cap, false)

    def addTransientMType(sym: Symbol): MethodCapType = {
      var commonCap1: Option[Int] = None

      def commonCap: Int = {
        if (commonCap1.isEmpty)
          commonCap1 = Some(freshCap)
        commonCap1.get
      }

      // build mtype where everything is transient
      val paramGuards = if (sym.info.paramTypes.nonEmpty) {
        sym.info.paramTypes map { pt =>
          // do not mark as transient if value type
          if (!definitions.isPrimitiveValueClass(pt.typeSymbol))
            Some(mkGuard(commonCap))
          else
            None
        }
      } else
        List()

      val methCT = MethodCapType(
        Some(mkGuard(commonCap)),
        paramGuards,
        if (definitions.isPrimitiveValueClass(sym.info.resultType.typeSymbol)) None else Some(mkGuard(commonCap)),
        List(commonCap))

      mtype = mtype + (sym -> methCT)
      methCT
    }

    /* Checks whether symbol `sym` carries `@transient` annotation.
     */
    def hasTransientAnnot(sym: Symbol): Boolean = {
      (sym.info match {
        case AnnotatedType(attrs, _) =>
          attrs exists {
            _.atp == transAnnot.tpe
          }
        case _ =>
          false
      }) || (sym.annotations exists {
        _.atp == transAnnot.tpe
      })
    }

    /* Checks whether symbol `sym` carries `@unique` annotation.
     */
    def hasUniqueAnnot(sym: Symbol): Boolean = {
      (sym.info match {
        case AnnotatedType(attrs, _) =>
          attrs exists { _.atp == uniqAnnot.tpe }
        case _ => false
      }) || (sym.annotations exists { _.atp == uniqAnnot.tpe })
    }

    def annotatedWith(tpe: Type, annotTpe: Type): Boolean = tpe match {
      case AnnotatedType(attrs, _) =>
        attrs exists { _.atp == annotTpe }
      case _ => false
    }

    /* Checks whether the method symbol `sym` is a (generated)
     * case class method or the `toString` method.
     */
    def isCaseClassMethod(sym: Symbol): Boolean =
      sym.hasFlag(CASE) ||
        List("copy", "toString").contains(sym.name.toString)

    // is method `symbol` annotated?
    def isAnnotated(symbol: Symbol): Boolean =
      (symbol.annotations exists { annot =>
        annot.atp == transAnnot.tpe ||
          annot.atp == uniqAnnot.tpe
      }) || {
        val (paramSyms, resultType) = symbol.info match {
          case MethodType(pt, rt) => (pt, rt)
          case PolyType(tparams, MethodType(pt, rt)) => (pt, rt)
          case PolyType(tparams, rt) => (List(), rt)
        }
        (paramSyms exists { psym =>
          annotatedWith(psym.info, uniqAnnot.tpe) || annotatedWith(psym.info, transAnnot.tpe) ||
            annotatedWith(psym.info, peerAnnot.tpe)
        }) ||
          annotatedWith(resultType, uniqAnnot.tpe) ||
          annotatedWith(resultType, peerAnnot.tpe)
      }

    // expects an annotated method symbol
    // unit, tree for error reporting
    def buildMethodType(unit: CompilationUnit, tree: Tree, symbol: Symbol): MethodCapType = {
      val (paramSyms, resultType) = symbol.info match {
        case MethodType(pt, rt) => (pt, rt)
        case PolyType(tparams, MethodType(pt, rt)) => (pt, rt)
        case PolyType(tparams, rt) => (List(), rt)
      }

      // the capabilities of the parameter symbols
      var paramCap: Map[Symbol, Int] =
        if (symbol.annotations exists { annot =>
          annot.atp == transAnnot.tpe ||
            annot.atp == uniqAnnot.tpe
        })
          Map(symbol.owner -> freshCap)
        else Map.empty

      def peerCapability(tpe: Type): Option[Int] = tpe match {
        case AnnotatedType(attrs, _) =>
          val aInfo = attrs find {
            _.atp == peerAnnot.tpe
          }
          if (aInfo.isDefined) {
            if (aInfo.get.args.size > 1) {
              verror(unit, tree, "only one peer allowed")
              None
            } else {
              val arg = aInfo.get.args.head
              vinfoClass("peer arg", arg)
              vinfo(paramCap isDefinedAt arg.symbol)
              val peerCap = paramCap.get(arg.symbol)
              if (peerCap.isEmpty) {
                verror(unit, tree, arg + " is not unique")
                None
              } else {
                val cap = peerCap.get
                vinfo("cap of " + arg + " is " + cap)
                Some(cap)
              }
            }
          } else None

        case _ => None
      }

      if (paramSyms.nonEmpty) {
        paramSyms foreach { psym =>
          if (annotatedWith(psym.info, uniqAnnot.tpe) || annotatedWith(psym.info, transAnnot.tpe))
            paramCap = paramCap + (psym -> freshCap)
          else if (annotatedWith(psym.info, peerAnnot.tpe))
            peerCapability(psym.info) match {
              case Some(cap) =>
                paramCap = paramCap + (psym -> cap)
              case None => // do nothing
            }
        }
      }

      val receiverGuard = paramCap.get(symbol.owner) match {
        case Some(cap) =>
          vinfo("recv of " + symbol + " has cap " + cap)
          Some(mkGuard(cap))
        case None =>
          None
      }

      var providedCaps: List[Capability] = List()

      val paramGuards: List[Option[Guard]] = if (paramSyms.nonEmpty) {
        paramSyms map { psym =>
          if (annotatedWith(psym.info, uniqAnnot.tpe))
            Some((freshCap, false))
          else if (annotatedWith(psym.info, transAnnot.tpe)) {
            val cap = freshCap
            providedCaps = cap :: providedCaps
            Some((cap, false))
          } else if (annotatedWith(psym.info, peerAnnot.tpe)) {
            peerCapability(psym.info) match {
              case Some(cap) =>
                Some((cap, false))
              case None =>
                None
            }
          } else None // parameter not annotated
        }
      } else
        List()

      providedCaps = providedCaps ::: (if (symbol.annotations exists {
        _.atp == transAnnot.tpe
      })
        List(receiverGuard.get._1)
      else
        List())

      val resultGuard: Option[Guard] = if (annotatedWith(resultType, uniqAnnot.tpe)) {
        vinfo("result type is unique")
        val resultCap = freshCap
        providedCaps = resultCap :: providedCaps
        Some(mkGuard(resultCap))
      } else if (annotatedWith(resultType, peerAnnot.tpe)) {
        peerCapability(resultType) match {
          case Some(resultCap) =>
            vinfo("result type is unique")
            Some(mkGuard(resultCap))
          case None =>
            verror(unit, tree, "no peer found")
            None
        }
      } else if (annotatedWith(resultType, transAnnot.tpe)) {
        verror(unit, tree, "result type cannot be transient")
        None
      } else
        None

      providedCaps = providedCaps.distinct

      MethodCapType(
        receiverGuard,
        paramGuards,
        resultGuard,
        providedCaps)
    }

    // expects MethodSymbols
    def getMethodType(unit: CompilationUnit, tree: Tree, symbol: Symbol): Option[MethodCapType] = {
      // 1. try to look up method type in mtype map
      val meth = mtype.get(symbol)
      if (meth.isDefined)
        meth
      // 2. check whether owner is transient
      else if (!symbol.isLocal && isTransient(symbol.owner.info)) {
        Some(addTransientMType(symbol))
        // 3. if method is annotated, build a method type based on symbol info
      } else if (!isAnnotated(symbol))
        None
      else {
        val methCT = buildMethodType(unit, tree, symbol)
        mtype = mtype + (symbol -> methCT)
        ctype += symbol.owner
        Some(methCT)
      }
    }

    /* Collects method capability types for members of transient classes.
     */
    class TransientMethodTypeCollector extends Traverser {
      var unit: CompilationUnit = _

      override def traverse(tree: Tree): Unit = tree match {

        case cdef@ClassDef(mods, name, tparams, impl) =>
          vprintln("class definition " + name)
          vprintln("gThis " + gThis)

          vinfo("TransientMethodTypeCollector visits " + cdef.symbol)
          if (hasTransientAnnot(cdef.symbol)) {
            // check that extended types are transient or immutable
            val classInfo = cdef.symbol.info
            if (!(classInfo.parents forall (t => {
              val loc = isTransient(t, typeVarsLocal = true, verbose = false) || isImmutable(t)
              if (!loc)
                vinfo("not local: " + t)
              loc
            }))) {
              vinfo("ltype: " + ltype)
              verror(unit, tree, "extended type must be local or immutable")
            } else
              traverse(impl)
          } else
            super.traverse(impl)

        case dd@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
          // note missing type information is expressed by tpt = TypeTree()

          val verbose = false

          if (ltype.contains(dd.symbol.owner)) { // owner of method is transient class
            var commonCap1: Option[Int] = None

            def commonCap: Int = {
              if (commonCap1.isEmpty)
                commonCap1 = Some(freshCap)
              commonCap1.get
            }

            val paramGuards: List[Option[Guard]] =
            // constructor of case class
              if (dd.symbol.isConstructor &&
                dd.symbol.owner.info.parents.contains(definitions.ProductRootClass.tpe)) {
                if (vparamss.nonEmpty) {
                  vparamss.head map { p =>
                    val symLocal = isTransient(p.symbol.info, typeVarsLocal = true, verbose = true)
                    vinfo("looking at " + p)
                    vinfoClass("p.symbol.info", p.symbol.info)
                    vinfo("is value class " + definitions.isPrimitiveValueClass(p.symbol.info.typeSymbol))
                    vinfo("is immut " + isImmutable(p.symbol.info))
                    vinfo("is in ctype " + ctype.contains(p.symbol.info.typeSymbol))
                    vinfo("is local " + symLocal)
                    if (!symLocal) {
                      vinfo("ltype: " + ltype)
                      verror(unit, p, "parameter type must be transient")
                      None
                    } else
                      Some(mkGuard(commonCap))
                  }
                } else
                  List()
              } else if (!isCaseClassMethod(dd.symbol) && vparamss.nonEmpty) {
                // build method type where every param of @transient type is transient
                vparamss.head map { p =>
                  val symLocal = isTransient(p.symbol.info, typeVarsLocal = true, verbose = verbose)
                  if (verbose) {
                    vinfo("VERBOSE at " + p)
                    vinfoClass("p.symbol.info", p.symbol.info)
                    vinfo("is value class " + definitions.isPrimitiveValueClass(p.symbol.info.typeSymbol))
                    vinfo("is immut " + isImmutable(p.symbol.info))
                    vinfo("is in ctype " + ctype.contains(p.symbol.info.typeSymbol))
                    vinfo("is local " + symLocal)
                  }
                  // do not mark as guarded if type immutable
                  // do not mark as guarded if type not annotated or transient
                  if (annotatedWith(p.symbol.info, uniqAnnot.tpe)) {
                    verror(unit, tree, "parameter must not be unique in transient type")
                    None
                  } else if (!definitions.isPrimitiveValueClass(p.symbol.info.typeSymbol) &&
                    !isImmutable(p.symbol.info) &&
                    (ctype.contains(p.symbol.info.typeSymbol) ||
                      symLocal)) {
                    Some(mkGuard(commonCap))
                  } else {
                    vinfo("!!param " + p + " of meth " + dd.symbol + " is not local")
                    None
                  }
                }
              } else
                List()

            val methCT = MethodCapType(
              Some(mkGuard(commonCap)),
              paramGuards,
              if (isImmutable(dd.symbol.info.resultType))
                None
              else
                Some(mkGuard(commonCap)),
              List(commonCap))

            mtype = mtype + (dd.symbol -> methCT)
          }

        case _ => super.traverse(tree)
      }
    }

    /* Checks whether a `ClassDef` with symbol `sym` capturing references
     * guarded by `rho` is `@transient`.
     */
    class CheckTransientCollector(sym: Symbol, rho: Int) extends Traverser {
      var unit: CompilationUnit = _
      var local = true
      // MethodCapTypes collected for ClassDef
      var mtype = collection.immutable.Map.empty[Symbol, MethodCapType]

      override def traverse(tree: Tree): Unit = tree match {
        case cdef@ClassDef(mods, name, tparams, impl) =>
          vinfo("CheckTransientCollector visits " + cdef.symbol)

          if (cdef.symbol == sym) {
            // check that extended types are local or immutable
            val classInfo = cdef.symbol.info
            if (!(classInfo.parents forall (t => {
              isTransient(t, typeVarsLocal = true, verbose = false) || isImmutable(t)
            }))) {
              vinfo("ltype: " + ltype)
              local = false
            } else
              traverse(impl)
          } else
            super.traverse(impl)

        case dd@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
          // note missing type information is expressed by tpt = TypeTree()

          val verbose = false
          vinfo("dd.symbol.owner " + dd.symbol.owner)

          if (dd.symbol.owner == sym) {
            val commonCap = rho
            val paramGuards: List[Option[Guard]] =
            // constructor of case class
              if (dd.symbol.isConstructor && dd.symbol.owner.info.parents.contains(definitions.ProductRootClass.tpe)) {
                if (vparamss.nonEmpty) {
                  vparamss.head map { p =>
                    val symLocal = isTransient(p.symbol.info, typeVarsLocal = true, verbose = false)
                    vinfo("looking at " + p)
                    vinfoClass("p.symbol.info", p.symbol.info)
                    vinfo("is value class " + definitions.isPrimitiveValueClass(p.symbol.info.typeSymbol))
                    vinfo("is immut " + isImmutable(p.symbol.info))
                    vinfo("is in ctype " + ctype.contains(p.symbol.info.typeSymbol))
                    vinfo("is local " + symLocal)
                    if (!symLocal) {
                      local = false
                      None
                    } else
                      Some(mkGuard(commonCap))
                  }
                } else
                  List()
              } else if (!isCaseClassMethod(dd.symbol) && vparamss.nonEmpty) {
                // build mtype where every param of @transient type is transient
                vparamss.head map { p =>
                  val symLocal = isTransient(p.symbol.info, typeVarsLocal = true, verbose = verbose)
                  if (verbose) {
                    vinfo("VERBOSE at " + p)
                    vinfoClass("p.symbol.info", p.symbol.info)
                    vinfo("is value class " + definitions.isPrimitiveValueClass(p.symbol.info.typeSymbol))
                    vinfo("is immut " + isImmutable(p.symbol.info))
                    vinfo("is in ctype " + ctype.contains(p.symbol.info.typeSymbol))
                    vinfo("is local " + symLocal)
                  }
                  // do not mark as guarded if type immutable
                  // do not mark as guarded if type not annotated or transient
                  if (annotatedWith(p.symbol.info, uniqAnnot.tpe)) {
                    local = false
                    None
                  } else if (!definitions.isPrimitiveValueClass(p.symbol.info.typeSymbol) &&
                    !isImmutable(p.symbol.info) &&
                    (ctype.contains(p.symbol.info.typeSymbol) ||
                      symLocal)) {
                    Some(mkGuard(commonCap))
                  } else {
                    vinfo("!!param " + p + " of meth " + dd.symbol + " is not local")
                    None
                  }
                }
              } else
                List()

            val methCT = MethodCapType(
              Some(mkGuard(commonCap)),
              paramGuards,
              if (isImmutable(dd.symbol.info.resultType))
                None
              else
                Some(mkGuard(commonCap)),
              List(commonCap))

            mtype = mtype + (dd.symbol -> methCT)
          }

        case _ => super.traverse(tree)
      }
    }

    /* Collects symbols of class definitions marked as `@transient`.
     */
    class TransientCollector extends Traverser {
      var unit: CompilationUnit = _

      override def traverse(tree: Tree): Unit = tree match {
        case cdef@ClassDef(mods, name, tparams, impl) =>
          if (hasTransientAnnot(cdef.symbol)) {
            ltype += cdef.symbol
            ctype += cdef.symbol
            traverse(impl)
          } else
            super.traverse(impl)
        case _ => super.traverse(tree)
      }
    }

    /* Collects method capability types (fills `mtype` map).
     */
    class MTypeCollector extends Traverser {
      var unit: CompilationUnit = _

      override def traverse(tree: Tree): Unit = tree match {
        case dd@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          if (isAnnotated(dd.symbol)) {
            mtype = mtype + (dd.symbol -> buildMethodType(unit, tree, dd.symbol))
            ctype += dd.symbol.owner
          }
          // also want to catch local functions
          traverse(rhs)

        case _ => super.traverse(tree)
      }
    }

    class CapabilityChecker extends Traverser {

      class CapturedCollector(localOwner: Symbol) extends Traverser {
        var guards: List[Guard] = List()
        var syms: List[Symbol] = List()

        override def traverse(tree: Tree): Unit = tree match {
          case Select(id@Ident(_), _) if !id.symbol.ownerChain.contains(localOwner) =>
            gSym.get(id.symbol) match {
              case Some(guard) =>
                guards = guard :: guards
                syms = id.symbol :: syms
              case _ =>
            }

          case Select(qual, sel) =>
            // qual.sel
            //vinfo("qual "+qual+" sel "+sel)
            //vinfo("issetter "+tree.symbol.isSetter)
            var prop = true
            if (tree.symbol.isMethod) {
              if (!tree.symbol.isGetter)
                prop = false
              else {
                val restpe = tree.symbol.info.resultType
                if (definitions.isPrimitiveValueClass(restpe.typeSymbol) && !tree.symbol.isSetter) {
                  prop = false
                }
              }
            }
            traverse(qual)
            if (prop && !qual.symbol.ownerChain.contains(localOwner))
              prefix(qual) match {
                case Some(guard) =>
                  guards = guard :: guards
                  syms = qual.symbol :: syms
                case _ =>
              }

          case th@This(_) =>
            vprintln("found This " + tree)
            if (!th.symbol.ownerChain.contains(localOwner)) {
              syms = th.symbol :: syms
              gThis.get(th.symbol) match {
                case Some(guard) =>
                  guards = guard :: guards
                case _ =>
              }
            }

          case _ => super.traverse(tree)
        }
      }

      class BindCollector extends Traverser {
        var binds: List[Bind] = List()

        override def traverse(tree: Tree): Unit = tree match {
          case b@Bind(_, _) => binds = b :: binds
          case _ => super.traverse(tree)
        }
      }

      var unit: CompilationUnit = _
      var insideMethod = false
      var caps: List[Capability] = List(0) // we always have shared capability 0

      def prefix(t: Tree): Option[Guard] = {
        val optPre = if (isUniqueLiteral(t)) {
          val rho = freshCap
          caps = rho :: caps
          Some((rho, false))
        } else t match {
          case Ident(_) => gSym.get(t.symbol)
          case _ =>
            if (t.toString == "this" && gThis.get(t.symbol).isDefined) gThis.get(t.symbol)
            else gTree.get(t)
        }
        // check if type annotated with @uncheckedUnique
        if (annotatedWith(t.tpe, uncheckedUniqAnnot.tpe)) {
          if (optPre.isEmpty) {
            val rho = freshCap
            caps = rho :: caps
            Some((rho, false))
          } else {
            if (!caps.contains(optPre.get._1))
              caps = optPre.get._1 :: caps
            optPre
          }
        } else
          optPre
      }

      def setPrefix(t: Tree, p: Guard): Unit = t match {
        case Ident(_) => gSym += (t.symbol -> p)
        case _ => gTree += (t -> p)
      }

      //TODO: this can be generalized to immutable types
      def isUniqueLiteral(t: Tree): Boolean = t match {
        case Literal(Constant(null)) => true
        case Literal(Constant(s: String)) => true
        case Literal(_) => false
        case _ => false
      }

      def guardsConform(expected: Option[Guard], rhs: Tree): Unit =
        rhs match {
          case Literal(_) => // literals such as `null` always conform
          case Throw(_) => // throw always conforms
          case EmptyTree => // OK
          case _ => expected match {
            case Some((lg, _)) =>
              prefix(rhs) match {
                case Some((rg, black)) =>
                  if (lg != rg) {
                    if (caps contains rg) {
                      // implicitly insert capture
                      vprintln("inserting capture around " + rhs)
                      caps = caps.filterNot(_ == rg)
                      gTree += (rhs -> (lg, false))
                    } else
                      verror(unit, rhs, "found guard " + rg + ", expected guard " + lg)
                  } else
                    vprintln("OK, have same guard")
                case _ =>
                  if (!(isImmutable(rhs.symbol) || isImmutable(rhs.tpe.typeSymbol)))
                    verror(unit, rhs, rhs + " is not guarded [guardsConform1]")
              }
            case _ => prefix(rhs) match {
              case Some((rg, black)) =>
                if (!(isImmutable(rhs.symbol) || isImmutable(rhs.tpe.typeSymbol))) {
                  vinfo("expected prefix " + expected)
                  vinfo("prefix rhs " + prefix(rhs))
                  verror(unit, rhs, "capability of right-hand side is not shared")
                }
              // else, we are leaking it out under an immutable type

              case _ => // OK
            }
          }
        }

      def guardsConform(lhs: Tree, rhs: Tree): Unit =
        guardsConform(prefix(lhs), rhs)

      abstract class Subst extends PartialFunction[Int, Int] {
        def apply(x: Int): Int

        def isDefinedAt(x: Int): Boolean

        def extend(p: Int, q: Int): Subst = new Subst {
          def isDefinedAt(x: Int): Boolean =
            x == p || Subst.this.isDefinedAt(x)

          def apply(x: Int): Int =
            if (x == p) q
            else Subst.this.apply(x)

          override def toString: String =
            "[" + p + "->" + q + "]" + Subst.this.toString
        }

        def extend(s: Subst): Subst = new Subst {
          def isDefinedAt(x: Int): Boolean =
            s.isDefinedAt(x) || Subst.this.isDefinedAt(x)

          def apply(x: Int): Int =
            if (s.isDefinedAt(x)) s(x)
            else Subst.this (x)

          override def toString: String =
            s.toString + Subst.this.toString
        }
      }

      val mtSubst: Subst = new Subst {
        def isDefinedAt(x: Int) = false

        def apply(x: Int): Int = x
      }

      def subst(rho: Int, sigma: Int): Subst = new Subst {
        def isDefinedAt(x: Int): Boolean =
          x == rho

        def apply(x: Int): Int =
          if (x == rho) sigma
          else x

        override def toString: String =
          "[" + rho + "->" + sigma + "]"
      }

      def unifySimple(t1: Guard, t2: (Guard, Tree)): Option[Subst] =
        Some(subst(t1._1, t2._1._1))

      def unify(t1: Option[Guard], t2: (Option[Guard], Tree)): Option[Subst] =
        if (t1.isEmpty) {
          if (t2._1.isEmpty)
            Some(mtSubst)
          else if (isUniqueLiteral(t2._2))
            Some(mtSubst)
          else
            Some(subst(0, t2._1.get._1))
        } else {
          if (isImmutable(t2._2.tpe.typeSymbol)) {
            // create fresh capability for immutable argument
            val litCap = freshCap
            vprintln("creating fresh cap " + litCap + " for immutable arg of type " + t2._2.tpe.typeSymbol)
            caps = litCap :: caps
            Some(subst(t1.get._1, litCap))
          } else if (t2._1.isEmpty)
            Some(subst(t1.get._1, 0))
          else
            unifySimple(t1.get, (t2._1.get, t2._2))
        }

      def unify(expected: List[Option[Guard]], provided: List[(Option[Guard], Tree)]): Option[Subst] =
        (expected, provided) match {
          case (List(), List()) =>
            Some(mtSubst)
          case (x :: xs, y :: ys) =>
            val s = unify(x, y)
            if (s.isEmpty)
              None
            else {
              val r = unify(xs, ys)
              if (r.isEmpty)
                None
              else
                Some(r.get.extend(s.get))
            }
        }

      /* Check function applications.
       */
      def checkFunApply(tree: Tree, fun: Tree, qual: Tree, args: List[Tree], targs: List[Tree]): Unit = {
        vinfo("fun.symbol " + fun.symbol)
        val methCTOpt = getMethodType(unit, tree, fun.symbol)
        vinfo("methCT " + methCTOpt)

        if (methCTOpt.isDefined) {
          val methCT = methCTOpt.get
          val aps = args.map(prefix)
          vprintln("args prefix: " + aps)

          // unify parameter types
          val s = try {
            unify(methCT.params, args.map(prefix) zip args)
          } catch {
            case t: Throwable => None
          }
          if (s.isEmpty) {
            vinfo("try unifying\n" + methCT.params + " and\n" + args.map(prefix))
            verror(unit, tree, "cannot unify parameter types")
          } else {
            vprintln("could unify parameters")
            val subst = s.get
            vinfo("substitution: " + subst)

            // check that we have required caps
            val required = methCT.requiredCaps.map(subst(_))
            val provided = methCT.provided.map(subst(_))
            vinfo("required caps: " + required)
            if (required.forall(c => caps.contains(c) || c == 0))
              vprintln("all required caps available")
            else {
              vinfo("caps: " + caps)
              verror(unit, tree, "not all required capabilities available")
            }

            // make sure shared capability is not consumed
            if (required.contains(0) && !provided.contains(0))
              verror(unit, tree, "shared capability may not be consumed")

            // compute result caps
            val consumedCaps = required filterNot provided.contains
            vinfo("consumed caps " + consumedCaps)

            val withoutReq = caps filterNot required.contains
            caps = withoutReq ++ provided
            vinfo("caps: " + caps)
            // compute capability type of invocation tree
            if (methCT.result.isDefined) {
              if (methCT.isResultUnique) {
                val resultCap = freshCap
                caps = resultCap :: caps
                gTree += (tree -> (resultCap, false))
              } else {
                val (retg, retb) = methCT.result.get
                vinfo("guard " + tree + " with " + (subst(retg), retb))
                val resultGuard = subst(retg)
                if (resultGuard != 0)
                  gTree += (tree -> (resultGuard, retb))
              }
            }
          }
        }
      }

      /* Check method invocations. [t-invoke]
       */
      def checkApply(tree: Tree, fun: Tree, qual: Tree, args: List[Tree], targs: List[Tree]): Unit = {
        vinfo("method invocation " + tree)
        vprintln("caps before call " + caps)
        traverse(qual)

        if (qual.toString.contains("super") && gThis.get(qual.symbol).isDefined)
          gTree += (qual -> gThis(qual.symbol))

        val onLit = qual match {
          case Literal(_) =>
            vprintln("method selected on literal " + qual)
            true
          case _ => false
        }

        if (onLit) {
          vprintln("OK, calling method on literal")
        } else {
          var meth = getMethodType(unit, tree, fun.symbol)
          if (meth.isEmpty && transientMethods.contains(fun.symbol)) {
            meth = Some(addTransientMType(fun.symbol))
          }
          if (meth.isDefined) {
            val methCT = meth.get
            vinfo("cap type (" + fun.symbol + ") " + methCT)
            vinfo("prefix of " + qual + ": " + prefix(qual))
            // check receiver
            val r = unify(methCT.receiver, (prefix(qual), qual))
            if (r.isEmpty)
              verror(unit, tree, "cannot unify receiver type")
            else {
              vprintln("could unify receiver")
              vprintln("r = " + r)

              // check whether we have to do target capability typing for
              // function literal argument
              (args zip methCT.params) foreach {
                // TODO: support functions with more than one parameter
                case (fun@Function(List(vd), body), Some((g, false))) =>
                  checkFunction((g, false), fun, vd, body)
                case (arg, _) =>
                  traverse(arg)
              }
              vprintln("caps after traversing args " + caps)

              val s = try { // unify parameter types
                unify(methCT.params, args.map(prefix) zip args)
              } catch {
                case t: Throwable => None
              }
              if (s.isEmpty) {
                //vinfo("params "+args)
                vinfo("try unifying\n" + methCT.params + " and\n" + args.map(prefix))
                verror(unit, tree, "cannot unify parameter types")
              } else {
                vprintln("could unify parameters")
                //TODO: check that s and r agree on common capability (if any)
                val subst = s.get.extend(r.get)
                vprintln("subst: " + subst)

                // check that we have required caps
                val required = methCT.requiredCaps.map(subst(_))
                val provided = methCT.provided.map(subst(_))
                vinfo("required caps: " + required)
                if (required.forall(c => caps.contains(c) || c == 0))
                  vprintln("all required caps available")
                else {
                  vinfo("caps: " + caps)
                  verror(unit, tree, "not all required capabilities available")
                }

                // make sure shared capability is not consumed
                if (required.contains(0) && !provided.contains(0))
                  verror(unit, tree, "shared capability may not be consumed")

                // compute result caps
                val consumedCaps = required filterNot provided.contains
                vinfo("consumed caps " + consumedCaps)

                val withoutReq = caps filterNot required.contains
                caps = withoutReq ++ methCT.provided.map(subst(_))
                vinfo("caps: " + caps)

                // compute capability type of invocation tree
                if (methCT.result.isDefined) {
                  val (retg, retb) = methCT.result.get
                  val resultGuard = subst(retg)
                  if (resultGuard != 0) { // do not propagate shared guard
                    vinfo("meth invocation has guard " + resultGuard)
                    gTree += (tree -> (resultGuard, retb))
                  }
                }
              }
            }
          } else if (prefix(qual).isDefined) {
            // propagate guard only if result type is not immutable
            val restpe = fun.symbol.info match {
              case MethodType(_, result) => result
              case PolyType(_, MethodType(_, result)) => result
            }
            if (!isImmutable(restpe)) {
              vinfo("meth invocation has guard " + prefix(qual).get)
              gTree += (tree -> prefix(qual).get)
            }
          } else {
            // Case meth.isEmpty && prefix(qual).isEmpty

            if (fun.symbol.owner != definitions.IntClass &&
              fun.symbol.owner != definitions.StringClass) {
              if (qual != null && qual.symbol != null && definitions.isFunctionType(qual.symbol.tpe)) {
                qual.symbol.tpe match {
                  case TypeRef(pre, sym, args) =>
                    vinfo("args " + args)
                    val withoutRes = args take (args.size - 1)
                    val argsImmut = withoutRes forall (a => isImmutable(a))
                    vinfo("args immut? " + argsImmut)
                    //TODO: consume caps if needed
                    if (!argsImmut)
                      verror(unit, tree, "arguments are not immutable")
                  case _ =>
                  // do nothing
                }
              } else if (qual != null && qual.symbol != null && (isImmutable(qual.tpe) || isImmutable(qual.symbol))) {
                val targsLocal = targs.map(_.symbol.tpe).forall(isTransient)
                val aps = args.map(prefix)
                val region = argsInSameRegion(aps)
                if (targsLocal && region.isDefined)
                  gTree += (tree -> (region.get, false))
                else if (region.isDefined) { // consume common cap
                  if (!caps.contains(region.get))
                    verror(unit, tree, "capability to access parameters not available")
                  else {
                    vprintln("consume cap of parameters")
                    caps = caps filterNot {
                      _ == region.get
                    }
                  }
                } else mkArgsShared(args, fun.symbol.tpe.params.map(_.info))
              } else mkArgsShared(args, fun.symbol.tpe.params.map(_.info))
            }
          }
          vprintln("caps after call " + caps)
        }
      }

      def checkFunction(tree: Tree, vd: ValDef, body: Tree): Unit = {
        //vinfo("checking "+tree)
        val gc = new CapturedCollector(tree.symbol)
        gc.traverse(body)
        gc.guards = gc.guards.distinct
        //vinfo("collected guards "+gc.guards)
        val goodCapture = gc.guards match {
          case List(guard) =>
            gSym += (vd.symbol -> guard)
            val res = tree -> guard
            gTree += res
            true
          case List() =>
            // OK, closure does not have guard
            true
          case _ =>
            verror(unit, tree, "closure captures things that could escape")
            false
        }
        if (goodCapture) {
          traverse(body)
        }
      }

      def checkFunction(target: Guard, tree: Tree, vd: ValDef, body: Tree): Unit = {
        //vinfo("checking "+tree+" with target "+target)
        val paramCap = freshCap
        val paramGuard = (paramCap, false)
        gSym += (vd.symbol -> paramGuard)

        val caps0 = caps
        val classOwnerCaps =
          tree.symbol.ownerChain.filter(_.isClass).flatMap { owner =>
            if (gThis.get(owner).isEmpty) List()
            else List(gThis(owner)._1)
          }
        caps = paramCap :: classOwnerCaps

        traverse(body)
        vinfo("caps after checking function body " + caps)
        if (!caps.contains(paramCap))
          verror(unit, tree, "closure consumes capability of its parameter")

        caps = paramCap :: caps0
        gTree += (tree -> paramGuard)
      }

      def checkReact(fun: Tree): Unit = fun match {
        case Function(vparams, body) =>
          val beta = freshCap
          gSym += (vparams.head.symbol -> (beta, false))
          caps = beta :: caps
          traverse(body)
      }

      def checkBlock(tree: Tree, stats: List[Tree], expr: Tree, uniqueExp: Boolean) {
        stats foreach (s => {
          vprintln("ch: [" + s.getClass + "]" + s); traverseExp(s, uniqueExp)
        })
        traverseExp(expr, uniqueExp)
        // propagate guard of expr
        prefix(expr) match {
          case Some(guard) =>
            gTree += (tree -> guard)
          case None =>
        }
      }

      def isLiteral(t: Tree): Boolean = t match {
        case Literal(_) => true
        case _ => false
      }

      def hasValueType(t: Tree): Boolean = {
        val res =
          if (t != null && t.symbol != null)
            isImmutable(t.symbol.info)
          else false
        vinfo("has value type? " + t + ": " + res)
        res
      }

      def argsInSameRegion(aps: List[Option[Guard]]): Option[Int] = {
        val mutableGuards = aps flatMap { argPre =>
          if (argPre.isDefined)
            List(argPre.get._1)
          else
            List()
        }
        if (mutableGuards.nonEmpty && mutableGuards.forall(_ == mutableGuards.head))
          Some(mutableGuards.head)
        else
          None
      }

      def mkArgsShared(args: List[Tree], paramTypes: List[Type]): Unit =
        (args zip paramTypes) foreach {
          case (a, paramType) =>
            if (!isImmutable(paramType))
              prefix(a) match {
                case Some((guard, black)) =>
                  if (!caps.contains(guard)) {
                    vinfo("capability " + guard + " missing")
                    vinfoClass("tpe " + a.tpe, a.tpe)
                    verror(unit, a, "capability to access " + a + " not available")
                  } else {
                    vprintln("consume cap of " + a)
                    caps = caps filterNot {
                      _ == guard
                    }
                  }
                case None => // OK
              }
        }

      override def traverse(tree: Tree): Unit =
        traverseExp(tree, uniqueExpected = false)

      // traverse with expected uniqueness
      def traverseExp(tree: Tree, uniqueExpected: Boolean): Unit = tree match {
        case dd@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
          // note missing type information is expressed by tpt = TypeTree()

          //vinfo("dd.symbol "+dd.symbol.owner+" "+dd.symbol+" "+dd.symbol.hasFlag(DEFERRED))

          vinfo("checking body of " + dd.symbol)
          if (!dd.symbol.hasFlag(DEFERRED) && !dd.symbol.isGetter &&
            !isCaseClassMethod(dd.symbol)) {
            val mt = getMethodType(unit, tree, dd.symbol)
            val methCT = if (mt.isEmpty)
              MethodCapType(None, List(), None, List())
            else {
              vprintln("found methCT " + mt.get)
              mt.get
            }
            vinfo("methCT " + methCT)

            // type check each method definition with empty environment
            //gTree = collection.mutable.Map.empty[Tree, (Int, Boolean)]
            val gSym0 = gSym
            gSym = collection.mutable.Map.empty[Symbol, (Int, Boolean)]
            val caps0 = caps
            caps = List()

            if (vparamss.nonEmpty) {
              val vparams = vparamss.head
              (vparams zip methCT.params) foreach { pt => {
                val p = pt._1
                vinfo("p: " + p)
                vinfo("p.symbol: " + p.symbol)

                // extract guard from methCT
                pt._2 match {
                  case Some((rho, black)) =>
                    caps = rho :: caps
                    gSym += (p.symbol -> (rho, black))
                  case None => // parameter not annotated
                }
              }
              }
            }

            methCT.receiver match {
              case None =>
                gThis -= dd.symbol.owner
              case Some((rho, black)) =>
                caps = rho :: caps
                vinfo("this of " + dd.symbol.owner + " has guard " + methCT.receiver.get)
                gThis(dd.symbol.owner) = methCT.receiver.get
            }

            insideMethod = true
            traverse(rhs)

            if (!dd.symbol.isConstructor) {
              methCT.result match {
                case None =>
                // OK, if actual result is guarded or tracked
                // we can widen to raw type
                // (we trivially satisfy post condition)
                case Some(_) =>
                  // if we expect a capability type
                  // types must conform
                  if (methCT.isResultUnique)
                    prefix(rhs) match {
                      case None =>
                        verror(unit, tree, "expected unique result")
                      case Some((rhsCap, _)) =>
                        if (methCT.requiredCaps.contains(rhsCap))
                          verror(unit, tree, "expected unique result")
                    }
                  else
                    guardsConform(methCT.result, rhs)
              }

              // check that we have the required caps
              val missingProvided = methCT.provided.filter { providedCap => !(caps contains providedCap) }

              if (missingProvided.size == 1 &&
                prefix(rhs).isDefined &&
                caps.contains(prefix(rhs).get._1) &&
                methCT.result.isDefined &&
                missingProvided.contains(methCT.result.get._1)) {
                // OK, it's the fresh cap of result
              } else if (missingProvided.nonEmpty) {
                val pcapOpt =
                  if (methCT.result.isEmpty)
                    Some(missingProvided.head)
                  else
                    missingProvided find {
                      methCT.result.get._1 != _
                    }

                if (pcapOpt.isEmpty) {
                  vinfo("SHOULD NOT HAPPEN")
                  vinfo("methCT " + methCT)
                  vinfo("missing " + missingProvided)
                  vinfo("prefix rhs " + prefix(rhs))
                  vinfo("caps: " + caps)
                  verror(unit, tree, "should not happen")
                } else {
                  val pcap = pcapOpt.get
                  //TODO restrict search to param symbols
                  gSym find { kv => kv._2._1 == pcap } match {
                    case None => // must be cap of receiver
                      if (pcap == methCT.receiver.get._1)
                        verror(unit, tree, "method does not return capability of receiver")
                      else
                        verror(unit, tree, "should not happen")
                    case Some((psym, _)) =>
                      verror(unit, tree, "method does not return capability of " + psym)
                  }
                }
              }
            }

            gSym = gSym0
            caps = caps0
            insideMethod = false
            gThis -= dd.symbol.owner
          }

        //        // Actor.react
        //        case Apply(se@Select(_, _), params) if se.symbol == actorReactMethod =>
        //          checkReact(params.head)
        //
        //        // Actor.reactWithin
        //        case Apply(Apply(se@Select(_, _), timeout), params) if se.symbol == actorReactWithinMethod =>
        //          checkReact(params.head)

        // TODO: Actor.receive/receiveWithin (and InputChannel.react...)

        // Unique.share
        case Apply(TypeApply(se, tptr), params) if se.symbol == shareFun =>
          vinfo("found Unique.share")
          traverseTrees(params)
          val t = params.head
          prefix(t) match {
            case Some((cap, _)) =>
              if (!caps.contains(cap))
                verror(unit, tree, "capability unavailable")
              else {
                caps = caps filterNot (_ == cap)
                t match {
                  case Ident(_) => gSym -= t.symbol
                  case _ =>
                    if (t.toString == "this")
                      gThis.remove(t.symbol)
                    else
                      gTree -= t
                }
              }
            case _ => // do nothing
          }

        // Unique.share2
        case Apply(TypeApply(se, tptr), params) if se.symbol == share2Fun =>
          vinfo("found Unique.share2")
          traverseTrees(params)
          val p0 = params.head
          val p1 = params(1)
          prefix(p0) match {
            case Some((cap, _)) =>
              if (!caps.contains(cap))
                verror(unit, tree, "capability unavailable")
              else {
                val ps = if (prefix(p1).isEmpty) List(p0) else {
                  val Some((cap1, _)) = prefix(p1)
                  if (cap1 != cap) {
                    verror(unit, tree, "params must have same capability")
                    List(p0)
                  } else
                    List(p0, p1)
                }
                caps = caps filterNot (_ == cap)
                ps.foreach {
                  case t@Ident(_) => gSym -= t.symbol
                  case t =>
                    if (t.toString == "this")
                      gThis.remove(t.symbol)
                    else
                      gTree -= t
                }
              }
            case _ => // do nothing
          }

        // Unique.capture
        // [t-capture]
        case Apply(TypeApply(se, tptr), args) if se.symbol == captureFun =>
          vprintln("capture")
          traverseTrees(args)
          vprintln("caps before: " + caps)
          vinfo(args.head + " " + prefix(args.head))
          vinfo(args(1) + " " + prefix(args(1)))

          // first argument must be unique, and is consumed
          prefix(args.head) match {
            case None =>
              verror(unit, tree, "first argument of capture must be unique")
            case Some((fstCap, _)) =>
              if (!caps.contains(fstCap))
                verror(unit, tree, "capability of first argument not available")
              else {
                caps = caps filterNot fstCap.==
                // second argument is either shared or unique
                prefix(args(1)) match {
                  case None =>
                    //                    verror(unit, tree, "second argument of capture must be unique")
                    gTree -= tree // capturing argument has shared capability
                  case Some((sndCap, _)) =>
                    gTree += (tree -> (sndCap, false))
                }
              }
          }

        // Unique.mkCaptured[C](y).capturedBy[C](x)
        // [t-capture]
        case Apply(TypeApply(
        Select(
        Apply(TypeApply(mkCap, mkCapTptr), y),
        _
        ),
        cbTptr), x) if mkCap.symbol == mkCapturedFun =>
          vprintln("mkCaptured")

          traverseTrees(y)
          traverseTrees(x)
          vprintln("caps before: " + caps)
          vinfo(y.head + " " + prefix(y.head))
          vinfo(x.head + " " + prefix(x.head))

          // first argument must be unique, and is consumed
          prefix(y.head) match {
            case None =>
              verror(unit, tree, "first argument of capture must be unique")
            case Some((fstCap, _)) =>
              if (!caps.contains(fstCap))
                verror(unit, tree, "capability of first argument not available")
              else {
                caps = caps filterNot fstCap.==
                // second argument is either shared or unique
                prefix(x.head) match {
                  case None =>
                    //                    verror(unit, tree, "second argument of capture must be unique")
                    gTree -= tree // capturing argument has shared capability
                  case Some((sndCap, _)) =>
                    gTree += (tree -> (sndCap, false))
                }
              }
          }

        // [t-swap]
        // recognize swap of the form { val tmp = o.f; o.f = t; tmp }
        case b@Block(stats@List(ValDef(_, name, _, rhs),
        Apply(selSetter@Select(_, _), args)),
        expr@Ident(name2)) if name == name2 =>
          vinfo("found swap block " + b)
          // check whether selected field is unique
          rhs match {
            case sel@Select(_, _) if sel.symbol.isGetter =>
              val setter = sel.symbol.setter(sel.symbol.owner)
              if (!hasUniqueAnnot(setter.tpe.params.head))
                checkBlock(tree, stats, expr, uniqueExpected)
              else {
                traverse(rhs)
                traverseTrees(args)
                if (setter != selSetter.symbol)
                  verror(unit, tree, "must select twice the same field in a swap")
                else {
                  // 2. must be consumable unique term
                  prefix(args.head) match {
                    case None =>
                      verror(unit, tree, "right-hand side of swap must be unique")
                    case Some((rhsCap, _)) =>
                      if (!(caps contains rhsCap))
                        verror(unit, tree, "capability of right-hand side not available")
                      else
                        caps = caps filterNot rhsCap.==
                  }
                  // 3. result is unique with fresh cap
                  val resCap = freshCap
                  caps = resCap :: caps
                  gTree += (tree -> (resCap, false))
                }
              }
            case _ => checkBlock(tree, stats, expr, uniqueExpected)
          }

        // Unique.swap
        // [t-swap]
        case Apply(TypeApply(se, tptr), params) if se.symbol == swapFun =>
          traverseTrees(params)
          // 1. first param must refer to unique field
          params.head match {
            case sel@Select(_, _) if sel.symbol.isGetter =>
              val setter = sel.symbol.setter(sel.symbol.owner)
              if (!hasUniqueAnnot(setter.tpe.params.head))
                verror(unit, tree, "field must be unique")
              else {
                // 2. second param must be unique, and is consumed
                prefix(params(1)) match {
                  case None =>
                    verror(unit, tree, "rhs of swap must be unique")
                  case Some((rhsCap, _)) =>
                    if (!(caps contains rhsCap))
                      verror(unit, tree, "capability of rhs not available")
                    else
                      caps = caps filterNot rhsCap.==
                }
                // 3. result is unique with fresh cap
                val resCap = freshCap
                caps = resCap :: caps
                gTree += (tree -> (resCap, false))
              }

            case _ =>
              verror(unit, tree, "first parameter of swap must be a field selection")
          }

        // [t-new] instance creation
        case Apply(sel@Select(New(tpt), s), as) => // s = <init>
          //          vinfo("instance creation: "+tree)
          traverseTrees(as)

          // treat symbols captured by `tpt.symbol` as additional arguments
          //          vinfo("tpt.symbol "+tpt.symbol)
          //          vinfo("captured syms "+capturedSyms.get(tpt.symbol))
          val capturedPrefixes = capturedSyms.get(tpt.symbol) match {
            case Some(syms) => syms.map(sym => {
              if (sym.isInstanceOf[ClassSymbol]) {
                gThis.get(sym)
              } else {
                gSym.get(sym)
              }
            })
            case None => List()
          }
          //          vinfo("captured prefixes "+capturedPrefixes)
          val aps = as.map(prefix) ::: capturedPrefixes

          // case: create shared instance without consuming caps of args
          //       if expected param type is immutable and expected result
          //       type is not guarded
          //
          //TODO: find out in which test case this is required
          // I think it's required in partest Worker to not make things
          // unique, and having to consume capability if they are passed
          // to unchecked classes.
          val canCreateShared = (aps zip sel.symbol.tpe.params) forall {
            case (argPre, param) =>
              argPre.isEmpty || isImmutable(param.info)
          }

          val capturedParams = capturedSyms.get(tpt.symbol) match {
            case Some(syms) => syms
            case None => List()
          }
          val params = sel.symbol.tpe.params ::: capturedParams
          val paramTypesImmutable = params forall {
            param => isImmutable(param.info)
          }

          if (uniqueExpected && paramTypesImmutable) {
            val newcap = freshCap
            gTree += (tree -> (newcap, false))
            caps = newcap :: caps
          } else if (uniqueExpected || !canCreateShared) {
            // 1. If all args guarded by the same cap _or immutable_, then
            //    that cap is not consumed, and the new instance is guarded
            //    by that cap
            // 2. Otherwise, all argument caps are consumed. The new
            //    instance is shared if one of the arguments is shared.
            val region = argsInSameRegion(aps)
            if (region.isDefined)
              gTree += (tree -> (region.get, false))
            else {
              val capsToConsume = aps.flatMap(pre => if (pre.isEmpty) List() else List(pre.get._1))
              // these caps must all be available (otherwise a local object could point to them)
              val allCapsAvail = capsToConsume.forall(caps contains _)
              if (!allCapsAvail)
              //TODO: which param is not accessible?
                verror(unit, tree, "not all caps available for creating a new instance")
              else
                capsToConsume.foreach(c => caps = caps.filterNot(_ == c))

              // if one of the args is shared, new instance is shared
              val shared = aps.filter(_.isEmpty)
              if (shared.isEmpty) {
                val newcap = freshCap
                vinfo("new instance is tracked with " + newcap)
                gTree += (tree -> (newcap, false))
                caps = newcap :: caps
              }
            }
          }

        // field assignment
        case Apply(fun@Select(id@Ident(_), sel), args) if fun.symbol.isSetter =>
          vinfo("field assign " + tree)
          val rhs = args.head
          traverseExp(rhs, prefix(id).isDefined)
          guardsConform(id, rhs)

        case Apply(fun@Select(_, _), args) if fun.symbol.isSetter =>
          vinfo("field assign2 " + tree)
          val setterTpe = fun.symbol.tpe
          val param = setterTpe.params.head
          if (hasUniqueAnnot(param))
            verror(unit, tree, "cannot assign to unique field")
          else {
            val rhs = args.head
            traverse(fun)
            traverseExp(rhs, prefix(fun).isDefined)
            vinfo("lhs " + fun)
            vinfo(prefix(fun))
            vinfo("rhs " + args.head)
            vinfo(prefix(args.head))
            guardsConform(fun, rhs)
          }

        case Select(id@Ident(_), _) =>
          gSym.get(id.symbol) match {
            case Some((guard, _)) =>
              gTree += (tree -> (guard, false))
            case _ =>
          }

        case Select(qual, sel) if qual.symbol != null && qual.symbol.isMethod =>
          vinfo("selecting method")
          // qual.sel
          //          vinfo("qual "+qual)
          printClass("s", sel)
          //          vinfo("sel "+sel)
          traverse(qual)
          //          vinfo("prefix: "+prefix(qual))
          prefix(qual) match {
            case Some((guard, false)) =>
              gTree += (tree -> (guard, false))
            case _ =>
          }

        // field selection qual.sel [t-select]
        case Select(qual, sel) =>
          //          vinfo("qual "+qual+" sel "+sel)
          //          vinfo("issetter "+tree.symbol.isSetter)
          traverse(qual)
          var prop = true
          if (tree.symbol.isMethod) {
            val restpe = tree.symbol.info.resultType
            if (definitions.isPrimitiveValueClass(restpe.typeSymbol) && !tree.symbol.isSetter) {
              //vinfo("do not propagate guard")
              prop = false
            } else if (!tree.symbol.isSetter && !tree.symbol.isGetter) {
              val meth = getMethodType(unit, tree, tree.symbol)
              if (meth.isDefined) {
                prop = false
                val methCT = meth.get
                vinfo("cap type (" + tree.symbol + ") " + methCT)
                // check receiver
                if (prefix(qual).isEmpty && methCT.receiver.isDefined)
                  verror(unit, qual, "type of receiver not guarded")
                else {
                  val r = unify(methCT.receiver, (prefix(qual), qual))
                  if (r.isEmpty)
                    verror(unit, tree, "cannot unify receiver type")
                  else {
                    vprintln("could unify receiver")
                    val subst = r.get
                    // check that we have required caps
                    val required = methCT.requiredCaps.map(subst(_))
                    val provided = methCT.provided.map(subst(_))
                    if (required.forall(c => caps.contains(c) || c == 0))
                      vprintln("all required caps available")
                    else {
                      vinfo("caps: " + caps)
                      verror(unit, tree, "not all required capabilities available")
                    }

                    // make sure shared capability is not consumed
                    if (required.contains(0) && !provided.contains(0))
                      verror(unit, tree, "shared capability may not be consumed")

                    // compute result caps
                    val consumedCaps = required filterNot provided.contains
                    vinfo("consumed caps " + consumedCaps)

                    val withoutReq = caps filterNot required.contains
                    caps = withoutReq ++ methCT.provided.map(subst(_))
                    vinfo("caps: " + caps)
                    // compute cap type of invk tree
                    if (methCT.result.isDefined) {
                      val (retg, retb) = methCT.result.get
                      val resultGuard = subst(retg)
                      if (resultGuard != 0)
                        gTree += (tree -> (resultGuard, retb))
                    }
                  }
                }
              }
            }
          }
          if (prop) {
            vinfo("prefix: " + prefix(qual))
            prefix(qual) match {
              case Some((guard, false)) =>
                gTree += (tree -> (guard, false))
              case _ =>
            }
          }

        // method invocation
        case Apply(fun@Select(qual, _), args) if fun.symbol.isMethod =>
          checkApply(tree, fun, qual, args, List())

        case Apply(TypeApply(fun@Select(qual, _), targs), args) if fun.symbol.isMethod =>
          checkApply(tree, fun, qual, args, targs)

        case Apply(fun@Ident(_), args) =>
          vinfo("function application")
          checkFunApply(tree, fun, null, args, List())

        // pattern matching
        case Match(sel, cases) =>
          traverse(sel)
          prefix(sel) match {
            case None =>
            case Some(ctpe@(_, false)) =>
              cases foreach { case CaseDef(pat, _, _) =>
                val bc = new BindCollector
                bc.traverse(pat)
                bc.binds foreach { b =>
                  if (!definitions.isPrimitiveValueClass(b.symbol.info.typeSymbol))
                    gSym += (b.symbol -> ctpe)
                }
              }
            case Some(ctpe@(guard, true)) =>
              // consume guard
              if (!caps.contains(guard))
                verror(unit, sel, "capability to access " + sel + " not available")
              else {
                vprintln("consume cap of " + sel)
                caps = caps filterNot {
                  _ == guard
                }
              }

              cases foreach {
                case CaseDef(b@Bind(name, _), _, _) =>
                  if (!definitions.isPrimitiveValueClass(b.symbol.info.typeSymbol)) {
                    val newcap = freshCap
                    gSym += (b.symbol -> (newcap, true))
                    caps = newcap :: caps
                  }
                case other =>
                  verror(unit, other, "cannot match unique object")
              }
          }
          cases foreach {
            traverse(_)
          }
          // if all cases guarded by the same cap,
          // result of match is guarded by that as well
          val casePres = cases map (prefix(_))
          vinfo("guards of cases (" + sel + ") " + casePres)
          if (casePres.forall(p =>
            p.isDefined && p.get._1 == casePres.head.get._1))
            gTree += (tree -> casePres.head.get)
          else if (casePres.forall(p =>
            p.isDefined && caps.contains(p.get._1))) {
            // consume caps and create fresh unique result
            casePres.foreach(p => caps = caps filterNot (_ == p.get._1))
            val resultCap = freshCap
            caps = resultCap :: caps
            gTree += (tree -> (resultCap, false))
          }

        case cd@CaseDef(pat, guard, body) =>
          traverse(body)
          val preBody = prefix(body)
          vinfo("pre body (" + pat + ") " + preBody)
          preBody match {
            case Some(guard) =>
              gTree += (cd -> guard)
            case None =>
          }

        case Assign(id@Ident(_), rhs) =>
          //vinfo("assign")
          traverse(rhs)
          vprintln(tree)
          guardsConform(id, rhs)

        case Block(stats, expr) =>
          // { stats; expr }
          checkBlock(tree, stats, expr, uniqueExpected)

        // [t-try]
        case Try(block, catches, finalizer) =>
          traverse(block)
          prefix(block) match {
            case Some(guard) =>
              gTree += (tree -> guard)
            case None =>
          }

        // [t-valdef]
        case vd@ValDef(mods, name, tpt, rhs) =>
          // mods val name: tpt = rhs
          // note missing type information is expressed by tpt = TypeTree()

          // propagate guards from bottom to top
          if (hasUniqueAnnot(vd.symbol))
            traverseExp(rhs, uniqueExpected = true)
          else
            traverse(rhs)
          //          vinfo("rhs "+rhs)
          //          vinfo("prefix "+prefix(rhs))
          prefix(rhs) match {
            case Some(ctpe@(rhsCap, _)) =>
              if (vd.symbol.owner.isClass) {
                if (hasUniqueAnnot(vd.symbol)) {
                  rhs match {
                    case Literal(_) => // ok to treat literals as unique
                    case _ =>
                      verror(unit, tree, "cannot assign to unique field")
                  }
                }
              } else if (hasUniqueAnnot(vd.symbol)) {
                gSym += (vd.symbol -> ctpe)
                vinfo(vd.symbol + " is now guarded: " + ctpe)
              } else if (!caps.contains(rhsCap))
                verror(unit, rhs, "capability to access not available")
              else
                caps = caps filterNot (_ == rhsCap)

            case _ =>
              if (hasUniqueAnnot(vd.symbol))
                rhs match {
                  case Literal(_) => // ok to treat literals as unique
                  case Select(qual, name) =>
                    immutableSyms find { s =>
                      s.tpe == rhs.symbol.info
                    } match {
                      case None =>
                        verror(unit, rhs, "unique value expected")
                      case Some(_) =>
                      // do nothing
                    }
                  case _ =>
                    verror(unit, rhs, "unique value expected")
                }
          }

        // closure (function literal)
        case Function(List(vd@ValDef(_, _, _, _)), body) =>
          checkFunction(tree, vd, body)

        case This(_) =>
          vprintln("found This " + tree)
          gThis.get(tree.symbol) match {
            case Some((guard, false)) =>
              val res = tree -> (guard, false)
              vprintln("adding " + res + " for tree")
              gTree += res
            case _ =>
          }

        case TypeApply(select@Select(qual, sel), tpt) =>
          vinfo("sel " + sel)
          traverse(select)
          val pre = prefix(select)
          if (pre.isDefined)
            gTree += (tree -> pre.get)

        case If(cond, thenp, elsep) =>

          def consume(cap: Int, tree: Tree) {
            if (caps.contains(cap)) {
              // consume capability
              caps = caps.filterNot(_ == cap)
            } else
              verror(unit, tree, "capability to access " + tree + " not available")
          }

          vprintln("conditional " + tree)
          traverse(cond)
          val caps0 = caps

          traverse(thenp)
          var caps1 = caps // after thenp

          caps = caps0
          traverse(elsep)
          var caps2 = caps // after elsep

          val thenpre = prefix(thenp)
          val elsepre = prefix(elsep)
          val capTpe = (thenpre, elsepre) match {
            case (None, None) =>
              None
            case (Some((thenCap, true)), None) =>
              caps = caps1
              consume(thenCap, thenp)
              caps1 = caps
              None
            case (None, Some((elseCap, true))) =>
              caps = caps2
              consume(elseCap, elsep)
              caps2 = caps
              None
            case otherwise =>
              if (thenpre != elsepre) {
                //vinfo(unit, tree, "branches have incompatible types")
                None
              }
              else
                thenpre
          }
          if (capTpe.isDefined)
            gTree += (tree -> capTpe.get)

          caps = caps1 intersect caps2

        // checking nested classes
        case cdef@ClassDef(mods, name, tparams, impl) =>
          vinfo((if (cdef.symbol.isNestedClass) "nested " else "") + "class definition " + name)

          if (cdef.symbol.isNestedClass) {
            // 1. collect guards of captured references
            val gc = new CapturedCollector(cdef.symbol)
            gc.traverse(impl)
            gc.syms = gc.syms.distinct
            capturedSyms += (cdef.symbol -> gc.syms)

            gc.guards = gc.guards.distinct
            if (gc.guards.size > 1) {
              vinfo("guards: " + gc.guards)
              verror(unit, tree, "nested class captures more than one unique reference")
            } else if (gc.guards.size == 1) {
              // 2. check that template is transient, assuming
              //    parameters are guarded by the single guard found in step 1.
              val guard = gc.guards.head
              vinfo("nested class " + name + " captures " + guard)
              val checkTransCollector = new CheckTransientCollector(cdef.symbol, guard._1)
              checkTransCollector.unit = unit
              checkTransCollector(cdef)

              // 3. add mtypes of nested class to global mtype map
              mtype = mtype ++ checkTransCollector.mtype

              // 4. add guard for `this` of the anonymous class
              gThis(cdef.symbol) = guard
              vprintln("checking body " + impl)
              traverse(impl)
            } else
              traverse(impl)
          } else
            traverse(impl)

        case Template(parents, self, body) =>
          // extends parents { self => body }
          // if self is missing it is represented as emptyValDef
          vprintln("checking template")
          body foreach (t => traverse(t))

        case _ => super.traverse(tree)
      }
    }

    def enableChecking(unit: CompilationUnit, verbose: Boolean): Boolean = {
      val isChecked = true
      //        !unit.find {
      //          _.text.contains("-enable-unique")
      //        }.isEmpty
      if (verbose && isChecked)
        println("[unique] checking " + unit.source.path)
      isChecked
    }

    def run: Unit = {
      // 1st pass: collect extended method types
      val mtypeCollector = new MTypeCollector
      var iter = currentRun.units
      while (iter.hasNext) {
        val unit = iter.next
        if (enableChecking(unit, verbose = false)) {
          mtypeCollector.unit = unit
          mtypeCollector(unit.body)
        }
      }

      // 2nd pass: collect classes marked @transient
      val locCollector = new TransientCollector
      iter = currentRun.units
      while (iter.hasNext) {
        val unit = iter.next
        if (enableChecking(unit, verbose = false)) {
          locCollector.unit = unit
          locCollector(unit.body)
        }
      }

      // 3rd pass: collect extended method types for classes marked @transient
      val locMTypeCollector = new TransientMethodTypeCollector
      iter = currentRun.units
      while (iter.hasNext) {
        val unit = iter.next
        if (enableChecking(unit, verbose = false)) {
          locMTypeCollector.unit = unit
          locMTypeCollector(unit.body)
        }
      }

      // 4th pass: capability type checking
      val checker = new CapabilityChecker
      iter = currentRun.units
      while (iter.hasNext) {
        val unit = iter.next
        if (enableChecking(unit, /*true*/ verbose = false)) {
          checker.unit = unit
          checker(unit.body)
        }
      }
    }
  }

}

