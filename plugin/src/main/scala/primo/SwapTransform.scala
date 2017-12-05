package primo

import scala.tools.nsc.plugins._
import scala.tools.nsc.symtab._
import scala.tools.nsc.transform._

abstract class SwapTransform extends PluginComponent with Transform with Debug {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._
  import typer.typed    // methods to type trees

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new SwapTransformer(unit)

  class SwapTransformer(unit: CompilationUnit) extends Transformer {

    val uniqueMod = global.rootMirror.getRequiredModule("scala.annotation.UniqueOps")
    val swapFun = definitions.getMember(uniqueMod, "swap" : nme.NameType)

    override def transform(tree: Tree): Tree = tree match {
      case Apply(TypeApply(se, tptr), params) if se.symbol == swapFun =>
        params.head match {
          case sel @ Select(qual, field) if sel.symbol.isGetter =>
            // OK, 1st param is field select

            val valueTpe = params.head.tpe.withoutAnnotations // TODO: remove only ours!

            val sym = currentOwner.newValue(unit.fresh.newName("tmp"), tree.pos)
                        .setFlag(Flags.SYNTHETIC)
                        .setInfo(valueTpe)
            val firstExpr = typed(atPos(tree.pos)(ValDef(sym, params(0))))

            // obtain setter from getter
            val setter = sel.symbol.setter(sel.symbol.owner)
            val assign = typed(atPos(tree.pos)(
              Apply(Select(qual, setter), List(params(1)))))

            val result = typed(atPos(tree.pos)(Ident(sym)))

            treeCopy.Block(tree, List(firstExpr, assign), result)

          case _ =>
            verror(unit, tree, "first parameter of swap must be a field selection")
            tree
        }

      case _ => super.transform(tree)
    }
  }

}
