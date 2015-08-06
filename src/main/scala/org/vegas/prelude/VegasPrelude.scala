package org.vegas.prelude

import org.vegas.compiler.Compiler
import org.vegas.vtype._
import org.vegas.vtype.ast.{Expression, GenericExpression}

package object VegasPrelude extends Prelude {

    def init {
        Compiler.scope.add("$vegas", Some(Vegas))
    }
}
