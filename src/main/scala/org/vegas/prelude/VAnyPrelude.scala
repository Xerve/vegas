package org.vegas.prelude

import org.vegas.vtype._

package object VAnyPrelude extends Prelude {
    def init {
        VAny define "print" -> ((callee: ast.Expression, args: Seq[ast.Expression]) => "echo " + callee.eval)
    }
}
