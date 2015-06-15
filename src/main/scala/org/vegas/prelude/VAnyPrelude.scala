package org.vegas.prelude

import org.vegas.vtype._

package object VAnyPrelude extends Prelude {
    def init {
        VAny define "print" -> ((args: Seq[ast.Expression]) => "echo ")
    }
}
