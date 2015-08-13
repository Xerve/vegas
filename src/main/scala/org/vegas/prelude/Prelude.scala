package org.vegas.prelude

abstract class Prelude {
    def init: Unit
}

object Prelude {
    def init {
        VAnyPrelude.init
        VNumberPrelude.init
        VStringPrelude.init
        VCommentPrelude.init
        VConditionalPrelude.init
        VClassPrelude.init
    }
}
