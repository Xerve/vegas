package org.vegas.types

case class VType(val typename: String) {
    def eval = typename
}
