package sircle_lang

class SymbolBinding(val name: String, var value: Value)

object SymbolBinding {
  type Env = List[SymbolBinding]
}

