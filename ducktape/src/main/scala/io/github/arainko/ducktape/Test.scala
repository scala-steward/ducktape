package io.github.arainko.ducktape

enum Dupsko {
  case Dupal
  case Dupal2(int: Int, str: String)
  case Rec(dup: Option[Dupsko], either: Either[List[String], String])
}


// object Test {
//   Structure.print[Dupsko]
// }
