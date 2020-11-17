package sircle_lang

case class ScanError(errToken: List[Token]) extends RuntimeException
