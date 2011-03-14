package com.github.thekenny.Sudoku

import java.io.File
import java.net.URL
import scala.io.Source

object GridLoader {
  def loadDotSdk(file: File): Grid = Grid.fromURL(file.toURI.toURL)
  def loadDotSdk(url: URL): Grid = { // Supports SudoCue (.sdk) 
    val cueTransformer: String => Seq[Option[Int]] = {
      _.map{
        _ match {
          case '.'     => None
          case 'x'|'X' => None
          case '0'     => None
          case i       => Some(i.toString.toInt)
        }
      }
    }

    val s = Source.fromURL(url)
    val numberLines = s getLines() filterNot{_.head == '#'} map(cueTransformer) 
    
    Grid.fromLines(numberLines toList)                       
  }
}
