package com.github.thekenny.Sudoku

import scala.collection.mutable.{ArrayBuffer, Seq => MSeq}
import scala.io.Source
import java.io.File
import java.net.URL

class Grid(ois: Seq[Option[Int]]) {
  val data: MSeq[Option[Int]] = ArrayBuffer.concat(ois);
  require(data.length == 81)

  def emptyFields = data filter{_.isEmpty} length

  def getRow(i: Int): Seq[Option[Int]] = { 
    require(i >= 0 && i < 9)
    data drop(i*9) take(9)
  }

  def getColumn(i: Int): Seq[Option[Int]] = {
    require(i >= 0 && i < 9)
    data grouped(9) drop(i/9) map{_(i%9)} toList
  }

  def apply(x: Int, y: Int): Option[Int] = {
    require(x >= 0 && x < 9 && 
            y >= 0 && y < 9)
    data(9*y+x)
  }

  def getBlock(x: Int, y: Int): Seq[Option[Int]] = {
    require(3 > x && x >= 0 && 
            3 > y && y >= 0)

    (for(i <- 0 to 2) yield getRow(y*3+i).drop(x*3).take(3)) flatten
  }
} 

object Grid {
  def apply() = new Grid(Seq.fill(81){None})
  def apply(is: Option[Int]*) = new Grid(is)
  
  def fromSeq(is: Seq[Int]): Grid = new Grid(is.map{Some(_)})
  def fromLines(lines: Seq[Seq[Option[Int]]]): Grid = new Grid(lines.flatten)

  def fromFile(file: File): Grid = Grid.fromURL(file.toURI.toURL)
  def fromURL(url: URL): Grid = { // Supports SudoCue (.sdk) 
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
