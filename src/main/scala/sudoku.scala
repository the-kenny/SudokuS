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
    data sliding(9,9) drop(i/9) map{_(i%9)} toList
  }

  def apply(x: Int, y: Int): Option[Int] = {
    require(x >= 0 && x < 9 && y >= 0 && y < 9)
    data(9*y+x)
  }

  def getBlock(x: Int, y: Int): Seq[Option[Int]] = Seq()
} 

object Grid {
  def apply() = new Grid(Seq.fill(81){None})
  def apply(is: Option[Int]*) = new Grid(is)
  
  def fromSeq(is: Seq[Int]): Grid = new Grid(is.map{Some(_)})
  def fromLines(lines: Seq[Seq[Option[Int]]]): Grid = new Grid(lines.flatten)
  def fromFile(file: File): Grid = Grid.fromURL(file.toURI.toURL)

  //Supports SudoCue (.sdk) 
  def fromURL(url: URL): Grid = {
    val s = Source.fromURL(url)
    val numberLines = for(line <- s.getLines;
                          if line(0) != '#')
                      yield (line map{
                        _ match {
                          case '.' => None
                          case i   => Some(i.toString.toInt)
                        } 
                      } toList);
    
    Grid.fromLines(numberLines toList)                       
  }
}
