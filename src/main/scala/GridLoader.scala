package com.github.thekenny.Sudoku

import java.io.File
import java.net.URL
import scala.io.Source

object GridLoader {
  def loadDotSdk(file: File): Grid = loadDotSdk(file.toURI.toURL)
  def loadDotSdk(url: URL): Grid = { // Supports SudoCue (.sdk) 
    val cueTransformer: String => Seq[Option[Int]] = {
      _.map{
        _ match {
          case '.'     => None
          case i       => Some(i.toString.toInt)
        }
      }
    }

    val s = Source.fromURL(url)
    val numberLines = s getLines() filterNot{_.head == '#'} map(cueTransformer) 
    
    Grid.fromLines(numberLines toList)                       
  }

  def loadDotSdm(file: File): Seq[Grid] = loadDotSdm(file.toURI.toURL)
  def loadDotSdm(url: URL): Seq[Grid] = {
    val lines = Source.fromURL(url).getLines
    lines filter((l: String) => {
      l != null && l.length == 81
    }) map((line: String) => {
      Grid.fromSeq(line.map{_.toString.toInt})
    }) toList
  }

  def loadDotSs(file: File): Grid = loadDotSs(file.toURI.toURL)
  def loadDotSs(url: URL): Grid = {
    val droppedChars: Set[Char] = Set('-', '|')
    
    val ssTransformer: String => Seq[Option[Int]] = {
      _ map{
        _ match {
          case '.' => None
          case x   => Some(x.toString.toInt)
        }
      }
    }
    val lines: Iterator[String] = Source.fromURL(url) getLines()
    val data = lines map{_ filterNot{droppedChars(_)}} map(ssTransformer)
    Grid.fromLines(data toList)
  }
}
