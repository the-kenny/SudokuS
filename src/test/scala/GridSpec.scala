package com.github.thekenny.Sudoku.test

import org.specs._
import scala.util.Random
import com.github.thekenny.Sudoku._

class GridSpec extends Specification {
  var filledGrid: Grid = null
  var emptyGrid: Grid = null
  var randomGrid: Grid = null;

  doBeforeSpec {
    shareVariables() //Needed 
    filledGrid = new Grid(Seq.range(0,81) map{Some(_)})
    emptyGrid = new Grid(Seq.fill(81){None})

    randomGrid = new Grid(Seq.fill(81){ 
      if(Random.nextBoolean)
        Some(Random.nextInt(9))
      else
        None
    })
  }

  "Grid's `getRow`" should {
    "return a Seq[Option[Int]] with 9 elements for i from 0 to 8" in {
      for(i <- 0 to 8)
        filledGrid.getRow(i) must haveSize(9)
    }

    "throw an exception for 0 > i > 9" in {
      filledGrid.getRow(-1) must throwAn[IllegalArgumentException]
      filledGrid.getRow(9) must throwAn[IllegalArgumentException]
    }
  }

  "Grid's `getColumn`" should {
    "return a Seq[Option[Int]] with 9 elements for i from 0 to 8" in {
      for(i <- 0 to 8)
        filledGrid.getColumn(i) must haveSize(9)
    }

    "throw an exception for 0 > i > 9" in {
      filledGrid.getColumn(-1) must throwAn[IllegalArgumentException]
      filledGrid.getColumn(9) must throwAn[IllegalArgumentException]
    }
  }

  "Grid's `apply`" should {
    "return either Some[Int] or None for x,y between 0 and 8" in {
      for(x <- 0 to 8; y <- 0 to 8)
        randomGrid(x, y) must (beNone or beSome[Int])
    }

    "throw an exception for 0 > x||y > 8" in {
      val illegalArgs = Seq(-1, 9)
      for(x <- illegalArgs; y <- illegalArgs)
        randomGrid(x,y) must throwAn[IllegalArgumentException]
    }
  }
  
  "Grid's companion object" should {
    "be able to construct a Grid from a list of ints" in {
      Grid.fromSeq(Seq.iterate(0,81){_+1}) must notBeNull
    }

    "throw an exception when called with a seq with other than 81 elements" in {
      Grid.fromSeq(Seq(1,2,3)) must throwA[java.lang.Exception]
    }

    "be able to construct a Grid from a Seq of lines" in {
      val lines = Seq.tabulate(9,9){(a,b) => Some(a+1)}
      val grid = Grid.fromLines(lines)
      val oneToNine = Seq.range(1,10).map{Some(_)}

      for(i <- 0 to 8) {
        grid.getRow(i) mustEqual lines(i)
        grid.getColumn(i) mustEqual oneToNine
      } 
    }

    "be able to construct a Grid from a SudoCue file" in {
      val url = getClass.getResource("/sudoku1.sdk")
      val grid = Grid.fromURL(url)

      //The sudoku has 45 empty fields
      grid.emptyFields mustEqual 45
      //The sudoku has a 2 at (0/0)
      grid(0,0) mustEqual Some(2)
      //The sudoku has a 1 at (8/8)
      grid(8,8) mustEqual Some(1)
    }
  }

  "A Grid constructed with no arguments" should {
    "have 81 empty fields" in {
      Grid().emptyFields mustEqual 81
    }
  }

  "A Grid constructed with a seq of Ints" should {
    
  }
}
