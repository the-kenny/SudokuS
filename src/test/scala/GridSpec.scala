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

    "be able to create an empty object" in {
      Grid().emptyFields mustEqual 81
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

  "Grid's `getBlock(x,y)`" should {
    "throw an exception for 3 > x,y >= 0" in {
      val illegalArgs = Seq(-1, 3)
      for(x <- illegalArgs; y <- illegalArgs)
        randomGrid.getBlock(x,y) must throwA[IllegalArgumentException]
    }

    "return a Seq[Option[Int]] with 9 elements for every x,y from 0 to 2" in {
      for(x <- 0 to 2; y <- 0 to 2)
        randomGrid.getBlock(x,y) must haveSize(9)
    }

    "return the correct number sequence for the block" in {
      filledGrid.getBlock(0,0) must ==(Seq(0,1,2,9,10,11,18,19,20).map{Some(_)})
    }
  }
}
