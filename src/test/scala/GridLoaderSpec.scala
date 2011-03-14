import com.github.thekenny.Sudoku._

import org.specs._
import scala.io.Source

object GridLoaderSpec extends Specification {
  def verifySudoku1(s1: Grid) {
      //The sudoku has 45 empty fields
      s1.emptyFields mustEqual 45
      //The sudoku has a 2 at (0/0)
      s1(0,0) mustEqual Some(2)
      //The sudoku has a 1 at (8/8)
      s1(8,8) mustEqual Some(1)
  }
  
  "SudoCue-Loading" should {
    "be able to construct a Grid from an url to a file" in {
      val url = getClass.getResource("/sudoku1.sdk")
      val grid: Grid = GridLoader.loadDotSdk(url)

      verifySudoku1(grid)
    }
  }

  "SudokuPuzzleCollection-Loading" should {
    "be able to construct some Grids from a .sdm file" in {
      val url = getClass.getResource("/sudoku-collection.sdm")
      val grids: Seq[Grid] = GridLoader.loadDotSdm(url)
      val lines: Seq[String] = Source.fromURL(url).getLines.filter((l) => {
        l != null && l.length == 81
      }) toList

      //all vals stripped of Nothing in grid.data must be equal to the 
      //corresponding number in grids stripped of '0's
      for(i <- 0 to grids.length-1) {
        lines(i).filter{_ != '0'}.map{_.toString.toInt} mustEqual
        grids(i).data.flatten
      }
    }
  }

  "SimpleSudoku-Loading" should {
    "be able to construct a Grid from a .ss file" in {
      val url = getClass.getResource("/sudoku1.ss")
      val grid: Grid = GridLoader.loadDotSs(url)

      verifySudoku1(grid)
    }
  }
}
