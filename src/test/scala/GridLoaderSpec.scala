import com.github.thekenny.Sudoku._

import org.specs._

object GridLoaderSpec extends Specification {
  "SudoCue-Loading" should {
    "be able to construct a Grid from an url to a file" in {
      val url = getClass.getResource("/sudoku1.sdk")
      val grid: Grid = GridLoader.loadDotSdk(url)

      //The sudoku has 45 empty fields
      grid.emptyFields mustEqual 45
      //The sudoku has a 2 at (0/0)
      grid(0,0) mustEqual Some(2)
      //The sudoku has a 1 at (8/8)
      grid(8,8) mustEqual Some(1)
    }
    
    "be able to construct a Grid from a .sdk file" in {
  
    }
  }
}
