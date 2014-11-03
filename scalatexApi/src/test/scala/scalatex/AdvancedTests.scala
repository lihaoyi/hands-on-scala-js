//package scalatex
//
//import utest._
//import scalatex.stages._
//import scalatags.Text.all._
//
//
///**
//* Created by haoyi on 7/14/14.
//*/
//object AdvancedTests extends TestSuite{
//  import TestUtil._
//
//  val tests = TestSuite{
//    'localDef{
//      check(
//        tw("""
//          @lol(n: Int) = @{
//            "omg" * n
//          }
//
//          @lol(2)
//        """),
//        "omgomg"
//      )
//    }
//    'innerTemplate{
//      check(
//        tw("""
//          @lol(f: Int) =
//            omg @f
//
//          @lol(1)
//          @lol(2: Int)
//          @lol(3 + 1)
//        """),
//        tw("""
//          @lol(f: Int) ={
//            omg @f
//          }
//          @lol(1)
//          @lol(2: Int)
//          @lol(3 + 1)
//        """),
//        tw("""
//          @lol(f: Int) = {
//            omg @f
//          }
//          @lol(1)
//          @lol(2: Int)
//          @lol(3 + 1)
//        """),
//        """
//        omg1omg2omg4
//        """
//      )
//    }
//    'innerInnerTemplate{
//      check(
//        tw("""
//          @lol(f: Int) =
//            @wtf(g: Int) =
//              wtf @g
//
//            @wtf(1 + 2 + 3)
//            @wtf(f)
//
//          @lol(1)
//          @lol(2: Int)
//          @lol(3 + 1)
//        """),
//        tw("""
//          @lol(f: Int) = {
//            @wtf(g: Int) = {
//              wtf @g
//            }
//            @wtf(1 + 2 + 3)
//            @wtf(f)
//          }
//          @lol(1)
//          @lol(2: Int)
//          @lol(3 + 1)
//            """),
//        tw("""
//          @lol(f: Int) = {
//            @wtf(g: Int) =
//              wtf @g
//
//            @wtf(1 + 2 + 3)
//            @wtf(f)
//          }
//          @lol(1)
//          @lol(2: Int)
//          @lol(3 + 1)
//        """),
//        tw("""
//          @lol(f: Int) =
//            @wtf(g: Int) = {
//              wtf @g
//            }
//            @wtf(1 + 2 + 3)
//            @wtf(f)
//
//          @lol(1)
//          @lol(2: Int)
//          @lol(3 + 1)
//        """),
//        """
//        wtf6
//        wtf1
//        wtf6
//        wtf2
//        wtf6
//        wtf4
//        """
//      )
//    }
//
//  }
//}
