package me.rémi.s99

import org.specs2.mutable._

class TestS1 extends SpecificationWithJUnit {

    import S199List._

    "prob 1" should {

        "last of list" in {
            val xs = List(1,2,3)

            lastList(xs) mustEqual 3
            lastListMatch(xs) mustEqual 3
        }
    }
}
