package example

import scala.io.Source

object FirstDay {
    def measurements(): Int = {
        def measurements = Source.fromResource("first.txt").getLines().toList
           .map(_.toInt)

        def measurementsPaired = measurements zip measurements.tail

        measurementsPaired.count(it => it._1 < it._2)
    }

    def measurementsWindows(): Int = {
        def measurements = Source.fromResource("first.txt").getLines().toList
           .map(_.toInt)
           .sliding(3)
           .map(it => it.sum)
           .toList

        def measurementsPaired = measurements zip measurements.tail

        measurementsPaired.count(it => it._1 < it._2)
    }
}
