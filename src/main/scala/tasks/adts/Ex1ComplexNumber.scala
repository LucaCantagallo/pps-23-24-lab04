package tasks.adts

package u04lab
import scala.math.abs

import tasks.adts.u04lab.Ex1ComplexNumbers.ComplexADT

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    private case class ComplexImpl(re: Double, im: Double)

    // Change assignment below: should probably define a case class and use it?
    opaque type Complex = ComplexImpl
    def complex(re: Double, im: Double): Complex = ComplexImpl(re: Double, im: Double)
    extension (complex: Complex)
      def re(): Double = complex match
        case ComplexImpl(re, _) => re
      def im(): Double = complex match
        case ComplexImpl(_, im) => im
      def sum(other: Complex): Complex = (complex, other) match
        case (ComplexImpl(re, im), ComplexImpl(re2, im2)) => ComplexImpl(re + re2, im + im2)
      def subtract(other: Complex): Complex = (complex, other) match
        case (ComplexImpl(re, im), ComplexImpl(re2, im2)) => ComplexImpl(re - re2, im - im2)
      def asString(): String = complex match
        case ComplexImpl(re, im) if im == 0.0=> re+""
        case ComplexImpl(re, im) if re == 0.0 => im+"i"
        case ComplexImpl(re, im) if im < 0.0 => re+" - " + abs(im) + "i"
        case ComplexImpl(re, im) => re+" + "+im+"i"
