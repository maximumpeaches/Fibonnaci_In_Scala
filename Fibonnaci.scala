object Fibonnaci {
	def fibonnaci(count : Int) : Int = {
		def f(prev : Int, curr : Int, count : Int) : Int = {
			if (count == 0)
				curr
			else
				f(curr, prev + curr, count - 1)
		}
		if (count == 1) 1
		else if (count == 2) 1
		else f(1, 1, count - 2)
	}

	private def formatFib(value : Int) : String = {
		"The fibonacci of %d is %d".format(value, fibonnaci(value))
	}

	private def printFib(value: Int) : Unit = {
		println(formatFib(value))
	}

	def loopPrintFib(value: Int) : Unit = {
		def printOneToValue(curr: Int, value: Int) : Unit = {
			if (curr == value) printFib(curr)
			else {
				printFib(curr)
				printOneToValue(curr + 1, value)
			}
		}
		printOneToValue(1, value)
	}

	def main(args: Array[String]) : Unit = {
		loopPrintFib(8)
	}
}
