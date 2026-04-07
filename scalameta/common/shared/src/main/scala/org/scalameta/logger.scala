package org.scalameta

class FileLine(val fileValue: String, val lineValue: Int) extends Ordered[FileLine] {

  @deprecated("Use the other constructor with raw values", "4.16.0")
  def this(file: sourcecode.File, line: sourcecode.Line) = this(file.value, line.value)
  
  def file = sourcecode.File(fileValue)
  def line = sourcecode.Line(lineValue)
  
  override def toString: String = {
    val shortFilename = fileValue.replaceAll("(.*/|\\.scala)", "")
    Console.GREEN + s"$shortFilename:${lineValue}" + Console.RESET
  }
  override def compare(that: FileLine): Int = {
    val cmp = this.fileValue.compareTo(that.fileValue)
    if (cmp != 0) cmp else this.lineValue.compare(that.lineValue)
  }
  override def equals(obj: Any): Boolean = obj match {
    case that: FileLine => (that eq this) ||
      lineValue == that.lineValue && fileValue == that.fileValue
    case _ => false
  }
  override def hashCode(): Int = fileValue.## ^ lineValue.##
}

object FileLine {
  implicit def generate(implicit file: sourcecode.File, line: sourcecode.Line): FileLine =
    new FileLine(file.value, line.value)
}

object logger {
  
  /** Same as println except includes the file+line number of call-site. */
  def debug(x: Any)(implicit fileLine: FileLine): Unit = println(s"$fileLine $x")

  /** Replaces whitespace characters with non-whitespace characters */
  def revealWhitespace(s: String): String = s.map {
    case '\t' => '†'
    case '\n' => '¶'
    case ' ' => '∙'
    case ch => ch
  }
  

  /**
   * Prints out the value with and it's source code representation
   *
   * Example: logger.elem(x) // prints "MyFile:24 [x]: 42"
   */
  def elem(values: sourcecode.Text[Any]*)(implicit fileLine: FileLine): Unit = values.foreach { t =>
    val value = {
      val str = s"${t.value}"
      if (str.contains("\n")) s"\n$str" else str
    }
    println(s"$fileLine [${t.source}]: $value")
  }
}
