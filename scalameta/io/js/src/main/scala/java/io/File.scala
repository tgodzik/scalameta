package java.io

import scala.meta.internal.io._

import java.net.URI
import java.nio.file.Path

// obtained implementation by experimentation on the JDK.
class File(path: String) {
  def this(parent: String, child: String) = this(parent + File.separator + child)
  def this(parent: File, child: String) = this(parent.getPath, child)
  def this(uri: URI) = this(
    if (uri.getScheme != "file") throw new IllegalArgumentException("URI scheme is not \"file\"")
    else uri.getPath
  )
  def toPath: Path = NodeNIOPath(path)
  def toURI: URI = {
    val file = getAbsoluteFile.toString
    val uripath = if (file.startsWith("/")) file else "/" + file.replace(File.separator, "/")
    val withslash = if (isDirectory && !uripath.endsWith("/")) uripath + "/" else uripath
    new URI("file", null, withslash, null)
  }
  def isAbsolute: Boolean = toPath.isAbsolute
  def getAbsoluteFile: File = toPath.toAbsolutePath.toFile
  def getAbsolutePath: String = getAbsoluteFile.toString
  def getParentFile: File = toPath.getParent.toFile
  def mkdirs(): Unit =
    throw new UnsupportedOperationException("mkdirs() is not supported in Scala.js")
  def getPath: String = path
  def getName: String = JSPath.basename(path)
  def exists(): Boolean = JSIO.exists(path)
  def isFile: Boolean = JSIO.isFile(path)
  def isDirectory: Boolean = JSIO.isDirectory(path)
  override def toString: String = path
}

object File {
  def listRoots(): Array[File] =
    Array(new File(if (JSIO.isNode) JSPath.parse(JSPath.resolve()).root else "/"))

  def separatorChar: Char = separator.charAt(0)

  def separator: String = if (JSIO.isNode) JSPath.sep else "/"

  def pathSeparator: String = if (JSIO.isNode) JSPath.delimiter else ":"
}
