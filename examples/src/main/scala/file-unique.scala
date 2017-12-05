/*-enable-unique*/

import java.io.File

import scala.annotation.{unique, transient}
import scala.annotation.Unique.share

@transient
class LogFile(parent: File, child: String) extends File(parent, child) {
  var toDelete = false
}

trait FileManager {

  def basename(name: String): String = {
    val inx = name.lastIndexOf(".")
    if (inx < 0) name else name.substring(0, inx)
  }

  def getLogFile(dir: File, fileBase: String, kind: String): LogFile @unique = {
    val name = fileBase + "-" + kind + ".log"
    val file: LogFile @unique = new LogFile(dir, name)
    file
  }

  def getLogFile(file: File, kind: String): LogFile @unique = {
    val dir = file.getParentFile
    val fileBase = basename(file.getName)
    getLogFile(dir, fileBase, kind)
  }

}

object Test extends FileManager {

  def main(args: Array[String]) {
    val file = new File("tmp")
    val f: LogFile @unique = getLogFile(file, "pos")
  }
}
