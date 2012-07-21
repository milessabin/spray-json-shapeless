package org.ensime.util
import java.io._

object ProcessUtil {

  def readAllOutput(proc: Process, outputWriter: Writer, errorWriter: Writer): Either[Throwable, Int] = {
    try {
      val outputStream = proc.getInputStream()
      val outputReaderThread = new StreamReaderThread(outputStream, outputWriter)

      val errorStream = proc.getErrorStream()
      val errorReaderThread = new StreamReaderThread(errorStream, errorWriter)

      errorReaderThread.start()
      outputReaderThread.start()
      val exitVal = proc.waitFor()
      errorReaderThread.join()
      outputReaderThread.join()
      errorStream.close()
      outputStream.close()
      Right(exitVal)
    } catch {
      case  t: Throwable => Left(t)
    }
  }


  def readAllOutputToStrings(proc: Process): Either[Throwable, (String,String)] = {
    val outputWriter = new StringWriter()
    val errorWriter = new StringWriter()
    readAllOutput(proc, outputWriter, errorWriter) match{
      case Left(t) => Left(t)
      case Right(_) => Right(outputWriter.toString(),errorWriter.toString())
    }
  }

  def readAllOutputToConsole(proc: Process): Either[Throwable, Int] = {
    readAllOutput(proc, new PrintWriter(System.out), new PrintWriter(System.err)) match{
      case Left(t) => Left(t)
      case Right(status) => Right(status)
    }
  }


  def run(proc: Process): Either[Throwable, Int] = {
    try {
      val exitVal = proc.waitFor()
      Right(exitVal)
    } catch {
      case  t: Throwable => Left(t)
    }
  }


  class StreamReaderThread(is: InputStream, ow: Writer) extends Thread {
    override def run() {
      try{
	val isr = new InputStreamReader(is);
	val br = new BufferedReader(isr);
	var line:String = br.readLine()
	while (line != null) {
	  ow.write(line + "\n")
	  ow.flush()
	  line = br.readLine()
	}
      } catch {
	case e : Throwable => e.printStackTrace()
      }
    }

  }

}
