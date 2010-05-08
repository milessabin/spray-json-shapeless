set PORT_FILE=%1
set CLASSPATH="lib/scala-library.jar;lib/scala-compiler.jar;lib/ensime.jar"
java -classpath %CLASSPATH% com.ensime.server.Server %PORT_FILE%