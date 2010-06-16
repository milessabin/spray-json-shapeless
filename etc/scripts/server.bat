set PORT_FILE=%1
set CLASSPATH=<RUNTIME_CLASSPATH>
java -classpath %CLASSPATH% com.ensime.server.Server %PORT_FILE%