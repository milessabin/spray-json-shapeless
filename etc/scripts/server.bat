set PORT_FILE=%1
set CLASSPATH=<RUNTIME_CLASSPATH>
if "%ENSIME_JVM_ARGS%"=="" (set ENSIME_JVM_ARGS=-XX:+DoEscapeAnalysis -Xms256M -Xmx1512M -XX:PermSize=128m -Xss1M -Dfile.encoding=UTF-8)
java -classpath %CLASSPATH% %ENSIME_JVM_ARGS% org.ensime.server.Server %PORT_FILE%
