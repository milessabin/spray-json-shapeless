set PORT_FILE=%1
set BOOTCLASSPATH=<RUNTIME_BOOTCLASSPATH>
set CLASSPATH=<RUNTIME_CLASSPATH>
if not defined ENSIME_JVM_ARGS (set ENSIME_JVM_ARGS=-Xms256M -Xmx1512M -XX:PermSize=128m -Xss1M -Dfile.encoding=UTF-8)
java -Xbootclasspath/a:%BOOTCLASSPATH% -classpath %CLASSPATH% %ENSIME_JVM_ARGS% org.ensime.server.Server %PORT_FILE%
