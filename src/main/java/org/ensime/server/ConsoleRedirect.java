package org.ensime.server;

import java.io.OutputStream;

@SuppressWarnings("deprecation")
class ConsoleRedirect {
    public static void setOut(OutputStream out) {
        scala.Console$.MODULE$.setOut(out);
    }

    public static void setErr(OutputStream err) {
        scala.Console$.MODULE$.setErr(err);
    }
}
