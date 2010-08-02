package org.ensime.config

import java.lang.Character.isWhitespace
import java.io.{BufferedReader, File, FileInputStream, InputStreamReader, Reader, StringReader}
import java.net.{MalformedURLException, URL}
import java.util.regex.Pattern

class SbtConfig(val buildScalaVersion:String) extends NotNull{}
