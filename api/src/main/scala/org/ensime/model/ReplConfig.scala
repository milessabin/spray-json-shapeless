package org.ensime.model

import java.io.File

// bit of a rubbish class
// SH -- agreed. The client should be able to do all this without asking the server.
case class ReplConfig(classpath: Set[File])
