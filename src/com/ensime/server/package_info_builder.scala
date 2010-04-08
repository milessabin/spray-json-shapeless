package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import com.ensime.server.model.PackageInfo


object PackageInfoBuilder {

  def build(nsc:Global, path:String) = {
    new PackageInfo("ROOT", List())
  }

}

