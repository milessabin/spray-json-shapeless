package org.ensime.config

import java.io.File

import org.ensime.util._

import scala.concurrent.Promise

class SExpExploror {

}

class LateSet[T] {
  var value: Option[T] = None

  val promise = Promise[T]()
  def apply() = this.synchronized {
    value.getOrElse(throw new IllegalStateException("value is not set"))
  }

  def set(v: T): Unit = this.synchronized {
    if (value == None)
      value = Some(v)
    else
      throw new IllegalStateException("Value already set")
  }
}

class EnsimeConfig(val ensimeFile: File, val rootDir: File, val cacheDir: File, configExp: SExp) {

  val (name: String, scalaVersion: String, modules: Map[String, EnsimeModule], referenceSourceRoots: List[String]) = {
    val rootMap = SExpExplorer(configExp).asMap

    val name = rootMap.getString(":name")
    val scalaVersion = rootMap.getString(":scala-version")

    val referenceSourceRoots = rootMap.getStringList(":reference-source-roots")
    val subProjectsSExps: List[SExpMapExplorer] = rootMap.getList(":subprojects").map(_.asMap)

    val subModules = subProjectsSExps.map { entry =>
      val moduleName = entry.getString(":name")
      val target = entry.getString(":target")
      val testTarget = entry.getString(":test-target")
      val dependentModuleNames = entry.getStringList(":depends-on-modules")
      val compilerDeps = entry.getStringListOpt(":compiler-deps").getOrElse(List())
      val runtimeDeps = entry.getStringList(":runtime-deps")
      val testDeps = entry.getStringList(":test-deps")
      val sourceRoots = entry.getStringList(":source-roots")
      val referenceSourceRoots = entry.getStringList(":reference-source-roots")

      moduleName -> new EnsimeModule(moduleName, target, testTarget, dependentModuleNames,
        compilerDeps, runtimeDeps, testDeps, sourceRoots, referenceSourceRoots)
    }.toMap

    (name, scalaVersion, subModules, referenceSourceRoots)
  }

  def getModule(name: String): EnsimeModule =
    modules.getOrElse(name, throw new IllegalStateException("Module " + name + " does not exist"))

  case class EnsimeModule(name: String,
      target: String,
      testTarget: String,
      dependsOnNames: List[String],
      compilerDependencies: List[String],
      runtimeDependencies: List[String],
      testDependencies: List[String],
      sourceRoots: List[String],
      referenceSourceRoots: List[String]) {

    val config = EnsimeConfig.this
    lazy val dependsOnModules: List[config.EnsimeModule] = dependsOnNames.map(name => config.getModule(name))
  }

}
