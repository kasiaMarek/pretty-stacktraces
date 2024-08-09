package org.virtuslab.stacktraces.util
import coursierapi.Fetch
import coursierapi.Dependency
import scala.collection.JavaConverters.asScalaBufferConverter
import java.nio.file.FileSystems
import java.net.URI
import java.nio.file.Paths

object StdClasspath:
  def apply() =
    lazy val currentClassPath = System.getProperty("java.class.path").split(":").map(Paths.get(_)).toList
    def scalaLib() =
      Fetch.create().addDependencies(
        //Dependency.of("org.scala-lang", "scala3-library_3", "3.4.2"),
        Dependency.of("org.scala-lang", "scala2-library-tasty-experimental_3", "3.4.2")
      ).fetch().asScala.map(_.toPath).toList
    def javaLib() =
      def fromJRT = FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules/java.base")
      def fromBootCP =
        for
          bootClasspath <- Option(System.getProperty("sun.boot.class.path"))
          rtJars <- Some(bootClasspath.split(java.io.File.pathSeparatorChar).toList.filter { path =>
            path.endsWith("rt.jar") || path.endsWith("jce.jar")})
        yield rtJars.map(Paths.get(_))
      fromBootCP.getOrElse(List(fromJRT))
    // println(currentClassPath)
    currentClassPath ::: scalaLib() ::: javaLib()
