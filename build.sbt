lazy val commonSettings = Seq(
  organization               := "com.alexknvl",
  version                    := "0.0.1-SNAPSHOT",
  scalaVersion := "2.12.3"
)

lazy val library = project.in(file("library"))
  .settings(name := "primo")
  .settings(commonSettings:_*)

lazy val plugin = project.in(file("plugin"))
  .settings(name := "primo-plugin")
  .settings(commonSettings:_*)
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect"  % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  ))

lazy val examples = project.in(file("examples"))
  .settings(name := "primo-examples")
  .settings(commonSettings:_*)
  .dependsOn(library)
  .settings(scalacOptions ++= {
    val jar = (packageBin in Compile in plugin).value
    Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}") // ensures recompile
  })

lazy val root = project.in( file(".") )
  .settings( publishArtifact := false )
  .aggregate(plugin, library, examples)
  .settings(commonSettings : _*)