
enablePlugins(ScalaJSPlugin)

name := "DnDSheetJS"
scalaVersion := "2.13.1"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

scalacOptions += "-feature"
scalacOptions += "-deprecation"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"

libraryDependencies += "io.udash" %%% "udash-jquery" % "3.0.3"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.8.6"
