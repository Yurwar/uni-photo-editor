ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies += "org.jfree" % "jfreechart" % "1.5.3"

lazy val root = (project in file("."))
  .settings(
    name := "uni-photo-editor",
    idePackagePrefix := Some("com.yurwar.uni.photo.editor")
  )
