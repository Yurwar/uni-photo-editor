ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "uni-photo-editor",
    idePackagePrefix := Some("com.yurwar.uni.photo.editor")
  )
