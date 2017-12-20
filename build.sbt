import Dependencies._

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "io.github.yannick-cw",
      scalaVersion := "2.12.4",
      version := "0.1.0",
      scalafmtVersion := "1.0.0-RC4",
      scalafmtOnCompile := true,
      scalacOptions := compilerFlags
    )),
  name := "http4s-cache",
  libraryDependencies ++= Seq(http4s, scalaTest % Test, scalaCacheCaffeine % Test) ++ scalaCache
)
