name         := "InterPlay"
organization := "de.sciss"
scalaVersion := "2.11.8"
licenses     := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies ++= Seq(
  "de.sciss" %% "wolkenpumpe"          % "0.34",
  "de.sciss" %% "fscapejobs"           % "0.17",
  "de.sciss" %% "scalacolliderswing"   % "0.34"
)
