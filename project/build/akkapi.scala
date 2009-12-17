import sbt._

class AkkaPiProject(info: ProjectInfo) extends DefaultProject(info) {
  //  val nexus=
  val mavenLocal = "Local Maven Repository" at "file://" + Path.userHome + "/.m2/repository"
  val javaNet = "Java net Repository" at "http://download.java.net/maven/2/"
}