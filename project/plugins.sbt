resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Scala Code Coverage-github-repository" at "http://mtkopone.github.com/scct/maven-repo"
)

//Scala Uniform
addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

//Scoverage
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")