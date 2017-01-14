
// publish to bintray.com via: `sbt publish`
publishTo := Some(
  "TIWG" at
    s"https://api.bintray.com/content/tiwg/org.omg.tiwg/${moduleName.value}/${version.value}")

PgpKeys.useGpg := true

PgpKeys.useGpgAgent := true

pgpSecretRing := file("local.secring.gpg")

pgpPublicRing := file("local.pubring.gpg")

git.baseVersion := Versions.version

versionWithGit
