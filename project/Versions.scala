
import sbt.Process

object Versions {
  val scala = "2.11.7"
  
  val version_prefix = "0.19.0"
  val version_suffix = {
    val svnProc = Process(command = "svn", arguments = Seq("info"))
    val sedCommand = "s/^.*Last Changed Rev: \\([[:digit:]]\\{1,\\}\\).*$/\\1/p"
    val sedProc = Process(command = "sed", arguments = Seq("-n", sedCommand))
    val svnRevision = svnProc.#|(sedProc).!!.trim
    svnRevision
  }
  
  // OTI Trees version

  val version = version_prefix + "-" + version_suffix
  
  // OTI Core version
    
  val oti_core_prefix = version_prefix
  val oti_core_suffix = "445025"
  val oti_core_version = oti_core_prefix+"-"+oti_core_suffix

  // OTI Canonical XMI version
    
  val oti_canonicalXMI_prefix = version_prefix
  val oti_canonicalXMI_suffix = "445043"
  val oti_canonicalXMI_version = oti_canonicalXMI_prefix+"-"+oti_canonicalXMI_suffix
}
