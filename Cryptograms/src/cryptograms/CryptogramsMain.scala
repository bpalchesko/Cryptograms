package cryptograms


object CryptogramsMain {
  
  def main(args: Array[String]): Unit = {
     val text = "If at first you don't succeed try and try again"
     val code = "qwertyuiopasdfghjklzxcvbnm"
     val encodedText = Dave.encode(text, code)
     var discoveredCode = Dave.discoverObject(encodedText)
     println(encodedText)
     println(code.length)
     println(discoveredCode.length)
     println(discoveredCode)
     println(Dave.decode(encodedText, discoveredCode))
     
  }
  
}