package ch.isbsib.proteomics.mzviz.theoretical.models

/**
 * Created by tmartinc on 21/11/14.
 * @author Trinidad Martín
 */
class Fasta (acParam: String, seqParam: String) {

  var ac= acParam
  var seq= seqParam
  val version="1.0"
  println ("Ac received is " + ac + " and the sequence is: ")
  println(seq)

}
