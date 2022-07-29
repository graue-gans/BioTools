package BioTools

class SeqIO {
  def parse(fileName: String, fileType: FileType): Array[Rec] =
    var id  = ""
    var seq = ""
    val bufferedSource = scala.io.Source.fromFile(fileName)
    val output = scala.collection.mutable.ArrayBuffer.empty[Rec]
    for (s <- bufferedSource.getLines())
      s.head match
        case '>' =>
          if (seq.trim.nonEmpty)
            output += Rec(id, seq)
            id = ""
          id += s
        case _ => seq += s
    bufferedSource.close()
    output.toArray

  enum FileType:
    case fasta
}


class Rec(id: String, seq: String) {
  def length: Int = seq.length
}
