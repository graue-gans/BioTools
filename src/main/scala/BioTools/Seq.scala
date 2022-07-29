package BioTools

class Seq(val sequence: String = "") {
  /** Returns complement for sequence (String) of bases */
  def complement(seqType: SeqType = SeqType.DNA): String =
    seqType match
      case SeqType.DNA | SeqType.RNA =>
        sequence.map(c => c match
          case 'A' => if (seqType == SeqType.DNA) 'T' else 'U'
          case 'C' => 'G'
          case 'G' => 'C'
          case 'T' | 'U' => 'A')
      case SeqType.AA | SeqType.PROTEIN => sequence  // maybe throw error in future

  def reverseComplement(): String =
    complement().foldLeft("")((x, y) => y + x)

  enum SeqType:
    case DNA, RNA, AA, PROTEIN
}
