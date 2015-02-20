
structure HPDF_CompressionMode :
sig
  datatype t = HPDF_COMP_NONE
             | HPDF_COMP_TEXT
             | HPDF_COMP_IMAGE
             | HPDF_COMP_METADATA
             | HPDF_COMP_ALL
             | HPDF_COMP_MASK
  include BIT_FLAGS
end =
struct
  datatype t = HPDF_COMP_NONE
             | HPDF_COMP_TEXT
             | HPDF_COMP_IMAGE
             | HPDF_COMP_METADATA
             | HPDF_COMP_ALL
             (*
             | HPDF_COMP_BEST_COMPRESS
             | HPDF_COMP_BEST_SPEED
             *)
             | HPDF_COMP_MASK

  type flags = t

  fun toWord f : SysWord.word =
    case f
      of HPDF_COMP_NONE     => 0wx00
       | HPDF_COMP_TEXT     => 0wx01
       | HPDF_COMP_IMAGE    => 0wx02
       | HPDF_COMP_METADATA => 0wx04
       | HPDF_COMP_ALL      => 0wx0F
       | HPDF_COMP_MASK     => 0wxFF

  fun fromWord (w:SysWord.word) =
    case w
      of 0wx00 => HPDF_COMP_NONE
       | 0wx01 => HPDF_COMP_TEXT
       | 0wx02 => HPDF_COMP_IMAGE
       | 0wx04 => HPDF_COMP_METADATA
       | 0wx0F => HPDF_COMP_ALL
       | 0wxFF => HPDF_COMP_MASK
       | _ => raise Domain

  val all = HPDF_COMP_ALL

  fun flags fs = fromWord (foldl (fn (f,a)=> SysWord.orb(toWord f,a))
                                 (toWord HPDF_COMP_NONE)
                                 fs)

  fun intersect fs = fromWord (foldl (fn (f,a)=> SysWord.andb(toWord f,a))
                                     (toWord all)
                                     fs)

  fun clear (fl1, fl2) = fromWord(SysWord.andb(SysWord.notb (toWord fl1), toWord fl2))
  fun allSet (fl1, fl2) = SysWord.andb (toWord fl1, toWord fl2) = toWord fl1
  fun anySet (fl1, fl2) = SysWord.andb (toWord fl1, toWord fl2) <> 0w0
end

