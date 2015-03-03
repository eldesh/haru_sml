
structure HPDF_CompressionMode :>
sig
  datatype t = HPDF_COMP_NONE
             | HPDF_COMP_TEXT
             | HPDF_COMP_IMAGE
             | HPDF_COMP_METADATA

  val HPDF_COMP_ALL : t list
  val toWord   : t -> MLRep.Unsigned.word
  val fromWord : MLRep.Unsigned.word -> t list
end =
struct
  open MLRep.Unsigned

  datatype t = HPDF_COMP_NONE
             | HPDF_COMP_TEXT
             | HPDF_COMP_IMAGE
             | HPDF_COMP_METADATA
             (*
             | HPDF_COMP_BEST_COMPRESS
             | HPDF_COMP_BEST_SPEED
             *)

  val HPDF_COMP_ALL =
        [ HPDF_COMP_TEXT
        , HPDF_COMP_IMAGE
        , HPDF_COMP_METADATA
        ]

  fun toWord f : word =
    case f
      of HPDF_COMP_NONE     => 0wx00
       | HPDF_COMP_TEXT     => 0wx01
       | HPDF_COMP_IMAGE    => 0wx02
       | HPDF_COMP_METADATA => 0wx04

  fun fromWord (w:word) =
    List.filter (fn f=> andb(toWord f,w) <> 0w0)
           [ HPDF_COMP_NONE
           , HPDF_COMP_TEXT
           , HPDF_COMP_IMAGE
           , HPDF_COMP_METADATA
           ]
end

