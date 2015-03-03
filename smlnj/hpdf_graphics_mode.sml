
structure HPDF_GraphicsMode :>
sig
  datatype t = HPDF_GMODE_PAGE_DESCRIPTION
             | HPDF_GMODE_PATH_OBJECT
             | HPDF_GMODE_TEXT_OBJECT
             | HPDF_GMODE_CLIPPING_PATH
             | HPDF_GMODE_SHADING
             | HPDF_GMODE_INLINE_IMAGE
             | HPDF_GMODE_EXTERNAL_OBJECT

  val toWord   : t -> MLRep.Unsigned.word
  val fromWord : MLRep.Unsigned.word -> t list
end =
struct
  open MLRep.Unsigned
  (* Graphis mode *)
  datatype t = HPDF_GMODE_PAGE_DESCRIPTION
             | HPDF_GMODE_PATH_OBJECT
             | HPDF_GMODE_TEXT_OBJECT
             | HPDF_GMODE_CLIPPING_PATH
             | HPDF_GMODE_SHADING
             | HPDF_GMODE_INLINE_IMAGE
             | HPDF_GMODE_EXTERNAL_OBJECT

  fun toWord f : word =
    case f
      of HPDF_GMODE_PAGE_DESCRIPTION => 0wx0001
       | HPDF_GMODE_PATH_OBJECT      => 0wx0002
       | HPDF_GMODE_TEXT_OBJECT      => 0wx0004
       | HPDF_GMODE_CLIPPING_PATH    => 0wx0008
       | HPDF_GMODE_SHADING          => 0wx0010
       | HPDF_GMODE_INLINE_IMAGE     => 0wx0020
       | HPDF_GMODE_EXTERNAL_OBJECT  => 0wx0040

  fun fromWord (w:word) =
    List.filter (fn f=> andb(toWord f,w) <> 0w0)
       [ HPDF_GMODE_PAGE_DESCRIPTION
       , HPDF_GMODE_PATH_OBJECT
       , HPDF_GMODE_TEXT_OBJECT
       , HPDF_GMODE_CLIPPING_PATH
       , HPDF_GMODE_SHADING
       , HPDF_GMODE_INLINE_IMAGE
       , HPDF_GMODE_EXTERNAL_OBJECT
       ]

end

