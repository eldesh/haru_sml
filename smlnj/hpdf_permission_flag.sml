
(**
 * permission flag (only Revision 2 is supported)
 *)
structure HPDF_PermissionFlag :>
sig
  datatype t = HPDF_ENABLE_READ
             | HPDF_ENABLE_PRINT
             | HPDF_ENABLE_EDIT_ALL
             | HPDF_ENABLE_COPY
             | HPDF_ENABLE_EDIT

  val toWord   : t -> MLRep.Unsigned.word
  val fromWord : MLRep.Unsigned.word -> t list
end =
struct
  open MLRep.Unsigned

  datatype t = HPDF_ENABLE_READ
             | HPDF_ENABLE_PRINT
             | HPDF_ENABLE_EDIT_ALL
             | HPDF_ENABLE_COPY
             | HPDF_ENABLE_EDIT

  fun toWord f : word =
    case f
      of HPDF_ENABLE_READ     => 0w0
       | HPDF_ENABLE_PRINT    => 0w4
       | HPDF_ENABLE_EDIT_ALL => 0w8
       | HPDF_ENABLE_COPY     => 0w16
       | HPDF_ENABLE_EDIT     => 0w32

  fun fromWord (w:word) =
    List.filter (fn f=> andb(toWord f,w) <> 0w0)
        [ HPDF_ENABLE_READ
        , HPDF_ENABLE_PRINT
        , HPDF_ENABLE_EDIT_ALL
        , HPDF_ENABLE_COPY
        , HPDF_ENABLE_EDIT
        ]

end

