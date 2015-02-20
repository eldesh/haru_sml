
(**
 * permission flag (only Revision 2 is supported)
 *)
structure HPDF_PermissionFlag =
struct
  datatype t = HPDF_ENABLE_READ
             | HPDF_ENABLE_PRINT
             | HPDF_ENABLE_EDIT_ALL
             | HPDF_ENABLE_COPY
             | HPDF_ENABLE_EDIT

  type flags = t

  fun toWord f : SysWord.word =
    case f
      of HPDF_ENABLE_READ     => 0w0
       | HPDF_ENABLE_PRINT    => 0w4
       | HPDF_ENABLE_EDIT_ALL => 0w8
       | HPDF_ENABLE_COPY     => 0w16
       | HPDF_ENABLE_EDIT     => 0w32

  fun fromWord (w:SysWord.word) =
    case w
      of 0w0  => HPDF_ENABLE_READ
       | 0w4  => HPDF_ENABLE_PRINT
       | 0w8  => HPDF_ENABLE_EDIT_ALL
       | 0w16 => HPDF_ENABLE_COPY
       | 0w32 => HPDF_ENABLE_EDIT
       | _ => raise Domain

end

