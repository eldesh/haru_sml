
structure Hpdf =
struct
local
  fun using mk release f =
    let
      val resource = mk()
      val r = f resource
              handle exn => (release resource; raise exn)
    in
      r before release resource
    end

  fun use_cstring (s:string) f =
    using (fn()=> ZString.dupML' s)
          C.free'
          f
in
  structure CompressionMode = CompressionMode
  structure TextRenderingMode = E__HPDF_TextRenderingMode

  val HPDF_TRUE  : MLRep.Signed.int = 1
  val HPDF_FALSE : MLRep.Signed.int = 0

  val HPDF_OK      = 0
  val HPDF_NOERROR = 0

  fun Page_ShowText page text =
    use_cstring text (fn text =>
    F_HPDF_Page_ShowText.f'(page, text))

end (* local *)
end

