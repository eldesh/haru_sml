
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
  datatype z = datatype CompressionMode.t

  (* simplify enum type names *)
  structure TextRenderingMode = E__HPDF_TextRenderingMode
  structure WritingMode = E__HPDF_WritingMode
  structure WhenceMode = E__HPDF_WhenceMode
  structure TransitionStyle = E__HPDF_TransitionStyle
  structure TextAlignment = E__HPDF_TextAlignment
  structure StreamType = E__HPDF_StreamType
  structure StampAnnotName = E__HPDF_StampAnnotName
  structure PdfVer = E__HPDF_PdfVer
  structure PageSizes = E__HPDF_PageSizes
  structure PageNumStyle = E__HPDF_PageNumStyle
  structure PageMode = E__HPDF_PageMode
  structure PageLayout = E__HPDF_PageLayout
  structure PageDirection = E__HPDF_PageDirection
  structure PDFA_TYPE = E__HPDF_PDFA_TYPE
  structure NameDictKey = E__HPDF_NameDictKey
  structure LineJoin = E__HPDF_LineJoin
  structure LineCap = E__HPDF_LineCap
  structure LineAnnotEndingStyle = E__HPDF_LineAnnotEndingStyle
  structure LineAnnotCapPosition = E__HPDF_LineAnnotCapPosition
  structure InfoType = E__HPDF_InfoType
  structure FontType = E__HPDF_FontType
  structure FontDefType = E__HPDF_FontDefType
  structure EncryptMode = E__HPDF_EncryptMode
  structure EncodingType = E__HPDF_EncodingType
  structure EncoderType = E__HPDF_EncoderType
  structure DestinationType = E__HPDF_DestinationType
  structure ColorSpace = E__HPDF_ColorSpace
  structure ByteType = E__HPDF_ByteType
  structure BlendMode = E__HPDF_BlendMode
  structure BaseEncodings = E__HPDF_BaseEncodings
  structure BSSubtype = E__HPDF_BSSubtype
  structure AnnotType = E__HPDF_AnnotType
  structure AnnotIntent = E__HPDF_AnnotIntent
  structure AnnotIcon = E__HPDF_AnnotIcon
  structure AnnotHighlightMode = E__HPDF_AnnotHighlightMode
  structure AnnotFlgs = E__HPDF_AnnotFlgs

  (* publish enum constructors *)
  datatype z = datatype TextRenderingMode.mlrep
  datatype z = datatype WritingMode.mlrep
  datatype z = datatype WhenceMode.mlrep
  datatype z = datatype TransitionStyle.mlrep
  datatype z = datatype TextAlignment.mlrep
  datatype z = datatype StreamType.mlrep
  datatype z = datatype StampAnnotName.mlrep
  datatype z = datatype PdfVer.mlrep
  datatype z = datatype PageSizes.mlrep
  datatype z = datatype PageNumStyle.mlrep
  datatype z = datatype PageMode.mlrep
  datatype z = datatype PageLayout.mlrep
  datatype z = datatype PageDirection.mlrep
  datatype z = datatype PDFA_TYPE.mlrep
  datatype z = datatype NameDictKey.mlrep
  datatype z = datatype LineJoin.mlrep
  datatype z = datatype LineCap.mlrep
  datatype z = datatype LineAnnotEndingStyle.mlrep
  datatype z = datatype LineAnnotCapPosition.mlrep
  datatype z = datatype InfoType.mlrep
  datatype z = datatype FontType.mlrep
  datatype z = datatype FontDefType.mlrep
  datatype z = datatype EncryptMode.mlrep
  datatype z = datatype EncodingType.mlrep
  datatype z = datatype EncoderType.mlrep
  datatype z = datatype DestinationType.mlrep
  datatype z = datatype ColorSpace.mlrep
  datatype z = datatype ByteType.mlrep
  datatype z = datatype BlendMode.mlrep
  datatype z = datatype BaseEncodings.mlrep
  datatype z = datatype BSSubtype.mlrep
  datatype z = datatype AnnotType.mlrep
  datatype z = datatype AnnotIntent.mlrep
  datatype z = datatype AnnotIcon.mlrep
  datatype z = datatype AnnotHighlightMode.mlrep
  datatype z = datatype AnnotFlgs.mlrep

  datatype z = z (* dummy *)


  val HPDF_TRUE  : MLRep.Signed.int = 1
  val HPDF_FALSE : MLRep.Signed.int = 0

  val HPDF_OK      = 0
  val HPDF_NOERROR = 0

  fun Page_ShowText page text =
    use_cstring text (fn text =>
    F_HPDF_Page_ShowText.f'(page, text))

end (* local *)
end

