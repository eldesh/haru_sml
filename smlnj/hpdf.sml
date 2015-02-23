
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

  fun b2i true  : MLRep.Signed.int = 1
    | b2i false : MLRep.Signed.int = 0
in
  val HPDF_TRUE  : MLRep.Signed.int = 1
  val HPDF_FALSE : MLRep.Signed.int = 0

  (*
  val HPDF_OK      = 0
  val HPDF_NOERROR = 0
  *)

  structure CompressionMode = HPDF_CompressionMode
  structure PermissionFlag = HPDF_PermissionFlag
  structure ViewerPreference = HPDF_ViewerPreference
  structure Status = HPDF_Status

  datatype z = datatype CompressionMode.t
  datatype z = datatype PermissionFlag.t
  datatype z = datatype ViewerPreference.t
  datatype z = datatype Status.t


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

  structure Doc =
  struct
    fun New (error, data) =
      F_HPDF_New.f'(error, data)

    fun Free doc =
      F_HPDF_Free.f' doc

    fun NewDoc pdf =
      F_HPDF_NewDoc.f' pdf

    fun FreeDoc pdf =
      F_HPDF_NewDoc.f' pdf

    fun FreeDocAll pdf =
      F_HPDF_FreeDocAll.f' pdf

    fun HasDoc pdf =
      F_HPDF_HasDoc.f' pdf <> 0

    fun SaveToFile (pdf, file_name) =
      use_cstring file_name (fn file_name =>
      F_HPDF_SaveToFile.f'(pdf, file_name))

    fun GetError pdf =
      Status.fromWord (F_HPDF_GetError.f' pdf)

    fun GetErrorDetail pdf =
      Status.fromWord (F_HPDF_GetErrorDetail.f' pdf)

    fun ResetError pdf =
      F_HPDF_ResetError.f' pdf

    fun SetPagesConfiguration (pdf, page_per_pages) =
      let val page_per_pages = MLRep.Unsigned.fromLarge (Word.toLarge page_per_pages) in
        Status.fromWord (F_HPDF_SetPagesConfiguration.f' (pdf, page_per_pages))
      end

    fun GetPageByIndex (pdf, index) =
      let val index = MLRep.Unsigned.fromLarge (Word.toLarge index) in
        F_HPDF_GetPageByIndex.f'(pdf, index)
      end

    fun GetPageLayout pdf =
      F_HPDF_GetPageLayout.f' pdf

    fun SetPageLayout (pdf, layout) =
      Status.fromWord (F_HPDF_SetPageLayout .f' (pdf, layout))

    fun GetPageMode pdf =
      PageMode.i2m (F_HPDF_GetPageMode.f' pdf)

    fun SetPageMode (pdf, mode) =
      let val mode = PageMode.m2i mode in
        Status.fromWord (F_HPDF_SetPageMode.f'(pdf, mode))
      end

    fun SetOpenAction (pdf, open_action) =
      Status.fromWord (F_HPDF_SetOpenAction.f'(pdf, open_action))

    fun GetViewerPreference pdf =
      Word32.fromLarge (MLRep.Unsigned.toLarge (F_HPDF_GetViewerPreference.f' pdf))

    fun SetViewerPreference (pdf, value) =
      let val value = MLRep.Unsigned.fromLarge (Word32.toLarge value) in
        Status.fromWord (F_HPDF_SetViewerPreference.f'(pdf, value))
      end

    fun GetCurrentPage pdf =
      F_HPDF_GetCurrentPage.f' pdf

    fun AddPage pdf =
      F_HPDF_AddPage.f' pdf

    fun InsertPage (pdf, page) =
      F_HPDF_InsertPage.f'(pdf, page)

    fun GetFont (pdf, font_name, encoding_name) =
      use_cstring font_name (fn font_name =>
      use_cstring encoding_name (fn encoding_name =>
      F_HPDF_GetFont.f'(pdf, font_name, encoding_name)))

    fun LoadType1FontFromFile (pdf, afm_file_name, data_file_name) =
      use_cstring afm_file_name  (fn afm_file_name  =>
      use_cstring data_file_name (fn data_file_name =>
      ZString.toML'
        (F_HPDF_LoadType1FontFromFile.f'(pdf, afm_file_name, data_file_name))))

    fun LoadTTFontFromFile (pdf, file_name, embedding) =
      use_cstring file_name (fn file_name =>
      ZString.toML'
        (F_HPDF_LoadTTFontFromFile.f'(pdf, file_name, b2i embedding)))

    fun LoadTTFontFromFile2 (pdf, file_name, index, embedding) =
      let
        val index = MLRep.Unsigned.fromLarge (Word.toLarge index)
      in
        use_cstring file_name (fn file_name =>
        F_HPDF_LoadTTFontFromFile2.f'(pdf, file_name, index, b2i embedding))
      end

    (** to be allowed specify the NULL as prefix? *)
    fun AddPageLabel (pdf, page_num, style, first_page, prefix) =
      let
        val page_num = MLRep.Unsigned.fromLarge (Word.toLarge page_num)
        val style = PageNumStyle.m2i style
        val first_page = MLRep.Unsigned.fromLarge (Word.toLarge first_page)
      in
        use_cstring prefix (fn prefix =>
        Status.fromWord
          (F_HPDF_AddPageLabel.f'(pdf, page_num, style, first_page, prefix)))
      end

    fun UseJPFonts pdf = Status.fromWord (F_HPDF_UseJPFonts.f' pdf)
    fun UseKRFonts pdf = Status.fromWord (F_HPDF_UseKRFonts.f' pdf)
    fun UseCNSFonts pdf = Status.fromWord (F_HPDF_UseCNSFonts.f' pdf)
    fun UseCNTFonts pdf = Status.fromWord (F_HPDF_UseCNTFonts.f' pdf)

    fun CreateOutline (pdf, parent, title, encoder) =
      use_cstring title (fn title =>
      F_HPDF_CreateOutline.f'(pdf, parent, title, encoder))

    fun GetEncoder (pdf, encoding_name) =
      use_cstring encoding_name (fn encoding_name =>
      F_HPDF_GetEncoder.f'(pdf, encoding_name))

    fun GetCurrentEncoder pdf =
      F_HPDF_GetCurrentEncoder.f' pdf

    fun SetCurrentEncoder (pdf, encoding_name) =
      use_cstring encoding_name (fn encoding_name =>
      Status.fromWord (F_HPDF_SetCurrentEncoder.f'(pdf, encoding_name)))

    fun UseJPEncodings pdf = Status.fromWord (F_HPDF_UseJPEncodings.f' pdf)

    fun UseKREncodings pdf = Status.fromWord (F_HPDF_UseKREncodings.f' pdf)

    fun UseCNSEncodings pdf = Status.fromWord (F_HPDF_UseCNSEncodings.f' pdf)

    fun UseCNTEncodings pdf = Status.fromWord (F_HPDF_UseCNTEncodings.f' pdf)

    fun LoadPngImageFromFile (pdf, filename) =
      use_cstring filename (fn filename =>
      F_HPDF_LoadPngImageFromFile.f'(pdf, filename))

    fun LoadPngImageFromFile2 (pdf, filename) =
      use_cstring filename (fn filename =>
      F_HPDF_LoadPngImageFromFile2.f'(pdf, filename))

    fun LoadJpegImageFromFile (pdf, filename) =
      use_cstring filename (fn filename =>
      F_HPDF_LoadJpegImageFromFile.f'(pdf, filename))

    fun LoadRawImageFromFile (pdf, filename, width, height, color_space) =
      let
        val width = MLRep.Unsigned.fromLarge (Word.toLarge width)
        val height = MLRep.Unsigned.fromLarge (Word.toLarge height)
        val color_space = ColorSpace.m2i color_space
      in
        use_cstring filename (fn filename =>
        F_HPDF_LoadRawImageFromFile.f'(pdf, filename, width, height, color_space))
      end

    fun LoadRawImageFromMem (pdf, buf, width, height, color_space, bits_per_component) =
      let
        val ` = MLRep.Unsigned.fromLarge o Word8.toLarge
        val width  = `width
        val height = `height
        val color_space = ColorSpace.m2i color_space
        val bufp = C.alloc' C.S.uchar (Word.fromInt (Word8Vector.length buf))
      in
        Word8Vector.appi (fn (i,x)=>
          C.Set.uchar' (C.Ptr.sub' C.S.uchar (bufp, i), `x));
        F_HPDF_LoadRawImageFromMem.f'(pdf, C.Ptr.ro' bufp, width, height, color_space, bits_per_component)
        before
          C.free' bufp
      end

    fun SetInfoAttr (pdf, infotype, value) =
      use_cstring value (fn value =>
      Status.fromWord
        (F_HPDF_SetInfoAttr.f'(pdf, InfoType.m2i infotype, value)))

    fun SetInfoDateAttr (pdf, infotype, value) =
      Status.fromWord (F_HPDF_SetInfoDateAttr.f'(pdf, InfoType.m2i infotype, value))

    fun GetInfoAttr (pdf, infotype) =
      ZString.toML' (F_HPDF_GetInfoAttr.f'(pdf, InfoType.m2i infotype))

    fun SetPassword (pdf, owner_passwd, user_passwd) =
      use_cstring owner_passwd (fn owner_passwd =>
      use_cstring user_passwd (fn user_passwd =>
      Status.fromWord (F_HPDF_SetPassword.f'(pdf, owner_passwd, user_passwd))))

    fun SetPermission (pdf, permissions) =
      let
        val permission =
              (MLRep.Unsigned.fromLarge
                 (SysWord.toLarge
                    (foldl (fn (f,w)=>
                                 SysWord.orb(PermissionFlag.toWord f,w))
                           0w0
                           permissions)))
      in
        Status.fromWord (F_HPDF_SetPermission.f'(pdf, permission))
      end

    fun SetEncryptionMode (pdf, mode, key_len) =
      let
        val mode = EncryptMode.m2i mode
        val key_len = MLRep.Unsigned.fromLarge (Word.toLarge key_len)
      in
        Status.fromWord (F_HPDF_SetEncryptionMode.f'(pdf, mode, key_len))
      end

    fun SetCompressionMode (pdf, mode) =
      let val mode = MLRep.Unsigned.fromLarge
                       (SysWord.toLarge (CompressionMode.toWord mode)) in
        Status.fromWord (F_HPDF_SetCompressionMode.f'(pdf, mode))
      end

    fun CreateExtGState pdf =
      F_HPDF_CreateExtGState.f' pdf

  end


  fun GetVersion () =
    ZString.toML'(F_HPDF_GetVersion.f'())

  fun Page_ShowText page text =
    use_cstring text (fn text =>
    F_HPDF_Page_ShowText.f'(page, text))

end (* local *)
end

