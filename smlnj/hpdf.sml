
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

  fun to_mlreal r =
    MLRep.Real.fromLarge IEEEReal.TO_NEAREST (Real.toLarge r)

  fun to_real r =
    Real.fromLarge IEEEReal.TO_NEAREST (MLRep.Real.toLarge r)

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
  structure GraphicsMode = HPDF_GraphicsMode
  structure Status = HPDF_Status

  datatype z = datatype CompressionMode.t
  datatype z = datatype PermissionFlag.t
  datatype z = datatype ViewerPreference.t
  datatype z = datatype Status.t
  datatype z = datatype GraphicsMode.t


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

  structure Rect =
  struct
    type t = { left   : real
             , bottom : real
             , right  : real
             , top    : real
             }

  end

  structure Box =
  struct
    type t = Rect.t
  end

  structure Point =
  struct
    type t = { x : real
             , y : real
             }
  end

  structure TextWidth =
  struct
    type t = { numchars : Word.word
             , width    : Word.word
             , numspace : Word.word
             }
  end


  structure Cvt :
  sig
    val c_rect   : Rect.t -> (T_HPDF_Rect.t, 'c) C.obj
    val c_box    : Box.t  -> (T_HPDF_Box.t, 'c) C.obj

    val ml_box'       : (T_HPDF_Box.t      , 'c) C.obj' -> Box.t
    val ml_point'     : (T_HPDF_Point.t    , 'c) C.obj' -> Point.t
    val ml_textwidth' : (T_HPDF_TextWidth.t, 'c) C.obj' -> TextWidth.t
  end =
  struct
    fun c_rect (r:Rect.t) =
      let
        open S__HPDF_Rect
        val rect = C.new S__HPDF_Rect.typ
        val cvt = MLRep.Real.fromLarge IEEEReal.TO_NEAREST
      in
        C.Set.float (f_left   rect, cvt (#left   r));
        C.Set.float (f_bottom rect, cvt (#bottom r));
        C.Set.float (f_right  rect, cvt (#right  r));
        C.Set.float (f_top    rect, cvt (#top    r));
        C.rw rect
      end

    val c_box : Box.t -> (T_HPDF_Box.t, 'c) C.obj = c_rect

    fun ml_rect' rect =
      let
        open S__HPDF_Rect
        val cvt = MLRep.Real.fromLarge IEEEReal.TO_NEAREST
      in
        { left   = C.Get.float' (f_left'   rect)
        , bottom = C.Get.float' (f_bottom' rect)
        , right  = C.Get.float' (f_right'  rect)
        , top    = C.Get.float' (f_top'    rect)
        } 
      end

    val ml_box' = ml_rect';

    fun ml_point' p : Point.t =
      let open S__HPDF_Point in
        { x = to_real (C.Get.float' (f_x' p))
        , y = to_real (C.Get.float' (f_y' p))
        }
      end

    fun ml_textwidth' t : TextWidth.t =
      let
        open S__HPDF_TextWidth
        val cvt = Word.fromLarge o MLRep.Unsigned.toLarge
      in
        { numchars = cvt (C.Get.uint' (f_numchars' t))
        , width    = cvt (C.Get.uint' (f_width'    t))
        , numspace = cvt (C.Get.uint' (f_numspace' t))
        }
      end

  end

  fun use_rect' rect =
    using (fn()=> C.Light.obj (Cvt.c_rect rect))
          C.discard'


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


  structure Page =
  struct
    fun SetWidth (page, value) =
      Status.fromWord (F_HPDF_Page_SetWidth.f'(page, value))

    fun SetHeight (page, value) =
      Status.fromWord (F_HPDF_Page_SetHeight.f'(page, value))

    fun SetSize (page, size, direction) =
      let
        val direction = PageDirection.m2i direction
        val size = PageSizes.m2i size
      in
        Status.fromWord (F_HPDF_Page_SetSize.f'(page, size, direction))
      end

    fun SetRotate (page, angle) =
      let val angle = MLRep.Unsigned.fromLarge (Word.toLarge angle) in
        Status.fromWord (F_HPDF_Page_SetRotate.f'(page, angle))
      end

    fun CreateDestination page =
      F_HPDF_Page_CreateDestination.f' page

    fun CreateTextAnnot (page, rect, text, encoder) =
      use_rect' rect (fn rect =>
      use_cstring text (fn text =>
      F_HPDF_Page_CreateTextAnnot.f'(page, rect, text, encoder)))

    fun CreateLinkAnnot (page, rect, dst) =
      use_rect' rect (fn rect =>
      F_HPDF_Page_CreateLinkAnnot.f'(page, rect, dst))

    fun CreateURILinkAnnot (page, rect, uri) =
      use_rect' rect (fn rect =>
      use_cstring uri (fn uri =>
      F_HPDF_Page_CreateURILinkAnnot.f'(page, rect, uri)))

    fun TextWidth (page, text) =
      use_cstring text (fn text =>
      F_HPDF_Page_TextWidth.f'(page, text))

    fun MeasureText (page, text, width, wordwrap, real_width) =
      let
        val width = to_mlreal width
        val wordwrap = b2i wordwrap
        val real_widthp = C.new' C.S.float
      in
        C.Set.float' (real_widthp, to_mlreal (!real_width));
        use_cstring text (fn text =>
        Word.fromLarge
          (MLRep.Unsigned.toLarge
             (F_HPDF_Page_MeasureText.f'( page
                                        , text
                                        , width
                                        , wordwrap
                                        , C.Ptr.|&! real_widthp))))
        before
          real_width := to_real (C.Get.float' real_widthp)
      end

    fun GetWidth page =
      to_real (F_HPDF_Page_GetWidth.f' page)

    fun GetHeight page =
      to_real (F_HPDF_Page_GetHeight.f' page)

    fun GetGMode page =
      CompressionMode.fromWord (F_HPDF_Page_GetGMode.f' page)

    fun GetCurrentPos page =
      let
        val point = C.new' S__HPDF_Point.size
      in
        Cvt.ml_point'
          (F_HPDF_Page_GetCurrentPos.f'(point, page))
        before
          C.discard' point
      end

    fun GetCurrentTextPos page =
      let
        val point = C.new' S__HPDF_Point.size
      in
        Cvt.ml_point'
          (F_HPDF_Page_GetCurrentTextPos.f'(point, page))
        before
          C.discard' point
      end

    fun GetCurrentFont page =
      F_HPDF_Page_GetCurrentFont.f' page

    fun GetCurrentFontSize page =
      to_real (F_HPDF_Page_GetCurrentFontSize.f' page)

    fun GetTransMatrix page =
      F_HPDF_Page_GetTransMatrix.f' page

    fun GetLineWidth page =
      to_real (F_HPDF_Page_GetLineWidth.f' page)

    fun GetLineCap page =
      LineCap.i2m (F_HPDF_Page_GetLineCap.f' page)

    fun GetLineJoin page =
      LineJoin.i2m (F_HPDF_Page_GetLineJoin.f' page)

    fun GetMiterLimit page =
      to_real (F_HPDF_Page_GetMiterLimit.f' page)

    fun GetDash page =
      F_HPDF_Page_GetDash.f' page

    fun GetFlat page =
      to_real (F_HPDF_Page_GetFlat.f' page)

    fun GetCharSpace page =
      to_real (F_HPDF_Page_GetCharSpace.f' page)

    fun GetWordSpace page =
      to_real (F_HPDF_Page_GetWordSpace.f' page)

    fun GetHorizontalScalling page =
      to_real (F_HPDF_Page_GetHorizontalScalling.f' page)

    fun GetTextLeading page =
      to_real (F_HPDF_Page_GetTextLeading.f' page)

    fun GetTextRenderingMode page =
      TextRenderingMode.i2m (F_HPDF_Page_GetTextRenderingMode.f' page)

    fun GetTextRaise page =
      to_real (F_HPDF_Page_GetTextRaise.f' page)

    fun GetRGBFill page =
      F_HPDF_Page_GetRGBFill.f' page

    fun GetRGBStroke page =
      F_HPDF_Page_GetRGBStroke.f' page

    fun GetCMYKFill page =
      F_HPDF_Page_GetCMYKFill.f' page

    fun GetCMYKStroke page =
      F_HPDF_Page_GetCMYKStroke.f' page

    fun GetGrayFill page =
      to_real (F_HPDF_Page_GetGrayFill.f' page)

    fun GetGrayStroke page =
      to_real (F_HPDF_Page_GetGrayStroke.f' page)

    fun GetStrokingColorSpace page =
      F_HPDF_Page_GetStrokingColorSpace.f' page

    fun GetFillingColorSpace page =
      F_HPDF_Page_GetFillingColorSpace.f' page

    fun GetTextMatrix page =
      F_HPDF_Page_GetTextMatrix.f' page

    fun GetGStateDepth page =
      Word.fromLarge
        (MLRep.Unsigned.toLarge
            (F_HPDF_Page_GetGStateDepth.f' page))

    fun SetLineWidth (page, line_width) =
      Status.fromWord (F_HPDF_Page_SetLineWidth.f' (page, line_width))

    fun SetLineCap (page, line_cap) =
      let val line_cap = LineCap.m2i line_cap in
        Status.fromWord (F_HPDF_Page_SetLineCap.f'(page, line_cap))
      end

    fun SetLineJoin (page, line_join) =
      let val line_join = LineJoin.m2i line_join in
        Status.fromWord (F_HPDF_Page_SetLineJoin.f'(page, line_join))
      end

    fun SetMiterLimit (page, miter_limit) =
      let val miter_limit = to_mlreal miter_limit in
        Status.fromWord (F_HPDF_Page_SetMiterLimit.f'(page, miter_limit))
      end

    fun app_arr f (len,arr) =
      let
        fun loop n =
          if n = len
          then ()
          else (f (n, C.Ptr.sub (arr,n)); loop (n+1))
      in
        loop 0
      end

    fun vec2ptr vec =
      let
        val len = Vector.length vec
        val vec_ptr = C.alloc C.T.ushort (Word.fromInt len)
      in
        app_arr (fn (i,obj)=> C.Set.ushort (obj
                     , MLRep.Unsigned.fromLarge
                         (Word.toLarge
                           (Vector.sub(vec,i)))))
                (len, vec_ptr);
        C.Ptr.ro vec_ptr
      end

    fun SetDash (page, dash, phase) =
      let
        val dash_ptn = C.Light.ptr (vec2ptr dash)
        val num_param = MLRep.Unsigned.fromInt (Vector.length dash)
        val phase = MLRep.Unsigned.fromLarge (Word.toLarge phase)
      in
        Status.fromWord
          (F_HPDF_Page_SetDash.f'(page, dash_ptn, num_param, phase))
        before
          C.free' dash_ptn
      end

    fun SetFlat (page, flatness) =
      let val flatness = to_mlreal flatness in
        Status.fromWord (F_HPDF_Page_SetFlat.f'(page, flatness))
      end

    fun SetExtGState (page, ext_gstate) =
      Status.fromWord (F_HPDF_Page_SetExtGState.f'(page, ext_gstate))

    fun GSave page =
      Status.fromWord (F_HPDF_Page_GSave.f' page)

    fun GRestore page =
      Status.fromWord (F_HPDF_Page_GRestore.f' page)

    fun Concat (page, a, b, c, d, x, y) =
      let
        val a = to_mlreal a
        val b = to_mlreal b
        val c = to_mlreal c
        val d = to_mlreal d
        val x = to_mlreal x
        val y = to_mlreal y
      in
        Status.fromWord (F_HPDF_Page_Concat.f'(page, a, b, c, d, x, y))
      end

    fun MoveTo (page, x, y) =
      let
        val x = to_mlreal x
        val y = to_mlreal y
      in
        Status.fromWord (F_HPDF_Page_MoveTo.f'(page, x, y))
      end

    fun LineTo (page, x, y) =
      let
        val x = to_mlreal x
        val y = to_mlreal y
      in
        Status.fromWord (F_HPDF_Page_LineTo.f'(page, x, y))
      end

    fun CurveTo (page, x1, y1, x2, y2, x3, y3) =
      let
        val x1 = to_mlreal x1
        val y1 = to_mlreal y1
        val x2 = to_mlreal x2
        val y2 = to_mlreal y2
        val x3 = to_mlreal x3
        val y3 = to_mlreal y3
      in
        Status.fromWord (F_HPDF_Page_CurveTo.f'(page, x1, y1, x2, y2, x3, y3))
      end

    fun CurveTo2 (page, x2, y2, x3, y3) =
      let
        val x2 = to_mlreal x2
        val y2 = to_mlreal y2
        val x3 = to_mlreal x3
        val y3 = to_mlreal y3
      in
        Status.fromWord (F_HPDF_Page_CurveTo2.f'(page, x2, y2, x3, y3))
      end

    fun CurveTo3 (page, x1, y1, x3, y3) =
      let
        val x1 = to_mlreal x1
        val y1 = to_mlreal y1
        val x3 = to_mlreal x3
        val y3 = to_mlreal y3
      in
        Status.fromWord (F_HPDF_Page_CurveTo3.f'(page, x1, y1, x3, y3))
      end

    fun ClosePath page =
      Status.fromWord (F_HPDF_Page_ClosePath.f' page)

    fun Rectangle (page, x, y, width, height) =
      let
        val x = to_mlreal x
        val y = to_mlreal y
        val width = to_mlreal width
        val height = to_mlreal height
      in
        Status.fromWord
          (F_HPDF_Page_Rectangle.f'(page
                                   , x, y
                                   , width, height))
      end

    fun Stroke page =
      Status.fromWord (F_HPDF_Page_Stroke.f' page)

    fun ClosePathStroke page =
      Status.fromWord (F_HPDF_Page_ClosePathStroke.f' page)

    fun Fill page =
      Status.fromWord (F_HPDF_Page_Fill.f' page)

    fun Eofill page =
      Status.fromWord (F_HPDF_Page_Eofill.f' page)

    fun FillStroke page =
      Status.fromWord (F_HPDF_Page_FillStroke.f' page)

    fun EofillStroke page =
      Status.fromWord (F_HPDF_Page_EofillStroke.f' page)

    fun ClosePathFillStroke page =
      Status.fromWord (F_HPDF_Page_ClosePathFillStroke.f' page)

    fun ClosePathEofillStroke page =
      Status.fromWord (F_HPDF_Page_ClosePathEofillStroke.f' page)

    fun EndPath page =
      Status.fromWord (F_HPDF_Page_EndPath.f' page)

    fun Clip page =
      Status.fromWord (F_HPDF_Page_Clip.f' page)

    fun Eoclip page =
      Status.fromWord (F_HPDF_Page_Eoclip.f' page)

    fun BeginText page =
      Status.fromWord (F_HPDF_Page_BeginText.f' page)

    fun EndText page =
      Status.fromWord (F_HPDF_Page_EndText.f' page)

    fun SetCharSpace (page, value) =
      Status.fromWord (F_HPDF_Page_SetCharSpace.f'(page, to_mlreal value))

    fun SetWordSpace (page, value) =
      Status.fromWord (F_HPDF_Page_SetWordSpace.f'(page, to_mlreal value))

    fun SetHorizontalScalling (page, value) =
      Status.fromWord (F_HPDF_Page_SetHorizontalScalling.f'(page, to_mlreal value))

    fun SetTextLeading (page, value) =
      Status.fromWord (F_HPDF_Page_SetTextLeading.f'(page, to_mlreal value))

    fun SetFontAndSize (page, hfont, size) =
      Status.fromWord (F_HPDF_Page_SetFontAndSize.f'(page, hfont, to_mlreal size))

    fun SetTextRenderingMode (page, mode) =
      let val mode = TextRenderingMode.m2i mode in
        Status.fromWord (F_HPDF_Page_SetTextRenderingMode.f'(page, mode))
      end

    fun SetTextRise (page, value) =
      Status.fromWord (F_HPDF_Page_SetTextRise.f'(page, to_mlreal value))

    (**
     * This function is obsolete. Use Hpdf.Page.SetTextRise
     *)
    (*
    fun SetTextRaise (page, value) =
      Status.fromWord
        (F_HPDF_Page_SetTextRaise.f'(page, to_mlreal value))
    *)

    fun MoveTextPos (page, x, y) =
      let
        val x = to_mlreal x
        val y = to_mlreal y
      in
        Status.fromWord (F_HPDF_Page_MoveTextPos.f'(page, x, y))
      end

    fun MoveTextPos2 (page, x, y) =
      let
        val x = to_mlreal x
        val y = to_mlreal y
      in
        Status.fromWord (F_HPDF_Page_MoveTextPos2.f'(page, x, y))
      end

    fun SetTextMatrix (page, a, b, c, d, x, y) =
      let
        val a = to_mlreal a
        val b = to_mlreal b
        val c = to_mlreal c
        val d = to_mlreal d
        val x = to_mlreal x
        val y = to_mlreal y
      in
        Status.fromWord
          (F_HPDF_Page_SetTextMatrix.f'(page, a, b, c, d, x, y))
      end

    fun MoveToNextLine page =
      Status.fromWord (F_HPDF_Page_MoveToNextLine.f' page)

    fun Page_ShowText (page, text) =
      use_cstring text (fn text =>
      Status.fromWord (F_HPDF_Page_ShowText.f'(page, text)))

    fun ShowTextNextLine (page, text) =
      Status.fromWord (F_HPDF_Page_ShowTextNextLine.f'(page, text))

    fun ShowTextNextLineEx (page, word_space, char_space, text) =
      let
        val word_space = to_mlreal word_space
        val char_space = to_mlreal char_space
      in
        Status.fromWord
          (F_HPDF_Page_ShowTextNextLineEx.f'(page, word_space, char_space, text))
      end

    fun SetGrayFill (page, gray) =
      Status.fromWord (F_HPDF_Page_SetGrayFill.f'(page, to_mlreal gray))

    fun SetGrayStroke (page, gray) =
      Status.fromWord (F_HPDF_Page_SetGrayStroke.f'(page, to_mlreal gray))

    fun SetRGBFill (page, r, g, b) =
      let
        val r = to_mlreal r
        val g = to_mlreal g
        val b = to_mlreal b
      in
        Status.fromWord (F_HPDF_Page_SetRGBFill.f'(page, r, g, b))
      end

    fun SetRGBStroke (page, r, g, b) =
      let
        val r = to_mlreal r
        val g = to_mlreal g
        val b = to_mlreal b
      in
        Status.fromWord (F_HPDF_Page_SetRGBStroke.f'(page, r, g, b))
      end

    fun SetCMYKFill (page, c, m, y, k) =
      let
        val c = to_mlreal c
        val m = to_mlreal m
        val y = to_mlreal y
        val k = to_mlreal k
      in
        Status.fromWord (F_HPDF_Page_SetCMYKFill.f'(page, c, m, y, k))
      end

    fun SetCMYKStroke (page, c, m, y, k) =
      let
        val c = to_mlreal c
        val m = to_mlreal m
        val y = to_mlreal y
        val k = to_mlreal k
      in
        Status.fromWord (F_HPDF_Page_SetCMYKStroke.f'(page, c, m, y, k))
      end

    fun ExecuteXObject (page, obj) =
      Status.fromWord (F_HPDF_Page_ExecuteXObject.f'(page, obj))

    fun DrawImage (page, image, x, y, width, height) =
      let
        val x = to_mlreal x
        val y = to_mlreal y
        val width  = to_mlreal width
        val height = to_mlreal height
      in
        Status.fromWord
          (F_HPDF_Page_DrawImage.f'(page, image, x, y, width, height))
      end

    fun Circle (page, x, y, ray) =
      let
        val x = to_mlreal x
        val y = to_mlreal y
        val ray = to_mlreal ray
      in
        Status.fromWord (F_HPDF_Page_Circle.f'(page, x, y, ray))
      end

    fun Arc (page, x, y, ray, ang1, ang2) =
      let
        val x = to_mlreal x
        val y = to_mlreal y
        val ray = to_mlreal ray
        val ang1 = to_mlreal ang1
        val ang2 = to_mlreal ang2
      in
        Status.fromWord (F_HPDF_Page_Arc.f'(page, x, y, ray, ang1, ang2))
      end

    fun Ellipse (page, x, y, xray, yray) =
      let
        val x = to_mlreal x
        val y = to_mlreal y
        val xray = to_mlreal xray
        val yray = to_mlreal yray
      in
        Status.fromWord (F_HPDF_Page_Ellipse.f'(page, x, y, xray, yray))
      end

    fun TextOut (page, xpos, ypos, text) =
      let
        val xpos = to_mlreal xpos
        val ypos = to_mlreal ypos
      in
        use_cstring text (fn text =>
        Status.fromWord (F_HPDF_Page_TextOut.f'(page, xpos, ypos, text)))
      end

    fun TextRect (page, left, top, right, bottom, text, align, len) =
      let
        val left = to_mlreal left
        val top  = to_mlreal top
        val right = to_mlreal right
        val bottom = to_mlreal bottom
        val align = TextAlignment.m2i align
        val lenp = C.new' C.S.uint
      in
        C.Set.uint' (lenp, MLRep.Unsigned.fromLarge (Word.toLarge (!len)));
        Status.fromWord
          (F_HPDF_Page_TextRect.f'(page, left, top, right, bottom
                                  , text, align, C.Ptr.|&! lenp))
        before
          len := Word.fromLarge (MLRep.Unsigned.toLarge (C.Get.uint' lenp))
      end

    fun SetSlideShow (page, style, disp_time, trans_time) =
      let
        val style = TransitionStyle.m2i style
        val disp_time = to_mlreal disp_time
        val trans_time = to_mlreal trans_time
      in
        Status.fromWord
          (F_HPDF_Page_SetSlideShow.f'(page, style, disp_time, trans_time))
      end
  end

  fun maybeptr' ptr f null =
    if C.Ptr.isNull' ptr
    then f ptr
    else null

  structure Font =
  struct
    fun GetFontName font =
      maybeptr' (F_HPDF_Font_GetFontName.f' font)
                (SOME o ZString.toML')
                NONE

    fun GetEncodingName font =
      maybeptr' (F_HPDF_Font_GetEncodingName.f' font)
                (SOME o ZString.toML')
                NONE

    fun GetUnicodeWidth (font, code) =
      let val code = MLRep.Unsigned.fromLarge (Word.toLarge code) in
        Int.fromLarge
          (MLRep.Signed.toLarge
             (F_HPDF_Font_GetUnicodeWidth.f'(font, code)))
      end

    fun GetBBox font =
      Cvt.ml_box' (F_HPDF_Font_GetBBox.f' font)

    fun GetAscent font =
      Int.fromLarge
        (MLRep.Signed.toLarge
           (F_HPDF_Font_GetAscent.f' font))

    fun GetDescent font =
      Int.fromLarge
        (MLRep.Signed.toLarge
           (F_HPDF_Font_GetDescent.f' font))

    fun GetXHeight font =
      Word.fromLarge
        (MLRep.Unsigned.toLarge
           (F_HPDF_Font_GetXHeight.f' font))

    fun GetCapHeight font =
      Word.fromLarge
        (MLRep.Unsigned.toLarge
           (F_HPDF_Font_GetCapHeight.f' font))

    fun TextWidth (font, text, len) =
      let
        val tw = C.new' S__HPDF_TextWidth.size
        val len = MLRep.Unsigned.fromLarge (Word.toLarge len)
      in
        use_cstring text (fn text =>
        Cvt.ml_textwidth'
          (F_HPDF_Font_TextWidth.f'(tw, font, text, len)))
        before
          C.discard' tw
      end

    fun MeasureText (font, text, len
                    , width, font_size, char_space, word_space
                    , wordwrap, real_width) =
      let
        val width = to_real width
        val font_size = to_real font_size
        val char_space = to_real char_space
        val word_space = to_real word_space
        val wordwrap = b2i wordwrap
        val real_widthp = C.new' C.S.float
      in
        C.Set.float' (real_widthp, to_real (!real_width));
        Word.fromLarge
          (MLRep.Unsigned.toLarge
             (F_HPDF_Font_MeasureText.f'
                      (font, text, len
                      , width, font_size, char_space, word_space
                      , wordwrap, C.Ptr.|&! real_widthp)))
        before
          real_width := to_mlreal (C.Get.float' real_widthp)
        before
          C.discard' real_widthp
      end


  end (* Font *)


  fun GetVersion () =
    ZString.toML'(F_HPDF_GetVersion.f'())

end (* local *)
end

