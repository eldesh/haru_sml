

structure Demo =
struct
  structure Mode = Hpdf.TextRenderingMode
  open Hpdf

  fun check_null' p msg =
    if C.Ptr.isNull' p then raise Fail msg
    else ()

  fun using mk release f =
    let
      val resource = mk()
      val r = f resource handle exn => (release resource; raise exn)
    in
      r before release resource
    end

  fun new_pdf (err, data) f =
    using (fn()=>
            let val pdf = F_HPDF_New.f'(err, data) in
              pdf before
                    check_null' pdf ("cannot create PdfDoc object")
            end)
          F_HPDF_Free.f' 
          f

  fun use_cstring (s:string) f =
    using (fn()=> ZString.dupML' s)
          C.free'
          f

  fun for i cond succ f =
    if cond i then (f i; for (succ i) cond succ f)
    else ()

  fun ShowText page text =
    use_cstring text (fn text =>
    F_HPDF_Page_ShowText.f'(page, text))

  (**
   * draw grid to the page
   *)
  fun print_grid pdf page =
    let
      val height = F_HPDF_Page_GetHeight.f' page
      val width  = F_HPDF_Page_GetWidth.f' page
      val font = use_cstring "Helvetica" (fn h=>
                 F_HPDF_GetFont.f' (pdf, h, C.Ptr.null'))
    in
      F_HPDF_Page_SetFontAndSize.f' (page, font, real 5);
      F_HPDF_Page_SetGrayFill.f' (page, 0.5);
      F_HPDF_Page_SetGrayStroke.f' (page, 0.8);

      (* Draw horizontal lines *)
      for 0 (fn y=> real y < height) (fn y=>y+5) (fn y=>(
        if y mod 10 = 0
        then F_HPDF_Page_SetLineWidth.f' (page, 0.5 )
        else F_HPDF_Page_SetLineWidth.f' (page, 0.25);
        F_HPDF_Page_MoveTo.f'(page,   0.0, real y);
        F_HPDF_Page_LineTo.f'(page, width, real y);
        F_HPDF_Page_Stroke.f' page;
        if y mod 10 = 0 andalso y > 0 then
          (F_HPDF_Page_SetGrayStroke.f'(page, 0.5);
           F_HPDF_Page_MoveTo.f'(page, 0.0, real y);
           F_HPDF_Page_LineTo.f'(page, 5.0, real y);
           F_HPDF_Page_Stroke.f' page;
           F_HPDF_Page_SetGrayStroke.f'(page, 0.8);
           ())
        else
          ()
      ));

      (* Draw virtical lines *)
      for 0 (fn x=> real x < height) (fn x=>x+5) (fn x=>(
        if x mod 10 = 0
        then F_HPDF_Page_SetLineWidth.f' (page, 0.5 )
        else F_HPDF_Page_SetLineWidth.f' (page, 0.25);
        F_HPDF_Page_MoveTo.f' (page, real x, 0.0);
        F_HPDF_Page_LineTo.f' (page, real x, height);
        F_HPDF_Page_Stroke.f' page;
        if x mod 50 = 0 andalso x > 0 then
          (F_HPDF_Page_SetGrayStroke.f' (page, 0.5);
           F_HPDF_Page_MoveTo.f' (page, real x, 0.0);
           F_HPDF_Page_LineTo.f' (page, real x, 5.0);
           F_HPDF_Page_Stroke.f' page;
           F_HPDF_Page_MoveTo.f' (page, real x, height);
           F_HPDF_Page_LineTo.f' (page, real x, height - 5.0);
           F_HPDF_Page_Stroke.f' page;
           F_HPDF_Page_SetGrayStroke.f' (page, 0.8);
           ())
        else
          ()
      ));

      (* Draw horizontal text *)
      for 0 (fn y=> real y < height) (fn y=>y+5) (fn y=>(
        if y mod 10 = 0 andalso y > 0 then
          use_cstring (Int.toString y) (fn buf =>
          (F_HPDF_Page_BeginText.f' page;
           F_HPDF_Page_MoveTextPos.f' (page, 5.0, real y - 2.0);
           F_HPDF_Page_ShowText.f' (page, buf);
           F_HPDF_Page_EndText.f' page;
           ()))
        else
          ()
      ));

      (* Draw virtical text *)
      for 0 (fn x=> real x < width) (fn x=>x+5) (fn x=>(
        if x mod 50 = 0 andalso x > 0 then
          use_cstring (Int.toString x) (fn buf =>
          (F_HPDF_Page_BeginText.f' page;
           F_HPDF_Page_MoveTextPos.f' (page, real x, 5.0);
           F_HPDF_Page_ShowText.f' (page, buf);
           F_HPDF_Page_EndText.f' page;
           F_HPDF_Page_BeginText.f' page;
           F_HPDF_Page_MoveTextPos.f' (page, real x, height - 10.0);
           F_HPDF_Page_ShowText.f' (page, buf);
           F_HPDF_Page_EndText.f' (page);
           ()))
        else
          ()
      ));
      F_HPDF_Page_SetGrayFill.f' (page, 0.0);
      F_HPDF_Page_SetGrayStroke.f'(page, 0.0)
    end

  local
    datatype z = datatype Mode.mlrep
  in
  fun show_description page x y text =
    let
      open S__HPDF_RGBColor
      val fsize = F_HPDF_Page_GetCurrentFontSize.f' page
      val font = F_HPDF_Page_GetCurrentFont.f' page
      val rgb = C.Light.obj (C.new T_HPDF_RGBColor.typ)
      val c = F_HPDF_Page_GetRGBFill.f' (rgb, page)
      val float = C.Get.float'
    in
      F_HPDF_Page_BeginText.f' page;
      F_HPDF_Page_SetRGBFill.f' (page, 0.0, 0.0, 0.0);
      F_HPDF_Page_SetTextRenderingMode.f' (page, Mode.m2i Mode.e_HPDF_FILL);
      F_HPDF_Page_SetFontAndSize.f' (page, font, 10.0);
      use_cstring text (fn text =>
      F_HPDF_Page_TextOut.f' (page, x, y - 12.0, text));
      F_HPDF_Page_EndText.f' page;

      F_HPDF_Page_SetFontAndSize.f' (page, font, fsize);
      F_HPDF_Page_SetRGBFill.f' (page, float (f_r' c), float (f_g' c), float (f_b' c))
    end
  end

  fun show_stripe_pattern page x y =
    let
      val iy = ref 0
    in
      while !iy < 50 do (
        F_HPDF_Page_SetRGBStroke.f'(page, 0.0, 0.0, 0.5);
        F_HPDF_Page_SetLineWidth.f'(page, 1.0);
        F_HPDF_Page_MoveTo.f'(page, x, y + real (!iy));
        use_cstring "ABCabc123" (fn text =>
        F_HPDF_Page_LineTo.f'(page, x + F_HPDF_Page_TextWidth.f'(page, text), y + real(!iy)));
        F_HPDF_Page_Stroke.f' page;
        iy := (!iy) + 3
      );
      F_HPDF_Page_SetLineWidth.f' (page, 2.5)
    end

  local
    datatype z = datatype Mode.mlrep
  in
  fun text_demo fname =
    use_cstring fname                        (fn fname      =>
    use_cstring "Text Demo"                  (fn page_title =>
    use_cstring "abcdefgABCDEFG123!#$%&+-@?" (fn samp_text  =>
    use_cstring "The quick brown fox jumps over the lazy dog." (fn samp_text2 =>
    new_pdf (C.Ptr.fnull', C.Ptr.vNull) (fn pdf =>
    let
      val _ = F_HPDF_SetCompressionMode.f'(pdf, 0wxf)
      val font = F_HPDF_GetFont.f'(pdf, ZString.dupML' "Helvetica", C.Ptr.null')
      val page = F_HPDF_AddPage.f' pdf
    in
      print_grid pdf page;
      F_HPDF_Page_SetFontAndSize.f'(page, font, 24.0);
    let
      val tw = F_HPDF_Page_TextWidth.f'(page, page_title)
    in
      F_HPDF_Page_BeginText.f' page;
      F_HPDF_Page_TextOut.f' (page
                            , (F_HPDF_Page_GetWidth.f' page - tw) / 2.0
                            , F_HPDF_Page_GetHeight.f' page - 50.0
                            , page_title);
      F_HPDF_Page_EndText.f' page;
      F_HPDF_Page_BeginText.f' page;
      F_HPDF_Page_MoveTextPos.f'(page, 60.0, F_HPDF_Page_GetHeight.f' page - 60.0);
      (* font size *)
      for 8.0 (fn fsize => fsize < 60.0) (fn fsize => fsize * 1.5) (fn fsize =>
      ((* set style and size of font. *)
       F_HPDF_Page_SetFontAndSize.f'(page, font, fsize);
       (* set the position of the text. *)
       F_HPDF_Page_MoveTextPos.f'(page, 0.0, ~5.0 - fsize);
       (* measure the number of characters which include in the page. *)
       let
         val len = F_HPDF_Page_MeasureText.f'(page
                                            , samp_text
                                            , F_HPDF_Page_GetWidth.f' page - 120.0
                                            , Hpdf.HPDF_FALSE
                                            , C.Ptr.null')
         (* truncate the text *)
         val buf = String.extract (ZString.toML' samp_text
                                    , 0
                                    , SOME (MLRep.Unsigned.toInt len))
       in
         use_cstring buf (fn buf => F_HPDF_Page_ShowText.f'(page, buf));
         (* print the description. *)
         F_HPDF_Page_MoveTextPos.f'(page, 0.0, ~10.0);
         F_HPDF_Page_SetFontAndSize.f'(page, font, 8.0);
         use_cstring (concat["Fontsize=", Real.fmt (StringCvt.FIX (SOME 0)) fsize])
           (fn buf => F_HPDF_Page_ShowText.f'(page, buf))
       end
      ));
      (*
       * font color
       *)
      F_HPDF_Page_SetFontAndSize.f'(page, font, 8.0);
      F_HPDF_Page_MoveTextPos.f'(page, 0.0, ~30.0);
      ShowText page "Font color";

      F_HPDF_Page_SetFontAndSize.f'(page, font, 18.0);
      F_HPDF_Page_MoveTextPos.f'(page, 0.0, ~20.0);

    let
      val len = ZString.length' samp_text
      fun slice cstr i =
          Substring.string (
            Substring.extract (ZString.toML' cstr, i, SOME 1))
    in
      for 0 (fn i=> i < len) (fn i=>i+1) (fn i=>
        let
          val r = real i / real len
          val g = 1.0 - (real i / real len)
        in
          use_cstring (slice samp_text i) (fn buf =>
          (
            F_HPDF_Page_SetRGBFill.f'(page, r, g, 0.0);
            F_HPDF_Page_ShowText.f'(page, buf)
          ))
        end);
      F_HPDF_Page_MoveTextPos.f'(page, 0.0, ~25.0);

      for 0 (fn i=> i < len) (fn i=>i+1) (fn i=>
        let
          val r = real i / real len
          val b = 1.0 - (real i / real len)
        in
          use_cstring (slice samp_text i) (fn buf =>
          (
            F_HPDF_Page_SetRGBFill.f'(page, r, 0.0, b);
            F_HPDF_Page_ShowText.f'(page, buf)
          ))
        end);
      F_HPDF_Page_MoveTextPos.f'(page, 0.0, ~25.0);

      for 0 (fn i=> i < len) (fn i=>i+1) (fn i=>
        let
          val b = real i / real len
          val g = 1.0 - (real i / real len)
        in
          use_cstring (slice samp_text i) (fn buf =>
          (
            F_HPDF_Page_SetRGBFill.f'(page, 0.0, g, b);
            F_HPDF_Page_ShowText.f'(page, buf)
          ))
        end);
      F_HPDF_Page_EndText.f' page;
    let
      val ypos = 450.0
    in
      (*
       * Font rendering mode
       *)
      F_HPDF_Page_SetFontAndSize.f'(page, font, 32.0);
      F_HPDF_Page_SetRGBFill.f'(page, 0.5, 0.5, 0.0);
      F_HPDF_Page_SetLineWidth.f'(page, 1.5);

      (* PDF_FILL *)
      show_description page 60.0 ypos "RenderingMode=PDF_FILL";
      F_HPDF_Page_SetTextRenderingMode.f' (page, Mode.m2i e_HPDF_FILL);
      F_HPDF_Page_BeginText.f' page;
      use_cstring "ABCabc123" (fn text =>
      F_HPDF_Page_TextOut.f' (page, 60.0, ypos, text));
      F_HPDF_Page_EndText.f' page;

      (* PDF_STROKE *)
      show_description page 60.0 (ypos - 50.0) "RenderingMode=PDF_STROKE";
      F_HPDF_Page_SetTextRenderingMode.f' (page, Mode.m2i e_HPDF_STROKE);
      F_HPDF_Page_BeginText.f' page;
      use_cstring "ABCabc123" (fn text =>
      F_HPDF_Page_TextOut.f' (page, 60.0, ypos - 50.0, text));
      F_HPDF_Page_EndText.f' page;

      (* PDF_FILL_THEN_STROKE *)
      show_description page 60.0 (ypos - 100.0) "RenderingMode=PDF_FILL_THEN_STROKE";
      F_HPDF_Page_SetTextRenderingMode.f' (page, Mode.m2i e_HPDF_FILL_THEN_STROKE);
      F_HPDF_Page_BeginText.f' page;
      use_cstring "ABCabc123" (fn text =>
      F_HPDF_Page_TextOut.f' (page, 60.0, ypos - 100.0, text));
      F_HPDF_Page_EndText.f' page;

      (* PDF_FILL_CLIPPING *)
      show_description page 60.0 (ypos - 150.0) "RenderingMode=PDF_FILL_CLIPPING";
      F_HPDF_Page_GSave.f' page;
      F_HPDF_Page_SetTextRenderingMode.f' (page, Mode.m2i e_HPDF_FILL_CLIPPING);
      F_HPDF_Page_BeginText.f' page;
      use_cstring "ABCabc123" (fn text =>
      F_HPDF_Page_TextOut.f' (page, 60.0, ypos - 150.0, text));
      F_HPDF_Page_EndText.f' page;
      show_stripe_pattern page 60.0 (ypos - 150.0);
      F_HPDF_Page_GRestore.f' page;

      (* PDF_STROKE_CLIPPING *)
      show_description page 60.0 (ypos - 200.0) "RenderingMode=PDF_STROKE_CLIPPING";
      F_HPDF_Page_GSave.f' page;
      F_HPDF_Page_SetTextRenderingMode.f' (page, Mode.m2i e_HPDF_STROKE_CLIPPING);
      F_HPDF_Page_BeginText.f' page;
      use_cstring "ABCabc123" (fn text =>
      F_HPDF_Page_TextOut.f' (page, 60.0, ypos - 200.0, text));
      F_HPDF_Page_EndText.f' page;
      show_stripe_pattern page 60.0 (ypos - 200.0);
      F_HPDF_Page_GRestore.f' page;

      (* PDF_FILL_STROKE_CLIPPING *)
      show_description page 60.0 (ypos - 250.0) "RenderingMode=PDF_FILL_STROKE_CLIPPING";
      F_HPDF_Page_GSave.f' page;
      F_HPDF_Page_SetTextRenderingMode.f' (page, Mode.m2i e_HPDF_FILL_STROKE_CLIPPING);
      F_HPDF_Page_BeginText.f' page;
      use_cstring "ABCabc123" (fn text =>
      F_HPDF_Page_TextOut.f' (page, 60.0, ypos - 250.0, text));
      F_HPDF_Page_EndText.f' page;
      show_stripe_pattern page 60.0 (ypos - 250.0);
      F_HPDF_Page_GRestore.f' page;

      (* Reset text attributes *)
      F_HPDF_Page_SetTextRenderingMode.f' (page, Mode.m2i e_HPDF_FILL);
      F_HPDF_Page_SetRGBFill.f' (page, 0.0, 0.0, 0.0);
      F_HPDF_Page_SetFontAndSize.f' (page, font, 30.0);

    let
      (*
       * Rotating text
       *)
      val angle1 = 30.0 (* A rotation of 30 degrees. *)
      val rad1 = angle1 / 180.0 * Math.pi (* Calcurate the radian value. *)
    in
      show_description page 320.0 (ypos - 60.0) "Rotating text";
      F_HPDF_Page_BeginText.f' page;
      F_HPDF_Page_SetTextMatrix.f'(page
                                    , Math.cos rad1
                                    , Math.sin rad1
                                    , ~ (Math.sin rad1)
                                    , Math.cos rad1
                                    , 330.0
                                    , ypos - 60.0);
      use_cstring "ABCabc123" (fn text =>
      F_HPDF_Page_ShowText.f' (page, text));
      F_HPDF_Page_EndText.f' page;

    let
      (*
       * Skewing text
       *)
      val angle1 = 10.0
      val angle2 = 20.0
      val rad1 = angle1 / 180.0 * Math.pi
      val rad2 = angle2 / 180.0 * Math.pi
    in
      show_description page 320.0 (ypos - 120.0) "Skewing text";
      F_HPDF_Page_BeginText.f' page;
      F_HPDF_Page_SetTextMatrix.f' (page, 1.0, Math.tan rad1, Math.tan rad2,
      1.0, 320.0, ypos - 120.0);
      ShowText page "ABCabc123";
      F_HPDF_Page_EndText.f' page;

      (*
       * scaling text (X direction)
       *)
      show_description page 320.0 (ypos - 175.0) "Scaling text (X direction)";
      F_HPDF_Page_BeginText.f' page;
      F_HPDF_Page_SetTextMatrix.f'(page, 1.5, 0.0, 0.0, 1.0, 320.0, ypos-175.0);
      ShowText page "ABCabc12";
      F_HPDF_Page_EndText.f' page;

      (*
       * scaling text (Y direction)
       *)
      show_description page 320.0 (ypos - 250.0) "Scaling text (Y direction)";
      F_HPDF_Page_BeginText.f' page;
      F_HPDF_Page_SetTextMatrix.f'(page, 1.0, 0.0, 0.0, 2.0, 320.0, ypos-250.0);
      ShowText page "ABCabc123";
      F_HPDF_Page_EndText.f' page;

      (*
       * char spacing, word spacing
       *)
      show_description page 60.0 140.0 "char-spacing 0";
      show_description page 60.0 100.0 "char-spacing 1.5";
      show_description page 60.0  60.0 "char-spacing 1.5, word-spacing 2.5";

      F_HPDF_Page_SetFontAndSize.f'(page, font, 20.0);
      F_HPDF_Page_SetRGBFill.f'(page, 0.1, 0.3, 0.1);

      (* char-spacing 0 *)
      F_HPDF_Page_BeginText.f' page;
      F_HPDF_Page_TextOut.f'(page, 60.0, 140.0, samp_text2);
      F_HPDF_Page_EndText.f' page;

      (* char-spacing 1.5 *)
      F_HPDF_Page_SetCharSpace.f' (page, 1.5);

      F_HPDF_Page_BeginText.f' page;
      F_HPDF_Page_TextOut.f'(page, 60.0, 100.0, samp_text2);
      F_HPDF_Page_EndText.f' page;

      (* char-spacing 1.5, word-spacing 3.5 *)
      F_HPDF_Page_SetWordSpace.f' (page, 2.5);

      F_HPDF_Page_BeginText.f' page;
      F_HPDF_Page_TextOut.f'(page, 60.0, 60.0, samp_text2);
      F_HPDF_Page_EndText.f' page;

      (* save the document to a file *)
      F_HPDF_SaveToFile.f'(pdf, fname);

      OS.Process.success
    end end end end end end)))))
  end (* local *)


  fun main (name, args) =
    (case args
       of [] => OS.Process.success
        | file_name::_ => text_demo (file_name ^ ".pdf")
    ) handle exn =>
        (print(exnMessage exn^"\n")
        ;OS.Process.failure)

end

