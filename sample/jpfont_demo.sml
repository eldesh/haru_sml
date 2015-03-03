

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

  fun for i cond succ f =
    if cond i then (f i; for (succ i) cond succ f)
    else ()

  fun flip f x y = f y x

  fun jpfont_demo fname =
    new_pdf (C.Ptr.fnull', C.Ptr.vNull) (fn pdf =>
    let
      val PAGE_HEIGHT = 210.0
      val samp_text = using (fn()=> TextIO.openIn "sample/mbtext/sjis.txt")
                            TextIO.closeIn
                            TextIO.inputAll
    in
      (* configure pdf-document to be compressed. *)
      Doc.SetCompressionMode (pdf, HPDF_COMP_ALL);
      (* declaration for using Japanese font, encoding. *)
      Doc.UseJPEncodings pdf;
      Doc.UseJPFonts pdf;
    let
      val fonts =
        map (fn (font,enc) => Doc.GetFont (pdf, font, SOME enc))
        [ ("MS-Mincho", "90ms-RKSJ-H")
        , ("MS-Mincho,Bold", "90ms-RKSJ-H")
        , ("MS-Mincho,Italic", "90ms-RKSJ-H")
        , ("MS-Mincho,BoldItalic", "90ms-RKSJ-H")
        , ("MS-PMincho", "90msp-RKSJ-H")
        , ("MS-PMincho,Bold", "90msp-RKSJ-H")
        , ("MS-PMincho,Italic", "90msp-RKSJ-H")
        , ("MS-PMincho,BoldItalic", "90msp-RKSJ-H")
        , ("MS-Gothic", "90ms-RKSJ-H")
        , ("MS-Gothic,Bold", "90ms-RKSJ-H")
        , ("MS-Gothic,Italic", "90ms-RKSJ-H")
        , ("MS-Gothic,BoldItalic", "90ms-RKSJ-H")
        , ("MS-PGothic", "90msp-RKSJ-H")
        , ("MS-PGothic,Bold", "90msp-RKSJ-H")
        , ("MS-PGothic,Italic", "90msp-RKSJ-H")
        , ("MS-PGothic,BoldItalic", "90msp-RKSJ-H")
        ]
    in
      (* Set page mode to use outines. *)
      Doc.SetPageMode (pdf, e_HPDF_PAGE_MODE_USE_OUTLINE);
    let
      (* create outline root. *)
      val root = Doc.CreateOutline (pdf, C.Ptr.null', SOME "JP font demo", C.Ptr.null')
    in
      Outline.SetOpened (root, true);

      flip app fonts (fn font =>
      let
        (* add a new page object. *)
        val page = Doc.AddPage pdf
        (* create outline entry *)
        val outline = Doc.CreateOutline (pdf, root
                            , Font.GetFontName font, C.Ptr.null')
        val dst = Page.CreateDestination page
      in
        Outline.SetDestination (outline, dst);
      let
        val title_font = Doc.GetFont (pdf, "Helvetica", NONE)
        open Page
      in
        SetFontAndSize (page, title_font, 10.0);
        BeginText page;
        (* move the position of the text to top of the page. *)
        MoveTextPos (page, 10.0, 190.0);
        ShowText (page, valOf (Font.GetFontName font));

        SetFontAndSize (page, font, 15.0);
        MoveTextPos (page, 10.0, ~20.0);
        ShowText (page, "abcdefghijklmnopqrstuvwxyz");

        MoveTextPos (page,  0.0, ~20.0);
        ShowText (page, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");

        MoveTextPos (page,  0.0, ~20.0);
        ShowText (page, "1234567890");
        MoveTextPos (page,  0.0, ~20.0);

        SetFontAndSize (page, font, 10.0);
        ShowText (page, samp_text);
        MoveTextPos (page,  0.0, ~18.0);

        SetFontAndSize (page, font, 16.0);
        ShowText (page, samp_text);
        MoveTextPos (page,  0.0, ~27.0);

        SetFontAndSize (page, font, 23.0);
        ShowText (page, samp_text);
        MoveTextPos (page,  0.0, ~36.0);

        SetFontAndSize (page, font, 30.0);
        ShowText (page, samp_text);
      let
        val p = GetCurrentTextPos page
      in
        (* finish to print text. *)
        EndText page;
        SetLineWidth (page, 0.5);
      let
        val x_pos = 20.0
      in
        for 0 (fn j=> j < size samp_text div 2) (fn j=>j+1) (fn j=>
        (
          MoveTo (page, x_pos + 30.0 * real j, #y p - 10.0);
          LineTo (page, x_pos + 30.0 * real j, #y p - 12.0);
          Stroke page
        ));
        SetWidth (page, #x p + 20.0);
        SetHeight(page, PAGE_HEIGHT);

        MoveTo (page, 10.0, PAGE_HEIGHT - 25.0);
        LineTo (page, #x p + 10.0, PAGE_HEIGHT - 25.0);
        Stroke page;

        MoveTo (page, 10.0, PAGE_HEIGHT - 85.0);
        LineTo (page, #x p + 10.0, PAGE_HEIGHT - 85.0);
        Stroke page;

        MoveTo (page, 10.0, #y p - 12.0);
        LineTo (page, #x p + 10.0, #y p - 12.0);
        Stroke page;
        ()
      end end end end); (* app fonts *)
      Doc.SaveToFile (pdf, fname);
      OS.Process.success
    end
    end
    end)


  fun main (name, args) =
    (case args
       of [] => OS.Process.success
        | file_name::_ => jpfont_demo (file_name ^ ".pdf")
    ) handle exn =>
        (print(exnMessage exn^"\n")
        ;OS.Process.failure)

end

