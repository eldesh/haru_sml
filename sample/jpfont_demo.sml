

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


  fun jpfont_demo file_name =
    new_pdf (C.Ptr.fnull', C.Ptr.vNull) (fn pdf =>
    let
    in
      Doc.SetCompressionMode (pdf, HPDF_COMP_ALL);
      Doc.UseJPEncodings pdf;
      Doc.UseJPFonts pdf;
    let
      val fonts =
        Vector.map (fn (font,enc) => Doc.GetFont (pdf, SOME font, SOME enc))
        (Vector.fromList
        [ ("MS-Mincyo", "90ms-RKSJ-H")
        , ("MS-Mincyo,Bold", "90ms-RKSJ-H")
        , ("MS-Mincyo,Italic", "90ms-RKSJ-H")
        , ("MS-Mincyo,BoldItalic", "90ms-RKSJ-H")
        , ("MS-PMincyo", "90msp-RKSJ-H")
        , ("MS-PMincyo,Bold", "90msp-RKSJ-H")
        , ("MS-PMincyo,Italic", "90msp-RKSJ-H")
        , ("MS-PMincyo,BoldItalic", "90msp-RKSJ-H")
        , ("MS-Gothic", "90ms-RKSJ-H")
        , ("MS-Gothic,Bold", "90ms-RKSJ-H")
        , ("MS-Gothic,Italic", "90ms-RKSJ-H")
        , ("MS-Gothic,BoldItalic", "90ms-RKSJ-H")
        , ("MS-PGothic", "90msp-RKSJ-H")
        , ("MS-PGothic,Bold", "90msp-RKSJ-H")
        , ("MS-PGothic,Italic", "90msp-RKSJ-H")
        , ("MS-PGothic,BoldItalic", "90msp-RKSJ-H")
        ])
    in
      (* Set page mode to use outines. *)
      Doc.SetPageMode (pdf, e_HPDF_PAGE_MODE_USE_OUTLINE);
    let
      (* create outline root. *)
      val root = Doc.CreateOutline (pdf, C.Ptr.null', SOME "JP font demo", C.Ptr.null')
    in
      Outline.SetOpened (root, true);

      for 0 (fn i=> i <= 15) (fn i=> i+1) (fn i =>
      let
        (* add a new page object. *)
        val page = Doc.AddPage pdf
        (* create outline entry *)
        val font = Font.GetFontName (Vector.sub(fonts,i))
        (*
        val outline = Doc.CreateOutline (pdf, root
                            , font, C.Ptr.null')
                            *)
        (*
        val dst = Page.CreateDestination page
        *)
      in
        (*
        Outline.SetDestination (outline, dst);
        *)
      let
        val title_font = () (* Doc.GetFont (pdf, SOME "Helvetica", NONE) *)
      in
        ()
      end
      end);
      ();
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

