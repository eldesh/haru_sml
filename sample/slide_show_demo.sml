

structure Demo =
struct
local
  open Hpdf
in

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
            let val pdf = Doc.New (err, data) in
              pdf before
                    check_null' pdf ("cannot create PdfDoc object")
            end)
          Doc.Free
          f

  local
    val seed = ref Rand.randMin
  in
    fun rand () =
      Rand.norm (!seed)
      before
        seed := Rand.random (!seed)
  end

  fun TransitionStyle_toString style =
    case style
      of e_HPDF_TS_WIPE_RIGHT => "HPDF_TS_WIPE_RIGHT"
       | e_HPDF_TS_WIPE_UP => "HPDF_TS_WIPE_UP" 
       | e_HPDF_TS_WIPE_LEFT => "HPDF_TS_WIPE_LEFT"
       | e_HPDF_TS_WIPE_DOWN => "HPDF_TS_WIPE_DOWN" 
       | e_HPDF_TS_BARN_DOORS_HORIZONTAL_OUT => "HPDF_TS_BARN_DOORS_HORIZONTAL_OUT"
       | e_HPDF_TS_BARN_DOORS_HORIZONTAL_IN => "HPDF_TS_BARN_DOORS_HORIZONTAL_IN"
       | e_HPDF_TS_BARN_DOORS_VERTICAL_OUT => "HPDF_TS_BARN_DOORS_VERTICAL_OUT"
       | e_HPDF_TS_BARN_DOORS_VERTICAL_IN => "HPDF_TS_BARN_DOORS_VERTICAL_IN" 
       | e_HPDF_TS_BOX_OUT => "HPDF_TS_BOX_OUT"
       | e_HPDF_TS_BOX_IN => "HPDF_TS_BOX_IN" 
       | e_HPDF_TS_BLINDS_HORIZONTAL => "HPDF_TS_BLINDS_HORIZONTAL"
       | e_HPDF_TS_BLINDS_VERTICAL => "HPDF_TS_BLINDS_VERTICAL" 
       | e_HPDF_TS_DISSOLVE => "HPDF_TS_DISSOLVE"
       | e_HPDF_TS_GLITTER_RIGHT => "HPDF_TS_GLITTER_RIGHT" 
       | e_HPDF_TS_GLITTER_DOWN => "HPDF_TS_GLITTER_DOWN"
       | e_HPDF_TS_GLITTER_TOP_LEFT_TO_BOTTOM_RIGHT => "HPDF_TS_GLITTER_TOP_LEFT_TO_BOTTOM_RIGHT" 
       | e_HPDF_TS_REPLACE => "HPDF_TS_REPLACE"
       | e_HPDF_TS_EOF => "HPDF_TS_EOF"

  fun print_page page font style prev next =
  let
    val r = rand ()
    val g = rand ()
    val b = rand ()
    val caption = TransitionStyle_toString style
  in
    Page.SetWidth   (page, 800.0);
    Page.SetHeight  (page, 600.0);
    Page.SetRGBFill (page, r, g, b);
    Page.Rectangle  (page, 0.0, 0.0, 800.0, 600.0);
    Page.Fill        page;
    Page.SetRGBFill (page, 1.0-r, 1.0-g, 1.0-b);
    Page.SetFontAndSize(page, font, 30.0);

    Page.BeginText page;
    Page.SetTextMatrix (page, 0.8, 0.0, 0.0, 1.0, 0.0, 0.0);
    Page.TextOut (page, 50.0, 530.0, caption);

    Page.SetTextMatrix (page, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0);
    Page.SetFontAndSize(page, font, 20.0);
    Page.TextOut (page, 55.0, 300.0
                    , "Type \"Ctrl+L\" in order to return from full screen mode.");
    Page.EndText page;

    Page.SetSlideShow (page, style, 5.0, 1.0);
    Page.SetFontAndSize(page, font, 20.0);

    if not (C.Ptr.isNull' next) then
      (Page.BeginText page;
       Page.TextOut (page, 680.0, 50.0, "Next=>");
       Page.EndText page;
       let
         val dst = Page.CreateDestination next
       in
         Destination.SetFit dst;
       let
         val rect = { left   = 680.0
                    , right  = 750.0
                    , top    =  70.0
                    , bottom =  50.0
                    }
         val annot = Page.CreateLinkAnnot (page, rect, dst)
       in
         Annotation.SetBorderStyle (annot, 0.0, 0w0, 0w0);
         Annotation.SetHighlightMode (annot, e_HPDF_ANNOT_INVERT_BOX)
       end end; ())
    else ();

    if not (C.Ptr.isNull' prev) then
      (Page.BeginText page;
       Page.TextOut (page, 50.0, 50.0, "<=Prev");
       Page.EndText page;

       let
         val dst = Page.CreateDestination prev
       in
         Destination.SetFit dst;
       let
         val rect = { left   =  50.0
                    , right  = 110.0
                    , top    =  70.0
                    , bottom =  50.0
                    }
         val annot = Page.CreateLinkAnnot (page, rect, dst)
       in
         Annotation.SetBorderStyle (annot, 0.0, 0w0, 0w0);
         Annotation.SetHighlightMode (annot, e_HPDF_ANNOT_INVERT_BOX)
       end end; ())
    else ()
  end

  val TransitionStyles =
    Vector.fromList
    [ e_HPDF_TS_WIPE_RIGHT
    , e_HPDF_TS_WIPE_UP
    , e_HPDF_TS_WIPE_LEFT
    , e_HPDF_TS_WIPE_DOWN
    , e_HPDF_TS_BARN_DOORS_HORIZONTAL_OUT
    , e_HPDF_TS_BARN_DOORS_HORIZONTAL_IN
    , e_HPDF_TS_BARN_DOORS_VERTICAL_OUT
    , e_HPDF_TS_BARN_DOORS_VERTICAL_IN
    , e_HPDF_TS_BOX_OUT
    , e_HPDF_TS_BOX_IN
    , e_HPDF_TS_BLINDS_HORIZONTAL
    , e_HPDF_TS_BLINDS_VERTICAL
    , e_HPDF_TS_DISSOLVE
    , e_HPDF_TS_GLITTER_RIGHT
    , e_HPDF_TS_GLITTER_DOWN
    , e_HPDF_TS_GLITTER_TOP_LEFT_TO_BOTTOM_RIGHT
    , e_HPDF_TS_REPLACE
    , e_HPDF_TS_EOF
    ]

  fun slide_show_demo file_name =
    new_pdf (C.Ptr.fnull', C.Ptr.vNull) (fn pdf =>
    let
      (* create default-font *)
      val font = Doc.GetFont (pdf, SOME "Courier", SOME "StandardEncoding")
      (* Add 17 pages to the document. *)
      val page = Vector.tabulate(17, fn _=> Doc.AddPage pdf)
      infix :@ 
      val op:@ = Vector.sub
    in
      Vector.foldli (fn (i,curr,prev) =>
                      (print_page curr font (TransitionStyles :@ i)
                         prev
                         (if i=Vector.length page-1
                          then C.Ptr.null'
                          else page :@ (i+1)) (* next page *)
                      ; curr))
                     C.Ptr.null'
                     page;
      Doc.SetPageMode (pdf, e_HPDF_PAGE_MODE_FULL_SCREEN);
      (* save the document to a file *)
      Doc.SaveToFile (pdf, file_name^".pdf");
      OS.Process.success
    end)

  fun main (name, args) =
    (case args
       of [] => OS.Process.failure
        | file_name::_ => slide_show_demo file_name
    ) handle exn => (print(exnMessage exn^"\n")
                    ;OS.Process.failure)

end (* local *)
end

