
structure Demo =
struct

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

  fun HPDF_Page_GetCurrentPos page =
    let
      fun get_x obj = C.Get.float' (S__HPDF_Point.f_x' obj)
      fun get_y obj = C.Get.float' (S__HPDF_Point.f_y' obj)

      val point = C.new' S__HPDF_Point.size
    in
      F_HPDF_Page_GetCurrentPos.f' (point, page);
      {x=get_x point, y=get_y point}
      before
        C.discard' point
    end

  fun main (name, args) =
    (case args
       of [] => OS.Process.failure
        | file_name::_ =>
            new_pdf (C.Ptr.fnull', C.Ptr.vNull) (fn pdf =>
            let
              val page = F_HPDF_AddPage.f' pdf
            in
              F_HPDF_Page_SetHeight.f'(page, 220.0);
              F_HPDF_Page_SetWidth.f'(page, 200.0);
              print_grid pdf page;

              (* draw pie chart
               *
               *   A: 45% Red
               *   B: 25% Blue
               *   C: 15% green
               *   D: other yellow
               *)
              (* A *)
              F_HPDF_Page_SetRGBFill.f'(page, 1.0, 0.0, 0.0);
              F_HPDF_Page_MoveTo.f'(page, 100.0, 100.0);
              F_HPDF_Page_LineTo.f'(page, 100.0, 180.0);
              F_HPDF_Page_Arc.f'(page, 100.0, 100.0, 80.0, 0.0, 360.0 * 0.45);
            let
              val pos = HPDF_Page_GetCurrentPos page
            in
              F_HPDF_Page_LineTo.f'(page, 100.0, 100.0);
              F_HPDF_Page_Fill.f' page;

              (* B *)
              F_HPDF_Page_SetRGBFill.f'(page, 0.0, 0.0, 1.0);
              F_HPDF_Page_MoveTo.f'(page, 100.0, 100.0);
              F_HPDF_Page_LineTo.f'(page, #x pos, #y pos);
              F_HPDF_Page_Arc.f'(page, 100.0, 100.0, 80.0, 360.0 * 0.45, 360.0 * 0.7);
            let
              val pos = HPDF_Page_GetCurrentPos page
            in
              F_HPDF_Page_LineTo.f'(page, 100.0, 100.0);
              F_HPDF_Page_Fill.f' page;

              (* C *)
              F_HPDF_Page_SetRGBFill.f'(page, 0.0, 1.0, 0.0);
              F_HPDF_Page_MoveTo.f'(page, 100.0, 100.0);
              F_HPDF_Page_LineTo.f'(page, #x pos, #y pos);
              F_HPDF_Page_Arc.f'(page, 100.0, 100.0, 80.0, 360.0 * 0.7, 360.0 * 0.85);
            let
              val pos = HPDF_Page_GetCurrentPos page
            in
              F_HPDF_Page_LineTo.f'(page, 100.0, 100.0);
              F_HPDF_Page_Fill.f' page;

              (* D *)
              F_HPDF_Page_SetRGBFill.f' (page, 1.0, 1.0, 0.0);
              F_HPDF_Page_MoveTo.f' (page, 100.0, 100.0);
              F_HPDF_Page_LineTo.f'(page, #x pos, #y pos);
              F_HPDF_Page_Arc.f' (page, 100.0, 100.0, 80.0, 360.0 * 0.85, 360.0);
            let
              val pos = HPDF_Page_GetCurrentPos page
            in
              F_HPDF_Page_LineTo.f' (page, 100.0, 100.0);
              F_HPDF_Page_Fill.f' page;

              (* draw center circle *)
              F_HPDF_Page_SetGrayStroke.f' (page, 0.0);
              F_HPDF_Page_SetGrayFill.f' (page, 1.0);
              F_HPDF_Page_Circle.f' (page, 100.0, 100.0, 30.0);
              F_HPDF_Page_Fill.f' page;

              use_cstring (file_name^".pdf") (fn fname =>
              F_HPDF_SaveToFile.f'(pdf, fname));
              OS.Process.success
            end end end end end)
    ) handle exn => (print(exnMessage exn^"\n")
                    ;OS.Process.failure)

end

