
(**
 * viewer preferences definitions
 *)
structure HPDF_ViewerPreference =
struct
  datatype t = HPDF_HIDE_TOOLBAR
             | HPDF_HIDE_MENUBAR
             | HPDF_HIDE_WINDOW_UI
             | HPDF_FIT_WINDOW
             | HPDF_CENTER_WINDOW
             | HPDF_PRINT_SCALING_NONE

  fun toWord f : SysWord.word =
    case f
      of HPDF_HIDE_TOOLBAR       => 0w1
       | HPDF_HIDE_MENUBAR       => 0w2
       | HPDF_HIDE_WINDOW_UI     => 0w4
       | HPDF_FIT_WINDOW         => 0w8
       | HPDF_CENTER_WINDOW      => 0w16
       | HPDF_PRINT_SCALING_NONE => 0w32

  fun fromWord (w:SysWord.word) =
    case w
      of 0w1  => HPDF_HIDE_TOOLBAR
       | 0w2  => HPDF_HIDE_MENUBAR
       | 0w4  => HPDF_HIDE_WINDOW_UI
       | 0w8  => HPDF_FIT_WINDOW
       | 0w16 => HPDF_CENTER_WINDOW
       | 0w32 => HPDF_PRINT_SCALING_NONE
end

