
structure HPDF_Status =
struct
  (* error-code *)
  datatype t = HPDF_ARRAY_COUNT_ERR
             | HPDF_ARRAY_ITEM_NOT_FOUND
             | HPDF_ARRAY_ITEM_UNEXPECTED_TYPE
             | HPDF_BINARY_LENGTH_ERR
             | HPDF_CANNOT_GET_PALLET
             | HPDF_DICT_COUNT_ERR
             | HPDF_DICT_ITEM_NOT_FOUND
             | HPDF_DICT_ITEM_UNEXPECTED_TYPE
             | HPDF_DICT_STREAM_LENGTH_NOT_FOUND
             | HPDF_DOC_ENCRYPTDICT_NOT_FOUND
             | HPDF_DOC_INVALID_OBJECT
             (*                                                0x100D *)
             | HPDF_DUPLICATE_REGISTRATION
             | HPDF_EXCEED_JWW_CODE_NUM_LIMIT
             (*                                                0x1010 *)
             | HPDF_ENCRYPT_INVALID_PASSWORD
             (*                                                0x1012 *)
             | HPDF_ERR_UNKNOWN_CLASS
             | HPDF_EXCEED_GSTATE_LIMIT
             | HPDF_FAILD_TO_ALLOC_MEM
             | HPDF_FILE_IO_ERROR
             | HPDF_FILE_OPEN_ERROR
             (*                                                0x1018 *)
             | HPDF_FONT_EXISTS
             | HPDF_FONT_INVALID_WIDTHS_TABLE
             | HPDF_INVALID_AFM_HEADER
             | HPDF_INVALID_ANNOTATION
             (*                                                0x101D *)
             | HPDF_INVALID_BIT_PER_COMPONENT
             | HPDF_INVALID_CHAR_MATRICS_DATA
             | HPDF_INVALID_COLOR_SPACE
             | HPDF_INVALID_COMPRESSION_MODE
             | HPDF_INVALID_DATE_TIME
             | HPDF_INVALID_DESTINATION
             (*                                                0x1024 *)
             | HPDF_INVALID_DOCUMENT
             | HPDF_INVALID_DOCUMENT_STATE
             | HPDF_INVALID_ENCODER
             | HPDF_INVALID_ENCODER_TYPE
             (*                                                0x1029 *)
             (*                                                0x102A *)
             | HPDF_INVALID_ENCODING_NAME
             | HPDF_INVALID_ENCRYPT_KEY_LEN
             | HPDF_INVALID_FONTDEF_DATA
             | HPDF_INVALID_FONTDEF_TYPE
             | HPDF_INVALID_FONT_NAME
             | HPDF_INVALID_IMAGE
             | HPDF_INVALID_JPEG_DATA
             | HPDF_INVALID_N_DATA
             | HPDF_INVALID_OBJECT
             | HPDF_INVALID_OBJ_ID
             | HPDF_INVALID_OPERATION
             | HPDF_INVALID_OUTLINE
             | HPDF_INVALID_PAGE
             | HPDF_INVALID_PAGES
             | HPDF_INVALID_PARAMETER
             (*                                                0x103A *)
             | HPDF_INVALID_PNG_IMAGE
             | HPDF_INVALID_STREAM
             | HPDF_MISSING_FILE_NAME_ENTRY
             (*                                                0x103E *)
             | HPDF_INVALID_TTC_FILE
             | HPDF_INVALID_TTC_INDEX
             | HPDF_INVALID_WX_DATA
             | HPDF_ITEM_NOT_FOUND
             | HPDF_LIBPNG_ERROR
             | HPDF_NAME_INVALID_VALUE
             | HPDF_NAME_OUT_OF_RANGE
             (*                                                0x1046 *)
             (*                                                0x1047 *)
             | HPDF_PAGE_INVALID_PARAM_COUNT
             | HPDF_PAGES_MISSING_KIDS_ENTRY
             | HPDF_PAGE_CANNOT_FIND_OBJECT
             | HPDF_PAGE_CANNOT_GET_ROOT_PAGES
             | HPDF_PAGE_CANNOT_RESTORE_GSTATE
             | HPDF_PAGE_CANNOT_SET_PARENT
             | HPDF_PAGE_FONT_NOT_FOUND
             | HPDF_PAGE_INVALID_FONT
             | HPDF_PAGE_INVALID_FONT_SIZE
             | HPDF_PAGE_INVALID_GMODE
             | HPDF_PAGE_INVALID_INDEX
             | HPDF_PAGE_INVALID_ROTATE_VALUE
             | HPDF_PAGE_INVALID_SIZE
             | HPDF_PAGE_INVALID_XOBJECT
             | HPDF_PAGE_OUT_OF_RANGE
             | HPDF_REAL_OUT_OF_RANGE
             | HPDF_STREAM_EOF
             | HPDF_STREAM_READLN_CONTINUE
             (*                                                0x105A *)
             | HPDF_STRING_OUT_OF_RANGE
             | HPDF_THIS_FUNC_WAS_SKIPPED
             | HPDF_TTF_CANNOT_EMBEDDING_FONT
             | HPDF_TTF_INVALID_CMAP
             | HPDF_TTF_INVALID_FOMAT
             | HPDF_TTF_MISSING_TABLE
             | HPDF_UNSUPPORTED_FONT_TYPE
             | HPDF_UNSUPPORTED_FUNC
             | HPDF_UNSUPPORTED_JPEG_FORMAT
             | HPDF_UNSUPPORTED_TYPE1_FONT
             | HPDF_XREF_COUNT_ERR
             | HPDF_ZLIB_ERROR
             | HPDF_INVALID_PAGE_INDEX
             | HPDF_INVALID_URI
             | HPDF_PAGE_LAYOUT_OUT_OF_RANGE
             | HPDF_PAGE_MODE_OUT_OF_RANGE
             | HPDF_PAGE_NUM_STYLE_OUT_OF_RANGE
             | HPDF_ANNOT_INVALID_ICON
             | HPDF_ANNOT_INVALID_BORDER_STYLE
             | HPDF_PAGE_INVALID_DIRECTION
             | HPDF_INVALID_FONT
             | HPDF_PAGE_INSUFFICIENT_SPACE
             | HPDF_PAGE_INVALID_DISPLAY_TIME
             | HPDF_PAGE_INVALID_TRANSITION_TIME
             | HPDF_INVALID_PAGE_SLIDESHOW_TYPE
             | HPDF_EXT_GSTATE_OUT_OF_RANGE
             | HPDF_INVALID_EXT_GSTATE
             | HPDF_EXT_GSTATE_READ_ONLY
             | HPDF_INVALID_U3D_DATA
             | HPDF_NAME_CANNOT_GET_NAMES
             | HPDF_INVALID_ICC_COMPONENT_NUM

  fun toWord f : MLRep.Unsigned.word =
    case f
      of HPDF_ARRAY_COUNT_ERR => 0wx1001
       | HPDF_ARRAY_ITEM_NOT_FOUND => 0wx1002
       | HPDF_ARRAY_ITEM_UNEXPECTED_TYPE => 0wx1003
       | HPDF_BINARY_LENGTH_ERR => 0wx1004
       | HPDF_CANNOT_GET_PALLET => 0wx1005
       | HPDF_DICT_COUNT_ERR => 0wx1007
       | HPDF_DICT_ITEM_NOT_FOUND => 0wx1008
       | HPDF_DICT_ITEM_UNEXPECTED_TYPE => 0wx1009
       | HPDF_DICT_STREAM_LENGTH_NOT_FOUND => 0wx100A
       | HPDF_DOC_ENCRYPTDICT_NOT_FOUND => 0wx100B
       | HPDF_DOC_INVALID_OBJECT => 0wx100C
       (*                                                 0x100D  *)
       | HPDF_DUPLICATE_REGISTRATION => 0wx100E
       | HPDF_EXCEED_JWW_CODE_NUM_LIMIT => 0wx100F
       (*                                                 0x1010  *)
       | HPDF_ENCRYPT_INVALID_PASSWORD => 0wx1011
       (*                                                 0x1012  *)
       | HPDF_ERR_UNKNOWN_CLASS => 0wx1013
       | HPDF_EXCEED_GSTATE_LIMIT => 0wx1014
       | HPDF_FAILD_TO_ALLOC_MEM => 0wx1015
       | HPDF_FILE_IO_ERROR => 0wx1016
       | HPDF_FILE_OPEN_ERROR => 0wx1017
       (*                                                 0x1018  *)
       | HPDF_FONT_EXISTS => 0wx1019
       | HPDF_FONT_INVALID_WIDTHS_TABLE => 0wx101A
       | HPDF_INVALID_AFM_HEADER => 0wx101B
       | HPDF_INVALID_ANNOTATION => 0wx101C
       (*                                                 0x101D  *)
       | HPDF_INVALID_BIT_PER_COMPONENT => 0wx101E
       | HPDF_INVALID_CHAR_MATRICS_DATA => 0wx101F
       | HPDF_INVALID_COLOR_SPACE => 0wx1020
       | HPDF_INVALID_COMPRESSION_MODE => 0wx1021
       | HPDF_INVALID_DATE_TIME => 0wx1022
       | HPDF_INVALID_DESTINATION => 0wx1023
       (*                                                 0x1024  *)
       | HPDF_INVALID_DOCUMENT => 0wx1025
       | HPDF_INVALID_DOCUMENT_STATE => 0wx1026
       | HPDF_INVALID_ENCODER => 0wx1027
       | HPDF_INVALID_ENCODER_TYPE => 0wx1028
       (*                                                 0x1029  *)
       (*                                                 0x102A  *)
       | HPDF_INVALID_ENCODING_NAME => 0wx102B
       | HPDF_INVALID_ENCRYPT_KEY_LEN => 0wx102C
       | HPDF_INVALID_FONTDEF_DATA => 0wx102D
       | HPDF_INVALID_FONTDEF_TYPE => 0wx102E
       | HPDF_INVALID_FONT_NAME => 0wx102F
       | HPDF_INVALID_IMAGE => 0wx1030
       | HPDF_INVALID_JPEG_DATA => 0wx1031
       | HPDF_INVALID_N_DATA => 0wx1032
       | HPDF_INVALID_OBJECT => 0wx1033
       | HPDF_INVALID_OBJ_ID => 0wx1034
       | HPDF_INVALID_OPERATION => 0wx1035
       | HPDF_INVALID_OUTLINE => 0wx1036
       | HPDF_INVALID_PAGE => 0wx1037
       | HPDF_INVALID_PAGES => 0wx1038
       | HPDF_INVALID_PARAMETER => 0wx1039
       (*                                                 0x103A  *)
       | HPDF_INVALID_PNG_IMAGE => 0wx103B
       | HPDF_INVALID_STREAM => 0wx103C
       | HPDF_MISSING_FILE_NAME_ENTRY => 0wx103D
       (*                                                 0x103E  *)
       | HPDF_INVALID_TTC_FILE => 0wx103F
       | HPDF_INVALID_TTC_INDEX => 0wx1040
       | HPDF_INVALID_WX_DATA => 0wx1041
       | HPDF_ITEM_NOT_FOUND => 0wx1042
       | HPDF_LIBPNG_ERROR => 0wx1043
       | HPDF_NAME_INVALID_VALUE => 0wx1044
       | HPDF_NAME_OUT_OF_RANGE => 0wx1045
       (*                                                 0x1046  *)
       (*                                                 0x1047  *)
       | HPDF_PAGE_INVALID_PARAM_COUNT => 0wx1048
       | HPDF_PAGES_MISSING_KIDS_ENTRY => 0wx1049
       | HPDF_PAGE_CANNOT_FIND_OBJECT => 0wx104A
       | HPDF_PAGE_CANNOT_GET_ROOT_PAGES => 0wx104B
       | HPDF_PAGE_CANNOT_RESTORE_GSTATE => 0wx104C
       | HPDF_PAGE_CANNOT_SET_PARENT => 0wx104D
       | HPDF_PAGE_FONT_NOT_FOUND => 0wx104E
       | HPDF_PAGE_INVALID_FONT => 0wx104F
       | HPDF_PAGE_INVALID_FONT_SIZE => 0wx1050
       | HPDF_PAGE_INVALID_GMODE => 0wx1051
       | HPDF_PAGE_INVALID_INDEX => 0wx1052
       | HPDF_PAGE_INVALID_ROTATE_VALUE => 0wx1053
       | HPDF_PAGE_INVALID_SIZE => 0wx1054
       | HPDF_PAGE_INVALID_XOBJECT => 0wx1055
       | HPDF_PAGE_OUT_OF_RANGE => 0wx1056
       | HPDF_REAL_OUT_OF_RANGE => 0wx1057
       | HPDF_STREAM_EOF => 0wx1058
       | HPDF_STREAM_READLN_CONTINUE => 0wx1059
       (*                                                 0x105A  *)
       | HPDF_STRING_OUT_OF_RANGE => 0wx105B
       | HPDF_THIS_FUNC_WAS_SKIPPED => 0wx105C
       | HPDF_TTF_CANNOT_EMBEDDING_FONT => 0wx105D
       | HPDF_TTF_INVALID_CMAP => 0wx105E
       | HPDF_TTF_INVALID_FOMAT => 0wx105F
       | HPDF_TTF_MISSING_TABLE => 0wx1060
       | HPDF_UNSUPPORTED_FONT_TYPE => 0wx1061
       | HPDF_UNSUPPORTED_FUNC => 0wx1062
       | HPDF_UNSUPPORTED_JPEG_FORMAT => 0wx1063
       | HPDF_UNSUPPORTED_TYPE1_FONT => 0wx1064
       | HPDF_XREF_COUNT_ERR => 0wx1065
       | HPDF_ZLIB_ERROR => 0wx1066
       | HPDF_INVALID_PAGE_INDEX => 0wx1067
       | HPDF_INVALID_URI => 0wx1068
       | HPDF_PAGE_LAYOUT_OUT_OF_RANGE => 0wx1069
       | HPDF_PAGE_MODE_OUT_OF_RANGE => 0wx1070
       | HPDF_PAGE_NUM_STYLE_OUT_OF_RANGE => 0wx1071
       | HPDF_ANNOT_INVALID_ICON => 0wx1072
       | HPDF_ANNOT_INVALID_BORDER_STYLE => 0wx1073
       | HPDF_PAGE_INVALID_DIRECTION => 0wx1074
       | HPDF_INVALID_FONT => 0wx1075
       | HPDF_PAGE_INSUFFICIENT_SPACE => 0wx1076
       | HPDF_PAGE_INVALID_DISPLAY_TIME => 0wx1077
       | HPDF_PAGE_INVALID_TRANSITION_TIME => 0wx1078
       | HPDF_INVALID_PAGE_SLIDESHOW_TYPE => 0wx1079
       | HPDF_EXT_GSTATE_OUT_OF_RANGE => 0wx1080
       | HPDF_INVALID_EXT_GSTATE => 0wx1081
       | HPDF_EXT_GSTATE_READ_ONLY => 0wx1082
       | HPDF_INVALID_U3D_DATA => 0wx1083
       | HPDF_NAME_CANNOT_GET_NAMES => 0wx1084
       | HPDF_INVALID_ICC_COMPONENT_NUM => 0wx1085

  fun fromWord (w:MLRep.Unsigned.word) =
    case w
      of 0wx1001 => HPDF_ARRAY_COUNT_ERR
       | 0wx1002 => HPDF_ARRAY_ITEM_NOT_FOUND
       | 0wx1003 => HPDF_ARRAY_ITEM_UNEXPECTED_TYPE
       | 0wx1004 => HPDF_BINARY_LENGTH_ERR
       | 0wx1005 => HPDF_CANNOT_GET_PALLET
       | 0wx1007 => HPDF_DICT_COUNT_ERR
       | 0wx1008 => HPDF_DICT_ITEM_NOT_FOUND
       | 0wx1009 => HPDF_DICT_ITEM_UNEXPECTED_TYPE
       | 0wx100A => HPDF_DICT_STREAM_LENGTH_NOT_FOUND
       | 0wx100B => HPDF_DOC_ENCRYPTDICT_NOT_FOUND
       | 0wx100C => HPDF_DOC_INVALID_OBJECT
       (*                                                 0x100D  *)
       | 0wx100E => HPDF_DUPLICATE_REGISTRATION
       | 0wx100F => HPDF_EXCEED_JWW_CODE_NUM_LIMIT
       (*                                                 0x1010  *)
       | 0wx1011 => HPDF_ENCRYPT_INVALID_PASSWORD
       (*                                                 0x1012  *)
       | 0wx1013 => HPDF_ERR_UNKNOWN_CLASS
       | 0wx1014 => HPDF_EXCEED_GSTATE_LIMIT
       | 0wx1015 => HPDF_FAILD_TO_ALLOC_MEM
       | 0wx1016 => HPDF_FILE_IO_ERROR
       | 0wx1017 => HPDF_FILE_OPEN_ERROR
       (*                                                 0x1018  *)
       | 0wx1019 => HPDF_FONT_EXISTS
       | 0wx101A => HPDF_FONT_INVALID_WIDTHS_TABLE
       | 0wx101B => HPDF_INVALID_AFM_HEADER
       | 0wx101C => HPDF_INVALID_ANNOTATION
       (*                                                 0x101D  *)
       | 0wx101E => HPDF_INVALID_BIT_PER_COMPONENT
       | 0wx101F => HPDF_INVALID_CHAR_MATRICS_DATA
       | 0wx1020 => HPDF_INVALID_COLOR_SPACE
       | 0wx1021 => HPDF_INVALID_COMPRESSION_MODE
       | 0wx1022 => HPDF_INVALID_DATE_TIME
       | 0wx1023 => HPDF_INVALID_DESTINATION
       (*                                                 0x1024  *)
       | 0wx1025 => HPDF_INVALID_DOCUMENT
       | 0wx1026 => HPDF_INVALID_DOCUMENT_STATE
       | 0wx1027 => HPDF_INVALID_ENCODER
       | 0wx1028 => HPDF_INVALID_ENCODER_TYPE
       (*                                                 0x1029  *)
       (*                                                 0x102A  *)
       | 0wx102B => HPDF_INVALID_ENCODING_NAME
       | 0wx102C => HPDF_INVALID_ENCRYPT_KEY_LEN
       | 0wx102D => HPDF_INVALID_FONTDEF_DATA
       | 0wx102E => HPDF_INVALID_FONTDEF_TYPE
       | 0wx102F => HPDF_INVALID_FONT_NAME
       | 0wx1030 => HPDF_INVALID_IMAGE
       | 0wx1031 => HPDF_INVALID_JPEG_DATA
       | 0wx1032 => HPDF_INVALID_N_DATA
       | 0wx1033 => HPDF_INVALID_OBJECT
       | 0wx1034 => HPDF_INVALID_OBJ_ID
       | 0wx1035 => HPDF_INVALID_OPERATION
       | 0wx1036 => HPDF_INVALID_OUTLINE
       | 0wx1037 => HPDF_INVALID_PAGE
       | 0wx1038 => HPDF_INVALID_PAGES
       | 0wx1039 => HPDF_INVALID_PARAMETER
       (*                                                 0x103A  *)
       | 0wx103B => HPDF_INVALID_PNG_IMAGE
       | 0wx103C => HPDF_INVALID_STREAM
       | 0wx103D => HPDF_MISSING_FILE_NAME_ENTRY
       (*                                                 0x103E  *)
       | 0wx103F => HPDF_INVALID_TTC_FILE
       | 0wx1040 => HPDF_INVALID_TTC_INDEX
       | 0wx1041 => HPDF_INVALID_WX_DATA
       | 0wx1042 => HPDF_ITEM_NOT_FOUND
       | 0wx1043 => HPDF_LIBPNG_ERROR
       | 0wx1044 => HPDF_NAME_INVALID_VALUE
       | 0wx1045 => HPDF_NAME_OUT_OF_RANGE
       (*                                                 0x1046  *)
       (*                                                 0x1047  *)
       | 0wx1048 => HPDF_PAGE_INVALID_PARAM_COUNT
       | 0wx1049 => HPDF_PAGES_MISSING_KIDS_ENTRY
       | 0wx104A => HPDF_PAGE_CANNOT_FIND_OBJECT
       | 0wx104B => HPDF_PAGE_CANNOT_GET_ROOT_PAGES
       | 0wx104C => HPDF_PAGE_CANNOT_RESTORE_GSTATE
       | 0wx104D => HPDF_PAGE_CANNOT_SET_PARENT
       | 0wx104E => HPDF_PAGE_FONT_NOT_FOUND
       | 0wx104F => HPDF_PAGE_INVALID_FONT
       | 0wx1050 => HPDF_PAGE_INVALID_FONT_SIZE
       | 0wx1051 => HPDF_PAGE_INVALID_GMODE
       | 0wx1052 => HPDF_PAGE_INVALID_INDEX
       | 0wx1053 => HPDF_PAGE_INVALID_ROTATE_VALUE
       | 0wx1054 => HPDF_PAGE_INVALID_SIZE
       | 0wx1055 => HPDF_PAGE_INVALID_XOBJECT
       | 0wx1056 => HPDF_PAGE_OUT_OF_RANGE
       | 0wx1057 => HPDF_REAL_OUT_OF_RANGE
       | 0wx1058 => HPDF_STREAM_EOF
       | 0wx1059 => HPDF_STREAM_READLN_CONTINUE
       (*                                                 0x105A  *)
       | 0wx105B => HPDF_STRING_OUT_OF_RANGE
       | 0wx105C => HPDF_THIS_FUNC_WAS_SKIPPED
       | 0wx105D => HPDF_TTF_CANNOT_EMBEDDING_FONT
       | 0wx105E => HPDF_TTF_INVALID_CMAP
       | 0wx105F => HPDF_TTF_INVALID_FOMAT
       | 0wx1060 => HPDF_TTF_MISSING_TABLE
       | 0wx1061 => HPDF_UNSUPPORTED_FONT_TYPE
       | 0wx1062 => HPDF_UNSUPPORTED_FUNC
       | 0wx1063 => HPDF_UNSUPPORTED_JPEG_FORMAT
       | 0wx1064 => HPDF_UNSUPPORTED_TYPE1_FONT
       | 0wx1065 => HPDF_XREF_COUNT_ERR
       | 0wx1066 => HPDF_ZLIB_ERROR
       | 0wx1067 => HPDF_INVALID_PAGE_INDEX
       | 0wx1068 => HPDF_INVALID_URI
       | 0wx1069 => HPDF_PAGE_LAYOUT_OUT_OF_RANGE
       | 0wx1070 => HPDF_PAGE_MODE_OUT_OF_RANGE
       | 0wx1071 => HPDF_PAGE_NUM_STYLE_OUT_OF_RANGE
       | 0wx1072 => HPDF_ANNOT_INVALID_ICON
       | 0wx1073 => HPDF_ANNOT_INVALID_BORDER_STYLE
       | 0wx1074 => HPDF_PAGE_INVALID_DIRECTION
       | 0wx1075 => HPDF_INVALID_FONT
       | 0wx1076 => HPDF_PAGE_INSUFFICIENT_SPACE
       | 0wx1077 => HPDF_PAGE_INVALID_DISPLAY_TIME
       | 0wx1078 => HPDF_PAGE_INVALID_TRANSITION_TIME
       | 0wx1079 => HPDF_INVALID_PAGE_SLIDESHOW_TYPE
       | 0wx1080 => HPDF_EXT_GSTATE_OUT_OF_RANGE
       | 0wx1081 => HPDF_INVALID_EXT_GSTATE
       | 0wx1082 => HPDF_EXT_GSTATE_READ_ONLY
       | 0wx1083 => HPDF_INVALID_U3D_DATA
       | 0wx1084 => HPDF_NAME_CANNOT_GET_NAMES
       | 0wx1085 => HPDF_INVALID_ICC_COMPONENT_NUM
       | _ => raise Domain

end


