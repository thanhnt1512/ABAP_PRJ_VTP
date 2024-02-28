FUNCTION-POOL ZFG_AIAB_COP.                     "MESSAGE-ID ..

* INCLUDE LZFG_AIABD...                      " Local class definition
TYPES:
  BEGIN OF gty_s_postab.                                                 "Begin of S2I
    INCLUDE TYPE aimtv.
TYPES:objnr         TYPE anla-objnr,              " merker co-objekt
      zaehl         TYPE sy-tabix,    " indexzaehler
      workf(1)      TYPE c, " value ' ', " workflag = x wenn bureg bearb.
      ampel(1)      TYPE c, "value '0', " alv ampel    "4.6c
      xextend_afabe TYPE xfeld,
      END OF gty_s_postab .
TYPES:
*    TYPES:
*      gty_t_postab TYPE STANDARD TABLE OF gty_s_postab .
  BEGIN OF gty_s_ass_post.
TYPES :asset  TYPE anla-anln1,
       postab TYPE STANDARD TABLE OF gty_s_postab WITH DEFAULT KEY,

       END OF gty_s_ass_post .
TYPES:
  gty_t_ass_post TYPE TABLE OF gty_s_ass_post .
TYPES:
  BEGIN OF gty_s_ass_n,
    anln1 TYPE anln1,
  END OF  gty_s_ass_n .
TYPES:
  gty_t_ass_n TYPE TABLE OF gty_s_ass_n .

DATA : gt_ass_postab TYPE gty_t_ass_post.

TYPES:
  BEGIN OF lty_s_anek_ext.
    INCLUDE STRUCTURE anek.
TYPES:
  subta TYPE acdoca-subta,
  END OF lty_s_anek_ext.

TYPES: BEGIN OF ty_cobra_buf.
    INCLUDE STRUCTURE cobra.
TYPES: uflag LIKE dkobr-upd_flag,
       END OF ty_cobra_buf.
TYPES: ty_t_cobra_buf TYPE ty_cobra_buf OCCURS 10.

*.COBRB-Puffer mit Änderungsflag
TYPES: BEGIN OF ty_cobrb_buf.
    INCLUDE STRUCTURE cobrb.
TYPES: uflag LIKE dkobr-upd_flag,
       END OF ty_cobrb_buf.
TYPES: ty_t_cobrb_buf TYPE ty_cobrb_buf OCCURS 10.
