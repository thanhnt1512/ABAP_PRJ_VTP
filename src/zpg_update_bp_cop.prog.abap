*&---------------------------------------------------------------------*
*& Report ZPG_UPDATE_BP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
INCLUDE ZIN_UPDATE_BP_COP_TOP.
*INCLUDE zin_update_bp_top.
INCLUDE ZIN_UPDATE_BP_COP_F01.
*INCLUDE zin_update_bp_f01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM get_name_file.

INITIALIZATION.
  CONCATENATE icon_export 'Download Template'
               INTO sscrfields-functxt_01 SEPARATED BY space.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM download_template.
  ENDCASE.

START-OF-SELECTION.
*  IF p_file IS INITIAL.
*    MESSAGE 'Fill out all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
  PERFORM main.
