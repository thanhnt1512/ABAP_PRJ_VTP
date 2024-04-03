*&---------------------------------------------------------------------*
*& Report ZPG_DOWN_UP_FILE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&    Program name  : ZPG_DOWN_UP_FILE
*&    Function Code : ZFILE
*&    Report Name   : Program Upload/Download files from app server
*&    Copied from   :
*&=====================================================================*
*&            V  E  R  S  I  O  N       D  E  T  A  I  L  S
*&=====================================================================*
*& Version| Created by | Date time           | Description
*&=====================================================================*
*& 0.01   | MinhDV     | 29.06.2020 10:47:37 | Create new program
*&        |            |                     |
*&---------------------------------------------------------------------*
INCLUDE ZIN_DOWN_UP_FILE_COP_TOP.
*include zin_down_up_file_top                    .    " Global Data

* INCLUDE ZIN_DOWN_UP_FILE_O01                    .  " PBO-Modules
* INCLUDE ZIN_DOWN_UP_FILE_I01                    .  " PAI-Modules
INCLUDE ZIN_DOWN_UP_FILE_COP_F01.
*include zin_down_up_file_f01                    .  " FORM-Routines

at selection-screen on value-request for p_local.
  lcl_main=>get_local_path( ).

at selection-screen on value-request for s_files-low.
  lcl_main=>get_file( s_files-low ).

at selection-screen on value-request for s_files-high.
  lcl_main=>get_file( s_files-high ).

start-of-selection.
  lcl_main=>start( ).
