*&---------------------------------------------------------------------*
*& Include ZIN_DOWN_UP_FILE_TOP                     - Report ZPG_DOWN_UP_FILE
*&---------------------------------------------------------------------*
report zpg_down_up_file.

tables: rcgfiletr.
selection-screen begin of block bl1 with frame title text-001.
parameters:
  p_sv    type eseftappl default '/usr/sap/trans/tmp',
  p_local type eseftfront.
select-options:
  s_files for rcgfiletr-ftappl no intervals.
selection-screen end of block bl1.

selection-screen begin of block bl2 with frame title text-002.
selection-screen begin of line.
parameters:
  p_dow  radiobutton group rg1  user-command rg1.
selection-screen comment 3(13) text-003.
parameters:
  p_upl  radiobutton group rg1 default 'X' .
selection-screen comment 19(13) text-004 .
selection-screen end of line.
selection-screen end of block bl2.
