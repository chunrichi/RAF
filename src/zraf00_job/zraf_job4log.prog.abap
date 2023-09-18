*&---------------------------------------------------------------------*
*& Report ZRAF_LOG_JOB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zraf_job4log.

" 定时清理 LOG 和 LOG_DATA

" feature: 定时同步 log 到外围服务器

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: ztraf_config.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gv_edit_01 TYPE rs_bool.
DATA: gv_edit_02 TYPE rs_bool.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  " 定时删除 ztraf_log
  PARAMETERS: p_dlog AS CHECKBOX USER-COMMAND dl DEFAULT 'X'.

  SELECTION-SCREEN COMMENT /10(10) skip01 MODIF ID 01.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(10) t_dl01 FOR FIELD p_dl01 MODIF ID 01.
    PARAMETERS: p_dl01 TYPE i MODIF ID 01.
    SELECTION-SCREEN PUSHBUTTON 30(6) be01 USER-COMMAND be01 MODIF ID 01.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  " 定时删除 ztraf_log_data
  PARAMETERS: p_dlogd AS CHECKBOX USER-COMMAND dld DEFAULT 'X'.

  SELECTION-SCREEN COMMENT /10(10) skip02 MODIF ID 02.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(10) t_dl02 FOR FIELD p_dl02 MODIF ID 02.
    PARAMETERS: p_dl02 TYPE i MODIF ID 02.
    SELECTION-SCREEN PUSHBUTTON 30(6) be02 USER-COMMAND be02 MODIF ID 02.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck2.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  t_dl01 = t_dl02 = '保留天数'(t01).
  be01 = be02 = '编辑'(t02).

  DATA(gr_config) = NEW zcl_raf_config( ).

  PERFORM frm_load_config_days.

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF p_dlog = '' AND screen-group1 = '01'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF p_dlogd = '' AND screen-group1 = '02'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'P_DL01' AND gv_edit_01 = ''.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'P_DL02' AND gv_edit_02 = ''.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&----------------------------------------------------------------------
*                     At Selection Screen
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'BE01'.
      gv_edit_01 = boolc( gv_edit_01 = '' ).
      be01 = COND #( WHEN gv_edit_01 = 'X' THEN '保存'(t03) ELSE '编辑'(t02) ).

      IF gv_edit_01 = ''.
        gr_config->set( set = zcl_raf_config=>c_job_keep_log_days val = |{ p_dl01 }| ).
      ENDIF.
    WHEN 'BE02'.
      gv_edit_02 = boolc( gv_edit_02 = '' ).
      be02 = COND #( WHEN gv_edit_02 = 'X' THEN '保存'(t03) ELSE '编辑'(t02) ).

      IF gv_edit_01 = ''.
        gr_config->set( set = zcl_raf_config=>c_job_keep_log_data_days val = |{ p_dl02 }| ).
      ENDIF.
  ENDCASE.

*&----------------------------------------------------------------------
*                     Start Of Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  IF p_dlog = 'X'.
    PERFORM frm_job_del_log.
  ENDIF.

  IF p_dlogd = 'X'.
    PERFORM frm_job_del_log_data.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form frm_load_config_days
*&---------------------------------------------------------------------*
*&  加载天数
*&---------------------------------------------------------------------*
FORM frm_load_config_days .

  DATA(keep_days) = gr_config->get( zcl_raf_config=>c_job_keep_log_days ).
  IF gr_config->subrc = 0.
    p_dl01 = keep_days.
  ELSE.
    p_dl01 = 90.
  ENDIF.

  DATA(keep_data_days) = gr_config->get( zcl_raf_config=>c_job_keep_log_data_days ).
  IF gr_config->subrc = 0.
    p_dl02 = keep_data_days.
  ELSE.
    p_dl02 = 90.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_JOB_DEL_LOG
*&---------------------------------------------------------------------*
*& 删除日志
*&---------------------------------------------------------------------*
FORM frm_job_del_log .
  DATA: lv_timestamp TYPE timestamp.

  GET TIME STAMP FIELD lv_timestamp.

  CONVERT TIME STAMP lv_timestamp TIME ZONE 'UTC+8' INTO DATE DATA(l_data) TIME DATA(l_time).

  l_data = l_data - p_dl01.
  l_time = '000000'.

  CONVERT DATE l_data TIME l_time INTO TIME STAMP lv_timestamp TIME ZONE 'UTC+8'.

  DELETE FROM ztraf_log WHERE b_stamp_l < lv_timestamp.
  COMMIT WORK.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_job_del_log_data
*&---------------------------------------------------------------------*
*& 删除日志数据（报文数据）
*&---------------------------------------------------------------------*
FORM frm_job_del_log_data .
  DATA: lv_timestamp TYPE timestamp.

  GET TIME STAMP FIELD lv_timestamp.

  CONVERT TIME STAMP lv_timestamp TIME ZONE 'UTC+8' INTO DATE DATA(l_data) TIME DATA(l_time).

  l_data = l_data - p_dl02.
  l_time = '000000'.

  CONVERT DATE l_data TIME l_time INTO TIME STAMP lv_timestamp TIME ZONE 'UTC+8'.

  DELETE FROM ztraf_log_data WHERE timestamp < lv_timestamp.
  COMMIT WORK.

ENDFORM.
