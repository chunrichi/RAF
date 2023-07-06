REPORT zdemo_raf_init.

DATA: gt_exclude TYPE TABLE OF sy-ucomm.
CONSTANTS: gc_demo_in TYPE ztraf_maintain-apino VALUE `demo_in`.
CONSTANTS: gc_demo_out TYPE ztraf_maintain-apino VALUE `demo_out`.

SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 10(20) btn1 USER-COMMAND btn1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 10(20) btn2 USER-COMMAND btn2.
    SELECTION-SCREEN PUSHBUTTON 32(20) obtn USER-COMMAND obtn.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blck1.

INITIALIZATION.

  SELECT COUNT(*) FROM ztraf_maintain WHERE apino = gc_demo_in.
  IF sy-subrc = 0.
    btn1 = `重置 in 示例配置`.
  ELSE.
    btn1 = `生成 in 示例配置`.
  ENDIF.

  SELECT COUNT(*) FROM ztraf_maintain WHERE apino = gc_demo_out.
  IF sy-subrc = 0.
    btn2 = `重置 out 示例配置`.
  ELSE.
    btn2 = `生成 out 示例配置`.
  ENDIF.

  obtn = '推送测试'.

  " --> 执行按钮
  APPEND 'ONLI' TO gt_exclude.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = gt_exclude.
  " <--

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'OBTN' AND btn2 = `生成 out 示例配置`.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'BTN1'.
    PERFORM frm_set_inbound_conf.
  ELSEIF sy-ucomm = 'BTN2'.
    PERFORM frm_set_outbound_conf.
  ELSEIF sy-ucomm = 'OBTN'.
    PERFORM frm_send_test.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form FRM_SET_INBOUND_CONF
*&---------------------------------------------------------------------*
*&  设置入栈配置
*&---------------------------------------------------------------------*
FORM frm_set_inbound_conf .

  DELETE FROM ztraf_maintain WHERE apino = gc_demo_in.
  DELETE FROM ztraf_iconf WHERE apino = gc_demo_in.
  COMMIT WORK AND WAIT.

  DATA: ls_ztraf_main TYPE ztraf_maintain.
  DATA: ls_ztraf_conf TYPE ztraf_iconf.
  DATA: lv_message TYPE string.

  ls_ztraf_main-apino = gc_demo_in.
  ls_ztraf_main-adesc   = `进栈方向接口 demo`.
  ls_ztraf_main-deact   = space.
  ls_ztraf_main-targt   = 'DEMO'.
  ls_ztraf_main-dirio   = 'I'.
  ls_ztraf_main-logdt   = 'A'. " A 记录所有日志
  ls_ztraf_main-jmode   = 'L'.
  ls_ztraf_main-changer = 'ZDEMO_RAF_INIT'.
  GET TIME STAMP FIELD ls_ztraf_main-ctimestamp.

  ls_ztraf_conf-apino = gc_demo_in.
  ls_ztraf_conf-func_name = 'ZFM_RAF00_IN_DEMO'.
  ls_ztraf_conf-changer = 'ZDEMO_RAF_INIT'.
  GET TIME STAMP FIELD ls_ztraf_conf-ctimestamp.

  MODIFY ztraf_maintain FROM ls_ztraf_main.
  MODIFY ztraf_iconf FROM ls_ztraf_conf.
  COMMIT WORK AND WAIT.

  lv_message = btn1 && '结束'.
  MESSAGE lv_message TYPE 'S'.

  btn1 = `重置 in 示例配置`.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_OUTbound_conf
*&---------------------------------------------------------------------*
*&  设置出栈配置
*&---------------------------------------------------------------------*
FORM frm_set_outbound_conf .

  DELETE FROM ztraf_maintain WHERE apino = gc_demo_out.
  DELETE FROM ztraf_oconf    WHERE apino = gc_demo_out.
  DELETE FROM ztraf_url_base WHERE targt = 'DEMO'.
  COMMIT WORK AND WAIT.

  DATA: ls_ztraf_main TYPE ztraf_maintain.
  DATA: ls_ztraf_conf TYPE ztraf_oconf.
  DATA: ls_ztraf_base TYPE ztraf_url_base.
  DATA: lv_message TYPE string.

  ls_ztraf_main-apino = gc_demo_out.
  ls_ztraf_main-adesc   = `出栈方向接口 demo`.
  ls_ztraf_main-deact   = space.
  ls_ztraf_main-targt   = 'DEMO'.
  ls_ztraf_main-dirio   = 'O'.
  ls_ztraf_main-logdt   = 'A'. " A 记录所有日志
  ls_ztraf_main-jmode   = 'L'.
  ls_ztraf_main-changer = 'ZDEMO_RAF_INIT'.
  GET TIME STAMP FIELD ls_ztraf_main-ctimestamp.

  ls_ztraf_conf-apino = gc_demo_out.
  ls_ztraf_conf-class_name = 'ZCL_RAF_OUTBOUND_FUNC'.
  ls_ztraf_conf-method     = 'POST'.
  ls_ztraf_conf-url        = '?client=200&apino=' && gc_demo_in.     " 配置链接 变化的地方
  ls_ztraf_conf-changer    = 'ZDEMO_RAF_INIT'.
  GET TIME STAMP FIELD ls_ztraf_conf-ctimestamp.

  ls_ztraf_base-targt = 'DEMO'.
  ls_ztraf_base-sysid = sy-sysid.
  ls_ztraf_base-url     = ''.                              " 配置基础链接 -> http://sapsicfcom:port/sap/zraf
  ls_ztraf_base-changer = 'ZDEMO_RAF_INIT'.
  GET TIME STAMP FIELD ls_ztraf_base-ctimestamp.

  MODIFY ztraf_maintain FROM ls_ztraf_main.
  MODIFY ztraf_oconf FROM ls_ztraf_conf.
  MODIFY ztraf_url_base FROM ls_ztraf_base.
  COMMIT WORK AND WAIT.

  " -> 跳转到 ls_ztraf_conf 补全 URL

  " 表维护
  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      action                       = 'U'
      view_name                    = 'ZTRAF_URL_BASE'
    EXCEPTIONS
      client_reference             = 1
      foreign_lock                 = 2
      invalid_action               = 3
      no_clientindependent_auth    = 4
      no_database_function         = 5
      no_editor_function           = 6
      no_show_auth                 = 7
      no_tvdir_entry               = 8
      no_upd_auth                  = 9
      only_show_allowed            = 10
      system_failure               = 11
      unknown_field_in_dba_sellist = 12
      view_not_found               = 13
      maintenance_prohibited       = 14
      OTHERS                       = 15.


  lv_message = btn1 && '结束'.
  MESSAGE lv_message TYPE 'S'.

  btn2 = `重置 out 示例配置`.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_send_test
*&---------------------------------------------------------------------*
*&  推送测试
*&---------------------------------------------------------------------*
FORM frm_send_test .
  DATA: lv_message TYPE string.

  CALL FUNCTION 'ZFM_RAF00_OUT_DEMO'
    EXPORTING
      _apino  = gc_demo_out
      test    = |{ sy-uname } test out|
    IMPORTING
      message = lv_message.

  MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'S'.
ENDFORM.
