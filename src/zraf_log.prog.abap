REPORT zraf_log.

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: abap, tfdir, ztraf_log, sscrfields.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES:BEGIN OF ty_display.
        INCLUDE TYPE ztraf_log.
TYPES:  cbox     TYPE c,
        style    TYPE lvc_t_styl,
        light    TYPE icon-id,
        poxml    TYPE icon-id,

        log_i    TYPE char4,
        log_o    TYPE char4,
        log_iid  TYPE ztraf_log-dguid,
        log_oid  TYPE ztraf_log-dguid,
        sdiff    TYPE timestampl,

        apitx    TYPE ztraf_maintain-adesc,
        rdate    TYPE ztraf_log-sdate,
        rtime    TYPE ztraf_log-stime,

        sort_key TYPE char26.    " 排序主键
TYPES END OF ty_display.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gt_display TYPE TABLE OF ty_display.

DATA: gr_alv_grid TYPE REF TO cl_gui_alv_grid.
DATA: gs_layout   TYPE lvc_s_layo,
      gt_fieldcat TYPE lvc_t_fcat.

" 定时器
DATA: gr_timer TYPE REF TO cl_gui_timer.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME TITLE TEXT-t01. " 接口数据筛选条件

  SELECT-OPTIONS: s_apino  FOR ztraf_log-apino.             " 接口ID
*                 s_progn  FOR ZTRAF_MAINTAIN-progname.          " 调用程序
  PARAMETERS: p_dirio TYPE ztraf_log-dirio AS LISTBOX VISIBLE LENGTH 10.
  SELECT-OPTIONS: s_sdate  FOR ztraf_log-sdate DEFAULT sy-datum,            " 请求日期
                  s_stime  FOR ztraf_log-stime,             " 请求时间
                  s_name   FOR ztraf_log-uname,             " 处理人
                  s_logid  FOR ztraf_log-logid,             " 日志标识
                  s_mgty   FOR ztraf_log-msgty NO-DISPLAY.  " 消息类型 (SAP)

SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME TITLE TEXT-t02. " 业务数据筛选条件

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_succ AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 10(10) TEXT-suc MODIF ID gp1 FOR FIELD p_succ.
    SELECTION-SCREEN POSITION 20.
    PARAMETERS: p_eror AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 25(10) TEXT-err MODIF ID gp1 FOR FIELD p_eror.
    SELECTION-SCREEN POSITION 36.
    PARAMETERS: p_info AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 40(10) TEXT-inf MODIF ID gp1 FOR FIELD p_info.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN SKIP.

  SELECT-OPTIONS: s_mgtx  FOR ztraf_log-msgtx.             " 消息文本 (SAP)
  SELECT-OPTIONS: s_bskey FOR ztraf_log-bskey MODIF ID bsk." 业务关键字
SELECTION-SCREEN END OF BLOCK blck2.

SELECTION-SCREEN FUNCTION KEY 1.

*&----------------------------------------------------------------------
*                     Class Definition
*&----------------------------------------------------------------------
CLASS lcl_alv_events DEFINITION CREATE PUBLIC FINAL.
  PUBLIC SECTION.
    CLASS-METHODS init_events IMPORTING i_alv TYPE REF TO cl_gui_alv_grid.

    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm sender.
    METHODS handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.
    METHODS handle_menu_button FOR EVENT menu_button OF cl_gui_alv_grid
      IMPORTING e_object e_ucomm.
    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed e_ucomm.
    METHODS handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column es_row_no.

    METHODS handle_timer FOR EVENT finished OF cl_gui_timer
      IMPORTING sender.
  PRIVATE SECTION.
    DATA: lv_timer_run TYPE abap_bool.

    " 双击 显示 xml
    METHODS double_click_log IMPORTING e_row     TYPE lvc_s_row
                                       e_column  TYPE lvc_s_col
                                       es_row_no TYPE lvc_s_roid.
    " 按钮 刷新
    METHODS btn_refresh IMPORTING grid TYPE REF TO cl_gui_alv_grid.
ENDCLASS.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  " 初始化
  s_stime-low  = '000000'." sy-uzeit - 3600 * 6.
  s_stime-high = '235959'.
  APPEND s_stime TO s_stime[].

  " 下拉框
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_DIRIO'
      values = VALUE vrm_values( ( key = '' text = '' )
                                 ( key = 'I' text = '入站' )
                                 ( key = 'O' text = '出站' ) ).

  sscrfields-functxt_01 = VALUE smp_dyntxt(
    quickinfo = '条目数统计'
    text      = '条目数量' ).

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

*&----------------------------------------------------------------------
*                     Value-request
*&----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_apino-low.
  PERFORM frm_help_apino.

*&----------------------------------------------------------------------
*                     At Selection-Screen
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.
  IF sy-ucomm = 'FC01'.
    PERFORM frm_cout_data.
  ENDIF.

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_get_data.

  PERFORM frm_fix_data.

  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*& Form frm_help_APINO
*&---------------------------------------------------------------------*
*&  接口编码搜索帮助
*&---------------------------------------------------------------------*
FORM frm_help_apino .

  SELECT
    apino,
    adesc
    FROM ztraf_maintain
    INTO TABLE @DATA(lt_apino).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'APINO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'S_APINO-LOW'
      value_org       = 'S'
    TABLES
      value_tab       = lt_apino
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_cout_data
*&---------------------------------------------------------------------*
*&  获取数据量
*&---------------------------------------------------------------------*
FORM frm_cout_data .
  DATA: lt_range_io TYPE RANGE OF ztraf_log-dirio,
        lv_count    TYPE i.

  " 接口方向
  lt_range_io = COND #( WHEN p_dirio <> '' THEN VALUE #( sign = 'I' option = 'EQ' ( low = p_dirio ) ) ).

  " 接口状态
  IF p_succ = '' AND p_eror = '' AND p_info = ''.
    MESSAGE '至少选择一个消息类型' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    IF p_succ = 'X'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'S' ) TO s_mgty[].
    ENDIF.

    IF p_eror = 'X'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'E' ) TO s_mgty[].
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'A' ) TO s_mgty[].
    ENDIF.

    IF p_info = 'X'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'I' ) TO s_mgty[].
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ''  ) TO s_mgty[].
    ENDIF.

    IF p_succ = 'X' AND p_eror = 'X' AND p_info = 'X'.
      CLEAR s_mgty[].
    ENDIF.
  ENDIF.

  " 取数
  SELECT
    COUNT(*)
    FROM ztraf_log
    INTO lv_count
    WHERE apino    IN s_apino
      AND logid    IN s_logid
      AND dirio    IN lt_range_io
      AND sdate    IN s_sdate
      AND stime    IN s_stime
      AND uname    IN s_name
      AND msgty    IN s_mgty
      AND msgtx    IN s_mgtx
      AND bskey    IN s_bskey.

  DATA: lv_message TYPE string.

  lv_message = |找到条目    { lv_count }|.
  MESSAGE lv_message TYPE 'I'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_data
*&---------------------------------------------------------------------*
*&  获取数据
*&---------------------------------------------------------------------*
FORM frm_get_data .
  DATA: lt_range_io TYPE RANGE OF ztraf_log-dirio.

  " 接口方向
  lt_range_io = COND #( WHEN p_dirio <> '' THEN VALUE #( sign = 'I' option = 'EQ' ( low = p_dirio ) ) ).

  " 接口状态
  IF p_succ = '' AND p_eror = '' AND p_info = ''.
    MESSAGE '至少选择一个消息类型' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    IF p_succ = 'X'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'S' ) TO s_mgty[].
    ENDIF.

    IF p_eror = 'X'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'E' ) TO s_mgty[].
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'A' ) TO s_mgty[].
    ENDIF.

    IF p_info = 'X'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'I' ) TO s_mgty[].
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ''  ) TO s_mgty[].
    ENDIF.

    IF p_succ = 'X' AND p_eror = 'X' AND p_info = 'X'.
      CLEAR s_mgty[].
    ENDIF.
  ENDIF.

  " 取数
  SELECT
    *
    FROM ztraf_log
    INTO CORRESPONDING FIELDS OF TABLE gt_display
    WHERE apino    IN s_apino
      AND logid    IN s_logid
      AND dirio    IN lt_range_io
      AND sdate    IN s_sdate
      AND stime    IN s_stime
      AND uname    IN s_name
      AND msgty    IN s_mgty
      AND msgtx    IN s_mgtx
      AND bskey    IN s_bskey.

  IF sy-subrc NE 0.
    " 未查询到数据
    MESSAGE s001(00) WITH TEXT-m01 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  " 附加描述
  SELECT
    apino,
    adesc
    FROM ztraf_maintain
    INTO TABLE @DATA(lt_ztapi001t).
  SORT lt_ztapi001t BY apino.

  DATA: lv_sort_key TYPE ty_display-sort_key.

  SORT gt_display BY logid.

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<ls_display>).
    " 排序主键
    AT NEW logid.
      lv_sort_key = <ls_display>-b_stamp_l.
    ENDAT.

    " 消息图例
    IF <ls_display>-msgty = 'E' OR <ls_display>-msgty = 'A'.
      <ls_display>-light = icon_led_red.
    ELSEIF <ls_display>-msgty = 'S'.
      <ls_display>-light = icon_led_green.
    ELSE.
      <ls_display>-light = icon_led_yellow. " ICON_LED_INACTIVE
    ENDIF.

    " > 排序主键
    <ls_display>-sort_key = lv_sort_key && <ls_display>-ifpos.

    " > 接口描述
    IF <ls_display>-apitx IS INITIAL.
      READ TABLE lt_ztapi001t INTO DATA(ls_ztapi001t) WITH KEY apino = <ls_display>-apino BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_display>-apitx = ls_ztapi001t-adesc.
      ENDIF.
    ENDIF.

    " 返回时间
    CONVERT TIME STAMP <ls_display>-e_stamp_l
      TIME ZONE sy-zonlo
        INTO DATE <ls_display>-rdate
             TIME <ls_display>-rtime.

  ENDLOOP.

  SORT gt_display BY sort_key DESCENDING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_fix_data
*&---------------------------------------------------------------------*
*&  补充数据
*&---------------------------------------------------------------------*
FORM frm_fix_data .
  " 补充数据内容

  CHECK gt_display IS NOT INITIAL.

  SELECT
    apino,
    dirio,
    dguid
    FROM ztraf_log_data
    FOR ALL ENTRIES IN @gt_display
    WHERE relid = 'ZZ'
      AND apino = @gt_display-apino
      AND dguid = @gt_display-dguid
    INTO TABLE @DATA(lt_logd).
  SORT lt_logd BY apino dguid dirio.

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<ls_display>).

    IF <ls_display>-b_stamp_l IS NOT INITIAL
      AND <ls_display>-e_stamp_l IS NOT INITIAL.
      <ls_display>-sdiff = cl_abap_tstmp=>subtract( tstmp1 = <ls_display>-e_stamp_l tstmp2 = <ls_display>-b_stamp_l ).
    ENDIF.

    CHECK <ls_display>-dguid IS NOT INITIAL.

    " ICON_ABAP
    READ TABLE lt_logd INTO DATA(ls_logd) WITH KEY apino = <ls_display>-apino
                                                   dguid = <ls_display>-dguid
                                                   dirio = 'I' BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_display>-log_i = icon_abap.
      <ls_display>-log_iid = ls_logd-dguid.
    ENDIF.

    READ TABLE lt_logd INTO ls_logd WITH KEY apino = <ls_display>-apino
                                             dguid = <ls_display>-dguid
                                             dirio = 'O' BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_display>-log_o = icon_abap.
      <ls_display>-log_oid = ls_logd-dguid.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'STANDARD'.
*  SET TITLEBAR 'TITLEBAR'.

  IF gr_alv_grid IS INITIAL.
    CREATE OBJECT gr_alv_grid
      EXPORTING
        i_parent = cl_gui_container=>screen0.

    PERFORM frm_set_fieldcat.
    PERFORM frm_set_layout.
    PERFORM frm_alv_display.
  ELSE.
    gr_alv_grid->refresh_table_display(
      EXPORTING
       is_stable = VALUE #( row = abap_true col = abap_true )
      EXCEPTIONS
       finished  = 1
       OTHERS    = 2 ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm .
    WHEN '&F03' OR '&F15' OR  '&F12'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*&  设置 FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat .
  REFRESH gt_fieldcat.

  PERFORM frm_set_fcat USING 'LIGHT'     ''           ''              TEXT-001. " 指示灯
  PERFORM frm_set_fcat USING 'LOGID'     'ZTRAF_LOG'  'LOGID'         TEXT-002. " 日志ID
  PERFORM frm_set_fcat USING 'APINO'     'ZTRAF_LOG'  'APINO'         TEXT-003. " 接口ID
  PERFORM frm_set_fcat USING 'APITX'     'ZTRAF_MAINTAIN' 'APITX'     TEXT-004. " 接口描述
* PERFORM frm_set_fcat USING 'PROGNAME'  'ZTRAF_LOG'  'PROGNAME'      TEXT-0005 " 调用程序
  PERFORM frm_set_fcat USING 'DIRIO'     'ZTRAF_LOG'  'DIRIO'         TEXT-006. " 方向
  PERFORM frm_set_fcat USING 'LOG_I'     ''           ''              TEXT-007. " 请求报文
  PERFORM frm_set_fcat USING 'LOG_O'     ''           ''              TEXT-008. " 返回报文
  PERFORM frm_set_fcat USING 'DGUID'     'ZTRAF_LOG'  'DGUID'         TEXT-009. " 数据ID
  PERFORM frm_set_fcat USING 'SDATE'     'ZTRAF_LOG'  'SDATE'         TEXT-010. " 请求日期
  PERFORM frm_set_fcat USING 'STIME'     'ZTRAF_LOG'  'STIME'         TEXT-011. " 请求时间
  PERFORM frm_set_fcat USING 'RDATE'     'ZTRAF_LOG'  'SDATE'         TEXT-012. " 返回日期
  PERFORM frm_set_fcat USING 'RTIME'     'ZTRAF_LOG'  'STIME'         TEXT-013. " 返回时间
  PERFORM frm_set_fcat USING 'SDIFF'     'ZTRAF_LOG'  'B_STAMP_L'     TEXT-014. " 耗时
  PERFORM frm_set_fcat USING 'BATCH'     'ZTRAF_LOG'  'BATCH'         TEXT-015. " 后台执行
  PERFORM frm_set_fcat USING 'UNAME'     'ZTRAF_LOG'  'UNAME'         TEXT-016. " 用户名
* PERFORM frm_set_fcat USING 'TUNAM'     'ZTRAF_LOG'  'TUNAM'         TEXT-017. " PO用户名
  PERFORM frm_set_fcat USING 'MSGTY'     'ZTRAF_LOG'  'MSGTY'         TEXT-018. " 消息类型
  PERFORM frm_set_fcat USING 'MSGTX'     'ZTRAF_LOG'  'MSGTX'         TEXT-019. " 消息文本
  PERFORM frm_set_fcat USING 'BSKEY'     'ZTRAF_LOG'  'BSKEY'         TEXT-020. " 业务关键字

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FCAT
*&---------------------------------------------------------------------*
*&  设置fcat
*&---------------------------------------------------------------------*
FORM frm_set_fcat USING   p_fieldname
                          p_ref_table TYPE lvc_s_fcat-ref_table
                          p_ref_field TYPE lvc_s_fcat-ref_field
                          p_coltext.
  DATA: lv_filedname TYPE lvc_s_fcat-fieldname,
        lv_coltext   TYPE lvc_s_fcat-coltext.

  lv_filedname = p_fieldname.
  lv_coltext   = p_coltext.

  APPEND VALUE lvc_s_fcat( fieldname  = lv_filedname
                           coltext    = p_coltext
                           ref_table  = p_ref_table
                           ref_field  = p_ref_field
                           col_opt    = 'X'
                           scrtext_l  = p_coltext
                           scrtext_m  = p_coltext
                           scrtext_s  = p_coltext ) TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).

  IF lv_filedname = 'CBOX' OR lv_filedname = 'BATCH'.
    <ls_fieldcat>-checkbox = abap_true.
  ENDIF.

  IF lv_filedname = 'RTIME' OR lv_filedname = 'SDIFF'.
    <ls_fieldcat>-no_zero = abap_true.
  ENDIF.

  IF lv_filedname = 'LIGHT' OR lv_filedname = 'LOG_O' OR lv_filedname = 'LOG_I'.
    <ls_fieldcat>-icon = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_LAYOUT
*&---------------------------------------------------------------------*
*&  设置 LAYOUT
*&---------------------------------------------------------------------*
FORM frm_set_layout .

  gs_layout = VALUE #(
                       zebra = 'X'
                       cwidth_opt = 'X'
                     " no_rowmark = 'X'     " 禁用行选择
                       stylefname = 'STYLE'
                      ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_alv_display
*&---------------------------------------------------------------------*
*&  ALV 展示
*&---------------------------------------------------------------------*
FORM frm_alv_display .
  DATA: lt_exclude TYPE ui_functions.
  DATA: ls_variant TYPE disvariant.
  DATA: lv_lines TYPE i.

  CHECK gr_alv_grid IS BOUND.

  lv_lines = lines( gt_display ).
  IF lv_lines <> 0.
    MESSAGE s008(zraf00) WITH lv_lines. " 查到 & 条数据
  ENDIF.

  ls_variant-report = sy-repid.

  APPEND cl_gui_alv_grid=>mc_fc_loc_cut        TO lt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO lt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO lt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste      TO lt_exclude.   " 粘贴
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO lt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy       TO lt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO lt_exclude.   " 新增
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row   TO lt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print          TO lt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo       TO lt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh        TO lt_exclude.

  gr_alv_grid->register_edit_event(
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2 ).
  gr_alv_grid->register_edit_event(
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2 ).

  lcl_alv_events=>init_events( gr_alv_grid ).

  gr_alv_grid->set_table_for_first_display(
     EXPORTING
       is_variant                    = ls_variant
       i_save                        = 'A'
       is_layout                     = gs_layout
       it_toolbar_excluding          = lt_exclude
     CHANGING
       it_outtab                     = gt_display
       it_fieldcatalog               =  gt_fieldcat
     EXCEPTIONS
       invalid_parameter_combination = 1
       program_error                 = 2
       too_many_lines                = 3
       OTHERS                        = 4 ).

  gr_alv_grid->set_ready_for_input( i_ready_for_input = 1 ).

ENDFORM.


CLASS lcl_alv_events IMPLEMENTATION.
  METHOD init_events.
    " Alv 事件注册
    DATA: lr_alv_event TYPE REF TO lcl_alv_events.

    CREATE OBJECT lr_alv_event.
    SET HANDLER lr_alv_event->handle_user_command FOR i_alv.
    SET HANDLER lr_alv_event->handle_toolbar      FOR i_alv.
    SET HANDLER lr_alv_event->handle_menu_button  FOR i_alv.
    SET HANDLER lr_alv_event->handle_data_changed FOR i_alv.
    SET HANDLER lr_alv_event->handle_double_click FOR i_alv.

  ENDMETHOD.
  METHOD handle_user_command.
    DATA: lt_rowid TYPE lvc_t_row.

    CASE e_ucomm.
      WHEN 'SEALL'.

      WHEN 'DEALL'.

      WHEN 'XMLEXP'.

      WHEN 'REFRESH'.
        " 刷新
        me->btn_refresh( sender ).

      WHEN 'RETIME'.
        me->lv_timer_run = COND #( WHEN me->lv_timer_run = abap_true THEN abap_false ELSE abap_true ).

        IF gr_timer IS INITIAL.
          gr_timer = NEW #( ).
          SET HANDLER me->handle_timer FOR gr_timer.

          gr_timer->interval = 2.   " 设置间隔为2秒
          CALL METHOD gr_timer->run " 激活定时器
            EXCEPTIONS
              OTHERS = 9.
        ELSE .
          CALL METHOD gr_timer->run " 重新激活定时器
            EXCEPTIONS
              OTHERS = 9.
        ENDIF .
    ENDCASE.

  ENDMETHOD.
  METHOD handle_toolbar.
    "分割符
    APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.

    " 问题复现
    " APPEND VALUE #( function  = 'REAPP'
    "                 text      = '重新执行'
    "                 icon      = icon_execute_object ) TO e_object->mt_toolbar.

    " 刷新
    APPEND VALUE #( function  = 'REFRESH'
                    text      = '刷新'
                    quickinfo = '立即刷新'
                    butn_type = 1
                    icon      = icon_refresh ) TO e_object->mt_toolbar.

    APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.

  ENDMETHOD.
  METHOD handle_menu_button.

    CASE e_ucomm.

      WHEN 'REFRESH'.
        IF me->lv_timer_run = abap_true.
          e_object->add_function( fcode = 'RETIME'
                                  text  = '取消定时刷新' ).
        ELSE.
          e_object->add_function( fcode = 'RETIME'
                                  text  = '定时2s刷新' ).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
  METHOD handle_data_changed.

  ENDMETHOD.
  METHOD handle_double_click.
    DATA: ls_alv TYPE ty_display.
    DATA: ls_clustkey TYPE sxmsclustkey,
          lt_xres     TYPE sxmsxrest,
          lv_result   TYPE string.

    CASE e_column-fieldname.
      WHEN 'LOG_I' OR 'LOG_O'.
        " 显示 xml 报文
        me->double_click_log( e_row = e_row e_column = e_column es_row_no = es_row_no ).
    ENDCASE.

  ENDMETHOD.

  METHOD double_click_log.
    " 显示 log 报文
    DATA: BEGIN OF ls_clustkey,
            apino TYPE ztraf_log_data-apino,
            dirio TYPE ztraf_log_data-dirio,
            dguid TYPE ztraf_log_data-dguid,
          END OF ls_clustkey.
    DATA: lv_result TYPE string.

    READ TABLE gt_display INTO DATA(ls_display) INDEX e_row-index.
    IF sy-subrc = 0.
      " 获取双击的字段
      ASSIGN COMPONENT e_column-fieldname OF STRUCTURE ls_display TO FIELD-SYMBOL(<lv_log_icon>).
      IF sy-subrc = 0.
        " 检查字段是否有值
        CHECK <lv_log_icon> IS NOT INITIAL.

        ls_clustkey-apino = ls_display-apino.
        ls_clustkey-dirio = e_column-fieldname+4(1).
        ls_clustkey-dguid = ls_display-dguid.

        IMPORT str = lv_result FROM DATABASE ztraf_log_data(zz) CLIENT sy-mandt ID ls_clustkey.
        IF sy-subrc = 0.

          TRY.
              CALL TRANSFORMATION sjson2html SOURCE XML lv_result
                RESULT XML DATA(lv_html).
            CATCH cx_xslt_runtime_error INTO DATA(lo_err).
              DATA(lv_err_text) = lo_err->get_text( ).
              MESSAGE lv_err_text TYPE 'S' DISPLAY LIKE 'E'.
              RETURN .
          ENDTRY.

          DATA(lv_convert) = cl_abap_codepage=>convert_from( lv_html ).
          cl_abap_browser=>show_html( html_string = lv_convert ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD btn_refresh.
    DATA: lv_message TYPE string.

    " 刷新
    REFRESH gt_display.

    PERFORM frm_get_data.
    PERFORM frm_fix_data.

    grid->refresh_table_display(
      EXPORTING
       is_stable = VALUE #( row = abap_true col = abap_true )
      EXCEPTIONS
       finished  = 1
       OTHERS    = 2 ).

    lv_message = |刷新完成, { lines( gt_display ) } 条|.
    MESSAGE lv_message TYPE 'S'.
  ENDMETHOD.

  METHOD handle_timer.

    IF me->lv_timer_run = abap_true.
      me->btn_refresh( gr_alv_grid ).

      sender->run( ).
    ELSE.
      " no run
      MESSAGE 'timer run stopped' TYPE 'S'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.                    "lcl_user_handle IMPLEMENTATION
