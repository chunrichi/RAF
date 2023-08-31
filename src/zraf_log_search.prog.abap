*&---------------------------------------------------------------------*
*& Report ZRAF_LOG_SEARCH
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zraf_log_search.


" NOTE 基于 报文进行数据查询
" 通过 HTML CONTROL 进行展示

*&----------------------------------------------------------------------
*                     Type-Pools
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: sxmspmast, ztraf_log.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: BEGIN OF ty_search_result,
         logid        TYPE ztraf_log-logid,       " 日志编号
         ifpos        TYPE ztraf_log-ifpos,       " 日志行号
         apino        TYPE ztraf_log-apino,       " 接口编号
         adesc        TYPE ztraf_maintain-adesc,  " 接口描述
         content_text TYPE text100,               " 查询内容上下文
         ztapi        TYPE flag,                  " 关联 API 日志
         dguid        TYPE ztraf_log-dguid,       " 报文编号
         msgty        TYPE ztraf_log-msgty,       " 返回状态
         msgtx        TYPE ztraf_log-msgtx,       " 返回描述

         direct       TYPE c,

         result       TYPE string,
         limit_indx   TYPE i,

         box          TYPE c,
       END OF ty_search_result.

TYPES: BEGIN OF ts_html,
         dataset(4096) TYPE c,
       END OF ts_html.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gs_layout   TYPE lvc_s_layo,
      gt_fieldcat TYPE lvc_t_fcat.

DATA: gt_search_result TYPE TABLE OF ty_search_result.

DATA: gt_html_data TYPE TABLE OF ts_html.

DATA: gr_container TYPE REF TO cl_gui_custom_container,
      gr_alv_grid  TYPE REF TO cl_gui_alv_grid.

DATA: gr_html_viewer TYPE REF TO cl_gui_html_viewer.
DATA: gv_url TYPE cndp_url.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  " 查询条件

  " ztraf_log
  SELECT-OPTIONS: s_apino FOR ztraf_log-apino MODIF ID dsa.
  SELECT-OPTIONS: s_logid FOR ztraf_log-logid MODIF ID dsa.
  SELECT-OPTIONS: s_bskey FOR ztraf_log-bskey MODIF ID dsa.
  SELECT-OPTIONS: s_msgty FOR ztraf_log-msgty MODIF ID dsa.
  SELECT-OPTIONS: s_msgtx FOR ztraf_log-msgtx MODIF ID dsa.
  SELECT-OPTIONS: s_datum FOR ztraf_log-sdate OBLIGATORY DEFAULT sy-datum MODIF ID dsa.

  " ztraf_log
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (31) ff_text FOR FIELD p_timl.
    PARAMETERS: p_timl TYPE syuzeit.
    SELECTION-SCREEN POSITION 52.
    SELECTION-SCREEN COMMENT (5) tf_text FOR FIELD p_timh.
    PARAMETERS: p_timh TYPE syuzeit.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  " 查询条件
  PARAMETERS: p_text TYPE text50.
SELECTION-SCREEN END OF BLOCK blck2.

SELECTION-SCREEN BEGIN OF BLOCK blck3 WITH FRAME.
  SELECTION-SCREEN COMMENT /2(77) t_001 MODIF ID bsa.
  SELECTION-SCREEN COMMENT /2(79) t_002 MODIF ID bsa.
SELECTION-SCREEN END OF BLOCK blck3.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  ff_text = '时间'.
  tf_text = '到'.
  p_timl = '000000'.
  p_timh = '240000'.
  t_001  = '基于 RAW DATA 的日志进行查询'.
  t_002  = '由于数据量和不确定性，限制较少，数据量较大，速度较慢'.

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
*                     Start-Of-Selection
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.

*&----------------------------------------------------------------------
*                     At Selection-Screen
*&----------------------------------------------------------------------
START-OF-SELECTION.
  " 基础数据获取
  PERFORM frm_get_ztapi.

  " 显示
  CALL SCREEN 8000.

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
    METHODS handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column es_row_no.

  PRIVATE SECTION.

    " 跳转到 LOG 界面
    METHODS btn_jump_log IMPORTING grid TYPE REF TO cl_gui_alv_grid.
ENDCLASS.
CLASS lcl_alv_events IMPLEMENTATION.
  METHOD init_events.
    " Alv 事件注册
    DATA: lr_alv_event TYPE REF TO lcl_alv_events.

    CREATE OBJECT lr_alv_event.
    SET HANDLER lr_alv_event->handle_user_command FOR i_alv.
    SET HANDLER lr_alv_event->handle_toolbar      FOR i_alv.
    SET HANDLER lr_alv_event->handle_double_click FOR i_alv.

  ENDMETHOD.
  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'LOGDETIAL'.
        " 跳转到 LOG 界面
        me->btn_jump_log( sender ).
    ENDCASE.

  ENDMETHOD.
  METHOD handle_toolbar.
    "分割符
    APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.

    " log detail
    APPEND VALUE #( function  = 'LOGDETIAL'
                    text      = '钻探跳转'
                    butn_type = 0
                    icon      = icon_next_hierarchy_level ) TO e_object->mt_toolbar.

    APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.
  ENDMETHOD.
  METHOD handle_double_click.
    DATA: lv_result   TYPE string.
    CASE e_column-fieldname.
      WHEN 'CONTENT_TEXT'.

        " 双击显示详情
        READ TABLE gt_search_result INTO DATA(ls_result) INDEX e_row-index.
        " MOVE-CORRESPONDING ls_result TO ls_clustkey.

        " IMPORT lt_xres TO lt_xres FROM DATABASE sxmsclur(is) CLIENT sy-mandt ID ls_clustkey.
        " READ TABLE lt_xres INTO DATA(ls_xres) INDEX 1.
        IF sy-subrc = 0.

          CALL TRANSFORMATION sjson2html SOURCE XML ls_result-result
            RESULT XML DATA(lv_html).

          lv_result = cl_abap_codepage=>convert_from( lv_html ).

          cl_abap_browser=>show_html( html_string = lv_result ).

        ENDIF.

      WHEN 'LOGID'.
        DATA: lt_selection_table TYPE rsparams_tt.

        " 双击跳转
        READ TABLE gt_search_result INTO ls_result INDEX e_row-index.
        IF sy-subrc = 0.
          lt_selection_table = VALUE #(
            ( selname = 'S_LOGID' kind = 'S' sign = 'I' option = 'EQ' low = ls_result-logid )
            ( selname = 'S_SDATE' kind = 'S' ) ).
          SUBMIT zraf_log
            WITH SELECTION-TABLE lt_selection_table
            AND RETURN.
        ENDIF.
    ENDCASE.

  ENDMETHOD.
  METHOD btn_jump_log.
    " 跳转到 LOG 界面

    DATA: lt_selection_table TYPE rsparams_tt.
    DATA: lt_rowid TYPE lvc_t_row.

    " 获取选中内容
    grid->get_selected_rows( IMPORTING et_index_rows = lt_rowid ).

    IF lt_rowid IS INITIAL.
      MESSAGE '至少选择一行有效内容' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_rowid INTO DATA(ls_rowid).

      lt_selection_table = VALUE #(
        ( selname = 'S_SDATE' kind = 'S' ) ).

      READ TABLE gt_search_result INTO DATA(ls_result) INDEX ls_rowid-index.
      IF sy-subrc = 0.
        APPEND VALUE #(
          selname = 'S_LOGID' kind = 'S' sign = 'I' option = 'EQ' low = ls_result-logid ) TO lt_selection_table.
      ENDIF.

      SUBMIT zraf_log
        WITH SELECTION-TABLE lt_selection_table
        AND RETURN.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

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
*& Form frm_get_ztapi
*&---------------------------------------------------------------------*
*&  基于 ztapi 获取数据
*&---------------------------------------------------------------------*
FORM frm_get_ztapi .
  DATA: lt_range_datum TYPE RANGE OF datum,
        lt_range_uzeit TYPE RANGE OF uzeit.
  DATA: ls_result TYPE ty_search_result.

  IF s_datum-low = '99991231'.
    " 查询所有日期
  ELSE.
    " 2022 08 12 19 00 16.0919780
    lt_range_datum = s_datum[].
  ENDIF.

  lt_range_uzeit = VALUE #( sign = 'I' option = 'BT' ( low = p_timl high = p_timh ) ).

  SELECT
    zt~dguid,
    zt~logid,   " 日志编号
    zt~ifpos,   " 日志行号
    zt~apino,   " 接口编号
    tt~adesc,   " 接口描述
    zt~msgty,
    zt~msgtx
    FROM ztraf_log AS zt
    LEFT JOIN ztraf_maintain AS tt ON tt~apino = zt~apino
    WHERE zt~apino IN @s_apino
      AND zt~logid IN @s_logid
      AND zt~bskey IN @s_bskey
      AND zt~msgty IN @s_msgty
      AND zt~msgtx IN @s_msgtx
      AND zt~sdate IN @lt_range_datum
      AND zt~stime IN @lt_range_uzeit
    INTO TABLE @DATA(lt_ztraf_log).

  SORT lt_ztraf_log BY dguid.

  IF lt_ztraf_log IS INITIAL.
    MESSAGE '未找到满足条件的数据' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT
    apino,
    dirio,
    dguid
    FROM ztraf_log_data AS dt
    FOR ALL ENTRIES IN @lt_ztraf_log
    WHERE dguid = @lt_ztraf_log-dguid
    INTO TABLE @DATA(lt_tag).
  SORT lt_tag BY apino dirio dguid.

  LOOP AT lt_ztraf_log INTO DATA(ls_ztraf_log).

    ls_result-ztapi = 'X'.
    ls_result-logid = ls_ztraf_log-logid.
    ls_result-ifpos = ls_ztraf_log-ifpos.
    ls_result-apino = ls_ztraf_log-apino.
    ls_result-adesc = ls_ztraf_log-adesc.
    ls_result-msgty = ls_ztraf_log-msgty.
    ls_result-msgtx = ls_ztraf_log-msgtx.
    ls_result-dguid = |{ ls_ztraf_log-dguid }|.

    READ TABLE lt_tag INTO DATA(ls_tad) WITH KEY apino = ls_ztraf_log-apino dirio = 'I' dguid = ls_ztraf_log-dguid BINARY SEARCH.
    IF sy-subrc = 0.
      ls_result-direct     = 'I'.
      PERFORM frm_check_content USING ls_result ls_result-direct.
    ENDIF.

    READ TABLE lt_tag INTO ls_tad WITH KEY apino = ls_ztraf_log-apino dirio = 'O' dguid = ls_ztraf_log-dguid BINARY SEARCH.
    IF sy-subrc = 0.
      ls_result-direct     = 'O'.
      PERFORM frm_check_content USING ls_result ls_result-direct.
    ENDIF.

  ENDLOOP.

  IF gt_search_result IS INITIAL.
    MESSAGE '未找到满足条件的数据' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_CONTENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_MAST_MSGGUID
*&      --> LS_MAST_PID
*&      --> <LS_TABLE>_CONTENT_TEXT
*&---------------------------------------------------------------------*
FORM frm_check_content  USING p_result TYPE ty_search_result p_direct TYPE ty_search_result-direct.

  DATA: BEGIN OF ls_clustkey,
          apino TYPE ztraf_log_data-apino,
          dirio TYPE ztraf_log_data-dirio,
          dguid TYPE ztraf_log_data-dguid,
        END OF ls_clustkey.

  DATA: lv_result TYPE string.

  DATA: lv_start  TYPE syfdpos,
        lv_length TYPE i.

  ls_clustkey-apino = p_result-apino.
  ls_clustkey-dirio = p_direct.
  ls_clustkey-dguid = p_result-dguid.

  IMPORT str = lv_result FROM DATABASE ztraf_log_data(zz) CLIENT sy-mandt ID ls_clustkey.
  IF sy-subrc = 0.
    " 返回消息检查

    " 匹配
    TRY.
        SEARCH lv_result FOR p_text.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO gt_search_result ASSIGNING FIELD-SYMBOL(<ls_table>).
          MOVE-CORRESPONDING p_result TO <ls_table>.
          lv_start = sy-fdpos.

          <ls_table>-result = lv_result.

          " 显示更多内容
          IF lv_start - 20 > 0.
            " 开始位置
            lv_start = lv_start - 20.
          ENDIF.

          lv_length = strlen( p_text ).
          IF lv_length + lv_start + 20 + 20 <= strlen( lv_result ).
            " 长度
            lv_length = lv_length + 20 + 20.
          ENDIF.

          <ls_table>-content_text = lv_result+lv_start(lv_length).
        ENDIF.

      CATCH cx_sy_regex INTO DATA(lr_regex_ex).
        DATA(lv_cx_msg) = lr_regex_ex->get_text( ).
        MESSAGE lv_cx_msg TYPE 'E'.
    ENDTRY.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*&  设置 FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat .
  REFRESH gt_fieldcat.

  PERFORM frm_set_fcat USING 'LOGID'        'ZTRAF_LOG'      'LOGID'     TEXT-001. " 日志编号
  PERFORM frm_set_fcat USING 'IFPOS'        'ZTRAF_LOG'      'IFPOS'     TEXT-002. " 日志行号
  PERFORM frm_set_fcat USING 'APINO'        'ZTRAF_LOG'      'APINO'     TEXT-003. " 接口编号
  PERFORM frm_set_fcat USING 'ADESC'        'ZTRAF_MAINTAIN' 'ADESC'     TEXT-004. " 接口描述
  PERFORM frm_set_fcat USING 'CONTENT_TEXT' ''               ''          TEXT-005. " 查询内容上下文
  PERFORM frm_set_fcat USING 'ZTAPI'        ''               ''          TEXT-006. " 关联 API 日志
  PERFORM frm_set_fcat USING 'DGUID'        'ZTRAF_LOG'      'DGUID'     TEXT-007. " 报文编号
  PERFORM frm_set_fcat USING 'MSGTY'        'ZTRAF_LOG'      'MSGTY'     TEXT-008. " 返回状态
  PERFORM frm_set_fcat USING 'MSGTX'        'ZTRAF_LOG'      'MSGTX'     TEXT-009. " 返回描述

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
  DATA: lv_fieldname TYPE lvc_s_fcat-fieldname,
        lv_coltext   TYPE lvc_s_fcat-coltext,
        lv_box       TYPE lvc_s_fcat-checkbox.

  lv_fieldname = p_fieldname.
  lv_coltext   = p_coltext.

  IF lv_fieldname = 'ZTAPI'.
    lv_box = 'X'.
  ENDIF.

  APPEND VALUE lvc_s_fcat( fieldname  = lv_fieldname
                           coltext    = p_coltext
                           ref_table  = p_ref_table
                           ref_field  = p_ref_field
                           checkbox   = lv_box
                           scrtext_l  = p_coltext
                           scrtext_m  = p_coltext
                           scrtext_s  = p_coltext ) TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).

  IF <ls_fieldcat>-fieldname = 'CONTENT_TEXT'.
    <ls_fieldcat>-intlen = 100.
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
                       box_fname  = 'BOX'
                       sel_mode   = 'A'
                      ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_alv_display
*&---------------------------------------------------------------------*
*&  ALV 展示
*&---------------------------------------------------------------------*
FORM frm_alv_display .
  DATA: lt_events TYPE slis_t_event WITH HEADER LINE.
  DATA: ls_variant TYPE disvariant.
  DATA: lt_sort TYPE lvc_t_sort.

  DATA: lv_lines TYPE i.
  lv_lines = lines( gt_search_result ).
  IF lv_lines <> 0.
    MESSAGE s008(zraf00) WITH lv_lines. " 查到 & 条数据
  ENDIF.

  CHECK gr_alv_grid IS BOUND.

  DATA: lt_exclude TYPE ui_functions.

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
       it_outtab                     = gt_search_result
       it_fieldcatalog               = gt_fieldcat
     EXCEPTIONS
       invalid_parameter_combination = 1
       program_error                 = 2
       too_many_lines                = 3
       OTHERS                        = 4 ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_8000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_8000 OUTPUT.
  SET PF-STATUS 'STANDARD'.
* SET TITLEBAR 'xxx'.

  IF gr_container IS INITIAL.
    gr_container = NEW #( container_name = 'GRID' ).
  ENDIF.

  IF gr_alv_grid IS INITIAL.

    gr_alv_grid = NEW #( i_parent = gr_container ).

    PERFORM frm_set_fieldcat.
    PERFORM frm_set_layout.

    PERFORM frm_alv_display.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8000 INPUT.
  CASE sy-ucomm.
    WHEN '&F03' OR '&F15'.
      LEAVE TO SCREEN 0.
    WHEN  '&F12'.
      LEAVE PROGRAM.
  ENDCASE.
  CLEAR sy-ucomm.
ENDMODULE.
