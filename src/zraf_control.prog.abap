REPORT zraf_control.

* 主要逻辑
*  - 选择屏幕[Select Screen]        设置按钮
*  - 选择屏幕初始化[Initialization] 设置按钮对应的事务码/表
*  - 选择屏幕输出[Selection Screen Output] 设置按钮的可点击状态(权限检查)
*    + 注：查表的 权限在本平台只控制到SE16，未到具体表
*  - 子历程[frm_transaction_tcode]  跳转到Initialization中维护的事务码

* 新增按钮步骤：
*  - SelectScreen 部分按大类新增 按钮
*  - Initialization 设置按钮对应的事务码 文本等(SE16需维护表名/SM30需维护事务码)
*  - [完]

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: BEGIN OF ts_tcode_map,
         ucomm     TYPE sy-ucomm,   " 按钮的功能代码
         tcode     TYPE sy-tcode,   " 按钮对应的事务码
         object    TYPE char20,     " 处理对象
         text      TYPE char40,     " 文本

         name_form TYPE char20,
       END OF ts_tcode_map.

TYPES: BEGIN OF ts_btn_name,
         name(132) TYPE c,
       END OF ts_btn_name.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gt_exclude TYPE TABLE OF sy-ucomm.
DATA: gt_tcode_map   TYPE TABLE OF ts_tcode_map,
      gs_tcode_map   TYPE ts_tcode_map,
      gv_tcode_value TYPE sy-ucomm.
DATA: gt_btn_name TYPE TABLE OF ts_btn_name.

*&----------------------------------------------------------------------
*                     Constants
*&----------------------------------------------------------------------
CONSTANTS: cns_exec(4) TYPE c VALUE 'ONLI'.


*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------

* 表维护
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME TITLE TEXT-t01." line 1

  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN: PUSHBUTTON 2(30)   tt01 USER-COMMAND t01 MODIF ID t01.
    SELECTION-SCREEN: PUSHBUTTON 40(30)  tt02 USER-COMMAND t02 MODIF ID t02.
    SELECTION-SCREEN: PUSHBUTTON 78(30)  tt03 USER-COMMAND t03 MODIF ID t03.
  SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN: PUSHBUTTON 2(30)   tt04 USER-COMMAND t04 MODIF ID t04.
    SELECTION-SCREEN: PUSHBUTTON 40(30)  tt05 USER-COMMAND t05 MODIF ID t05.
    SELECTION-SCREEN: PUSHBUTTON 78(30)  tt06 USER-COMMAND t06 MODIF ID t06.
  SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN END OF BLOCK blck1.

" 报表
SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME TITLE TEXT-t02.

  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN: PUSHBUTTON 2(30)   tr01 USER-COMMAND r01 MODIF ID r01.
    SELECTION-SCREEN: PUSHBUTTON 40(30)  tr02 USER-COMMAND r02 MODIF ID r02.
    SELECTION-SCREEN: PUSHBUTTON 78(30)  tr03 USER-COMMAND r03 MODIF ID r03.
  SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN END OF BLOCK blck2.

" 工具
SELECTION-SCREEN BEGIN OF BLOCK blck3 WITH FRAME TITLE TEXT-t03.

  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN: PUSHBUTTON 2(30)   tp01 USER-COMMAND p01 MODIF ID p01.
    SELECTION-SCREEN: PUSHBUTTON 40(30)  tp02 USER-COMMAND p02 MODIF ID p02.
    SELECTION-SCREEN: PUSHBUTTON 78(30)  tp03 USER-COMMAND p03 MODIF ID p03.
  SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN END OF BLOCK blck3.

" 日志功能
SELECTION-SCREEN BEGIN OF BLOCK blck4 WITH FRAME TITLE TEXT-t04.

  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN: PUSHBUTTON 2(30)   tl01 USER-COMMAND l01 MODIF ID l01.
    SELECTION-SCREEN: PUSHBUTTON 40(30)  tl02 USER-COMMAND l02 MODIF ID l02.
    SELECTION-SCREEN: PUSHBUTTON 78(30)  tl03 USER-COMMAND l03 MODIF ID l03.
  SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN END OF BLOCK blck4.

" JOB处理
SELECTION-SCREEN BEGIN OF BLOCK blck5 WITH FRAME TITLE TEXT-t05.

  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN: PUSHBUTTON 2(30)   tj01 USER-COMMAND j01 MODIF ID j01.
    SELECTION-SCREEN: PUSHBUTTON 40(30)  tj02 USER-COMMAND j02 MODIF ID j02.
    SELECTION-SCREEN: PUSHBUTTON 78(30)  tj03 USER-COMMAND j03 MODIF ID j03.
  SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN END OF BLOCK blck5.

SELECTION-SCREEN BEGIN OF SCREEN 1001.
  PARAMETERS: p_day TYPE i.
SELECTION-SCREEN END OF SCREEN 1001.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  PERFORM frm_set_tcodemap.

  PERFORM frm_remove_toolbar USING '1000'.

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  " --> 执行按钮
  APPEND cns_exec TO gt_exclude.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = gt_exclude.
  " <--

  LOOP AT SCREEN.
    " 找到所有按钮-用于更新按钮描述
    IF screen-name(1) = 'T' AND screen-group3 = 'PBU'. " 按钮
      APPEND screen-name TO gt_btn_name.

      " 隐藏按钮
      READ TABLE gt_tcode_map TRANSPORTING NO FIELDS WITH KEY ucomm = screen-name+1(3).
      IF sy-subrc <> 0.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  " 按钮文本设置
  PERFORM frm_init_name.

*&----------------------------------------------------------------------
*                     At Selection-Screen.
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.
  CASE sy-ucomm(1).
    WHEN 'L'   " 日志功能
      OR 'R'   " 报表查询
      OR 'P'   " 工具
      OR 'J'   " JOB处理
      OR 'T'.  " 表维护

      PERFORM frm_transaction_tcode USING sy-ucomm.
    WHEN OTHERS.
  ENDCASE.

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.


*&---------------------------------------------------------------------*
*& Form FRM_REMOVE_TOOLBAR
*&---------------------------------------------------------------------*
*&  删除 tool bar
*&---------------------------------------------------------------------*
FORM frm_remove_toolbar USING pv_dynnr TYPE sy-dynnr.

  DATA: ls_header               TYPE rpy_dyhead,
        lt_containers           TYPE dycatt_tab,
        lt_fields_to_containers TYPE dyfatc_tab,
        lt_flow_logic           TYPE swydyflow.

  CALL FUNCTION 'RPY_DYNPRO_READ'
    EXPORTING
      progname             = sy-cprog
      dynnr                = pv_dynnr
    IMPORTING
      header               = ls_header
    TABLES
      containers           = lt_containers
      fields_to_containers = lt_fields_to_containers
      flow_logic           = lt_flow_logic
    EXCEPTIONS
      cancelled            = 1
      not_found            = 2
      permission_error     = 3
      OTHERS               = 4.
  IF sy-subrc IS NOT INITIAL.
    RETURN. " Ignore errors, just exit
  ENDIF.

  IF ls_header-no_toolbar = abap_true.
    RETURN. " No change required
  ENDIF.

  ls_header-no_toolbar = abap_true.

  CALL FUNCTION 'RPY_DYNPRO_INSERT'
    EXPORTING
      header                 = ls_header
      suppress_exist_checks  = abap_true
    TABLES
      containers             = lt_containers
      fields_to_containers   = lt_fields_to_containers
      flow_logic             = lt_flow_logic
    EXCEPTIONS
      cancelled              = 1
      already_exists         = 2
      program_not_exists     = 3
      not_executed           = 4
      missing_required_field = 5
      illegal_field_value    = 6
      field_not_allowed      = 7
      not_generated          = 8
      illegal_field_position = 9
      OTHERS                 = 10.
  IF sy-subrc <> 2 AND sy-subrc <> 0.
    RETURN. " Ignore errors, just exit
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_TCODEMAP
*&---------------------------------------------------------------------*
*&  设置 TCODE MAP
*&---------------------------------------------------------------------*
FORM frm_set_tcodemap .
  REFRESH gt_tcode_map.

  gt_tcode_map = VALUE #(
    " 表维护
  " ( ucomm = 'T01' tcode = 'SM30' object = 'ZTRAF_MAINTAIN'   text = TEXT-101 )   " 接口配置表
  " ( ucomm = 'T02' tcode = 'SM30' object = 'ZTRAF_ICONF'      text = TEXT-102 )   " 入栈配置表
  " ( ucomm = 'T03' tcode = 'SM30' object = 'ZTRAF_OCONF'      text = TEXT-103 )   " 出栈配置表
  " ( ucomm = 'T04' tcode = 'SM30' object = 'ZTRAF_URL_BASE'   text = TEXT-104 )   " 系统环境URL
  " 上面四个视图改用 T05 视图簇实现
    ( ucomm = 'T05' tcode = 'SM34' object = 'ZVRAF_CONF'       text = TEXT-105 )   " 接口配置视图
    " 报表
*    ( ucomm = 'R01' tcode = 'SE38' object = 'ZAPIR_001'        text = TEXT-201 )   " 接口请求次数统计
*    ( ucomm = 'R02' tcode = 'SE38' object = 'ZAPIR_002'        text = TEXT-202 )   " 接口请求状态统计
*    ( ucomm = 'R03' tcode = 'SE38' object = 'ZAPIR_003'        text = TEXT-203 )   " PO 端消息概览
    " 工具
    ( ucomm = 'P01' tcode = 'SE38' object = 'ZRAF_GEN_JSON'    text = TEXT-301 )    " 基于函数测试数据生成 JSON
*    ( ucomm = 'P02' tcode = 'SE38' object = 'ZAPIMG_GEN_JSON'  text = TEXT-302 )   "
*    ( ucomm = 'P03' tcode = 'SE38' object = 'ZAPIMG_REST_TEST' text = TEXT-303 )   " REST 接口测试
    " 日志功能
    ( ucomm = 'L01' tcode = 'SE38' object = 'ZRAF_LOG'         text = TEXT-401 )   " 接口日志查询
    ( ucomm = 'L02' tcode = 'SE38' object = 'ZRAF_LOG_SEARCH'  text = TEXT-402 )   " 接口内容查询
    ( ucomm = 'L03' tcode = 'SE38' object = 'ZRAF_JOB4LOG'     text = TEXT-403 )   " 日志清理
    " JOB
    "( ucomm = 'J01' tcode = 'FORM' object = 'FRM_JOB_CHECK01'  text = TEXT-501
    "                name_form = 'FRM_NAME_J01' )   " JOB -> 定时清理报文 job （保留 7 天）
  ).

  SORT gt_tcode_map BY ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_TRANSACTION_TCODE
*&---------------------------------------------------------------------*
*&  处理跳转
*&---------------------------------------------------------------------*
FORM frm_transaction_tcode  USING p_ucomm TYPE sy-ucomm.
  DATA: lv_message TYPE string.

  " 获取对应的Tcode
  READ TABLE gt_tcode_map INTO DATA(ls_map) WITH KEY ucomm = p_ucomm BINARY SEARCH.
  IF sy-subrc = 0.
    gv_tcode_value = ls_map-tcode.
  ENDIF.

  IF gv_tcode_value IS INITIAL.
    MESSAGE s046(zbpc01) DISPLAY LIKE 'E'.  " 未维护对应事务码的Tcode
    STOP.
  ENDIF.

  TRY.

      CASE gv_tcode_value.
        WHEN 'SM30'.
          " 表维护
          PERFORM frm_call_sm30 USING ls_map-object.
        WHEN 'SM34'.
          PERFORM frm_call_sm34 USING ls_map-object.
        WHEN 'SE38'.
          SUBMIT (ls_map-object) VIA SELECTION-SCREEN AND RETURN.
        WHEN 'FORM'.
          PERFORM (ls_map-object) IN PROGRAM zapimg_control IF FOUND.
        WHEN 'SE16'.
          " 表显示
          SET PARAMETER ID 'DTB' FIELD ls_map-object.
          CALL TRANSACTION 'SE16' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
          " 事务码直接跳转
          CALL TRANSACTION gv_tcode_value.
      ENDCASE.

    CATCH cx_root INTO DATA(lr_cx_root).
      lv_message = '程序异常，请联系技术人员' && lr_cx_root->get_text( ).
      MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_INIT_NAME
*&---------------------------------------------------------------------*
*&  设置按钮描述
*&---------------------------------------------------------------------*
FORM frm_init_name .
  FIELD-SYMBOLS: <lv_btn> TYPE any.

  LOOP AT gt_btn_name INTO DATA(ls_btn_name).
    " 查出描述
    READ TABLE gt_tcode_map INTO DATA(ls_map) WITH KEY ucomm = ls_btn_name-name+1 BINARY SEARCH.
    IF sy-subrc = 0.
      " 描述写入
      ASSIGN (ls_btn_name) TO <lv_btn>.
      IF sy-subrc = 0.
        <lv_btn> = ls_map-text.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_call_sm30
*&---------------------------------------------------------------------*
*& SM30
*&---------------------------------------------------------------------*
FORM frm_call_sm30  USING p_table.

  DATA: lv_table TYPE tabname.

  lv_table = p_table.

  " 表维护
  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      action                       = 'U'
      view_name                    = lv_table
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
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_SM34
*&---------------------------------------------------------------------*
*&  SM34
*&---------------------------------------------------------------------*
FORM frm_call_sm34  USING p_vname.

  DATA: lv_vname TYPE vcldir-vclname.

  lv_vname = p_vname.

  CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
    EXPORTING
      viewcluster_name             = lv_vname
      maintenance_action           = 'U'
    EXCEPTIONS
      client_reference             = 1
      foreign_lock                 = 2
      viewcluster_not_found        = 3
      viewcluster_is_inconsistent  = 4
      missing_generated_function   = 5
      no_upd_auth                  = 6
      no_show_auth                 = 7
      object_not_found             = 8
      no_tvdir_entry               = 9
      no_clientindep_auth          = 10
      invalid_action               = 11
      saving_correction_failed     = 12
      system_failure               = 13
      unknown_field_in_dba_sellist = 14
      missing_corr_number          = 15
      OTHERS                       = 16.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
