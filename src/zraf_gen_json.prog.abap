REPORT zraf_gen_json.


*&---------------------------------------------------------------------*
*&  Types
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_id_field,
         fieldname TYPE fieldname,
         offset    TYPE i,
         length    TYPE i,
       END OF ty_id_field,

       BEGIN OF ty_expimp_table_info,
         tabname              TYPE tabname,
         "! <ul>
         "! <li>false: table with multiple rows (has got columns SRTF2 and CLUSTR, CLUSTD is of type RAW/X)</li>
         "! <li>true: table with one row (hasN'T got columns SRTF2 and CLUSTR, CLUSTD is of type RAWSTRING/XSTRING)</li>
         "! </ul>
         is_structure_one_row TYPE abap_bool,
         "! Name of client column - Empty if no client column
         client_fieldname     TYPE fieldname,
         id_fields            TYPE STANDARD TABLE OF ty_id_field WITH EMPTY KEY,
         "! Offset of RELID column, in number of characters (0 or 3, after eventual client column)
         area_offset          TYPE i,
         "! Offset of first ID field in number of characters (2 or 5, after RELID column)
         id_offset            TYPE i,
         "! Total length of ID fields in number of characters
         id_length            TYPE i,
         total_key_length     TYPE i,
         attr_fieldnames      TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY,
         "! Byte offset of CLUSTR column (only if structure is multiple rows, zero otherwise)
         offset_clustr        TYPE i,
         "! Number of bytes of SRTF2 column, 1, 2 or 4 (only if structure is multiple rows, zero otherwise)
         srtf2_length         TYPE i,
         offset_clustd        TYPE i,
         clustd_length        TYPE i,
       END OF ty_expimp_table_info.
*&---------------------------------------------------------------------*
*&  Global Data
*&---------------------------------------------------------------------*
DATA: gt_datadir TYPE STANDARD TABLE OF eudatadir.
DATA: go_data TYPE REF TO data.
DATA: gv_json TYPE string.


*&---------------------------------------------------------------------*
*&  Selection Screen
*&---------------------------------------------------------------------*
PARAMETERS: p_func TYPE tfdir-funcname OBLIGATORY.
PARAMETERS: p_nummer TYPE eudatadir-nummer.
"PARAMETERS p_api_no TYPE ztapi_main_conf-api_no.

SELECTION-SCREEN SKIP.

PARAMETERS: p_citem AS CHECKBOX DEFAULT ''.

*&---------------------------------------------------------------------*
*&  Search Help
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_nummer.
  PERFORM frm_number_f4.


*&---------------------------------------------------------------------*
*&  START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  CHECK p_func IS NOT INITIAL.
  CHECK p_nummer IS NOT INITIAL.

  "Check function name
  PERFORM frm_check_fname.
  "Check function test data
  PERFORM frm_check_testdata.
  "Load function test data
  PERFORM frm_load_testdata.
  "Generate json
  PERFORM frm_gen_json.
  "Show json
  PERFORM frm_show_json.
*&---------------------------------------------------------------------*
*& Form frm_number_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_number_f4 .


  TYPES: tt_datadir TYPE STANDARD TABLE OF eudatadir WITH EMPTY KEY.

  DATA(lt_dynpfield) = VALUE dynpread_tabtype( ( fieldname = 'P_FUNC' ) ).
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynpfield
    EXCEPTIONS
      OTHERS     = 9.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  DATA(lt_datadir) = VALUE tt_datadir( ).
  CALL FUNCTION 'RS_TESTDATA_DIRECTORY_GET'
    EXPORTING
      functionname       = CONV rs38l-name( lt_dynpfield[ fieldname = 'P_FUNC' ]-fieldvalue )
    TABLES
      te_datadir         = lt_datadir
    EXCEPTIONS
      function_not_found = 1
      no_data            = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_NUMMER'
      retfield    = 'NUMMER' " column name in VALUE_TAB
      value_org   = 'S'
    TABLES
      value_tab   = lt_datadir.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_check_fname
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check_fname .
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname           = p_func
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_check_testdata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check_testdata .
  TYPES: tt_datadir TYPE STANDARD TABLE OF eudatadir WITH EMPTY KEY.
  DATA(lt_datadir) = VALUE tt_datadir( ).
  CALL FUNCTION 'RS_TESTDATA_DIRECTORY_GET'
    EXPORTING
      functionname       = p_func
    TABLES
      te_datadir         = lt_datadir
    EXCEPTIONS
      function_not_found = 1
      no_data            = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  SORT lt_datadir BY nummer.
  READ TABLE lt_datadir TRANSPORTING NO FIELDS
    WITH KEY nummer = p_nummer BINARY SEARCH.
  IF sy-subrc <> 0.
    MESSAGE '测试数据有误' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_TESTDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_load_testdata .
  DATA: lv_xstring TYPE xstring.
  DATA: lt_cpar TYPE tab_cpar.
  DATA: cpar          TYPE REF TO cpar.

  PERFORM frm_import_as_xstring CHANGING lv_xstring.
  IF lv_xstring IS INITIAL.
    ASSERT 1 = 2.
  ENDIF.

  TRY.
      cl_abap_expimp_utilities=>dbuf_get_directory(
        EXPORTING
          dbuf = lv_xstring
        IMPORTING
          directory = DATA(directory) ).
      TRY.
          cl_abap_expimp_utilities=>dbuf_convert(
            EXPORTING
              dbuf_in  = lv_xstring
              targ_rel = CONV #( sy-saprl )
            IMPORTING
              dbuf_out = DATA(xstring2) ).
        CATCH cx_parameter_invalid_range cx_expimp_uc_not_supported.
          ASSERT 1 = 1.
      ENDTRY.
      lt_cpar = cl_abap_expimp_utilities=>dbuf_import_create_data( dbuf = lv_xstring ).

    CATCH cx_sy_import_format_error INTO DATA(lx).
      DATA(lv_err_text) = lx->get_text( ).
      MESSAGE lv_err_text TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.


  DELETE lt_cpar
      WHERE name = 'TIME1'
         OR name = 'V_RC'
         OR name = 'VEXCEPTION'
         OR name = 'G_UPPER'.
  SORT lt_cpar BY name.

  DATA: input_parameters TYPE abap_func_parmbind_tab.
  DATA: result_parameters TYPE abap_func_parmbind_tab.
  LOOP AT lt_cpar REFERENCE INTO cpar.
    IF cpar->name CP '%_I*'.
      INSERT VALUE abap_func_parmbind(
              kind  = COND #( WHEN line_exists( lt_cpar[ name = |%_O{ cpar->name+3 }| ] )
                                THEN abap_func_changing
                              WHEN line_exists( lt_cpar[ name = |%_V{ cpar->name+3 }| ] )
                                THEN abap_func_tables
                              ELSE abap_func_exporting )
              name  = cpar->name+3
              value = cpar->dref )
          INTO TABLE input_parameters. "param_bindings_pbo.
    ELSEIF cpar->name CP '%_V*'.
      INSERT VALUE abap_func_parmbind(
              kind  = COND #( WHEN line_exists( lt_cpar[ name = |%_O{ cpar->name+3 }| ] )
                                THEN abap_func_changing
                              WHEN line_exists( lt_cpar[ name = |%_I{ cpar->name+3 }| ] )
                                THEN abap_func_importing
                              ELSE abap_func_tables )
              name  = cpar->name+3
              value = cpar->dref )
          INTO TABLE result_parameters. "param_bindings_pai.
    ENDIF.
  ENDLOOP.

  PERFORM frm_gen_dyndata.

  ASSIGN go_data->* TO FIELD-SYMBOL(<fs_data>).
  LOOP AT input_parameters INTO DATA(ls_input_parameter).
    ASSIGN COMPONENT ls_input_parameter-name OF STRUCTURE <fs_data>
      TO FIELD-SYMBOL(<fs_para>).
    IF sy-subrc = 0.
      ASSIGN ls_input_parameter-value->* TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc = 0.
        MOVE <fs_value> TO <fs_para>.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "common structure
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_import_as_xstring
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_import_as_xstring CHANGING fc_xstring TYPE xstring .

  DATA:
    ref_table TYPE REF TO data,
    where     TYPE string,
    length    TYPE i.
  FIELD-SYMBOLS:
    <id_field>       TYPE ty_id_field,
    <id_new_field>   TYPE clike,
    <table>          TYPE STANDARD TABLE,
    <wa_fieldname>   TYPE any,
    <line_bytes>     TYPE x,
    <clustd>         TYPE any,
    <first_line>     TYPE any,
    <length2>        TYPE int2,
    <length4>        TYPE i,
    <database_field> TYPE any,
    <wa_field>       TYPE any.

  DATA: ls_info TYPE ty_expimp_table_info.
  DATA: lv_where TYPE string.
  DATA: ls_id TYPE functdir.

  PERFORM frm_get_info CHANGING ls_info.
  PERFORM frm_build_id CHANGING ls_id.
  PERFORM frm_build_where USING ls_info ls_id CHANGING lv_where.

  SELECT * FROM eufunc
        INTO TABLE @DATA(lt_eufunc)
        WHERE (lv_where).
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.


  " Sort by SRTF2 ascending
  SORT lt_eufunc BY table_line.

  " XSTRING
  IF ls_info-is_structure_one_row = abap_false.
    CLEAR fc_xstring.
    LOOP AT lt_eufunc ASSIGNING <line_bytes> CASTING.
      ASSIGN <line_bytes>+ls_info-offset_clustr(2) TO <length2> CASTING.
      IF <length2> <> 0.
        CONCATENATE fc_xstring <line_bytes>+ls_info-offset_clustd(<length2>) INTO fc_xstring IN BYTE MODE.
      ELSE.
        ASSERT 1 = 1. " Weird - That happened in table COVREF in 7.52 SP 0 developer edition.
      ENDIF.
    ENDLOOP.
  ELSE.
    ASSIGN lt_eufunc[ 1 ] TO <first_line>.
    ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <first_line> TO <clustd>.
    ASSERT sy-subrc = 0.
    fc_xstring = <clustd>.
  ENDIF.

*  " WA (+ avoid errors in case parameter WA is not passed/is not a structure)
*  DATA(rtti_wa) = cl_abap_typedescr=>describe_by_data( wa ).
*  IF rtti_wa->kind = rtti_wa->kind_struct.
*    ASSIGN <table>[ 1 ] TO <first_line>.
*    wa = CORRESPONDING #( <first_line> ).
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_INFO
*&---------------------------------------------------------------------*
FORM frm_get_info  CHANGING fc_s_info TYPE ty_expimp_table_info.

  DATA:
    area_index TYPE i,
    ref_line   TYPE REF TO data.
  FIELD-SYMBOLS:
    <first_field> TYPE any,
    <clustr>      TYPE any,
    <clustd>      TYPE any,
    <line>        TYPE any.

  fc_s_info = VALUE #( tabname = 'EUFUNC' ).

  " Table must be active and transparent
  SELECT COUNT(*) FROM dd02l
    WHERE tabname  = @fc_s_info-tabname
      AND as4local = 'A'
      AND as4vers  = 0
      AND tabclass = 'TRANSP'.
  IF sy-subrc <> 0.
*    RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>table_does_not_exist.
  ENDIF.

  " Table columns
  SELECT tabname, fieldname, keyflag, position, rollname, datatype, leng, inttype, intlen
      FROM dd03l
    INTO TABLE @DATA(lt_dd03l)
    WHERE tabname = @fc_s_info-tabname
      AND as4local = 'A'
      AND as4vers = 0
      AND fieldname NOT LIKE '.%'.
  IF sy-subrc <> 0.
*    RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>table_does_not_exist.
  ENDIF.

  SORT lt_dd03l BY position.

  DATA(number_of_key_fields) = 0.
  DATA(clnt_position) = 0.
  DATA(relid_position) = 0.
  DATA(srtf2_position) = 0.
  DATA(clustr_position) = 0.
  DATA(clustd_position) = 0.

  fc_s_info-area_offset = 0.
  fc_s_info-id_offset = 2.

  LOOP AT lt_dd03l ASSIGNING FIELD-SYMBOL(<ls_dd03l>).
    DATA(field_position) = sy-tabix.

    IF <ls_dd03l>-keyflag = 'X'.
      ADD 1 TO number_of_key_fields.
    ENDIF.

    IF field_position = 1
          AND <ls_dd03l>-keyflag = 'X'
          AND <ls_dd03l>-datatype = 'CLNT'.
      clnt_position = field_position.
      fc_s_info-client_fieldname = <ls_dd03l>-fieldname.
      ADD 3 TO fc_s_info-total_key_length.
      ADD 3 TO fc_s_info-area_offset.
      ADD 3 TO fc_s_info-id_offset.
    ENDIF.

    CASE <ls_dd03l>-fieldname.

      WHEN 'RELID'.
        IF <ls_dd03l>-datatype <> 'CHAR'
              AND <ls_dd03l>-leng <> 2.
*          RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
        ENDIF.
        relid_position = field_position.
        ADD 2 TO fc_s_info-total_key_length.

      WHEN 'SRTF2'.
        IF <ls_dd03l>-datatype <> 'INT1'
              AND <ls_dd03l>-datatype <> 'INT2'
              AND <ls_dd03l>-datatype <> 'INT4'.
*          RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
        ENDIF.
        srtf2_position = field_position.
        fc_s_info-srtf2_length = <ls_dd03l>-intlen.

      WHEN 'CLUSTR'.
        IF <ls_dd03l>-datatype <> 'INT2'.
*          RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
        ENDIF.
        clustr_position = field_position.
        ASSIGN <ls_dd03l> TO FIELD-SYMBOL(<dd03l_clustr>).

      WHEN 'CLUSTD'.
        IF <ls_dd03l>-datatype <> 'LRAW' " LRAW required for Multiple Rows data clusters
              AND <ls_dd03l>-datatype <> 'RSTR'. " RAWSTRING required for One Row data clusters
*          RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
        ENDIF.
        clustd_position = field_position.
        ASSIGN <ls_dd03l> TO FIELD-SYMBOL(<dd03l_clustd>).
    ENDCASE.

    IF <ls_dd03l>-keyflag = 'X'
          AND field_position <> clnt_position
          AND field_position <> relid_position
          AND field_position <> srtf2_position.
      CASE <ls_dd03l>-inttype.
        WHEN 'C' OR 'N' OR 'D' OR 'T'.
          DATA(ls_id_field) = VALUE ty_id_field( ).
          ls_id_field-offset = fc_s_info-id_length.
          ls_id_field-length = <ls_dd03l>-leng.
          ls_id_field-fieldname = <ls_dd03l>-fieldname.
          APPEND ls_id_field TO fc_s_info-id_fields.
          ADD <ls_dd03l>-leng TO fc_s_info-id_length.
          ADD <ls_dd03l>-leng TO fc_s_info-total_key_length.
        WHEN OTHERS.
*          RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
      ENDCASE.
    ENDIF.

    " WA
    IF <ls_dd03l>-keyflag = ' '
          AND field_position <> clustr_position
          AND field_position <> clustd_position.
      APPEND <ls_dd03l>-fieldname TO fc_s_info-attr_fieldnames.
    ENDIF.

  ENDLOOP.

  " The RELID field must be present, EITHER at first position
  " OR, if the table is client-dependent, right after the client
  IF ( fc_s_info-client_fieldname IS INITIAL AND relid_position <> 1 )
      OR ( fc_s_info-client_fieldname IS NOT INITIAL AND relid_position <> 2 ).
*    RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
  ENDIF.

  " There must be at least one ID field
  IF lines( fc_s_info-id_fields ) = 0.
*    RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
  ENDIF.

  " If SRTF2 is present, it must be the last key field
  IF srtf2_position <> 0 AND srtf2_position <> number_of_key_fields.
*    RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
  ENDIF.

  " If CLUSTR is present, it must be the penultimate field
  IF clustr_position <> 0 AND clustr_position <> lines( lt_dd03l ) - 1.
*    RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
  ENDIF.

  " CLUSTD must be present and must be the last field
  IF clustd_position <> lines( lt_dd03l ).
*    RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
  ENDIF.

  " Check kind of export/import table
  IF srtf2_position <> 0
      AND clustr_position <> 0
      AND <dd03l_clustd>-inttype = 'X'. " VARC or LRAW
    fc_s_info-is_structure_one_row = abap_false.
  ELSEIF sy-saprl >= '751'
      AND srtf2_position = 0
      AND clustr_position = 0
      AND <dd03l_clustd>-rollname = 'INDX_CLUST_BLOB'.
    fc_s_info-is_structure_one_row = abap_true.
  ELSE.
*    RAISE EXCEPTION TYPE zcx_expimp_table EXPORTING textid = zcx_expimp_table=>not_an_export_import_table.
  ENDIF.

  " Calculate the offsets of CLUSTR and CLUSTD
  CREATE DATA ref_line TYPE (fc_s_info-tabname).
  ASSIGN ref_line->* TO <line>.

  ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <first_field>.

  ASSIGN COMPONENT 'CLUSTR' OF STRUCTURE <line> TO <clustr>.
  IF sy-subrc = 0.
    DESCRIBE DISTANCE BETWEEN <first_field> AND <clustr> INTO fc_s_info-offset_clustr IN BYTE MODE.
  ENDIF.

  ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE <line> TO <clustd>.
  ASSERT sy-subrc = 0.
  DESCRIBE DISTANCE BETWEEN <first_field> AND <clustd> INTO fc_s_info-offset_clustd IN BYTE MODE.
  fc_s_info-clustd_length = <dd03l_clustd>-intlen.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_build_where
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_INFO
*&      <-- LV_WHERE
*&---------------------------------------------------------------------*
FORM frm_build_where  USING    fu_s_info TYPE ty_expimp_table_info
                               fu_s_id TYPE functdir
                      CHANGING fc_where TYPE string.

  FIELD-SYMBOLS:
    <client>       TYPE mandt,
    <area>         TYPE relid,
    <id_field>     TYPE ty_id_field,
    <id_new_field> TYPE clike.

  fc_where = ''.

  ASSIGN sy-mandt TO <client>.
  ASSIGN 'FL' TO <area>.

  IF fu_s_info-client_fieldname IS NOT INITIAL.
    fc_where = |{ fu_s_info-client_fieldname } = '{ <client> }' AND |.
  ENDIF.

  fc_where = |{ fc_where }RELID = { cl_abap_dyn_prg=>quote( <area> ) }|.

  " Fields part of the "ID" of the Export/Import Table.
  LOOP AT fu_s_info-id_fields ASSIGNING <id_field>.
    DATA(length) = nmin( val1 = <id_field>-length val2 = strlen( fu_s_id ) - <id_field>-offset ).
    IF length <= 0.
      fc_where = |{ fc_where } AND { <id_field>-fieldname } = ' '|.
    ELSE.
      " Don't use "substring" because this function doesn't support character structures
      " (ID could be a structure like FUNCTDIR for the export/import table EUFUNC).
      fc_where = |{ fc_where } AND { <id_field>-fieldname } = {
          cl_abap_dyn_prg=>quote( CONV string( fu_s_id+<id_field>-offset(length) ) ) }|.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_build_id
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_ID
*&---------------------------------------------------------------------*
FORM frm_build_id  CHANGING fc_s_id TYPE functdir.
  SELECT SINGLE pname
    FROM tfdir
    WHERE funcname = @p_func
    INTO @DATA(fugr_name).

  fc_s_id = VALUE functdir(
                          area   = fugr_name+4
                          progid = p_func
                          dataid = p_nummer ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_gen_dyndata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_gen_dyndata .
  DATA: ls_header TYPE header_fb,
        lt_tables TYPE rsfb_para,
        lt_import TYPE rsfb_para,
        lt_export TYPE rsfb_para,
        lt_change TYPE rsfb_para.
  DATA: lt_parameter TYPE rsfb_para.
  DATA: lo_table TYPE REF TO data.
  DATA: lo_req_tab  TYPE REF TO data,
        lo_req_data TYPE REF TO data.
  DATA: lt_dyn_comp TYPE cl_abap_structdescr=>component_table,
        ls_dyn_comp LIKE LINE OF lt_dyn_comp.
  FIELD-SYMBOLS: <fs_req_tab> TYPE ANY TABLE.
  FIELD-SYMBOLS: <fs_table> TYPE ANY TABLE.
  FIELD-SYMBOLS: <ls_req_data> TYPE any.

  CLEAR: lt_parameter[], ls_header .
  ls_header-name  = p_func.
  ls_header-state = 'A' .
  CALL METHOD cl_fb_parameter_db=>read
    IMPORTING
      tables = lt_tables
      import = lt_import
      export = lt_export
      change = lt_change
    CHANGING
      header = ls_header.
  APPEND LINES OF lt_import TO lt_parameter.
  APPEND LINES OF lt_change TO lt_parameter.
  APPEND LINES OF lt_tables TO lt_parameter.

  LOOP AT lt_parameter INTO DATA(ls_parameter).
    IF ls_parameter-typefield = 'LIKE'.
      CREATE DATA lo_table TYPE STANDARD TABLE OF (ls_parameter-structure).
      ASSIGN lo_table->* TO <fs_table>.
      CLEAR ls_dyn_comp.
      ls_dyn_comp-name = ls_parameter-parameter.
      ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_data( <fs_table> ).
      APPEND ls_dyn_comp TO lt_dyn_comp.
    ELSEIF ls_parameter-typefield = 'TYPE'.
      CLEAR ls_dyn_comp.
      ls_dyn_comp-name = ls_parameter-parameter.
      ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_name( ls_parameter-structure ).
      APPEND ls_dyn_comp TO lt_dyn_comp.
    ENDIF.
  ENDLOOP.


  DATA(lo_req_type) = cl_abap_structdescr=>create( lt_dyn_comp ).
  DATA(lo_req_tabdesc) = cl_abap_tabledescr=>create(
                  p_line_type  = lo_req_type
                  p_table_kind = cl_abap_tabledescr=>tablekind_std
                  p_unique     = abap_false ).
  CREATE DATA lo_req_tab TYPE HANDLE lo_req_tabdesc.
  ASSIGN lo_req_tab->* TO <fs_req_tab>.
  CREATE DATA go_data LIKE LINE OF <fs_req_tab>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_gen_json
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_gen_json .
  DATA: lv_sap_field TYPE string,
        lv_trd_field TYPE string.

  "生成json
  /ui2/cl_json=>serialize(
  EXPORTING
     data = go_data
     pretty_name = ''
  RECEIVING
     r_json = gv_json ).

  "字段名替换

  " >> 补充item
  IF p_citem = 'X'.
    REPLACE ALL OCCURRENCES OF `:[` IN gv_json WITH `:{"item":[`.
    REPLACE ALL OCCURRENCES OF `]` IN gv_json WITH `]}`.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_show_json
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_json .

  TRY.
      CALL TRANSFORMATION sjson2html SOURCE XML gv_json
        RESULT XML DATA(lv_html).
    CATCH cx_xslt_runtime_error INTO DATA(lo_err).
      DATA(lv_err_text) = lo_err->get_text( ).
      MESSAGE lv_err_text TYPE 'S' DISPLAY LIKE 'E'.
      RETURN .
  ENDTRY.

  DATA(lv_convert) = cl_abap_codepage=>convert_from( lv_html ).
  cl_abap_browser=>show_html( html_string = lv_convert ).
ENDFORM.
