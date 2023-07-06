*----------------------------------------------------------------------*
***INCLUDE LZFG_RAF00_TABLEF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_ZTRAF_MAINTAIN
*&---------------------------------------------------------------------*
*&  更改更新时戳等信息
*&---------------------------------------------------------------------*
FORM frm_change_ztraf_maintain .

  GET TIME STAMP FIELD ztraf_maintain-ctimestamp.
  ztraf_maintain-changer = sy-uname.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_ZTRAF_ICONF
*&---------------------------------------------------------------------*
*&  更改更新时戳等信息
*&---------------------------------------------------------------------*
FORM frm_change_ztraf_iconf .

  GET TIME STAMP FIELD ztraf_iconf-ctimestamp.
  ztraf_iconf-changer = sy-uname.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_ZTRAF_OCONF
*&---------------------------------------------------------------------*
*&  更改更新时戳等信息
*&---------------------------------------------------------------------*
FORM frm_change_ztraf_oconf .

  GET TIME STAMP FIELD ztraf_oconf-ctimestamp.
  ztraf_oconf-changer = sy-uname.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_ZTRAF_URL_BASE
*&---------------------------------------------------------------------*
*&  更改更新时戳等信息
*&---------------------------------------------------------------------*
FORM frm_change_ztraf_url_base .

  GET TIME STAMP FIELD ztraf_url_base-ctimestamp.
  ztraf_url_base-changer = sy-uname.

ENDFORM.
