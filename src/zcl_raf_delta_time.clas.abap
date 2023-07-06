CLASS zcl_raf_delta_time DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !object    TYPE ztraf_delta-object
        !logsys    TYPE ztraf_delta-logsys
        !safe      TYPE timestamp DEFAULT 20
      EXPORTING
        !timestamp TYPE timestamp .
    CLASS-METHODS set
      IMPORTING
        !timestamp TYPE timestamp
        !object    TYPE ztraf_delta-object
        !logsys    TYPE ztraf_delta-logsys .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_RAF_DELTA_TIME IMPLEMENTATION.


  METHOD get.

    SELECT SINGLE
      *
      FROM ztraf_delta
      INTO @DATA(ls_delta).
    IF sy-subrc = 0.
      timestamp = ls_delta-lastdt - safe.
    ELSE.
      DATA: lv_datum TYPE datum,
            lv_uzeit TYPE uzeit.

      lv_datum = sy-datum - 365.

      CONVERT DATE lv_datum TIME lv_uzeit INTO TIME STAMP timestamp
        TIME ZONE ''.

    ENDIF.

  ENDMETHOD.


  METHOD set.

    DATA: ls_ztraf_delta TYPE ztraf_delta.

    ls_ztraf_delta-object = object.
    ls_ztraf_delta-logsys = logsys.
    ls_ztraf_delta-uname  = sy-uname.

    ls_ztraf_delta-lastdt = timestamp.

    MODIFY ztraf_delta FROM ls_ztraf_delta.

  ENDMETHOD.
ENDCLASS.
