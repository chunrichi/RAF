CLASS zcl_raf_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA subrc TYPE sysubrc READ-ONLY .
    CLASS-DATA c_job_keep_log_days TYPE ztraf_config-rafset READ-ONLY VALUE `job/keep_log_days` ##NO_TEXT.
    CLASS-DATA c_job_keep_log_data_days TYPE ztraf_config-rafset READ-ONLY VALUE `job/keep_log_data_days` ##NO_TEXT.
    CLASS-DATA c_base_apino_in_subrouter TYPE ztraf_config-rafset READ-ONLY VALUE `base/apino_in_subrouter` ##NO_TEXT.

    METHODS constructor .
    METHODS get
      IMPORTING
        !rafset         TYPE ztraf_config-rafset
      RETURNING
        VALUE(rafvalue) TYPE ztraf_config-rafvalue .
    METHODS set
      IMPORTING
        VALUE(set) TYPE ztraf_config-rafset
        VALUE(val) TYPE ztraf_config-rafvalue .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      tt_configs TYPE TABLE OF ztraf_config .

    DATA configs TYPE tt_configs .
ENDCLASS.



CLASS ZCL_RAF_CONFIG IMPLEMENTATION.


  METHOD constructor.

    SELECT
      *
      FROM ztraf_config
      INTO TABLE @me->configs.
    SORT me->configs BY rafset.

  ENDMETHOD.


  METHOD get.
    CLEAR me->subrc.

    READ TABLE me->configs INTO DATA(ls_config) WITH KEY rafset = rafset BINARY SEARCH.
    me->subrc = sy-subrc.

    IF subrc = 0.
      rafvalue = ls_config-rafvalue.
    ENDIF.

  ENDMETHOD.


  METHOD set.
    DATA: ls_config TYPE ztraf_config.

    ls_config-rafset = set.
    ls_config-rafvalue = val.

    GET TIME STAMP FIELD ls_config-timestamp.
    ls_config-changer = sy-uname.

    MODIFY ztraf_config FROM ls_config.
    COMMIT WORK.

  ENDMETHOD.
ENDCLASS.
