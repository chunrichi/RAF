INTERFACE zif_raf_outbound
  PUBLIC .


  TYPES:
    BEGIN OF ty_result,
      type    TYPE ztraf_log-msgty,
      message TYPE ztraf_log-msgtx,
    END OF ty_result .

  DATA maintain_info TYPE ztraf_maintain .
  DATA oconf_info TYPE ztraf_oconf .
  DATA base_url TYPE ztraf_url_base-url .
  DATA result TYPE ty_result .

  EVENTS after_abap2json
    EXPORTING
      VALUE(er_json) TYPE REF TO data .

  METHODS request
    IMPORTING
      !i_data TYPE data
    EXPORTING
      !e_data TYPE data .
ENDINTERFACE.
