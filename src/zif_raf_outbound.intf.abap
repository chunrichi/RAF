interface ZIF_RAF_OUTBOUND
  public .


  types:
    BEGIN OF ty_result,
      type    TYPE ztraf_log-msgty,
      message TYPE ztraf_log-msgtx,
    END OF ty_result .

  data MAINTAIN_INFO type ZTRAF_MAINTAIN .
  data OCONF_INFO type ZTRAF_OCONF .
  data BASE_URL type ZTRAF_URL_BASE-URL .
  data RESULT type TY_RESULT .

  methods REQUEST
    importing
      !I_DATA type ref to DATA
    exporting
      !E_DATA type ref to DATA .
endinterface.
