*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTRAF_ICONF.....................................*
DATA:  BEGIN OF STATUS_ZTRAF_ICONF                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRAF_ICONF                   .
CONTROLS: TCTRL_ZTRAF_ICONF
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZTRAF_MAINTAIN..................................*
DATA:  BEGIN OF STATUS_ZTRAF_MAINTAIN                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRAF_MAINTAIN                .
CONTROLS: TCTRL_ZTRAF_MAINTAIN
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZTRAF_OCONF.....................................*
DATA:  BEGIN OF STATUS_ZTRAF_OCONF                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRAF_OCONF                   .
CONTROLS: TCTRL_ZTRAF_OCONF
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZTRAF_URL_BASE..................................*
DATA:  BEGIN OF STATUS_ZTRAF_URL_BASE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRAF_URL_BASE                .
CONTROLS: TCTRL_ZTRAF_URL_BASE
            TYPE TABLEVIEW USING SCREEN '0004'.
*.........table declarations:.................................*
TABLES: *ZTRAF_ICONF                   .
TABLES: *ZTRAF_MAINTAIN                .
TABLES: *ZTRAF_OCONF                   .
TABLES: *ZTRAF_URL_BASE                .
TABLES: ZTRAF_ICONF                    .
TABLES: ZTRAF_MAINTAIN                 .
TABLES: ZTRAF_OCONF                    .
TABLES: ZTRAF_URL_BASE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
