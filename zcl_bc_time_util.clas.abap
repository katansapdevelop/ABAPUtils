class ZCL_BC_TIME_UTIL definition
  public
  abstract
  final
  create public .

public section.

  types:
    BEGIN OF TY_DATE_TIME,
          date type sydatum,
          time type syuzeit,
          zone type TZNZONE,
         end of TY_DATE_TIME .
  types TY_UTC_ISOTIME type CHAR20 .

  class-methods CONVERT_TS_TO_ISO
    importing
      !IM_TIME_STAMP type TIMESTAMP
    returning
      value(RV_ISO_TIME_STAMP) type TY_UTC_ISOTIME .
  class-methods CONVERT_TSL_TO_TS
    importing
      !IM_TSL_TIMESTAMP type TIMESTAMPL
    returning
      value(RV_TS_TIMESTAMP) type TIMESTAMP .
  class-methods CONVERT_TSL_TO_ISO
    importing
      !IM_TSL_TIMESTAMP type TIMESTAMPL
    returning
      value(RV_ISO_TIMESTAMP) type TY_UTC_ISOTIME .
  class-methods CONVERT_DATE_TIME_TO_TS
    importing
      !IM_DATE type SYDATUM default SY-DATUM
      !IM_TIME type SYUZEIT default SY-UZEIT
      !IM_TIME_ZONE type TZNZONE default SY-ZONLO
    returning
      value(RV_TIME_STAMP) type TIMESTAMP .
  class-methods GET_SYSTEM_TIME_ZONE
    returning
      value(RV_TIME_ZONE) type TZNZONE .
  class-methods CONVERT_ISO_TO_TS
    importing
      !IM_ISO_TIME_STAMP type TY_UTC_ISOTIME
    returning
      value(RV_TIME_STAMP) type TIMESTAMP .
  class-methods CONVERT_TS_TO_DATE_TIME
    importing
      !IM_TIME_STAMP type TIMESTAMP
      !IM_TIME_ZONE type TZNZONE
    returning
      value(RV_DATE_TIME) type TY_DATE_TIME .
  class-methods GET_TS_DIFFERENCE
    importing
      !IM_TIMESTAMP1 type TIMESTAMP
      !IM_TIMESTAMP2 type TIMESTAMP
    returning
      value(RV_DIFFERENCE) type INT4 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BC_TIME_UTIL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_TIME_UTIL=>CONVERT_DATE_TIME_TO_TS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_DATE                        TYPE        SYDATUM (default =SY-DATUM)
* | [--->] IM_TIME                        TYPE        SYUZEIT (default =SY-UZEIT)
* | [--->] IM_TIME_ZONE                   TYPE        TZNZONE (default =SY-ZONLO)
* | [<-()] RV_TIME_STAMP                  TYPE        TIMESTAMP
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD convert_date_time_to_ts.
  CONVERT DATE im_date TIME im_time INTO TIME STAMP rv_time_stamp TIME ZONE im_time_zone.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_TIME_UTIL=>CONVERT_ISO_TO_TS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_ISO_TIME_STAMP              TYPE        TY_UTC_ISOTIME
* | [<-()] RV_TIME_STAMP                  TYPE        TIMESTAMP
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD convert_iso_to_ts.
  DATA: lv_date TYPE sydatum,
        lv_time TYPE syuzeit.

  " Get the UTC date
  lv_date = im_iso_time_stamp+0(4) && im_iso_time_stamp+5(2) && im_iso_time_stamp+8(2).

  " Get the UTC time
  lv_time = im_iso_time_stamp+11(2) && im_iso_time_stamp+14(2) && im_iso_time_stamp+17(2).

  " Convert the Date Time already in UTC to a UTC time stamp
  rv_time_stamp = convert_date_time_to_ts( im_date = lv_date im_time = lv_time  im_time_zone = 'UTC' ).
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_TIME_UTIL=>CONVERT_TSL_TO_ISO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TSL_TIMESTAMP               TYPE        TIMESTAMPL
* | [<-()] RV_ISO_TIMESTAMP               TYPE        TY_UTC_ISOTIME
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CONVERT_TSL_TO_ISO.

    DATA: lv_ts_timestamp TYPE timestamp.

    lv_ts_timestamp = convert_tsl_to_ts( im_tsl_timestamp ).
    rv_iso_timestamp = convert_ts_to_iso( lv_ts_timestamp ).


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_TIME_UTIL=>CONVERT_TSL_TO_TS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TSL_TIMESTAMP               TYPE        TIMESTAMPL
* | [<-()] RV_TS_TIMESTAMP                TYPE        TIMESTAMP
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CONVERT_TSL_TO_TS.

    DATA: lv_date     TYPE sy-datum,
          lv_time     TYPE sy-uzeit,
          lv_timezone TYPE tznzone.

    CONVERT TIME STAMP im_tsl_timestamp TIME ZONE lv_timezone INTO DATE lv_date TIME lv_time.
    CONVERT DATE lv_date TIME lv_time INTO TIME STAMP rv_ts_timestamp TIME ZONE lv_timezone.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_TIME_UTIL=>CONVERT_TS_TO_DATE_TIME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TIME_STAMP                  TYPE        TIMESTAMP
* | [--->] IM_TIME_ZONE                   TYPE        TZNZONE
* | [<-()] RV_DATE_TIME                   TYPE        TY_DATE_TIME
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD convert_ts_to_date_time.
  CONVERT TIME STAMP im_time_stamp TIME ZONE im_time_zone INTO DATE rv_date_time-date TIME rv_date_time-time.
  IF sy-subrc EQ 0.
    rv_date_time-zone = im_time_zone.
  ENDIF.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_TIME_UTIL=>CONVERT_TS_TO_ISO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TIME_STAMP                  TYPE        TIMESTAMP
* | [<-()] RV_ISO_TIME_STAMP              TYPE        TY_UTC_ISOTIME
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CONVERT_TS_TO_ISO.

    DATA: l_date TYPE sydatum,
          l_time TYPE syuzeit.

    " Get the time/date as UTC
    CALL FUNCTION 'RSSM_GET_TIME'
      EXPORTING
        i_timestamps = im_time_stamp
      IMPORTING
        e_datum_utc  = l_date
        e_uzeit_utc  = l_time.

    " Get the date
    rv_iso_time_stamp = l_date(4) && '-' && l_date+4(2)  && '-' &&  l_date+6(2).

    " Add the time
    rv_iso_time_stamp = rv_iso_time_stamp && 'T' && l_time(2) && ':' && l_time+2(2)  && ':' &&  l_time+4(2) && 'Z'.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_TIME_UTIL=>GET_SYSTEM_TIME_ZONE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_TIME_ZONE                   TYPE        TZNZONE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_system_time_zone.

  CALL FUNCTION '/OSP/SYSTEM_GET_TZONE'
    IMPORTING
      tzone_system = rv_time_zone.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_TIME_UTIL=>GET_TS_DIFFERENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TIMESTAMP1                  TYPE        TIMESTAMP
* | [--->] IM_TIMESTAMP2                  TYPE        TIMESTAMP
* | [<-()] RV_DIFFERENCE                  TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_ts_difference.
  DATA: lv_timestamp1 TYPE tzntimestp,
        lv_timestamp2 TYPE tzntimestp.

  lv_timestamp1 = im_timestamp1.
  lv_timestamp2 = im_timestamp2.

  CALL FUNCTION 'CCU_TIMESTAMP_DIFFERENCE'
    EXPORTING
      timestamp1 = lv_timestamp1
      timestamp2 = lv_timestamp2
    IMPORTING
      difference = rv_difference.
ENDMETHOD.
ENDCLASS.
