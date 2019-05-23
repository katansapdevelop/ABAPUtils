"! <p class="shorttext synchronized" lang="en">Time Utility Class</p>
"! This class provides a set of easy to use time functions to assist in programming and handling
"! time conversions.
"!
"! <p>SAP has specific handling requirements for time that must be considered when developing applications that consider time.  The timezone for time by default is based on the system timezone, unless the timezone is explicitly specified with
"! the specific time attribute</p>
"! <p>Note.  Timestamps implictly must be stored as UTC time</p>
CLASS zcl_bc_time_util DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Date Time with Time Zone</p>
      BEGIN OF ty_date_time,
        date TYPE sydatum,
        time TYPE syuzeit,
        zone TYPE tznzone,
      END OF ty_date_time .
    "! <p class="shorttext synchronized" lang="en">ISO Date Time (UTC)</p>
    TYPES ty_iso_datetime TYPE char20 .

    "! <p class="shorttext synchronized" lang="en">Convert Timestamp to ISO</p>
    "!
    "! @parameter IM_TIME_STAMP | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter RV_ISO_TIME_STAMP | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS convert_ts_to_iso
      IMPORTING
        !im_time_stamp           TYPE timestamp
      RETURNING
        VALUE(rv_iso_time_stamp) TYPE ty_iso_datetime .
    "! <p class="shorttext synchronized" lang="en">Convert TimeStamp Long to Timestamp</p>
    "!
    "! @parameter IM_TSL_TIMESTAMP | <p class="shorttext synchronized" lang="en">Timestamp Long</p>
    "! @parameter RV_TS_TIMESTAMP | <p class="shorttext synchronized" lang="en">Timestamp</p>
    CLASS-METHODS convert_tsl_to_ts
      IMPORTING
        !im_tsl_timestamp      TYPE timestampl
      RETURNING
        VALUE(rv_ts_timestamp) TYPE timestamp .
    "! <p class="shorttext synchronized" lang="en">Convert Timestamp Long to ISO</p>
    "!
    "! @parameter IM_TSL_TIMESTAMP | <p class="shorttext synchronized" lang="en">Timestamp Long</p>
    "! @parameter RV_ISO_TIMESTAMP | <p class="shorttext synchronized" lang="en">ISO TimeStamp</p>
    CLASS-METHODS convert_tsl_to_iso
      IMPORTING
        !im_tsl_timestamp       TYPE timestampl
      RETURNING
        VALUE(rv_iso_timestamp) TYPE ty_iso_datetime .
    CLASS-METHODS convert_date_time_to_ts
      IMPORTING
        !im_date             TYPE sydatum DEFAULT sy-datum
        !im_time             TYPE syuzeit DEFAULT sy-uzeit
        !im_time_zone        TYPE tznzone DEFAULT sy-zonlo
      RETURNING
        VALUE(rv_time_stamp) TYPE timestamp .
    CLASS-METHODS get_system_time_zone
      RETURNING
        VALUE(rv_time_zone) TYPE tznzone .
    CLASS-METHODS convert_iso_to_ts
      IMPORTING
        !im_iso_time_stamp   TYPE ty_iso_datetime
      RETURNING
        VALUE(rv_time_stamp) TYPE timestamp .
    CLASS-METHODS convert_ts_to_date_time
      IMPORTING
        !im_time_stamp      TYPE timestamp
        !im_time_zone       TYPE tznzone
      RETURNING
        VALUE(rv_date_time) TYPE ty_date_time .
    CLASS-METHODS get_ts_difference
      IMPORTING
        !im_timestamp1       TYPE timestamp
        !im_timestamp2       TYPE timestamp
      RETURNING
        VALUE(rv_difference) TYPE int4 .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_time_util IMPLEMENTATION.


  METHOD convert_date_time_to_ts.
    CONVERT DATE im_date TIME im_time INTO TIME STAMP rv_time_stamp TIME ZONE im_time_zone.
  ENDMETHOD.


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


  METHOD convert_tsl_to_iso.

    DATA: lv_ts_timestamp TYPE timestamp.

    lv_ts_timestamp = convert_tsl_to_ts( im_tsl_timestamp ).
    rv_iso_timestamp = convert_ts_to_iso( lv_ts_timestamp ).


  ENDMETHOD.


  METHOD convert_tsl_to_ts.

    DATA: lv_date     TYPE sy-datum,
          lv_time     TYPE sy-uzeit,
          lv_timezone TYPE tznzone.

    CONVERT TIME STAMP im_tsl_timestamp TIME ZONE lv_timezone INTO DATE lv_date TIME lv_time.
    CONVERT DATE lv_date TIME lv_time INTO TIME STAMP rv_ts_timestamp TIME ZONE lv_timezone.

  ENDMETHOD.


  METHOD convert_ts_to_date_time.
    CONVERT TIME STAMP im_time_stamp TIME ZONE im_time_zone INTO DATE rv_date_time-date TIME rv_date_time-time.
    IF sy-subrc EQ 0.
      rv_date_time-zone = im_time_zone.
    ENDIF.
  ENDMETHOD.


  METHOD convert_ts_to_iso.

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

  ENDMETHOD.


  METHOD get_system_time_zone.

    CALL FUNCTION '/OSP/SYSTEM_GET_TZONE'
      IMPORTING
        tzone_system = rv_time_zone.
  ENDMETHOD.


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
