*"* use this source file for your ABAP unit test classes
CLASS ltc_bc_time_util DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    CONSTANTS: date_value               TYPE sydatum VALUE '20190523'.
    CONSTANTS: time_value               TYPE syuzeit VALUE '085928'.
    CONSTANTS: timestamp_value          TYPE timestamp VALUE '20190523085928'.
    CONSTANTS: timestamp_value_qld_utc  TYPE timestamp VALUE '20190522225928'.
    CONSTANTS: timestamp_difference     TYPE int4 VALUE 36000.
    CONSTANTS: isotime_value            TYPE zcl_bc_time_util=>ty_iso_datetime VALUE '2019-05-23T08:59:28Z'.
    CONSTANTS: timestamp_long_value     TYPE timestampl VALUE '20190523085928.111222'.
    CONSTANTS: utc_timezone             TYPE tznzone VALUE 'UTC'.
    CONSTANTS: ausqld_timezone          TYPE tznzone VALUE 'AUSQLD'.
    DATA:      system_time_zone         TYPE tznzone.
    DATA:      date_time                TYPE zcl_bc_time_util=>ty_date_time.

    METHODS: convert_ts_to_iso FOR TESTING.
    METHODS: convert_tsl_to_ts FOR TESTING.
    METHODS: convert_tsl_to_iso FOR TESTING.
    METHODS: convert_date_time_to_ts_utc FOR TESTING.
    METHODS: convert_date_time_to_ts_qld FOR TESTING.
    METHODS: get_system_time_zone FOR TESTING.
    METHODS: convert_iso_to_ts FOR TESTING.
    METHODS: convert_ts_to_date_time FOR TESTING.
    METHODS: get_ts_difference FOR TESTING.

    METHODS: convert_date_time_to_ts IMPORTING time_zone          TYPE tznzone
                                               expected_timestamp TYPE timestamp.

ENDCLASS.

CLASS ltc_bc_time_util IMPLEMENTATION.
  METHOD convert_ts_to_iso.
    DATA(iso_time_actual) = zcl_bc_time_util=>convert_ts_to_iso( im_time_stamp = ltc_bc_time_util=>timestamp_value ).
    DATA(message) = 'Input Timestamp Value: '  && |{ ltc_bc_time_util=>timestamp_value }|.
    cl_abap_unit_assert=>assert_equals( act = iso_time_actual exp = ltc_bc_time_util=>isotime_value level = if_aunit_constants=>fatal msg = message ).
  ENDMETHOD.
  METHOD convert_tsl_to_ts.
    DATA(ts_actual) = zcl_bc_time_util=>convert_tsl_to_ts( im_tsl_timestamp = timestamp_long_value ).
    DATA(message) = 'Input Timestamp Value: '  && |{ ltc_bc_time_util=>timestamp_long_value }|.
    cl_abap_unit_assert=>assert_equals( act = ts_actual exp = ltc_bc_time_util=>timestamp_value level = if_aunit_constants=>fatal msg = message ).
  ENDMETHOD.
  METHOD convert_tsl_to_iso.
    DATA(iso_time_actual) = zcl_bc_time_util=>convert_tsl_to_iso( im_tsl_timestamp = ltc_bc_time_util=>timestamp_long_value ).
    DATA(message) = 'Input Timestamp Value: '  && |{ ltc_bc_time_util=>timestamp_long_value }|.
    cl_abap_unit_assert=>assert_equals( act = iso_time_actual exp = ltc_bc_time_util=>isotime_value level = if_aunit_constants=>fatal msg = message ).
  ENDMETHOD.
  METHOD convert_date_time_to_ts.
    DATA(ts_actual) = zcl_bc_time_util=>convert_date_time_to_ts( im_date = ltc_bc_time_util=>date_value im_time = ltc_bc_time_util=>time_value  im_time_zone = time_zone ).
    DATA(message) = 'Input Date/Time Value: '  && |{ ltc_bc_time_util=>date_value }| && '-' &&  |{ ltc_bc_time_util=>time_value }| .
    cl_abap_unit_assert=>assert_equals( act = ts_actual exp = expected_timestamp level = if_aunit_constants=>fatal msg = message ).
  ENDMETHOD.
  METHOD get_system_time_zone.
    system_time_zone = 'AUSQLD'.
    DATA(timezone_actual) = zcl_bc_time_util=>get_system_time_zone( ).
    cl_abap_unit_assert=>assert_equals( act = timezone_actual exp = system_time_zone  level = if_aunit_constants=>fatal ).
  ENDMETHOD.
  METHOD convert_iso_to_ts.
    DATA(ts_actual) = zcl_bc_time_util=>convert_iso_to_ts( im_iso_time_stamp = ltc_bc_time_util=>isotime_value ).
    DATA(message) = 'Input ISO Timestamp Value: '  && |{ ltc_bc_time_util=>isotime_value }| .
    cl_abap_unit_assert=>assert_equals( act = ts_actual exp = ltc_bc_time_util=>timestamp_value level = if_aunit_constants=>fatal msg = message ).
  ENDMETHOD.
  METHOD convert_ts_to_date_time.
    date_time-date = ltc_bc_time_util=>date_value.
    date_time-time = ltc_bc_time_util=>time_value.
    date_time-zone = ltc_bc_time_util=>utc_timezone.
    DATA(date_time_actual) =  zcl_bc_time_util=>convert_ts_to_date_time( im_time_stamp = ltc_bc_time_util=>timestamp_value im_time_zone = ltc_bc_time_util=>utc_timezone ).
    DATA(message) = 'Input Date/Time/Timezone: '  && |{ ltc_bc_time_util=>date_value }| && ' ' &&  ltc_bc_time_util=>time_value && ' ' &&  ltc_bc_time_util=>utc_timezone.
    cl_abap_unit_assert=>assert_equals( act = date_time_actual exp = date_time level = if_aunit_constants=>fatal msg = message ).
  ENDMETHOD.
  METHOD get_ts_difference.
    DATA(difference) = zcl_bc_time_util=>get_ts_difference( im_timestamp1 =  timestamp_value im_timestamp2 = timestamp_value_qld_utc ).
    DATA(message) = 'Timestamp 1: ' && timestamp_value && ' Timestamp 2:' &&  timestamp_value_qld_utc.
    cl_abap_unit_assert=>assert_equals( act = difference exp = ltc_bc_time_util=>timestamp_difference level = if_aunit_constants=>fatal msg = message ).
  ENDMETHOD.
  METHOD convert_date_time_to_ts_qld.
    convert_date_time_to_ts( time_zone = ltc_bc_time_util=>ausqld_timezone expected_timestamp = ltc_bc_time_util=>timestamp_value_qld_utc  ).
  ENDMETHOD.
  METHOD convert_date_time_to_ts_utc.
    convert_date_time_to_ts( time_zone = ltc_bc_time_util=>utc_timezone expected_timestamp = ltc_bc_time_util=>timestamp_value ).
  ENDMETHOD.
ENDCLASS.
