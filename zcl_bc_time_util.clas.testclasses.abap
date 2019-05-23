*"* use this source file for your ABAP unit test classes
CLASS ltc_bc_time_util DEFINITION FOR TESTING.
  PRIVATE SECTION.
    CONSTANTS: date_value TYPE sydatum VALUE '20190523'.
    CONSTANTS: time_value TYPE syuzeit VALUE '085928'.
    CONSTANTS: timestamp_value TYPE timestamp VALUE '20190523085928'.
    CONSTANTS: isotime_value TYPE zcl_bc_time_util=>ty_iso_datetime VALUE '2019-05-23T08:59:28Z'.
    CONSTANTS: timestamp_long_value TYPE timestampl VALUE '20190523085928.111222'.
    CONSTANTS: utc_timezone type tznzone value 'UTC'.

    METHODS: convert_ts_to_iso FOR TESTING.
    METHODS: convert_tsl_to_ts FOR TESTING.
    METHODS: convert_tsl_to_iso FOR TESTING.
    METHODS: convert_date_time_to_ts FOR TESTING.

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
    DATA(ts_actual) = zcl_bc_time_util=>convert_date_time_to_ts( im_date = ltc_bc_time_util=>date_value im_time = ltc_bc_time_util=>time_value  im_time_zone = ltc_bc_time_util=>utc_timezone ).
    DATA(message) = 'Input Date/Time Value: '  && |{ ltc_bc_time_util=>date_value }| && '-' &&  |{ ltc_bc_time_util=>time_value }| .
    cl_abap_unit_assert=>assert_equals( act = ts_actual exp = ltc_bc_time_util=>timestamp_value level = if_aunit_constants=>fatal msg = message ).
  ENDMETHOD.
ENDCLASS.
