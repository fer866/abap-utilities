class ZCL_UTILIDADES definition
  public
  create public .

public section.

  class-methods IS_PRODUCTION
    returning
      value(RESULT) type ABAP_BOOL .
  class-methods GET_MONTH_NAME
    importing
      !MONTH type FCMNR
    returning
      value(MONTH_NAME) type FCLTX .
  class-methods GET_LASTDAY_OF_MONTH
    importing
      !DATE_IN like SY-DATUM
    returning
      value(LASTDAY) like SY-DATUM .
  class-methods PUT_SIGN_IN_FRONT
    importing
      !CURR9 type WERTV9 default 0
      !DMBTR type DMBTR default 0
    preferred parameter CURR9
    returning
      value(STR_CURR) type STRING .
  class-methods FORMAT_DATE
    importing
      !DATUM type SY-DATUM
      !FORMAT type STRING
    returning
      value(RESULT) type STRING .
  class-methods STRING_TO_DATE
    importing
      !INPUT type STRING
    returning
      value(OUTPUT) type DATUM .
  type-pools TRUXS .
  class-methods CONVERT_RAW_CSV_TO_TABLE
    importing
      !RAW_TABLE type TRUXS_T_TEXT_DATA
      !HAS_HEADER type ABAP_BOOL default ABAP_TRUE
    changing
      !STR_TABLE type STANDARD TABLE .
endclass.

class ZCL_UTILIDADES IMPLEMENTATION.
  method IS_PRODUCTION.
    DATA: v_rol TYPE t000-cccategory.
*   P = Production T = Test C = Customizing

    SELECT cccategory INTO v_rol FROM t000
      WHERE mandt EQ sy-mandt.

      IF v_rol EQ 'P'.
        result = abap_true.
      ELSEIF v_rol EQ 'T'.
        result = abap_false.
      ENDIF.
    ENDSELECT.
  endmethod.

  method GET_MONTH_NAME.
    SELECT SINGLE ltx INTO month_name
      FROM t247
      WHERE spras EQ sy-langu AND   "Idioma
            mnr EQ month.           "Número de mes
    TRANSLATE month_name TO LOWER CASE.
  endmethod.

  method GET_LASTDAY_OF_MONTH.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in = date_in
      IMPORTING
        last_day_of_month = lastday.
  endmethod.

  method PUT_SIGN_IN_FRONT.
  IF curr9 IS NOT INITIAL.
    str_curr = curr9.
  ELSEIF dmbtr IS NOT INITIAL.
    str_curr = dmbtr.
  ENDIF.


  IF curr9 LT 0 OR dmbtr LT 0.
    SHIFT str_curr RIGHT DELETING TRAILING '-'.
    CONDENSE str_curr.
    str_curr = |-{ str_curr }|.
  ENDIF.
endmethod.

method FORMAT_DATE.
  DATA: lv_month TYPE fcltx.

  lv_month = zcl_utilidades=>get_month_name( datum+4(2) ).
  result = format.
  REPLACE ALL OCCURRENCES OF 'dd' IN result WITH datum+6(2).
  REPLACE ALL OCCURRENCES OF 'MMMM' IN result WITH lv_month.
  REPLACE ALL OCCURRENCES OF 'MMM' IN result WITH lv_month(3).
  REPLACE ALL OCCURRENCES OF 'MM' IN result WITH datum+4(2).
  REPLACE ALL OCCURRENCES OF 'yyyy' IN result WITH datum(4).
  REPLACE ALL OCCURRENCES OF 'yy' IN result WITH datum+2(2).

endmethod.

method STRING_TO_DATE.
  DATA: lv_nyear TYPE c LENGTH 4,
        lv_month TYPE c LENGTH 2,
        lv_cuday TYPE c LENGTH 2,
        lt_years TYPE TABLE OF i,
        lv_curyr TYPE i,
        lv_count TYPE i.

  IF input CP '*/*' OR input CP '*.*'.
    CASE strlen( input ).
      WHEN 10.  "31/12/2019
        lv_nyear = input+6(4).
        lv_month = input+3(2).
        lv_cuday = input(2).
      WHEN 8.   "01/12/19
        lv_nyear = input+6(2).
        lv_nyear = sy-datum(2) && lv_nyear.
        lv_month = input+3(2).
        lv_cuday = input(2).
    ENDCASE.
  ELSE.
    lv_curyr = sy-datum(4).
    APPEND lv_curyr TO lt_years.
    WHILE lv_count LE 30.
      IF lv_count LE 20.
        SUBTRACT 1 FROM lv_curyr. APPEND lv_curyr TO lt_years. ADD 1 TO lv_count.
      ELSE.
        IF lv_count EQ 21. lv_curyr = sy-datum(4). ENDIF.
        ADD 1 TO lv_curyr. APPEND lv_curyr TO lt_years. ADD 1 TO lv_count.
      ENDIF.
    ENDWHILE.

    READ TABLE lt_years TRANSPORTING NO FIELDS WITH KEY table_line = input(4).
    IF sy-subrc EQ 0.
      lv_nyear = input(4).
      lv_month = input+4(2).
      lv_cuday = input+6(2).
    ENDIF.

    READ TABLE lt_years TRANSPORTING NO FIELDS WITH KEY table_line = input+4(4).
    IF sy-subrc EQ 0.
      lv_nyear = input+4(4).
      lv_month = input+2(2).
      lv_cuday = input(2).
    ENDIF.
  ENDIF.

  CONCATENATE lv_nyear lv_month lv_cuday INTO output.
endmethod.

method CONVERT_RAW_CSV_TO_TABLE.
  DATA: ls_rawtb TYPE LINE OF truxs_t_text_data,
        lt_rawtb TYPE truxs_t_text_data,
        lt_comps TYPE STANDARD TABLE OF abap_componentdescr WITH KEY name,
        ls_comps TYPE abap_componentdescr,
        lo_struc TYPE REF TO cl_abap_structdescr,
        lo_custm TYPE REF TO data,
        lv_ctext TYPE string.
  FIELD-SYMBOLS: <struc> TYPE any,
                 <compt> TYPE any.

  lt_rawtb = raw_table.
  CREATE DATA lo_custm LIKE LINE OF str_table.
  lo_struc ?= cl_abap_typedescr=>describe_by_data_ref( lo_custm ).
  lt_comps = lo_struc->get_components( ).
  ASSIGN lo_custm->* TO <struc>.

  IF has_header EQ abap_true.
    DELETE lt_rawtb INDEX 1.    "Elimina cabecera
  ENDIF.
  DELETE lt_rawtb WHERE TABLE_LINE(1) EQ ','.    "Elimina renglones vacíos

  LOOP AT lt_rawtb INTO ls_rawtb.
    CLEAR <struc>.
    LOOP AT lt_comps INTO ls_comps.
      ASSIGN COMPONENT ls_comps-name OF STRUCTURE <struc> TO <compt>.
      CHECK sy-subrc EQ 0.
      IF ls_rawtb(1) EQ '"'.
*        SHIFT ls_rawtb BY 1 PLACES.
*        SPLIT ls_rawtb AT '"' INTO <compt> ls_rawtb.
*        SHIFT ls_rawtb BY 1 PLACES.
        WHILE ls_rawtb(1) EQ '"'.
          SHIFT ls_rawtb BY 1 PLACES.
          IF <compt> IS INITIAL.
            SPLIT ls_rawtb AT '"' INTO <compt> ls_rawtb.
          ELSE.
            SPLIT ls_rawtb AT '"' INTO lv_ctext ls_rawtb.
            IF lv_ctext(1) CA ''.
              <compt> = <compt> && `"` && lv_ctext.
            ELSE.
              <compt> = <compt> && ` "` && lv_ctext.
            ENDIF.
          ENDIF.
        ENDWHILE.
        SHIFT ls_rawtb BY 1 PLACES.
      ELSE.
        SPLIT ls_rawtb AT ',' INTO <compt> ls_rawtb.
      ENDIF.
    ENDLOOP.
    APPEND <struc> TO str_table.
  ENDLOOP.

endmethod.

endclass.