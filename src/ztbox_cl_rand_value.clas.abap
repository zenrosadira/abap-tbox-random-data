class ZTBOX_CL_RAND_VALUE definition
  public
  final
  create private

  global friends ZTBOX_CL_RAND
                 ZTBOX_CL_RAND_STRUCT
                 ZTBOX_CL_RAND_TABLE .

public section.

  methods FIXED
    importing
      !VALUE type ANY
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE .
  methods WORDS_NUMBER
    importing
      !WORDS_NUMBER type STRING
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE
    raising
      resumable(ZCX_TBOX_RAND) .
  methods WORDS_LEN
    importing
      !WORDS_LEN type STRING
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE
    raising
      resumable(ZCX_TBOX_RAND) .
  methods DECIMALS
    importing
      !DECIMALS type I
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE .
  methods LEN
    importing
      !LEN type STRING
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE
    raising
      resumable(ZCX_TBOX_RAND) .
  methods RANGE
    importing
      !RANGE type STRING
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE
    raising
      resumable(ZCX_TBOX_RAND) .
  methods ASCII
    importing
      !ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE .
  methods WORDS_UPPER
    importing
      !ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE .
  methods GENERATE
    exporting
      !VALUE type ANY
    raising
      resumable(ZCX_TBOX_RAND) .
  methods USE_CHECK_TABLE
    importing
      !ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE .
  PROTECTED SECTION.
private section.

  data _SEED type I .
  data _FIXED_VALUE type STRING .
  data _DOMAIN_VALUES type STRING_TABLE .
  data _RANDOMIZER type ref to CL_ABAP_RANDOM .
  data _CHECK_VALUES type STRING_TABLE .
  data _VALUE_DIMENSION type ref to ZTBOX_CL_RAND_DIMENSION .
  data _WORDS_NUMBER type ref to ZTBOX_CL_RAND_DIMENSION .
  data _USE_CHECK_VALUES type ABAP_BOOL .
  data _ASCII type ABAP_BOOL .
  data _WORDS_UPPER type ABAP_BOOL .
  data _DEC_DIGITS type STRING .
  data _INT_DIGITS type STRING .
  data _ELEM type ref to CL_ABAP_ELEMDESCR .
  data _FIXED_VALUE_SET type ABAP_BOOL .

  methods _CHECK_VALUES_GET
    returning
      value(R) type STRING .
  methods _DOMAIN_GET
    returning
      value(R) type STRING .
  methods CONSTRUCTOR
    importing
      !SEED type I .
  methods _GENERATE_DEC
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_RAND) .
  methods _GENERATE_DIGIT
    importing
      !NO_ZERO type ABAP_BOOL optional
    returning
      value(R) type STRING .
  methods _GENERATE_NUMC
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_RAND) .
  methods _GENERATE_INT
    returning
      value(R) type STRING .
  methods _GENERATE_TIME
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_RAND) .
  methods _GENERATE_DATE
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_RAND) .
  methods _GENERATE_XSTRING
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_RAND) .
  methods _GENERATE_HEX
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_RAND) .
  methods _GENERATE_CHAR
    returning
      value(R) type STRING .
  methods _GENERATE_WORD
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_RAND) .
  methods _GENERATE_STRING
    returning
      value(R) type STRING
    raising
      resumable(ZCX_TBOX_RAND) .
  methods _SET_CHECK_VALUES
    importing
      !VALUES type STRING_TABLE .
  methods _SET_DOMAIN_VALUES
    importing
      !VALUES type STRING_TABLE .
  methods _SET_ELEMENT
    importing
      !ELEM type ref to CL_ABAP_ELEMDESCR .
  methods _GET_STR_DIM
    returning
      value(R) type I .
  methods _GET_LEN
    returning
      value(R) type I .
ENDCLASS.



CLASS ZTBOX_CL_RAND_VALUE IMPLEMENTATION.


  METHOD ascii.

    _ascii = active.

    r = me.

  ENDMETHOD.


  METHOD constructor.

    _seed             = seed.

    _randomizer       = cl_abap_random=>create( seed = _seed ).

    _use_check_values = abap_true.

  ENDMETHOD.


  METHOD decimals.

    _dec_digits = decimals.

    r = me.

  ENDMETHOD.


  METHOD fixed.

    _fixed_value      = value.

    _fixed_value_set  = abap_true.

    r = me.

  ENDMETHOD.


  METHOD generate.

    IF _fixed_value_set EQ abap_true.
      value = _fixed_value.
      RETURN.
    ENDIF.

    IF _domain_values IS NOT INITIAL.
      value = _domain_get( ).
      RETURN.
    ENDIF.

    IF _check_values IS NOT INITIAL AND _use_check_values EQ abap_true.
      value = _check_values_get( ).
      RETURN.
    ENDIF.

    IF _elem IS NOT BOUND.
      DATA(data_desc) = cl_abap_typedescr=>describe_by_data( value ).
      CHECK data_desc->kind EQ cl_abap_typedescr=>kind_elem.
      _elem = CAST cl_abap_elemdescr( data_desc ).
    ENDIF.

    CASE _elem->type_kind.

      WHEN  cl_abap_typedescr=>typekind_int  OR cl_abap_typedescr=>typekind_int1 OR
            cl_abap_typedescr=>typekind_int2 OR cl_abap_typedescr=>typekind_int8.

        value = _generate_int( ).

      WHEN  cl_abap_typedescr=>typekind_packed      OR cl_abap_typedescr=>typekind_decfloat     OR
            cl_abap_typedescr=>typekind_decfloat16  OR cl_abap_typedescr=>typekind_decfloat34   OR
            cl_abap_typedescr=>typekind_float.

        value = _generate_dec( ).

      WHEN cl_abap_typedescr=>typekind_num.

        value = _generate_numc( ).

      WHEN cl_abap_typedescr=>typekind_time.

        value = _generate_time( ).

      WHEN cl_abap_typedescr=>typekind_date.

        value = _generate_date( ).

      WHEN cl_abap_typedescr=>typekind_char.

        value = _generate_word( ).

      WHEN cl_abap_typedescr=>typekind_string.

        value = _generate_string( ).

      WHEN  cl_abap_typedescr=>typekind_hex.

        value = _generate_hex( ).

      WHEN  cl_abap_typedescr=>typekind_xstring.

        value = _generate_xstring( ).

    ENDCASE.

  ENDMETHOD.


  METHOD len.

    range( len ).

    r = me.

  ENDMETHOD.


  METHOD range.

    _value_dimension = NEW #( range ).

    r = me.

  ENDMETHOD.


  METHOD use_check_table.

    _use_check_values = active.

    r = me.

  ENDMETHOD.


  METHOD words_len.

    _value_dimension = NEW #( words_len ).

    r = me.

  ENDMETHOD.


  METHOD words_number.

    _words_number = NEW #( words_number ).

    r = me.

  ENDMETHOD.


  METHOD words_upper.

    _words_upper = active.

    r = me.

  ENDMETHOD.


  METHOD _CHECK_VALUES_GET.

    DATA(ix) = _randomizer->intinrange( low = 1 high = lines( _check_values ) ).

    r = _check_values[ ix ].

  ENDMETHOD.


  METHOD _DOMAIN_GET.

    DATA(ix) = _randomizer->intinrange( low = 1 high = lines( _domain_values ) ).

    r = _domain_values[ ix ].

  ENDMETHOD.


  METHOD _generate_char.

    DATA sign TYPE x.
    DATA(ix) = _randomizer->intinrange( high = 25 ).

    IF _ascii EQ abap_false.

      r = to_lower( sy-abcde+ix(1) ).

    ELSE.

      sign = _randomizer->intinrange( low = 33 high = 255 ).
      cl_abap_conv_in_ce=>create( encoding = `1160` ignore_cerr = abap_true input = sign )->read( IMPORTING data = r ).

      CONDENSE r NO-GAPS.

    ENDIF.

  ENDMETHOD.


  METHOD _generate_date.

    r = COND d(
      WHEN _value_dimension IS BOUND AND _value_dimension->is_range( )
        THEN CONV d( _value_dimension->min( ) ) + _randomizer->intinrange( high = CONV i( CONV d( _value_dimension->max( ) ) ) - CONV i( CONV d( _value_dimension->min( ) ) ) )
      ELSE `00000000` + _randomizer->intinrange( high = 3652060 ) ).

  ENDMETHOD.


  METHOD _generate_dec.

    IF _value_dimension IS NOT BOUND.
      range( |[1, { 10 ** ( _elem->length * 2 - _elem->decimals - 1 ) - 1 }]| ).
    ENDIF.

    _dec_digits = COND #( WHEN _dec_digits IS INITIAL THEN _elem->decimals ELSE _dec_digits ).

    DATA(p) = COND #(
      WHEN _value_dimension IS BOUND AND _value_dimension->is_range( )
        THEN _randomizer->packedinrange( min = CONV cl_abap_random=>p31_0( _value_dimension->min( ) ) max = CONV cl_abap_random=>p31_0( _value_dimension->max( ) ) )
      ELSE _randomizer->packed( ) ).

    IF _dec_digits GE 1.
      DATA(dec) = _randomizer->packedinrange( min = 0 max = 10 ** _dec_digits - 1 ).
    ENDIF.

    r = COND #(
      WHEN dec IS NOT INITIAL THEN p && `.` && dec
      ELSE p ).

  ENDMETHOD.


  METHOD _generate_digit.

    r = SWITCH #( no_zero
      WHEN  abap_true THEN _randomizer->intinrange( low = 1 high = 9 )
      ELSE  _randomizer->intinrange( low = 0 high = 9 ) ).

  ENDMETHOD.


  METHOD _generate_hex.

    IF _value_dimension IS NOT BOUND.
      range( |{ _elem->length }| ).
    ENDIF.

    DATA(len) = _get_len( ).

    DATA hex TYPE x.

    DO len TIMES.

      hex = _randomizer->intinrange( low = 0 high = 255 ).
      r   = r && hex.

    ENDDO.

  ENDMETHOD.


  METHOD _generate_int.

    r = COND i(
      WHEN _value_dimension IS BOUND AND _value_dimension->is_range( )
        THEN _randomizer->intinrange( low = CONV i( _value_dimension->min( ) ) high = CONV i( _value_dimension->max( ) ) )
      ELSE _randomizer->int( ) ).

  ENDMETHOD.


  METHOD _generate_numc.

    IF _value_dimension IS NOT BOUND.
      range( |[1, { _elem->length / 2 }]| ).
    ENDIF.

    DATA(len) = _get_len( ).

    DO len TIMES.

      r = r && _generate_digit( no_zero = COND #( WHEN sy-index EQ len THEN abap_true ELSE abap_false ) ).

    ENDDO.

    CONDENSE r NO-GAPS.

  ENDMETHOD.


  METHOD _generate_string.

    IF _value_dimension IS NOT BOUND.
      words_len( `[1, 12]` ).
    ENDIF.

    IF _words_number IS NOT BOUND.
      words_number( `[1, 12]` ).
    ENDIF.

    DATA(words) = _get_str_dim( ).

    DO words TIMES.

      r = r && _generate_word( ).

      CHECK sy-index NE words.

      r = r && ` `.

    ENDDO.

  ENDMETHOD.


  METHOD _generate_time.

    r = COND t(
      WHEN _value_dimension IS BOUND AND _value_dimension->is_range( )
        THEN CONV t( _value_dimension->min( ) ) + _randomizer->intinrange( high = CONV i( CONV t( _value_dimension->max( ) ) ) - CONV i( CONV t( _value_dimension->min( ) ) ) )
      ELSE `000000` + _randomizer->intinrange( high = 86400 ) ).

  ENDMETHOD.


  METHOD _generate_word.

    IF _value_dimension IS NOT BOUND.
      range( |[1, { _elem->length / 2 }]| ).
    ENDIF.

    DATA(len) = _get_len( ).

    DO len TIMES.

      r = r && _generate_char( ).

    ENDDO.

    IF _words_upper EQ abap_true.
      r = to_upper( r ).
    ENDIF.

  ENDMETHOD.


  METHOD _generate_xstring.

    IF _value_dimension IS NOT BOUND.
      range( |[2, 255]| ).
    ENDIF.

    r = _generate_hex( ).

  ENDMETHOD.


  METHOD _get_len.

    r = COND #(
      WHEN _value_dimension->is_single( )
        THEN _value_dimension->get( )
      WHEN _value_dimension->is_range( )
        THEN _randomizer->intinrange( low = CONV i( _value_dimension->min( ) ) high = CONV i( _value_dimension->max( ) ) )
      ELSE 0 ).

  ENDMETHOD.


  METHOD _get_str_dim.

    r = COND #(
      WHEN _words_number->is_single( )
        THEN _words_number->get( )
      WHEN _words_number->is_range( )
        THEN _randomizer->intinrange( low = CONV i( _words_number->min( ) ) high = CONV i( _words_number->max( ) ) )
      ELSE 0 ).

  ENDMETHOD.


  METHOD _set_check_values.

    _check_values = values.

  ENDMETHOD.


  METHOD _set_domain_values.

    _domain_values = values.

  ENDMETHOD.


  METHOD _set_element.

    _elem = elem.

  ENDMETHOD.
ENDCLASS.
