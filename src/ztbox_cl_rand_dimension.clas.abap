class ZTBOX_CL_RAND_DIMENSION definition
  public
  final
  create private

  global friends ZTBOX_CL_RAND_TABLE
                 ZTBOX_CL_RAND_VALUE .

public section.
protected section.
private section.

  constants _INT_REGEX type STRING value `\[(\d*(?:.\d*)),(\d*(?:.\d*))]` ##NO_TEXT.
  data _SINGLE type STRING .
  data _MIN type STRING .
  data _MAX type STRING .
  data _IS_RANGE type ABAP_BOOL .
  data _IS_SINGLE type ABAP_BOOL .

  methods CONSTRUCTOR
    importing
      !TEXT type STRING .
  methods MIN
    returning
      value(R) type STRING .
  methods MAX
    returning
      value(R) type STRING .
  methods GET
    returning
      value(R) type STRING .
  methods IS_SINGLE
    returning
      value(R) type ABAP_BOOL .
  methods IS_RANGE
    returning
      value(R) type ABAP_BOOL .
ENDCLASS.



CLASS ZTBOX_CL_RAND_DIMENSION IMPLEMENTATION.


  METHOD constructor.

    DATA(matcher) = cl_abap_matcher=>create(
      pattern = _int_regex
      text    = text ).

    CASE matcher->match( ).

      WHEN abap_true.

        TRY.

            _min      = condense( matcher->get_submatch( 1 ) ).
            _max      = condense( matcher->get_submatch( 2 ) ).
            _is_range = abap_true.

          CATCH cx_sy_matcher.
        ENDTRY.

      WHEN abap_false.

        _single     = condense( text ).
        _is_single  = abap_true.

    ENDCASE.

  ENDMETHOD.


  METHOD get.

    r = _single.

  ENDMETHOD.


  METHOD is_range.

    r = _is_range.

  ENDMETHOD.


  METHOD is_single.

    r = _is_single.

  ENDMETHOD.


  METHOD max.

    r = _max.

  ENDMETHOD.


  METHOD min.

    r = _min.

  ENDMETHOD.
ENDCLASS.
