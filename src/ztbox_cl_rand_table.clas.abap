class ZTBOX_CL_RAND_TABLE definition
  public
  final
  create private

  global friends ZTBOX_CL_RAND .

public section.

  methods GENERATE
    exporting
      !TABLE type ANY TABLE
    raising
      resumable(ZCX_TBOX_RAND) .
  methods FIELD
    importing
      !FIELD_NAME type NAME_FELD optional
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE .
  methods ROWS
    importing
      !ROWS type STRING
    raising
      resumable(ZCX_TBOX_RAND) .
protected section.
private section.

  types:
    BEGIN OF ty_catalog,
      field_name TYPE name_feld,
      elem_descr TYPE REF TO cl_abap_elemdescr,
      randomizer TYPE REF TO ztbox_cl_rand_value,
    END OF ty_catalog .
  types:
    ty_catalog_t TYPE TABLE OF ty_catalog WITH DEFAULT KEY .

  data _SEED type I .
  data _ROWS_DIMENSION type ref to ZTBOX_CL_RAND_DIMENSION .
  data _RANDOMIZER type ref to CL_ABAP_RANDOM .
  data _STRUCT_RANDOMIZER type ref to ZTBOX_CL_RAND_STRUCT .
  data _VALUE_RANDOMIZER type ref to ZTBOX_CL_RAND_VALUE .

  methods CONSTRUCTOR
    importing
      !SEED type I .
  methods _GENERATE_VALUE
    importing
      !ELEM type ref to CL_ABAP_ELEMDESCR
    exporting
      !TABLE type ANY TABLE
    raising
      resumable(ZCX_TBOX_RAND) .
  methods _GENERATE_STRUCT
    importing
      !STRUCT type ref to CL_ABAP_STRUCTDESCR
    exporting
      !TABLE type ANY TABLE
    raising
      resumable(ZCX_TBOX_RAND) .
  methods _GET_LINES
    returning
      value(R) type I .
ENDCLASS.



CLASS ZTBOX_CL_RAND_TABLE IMPLEMENTATION.


  METHOD constructor.

    _seed               = seed.

    _randomizer         = cl_abap_random=>create( seed = _seed ).

    _struct_randomizer  = ztbox_cl_rand=>struct( ).
    _value_randomizer   = ztbox_cl_rand=>value( ).

  ENDMETHOD.


  METHOD field.

    r = COND #(
      WHEN field_name IS SUPPLIED
        THEN _struct_randomizer->field( field_name )
      ELSE _value_randomizer ).

  ENDMETHOD.


  METHOD generate.

    IF _rows_dimension IS NOT BOUND.
      rows( `100` ).
    ENDIF.

    DATA(tab_desc)    = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( table ) ).
    DATA(table_line)  = tab_desc->get_table_line_type( ).

    CASE table_line->kind.

      WHEN cl_abap_typedescr=>kind_struct.

        _generate_struct(
          EXPORTING struct  = CAST cl_abap_structdescr( table_line )
          IMPORTING table   = table ).

      WHEN cl_abap_typedescr=>kind_elem.

        _generate_value(
          EXPORTING elem  = CAST cl_abap_elemdescr( table_line )
          IMPORTING table = table ).

      WHEN OTHERS.

        RAISE EXCEPTION TYPE zcx_tbox_rand EXPORTING textid = zcx_tbox_rand=>table_type_not_supported.

    ENDCASE.

  ENDMETHOD.


  METHOD rows.

    _rows_dimension = NEW #( rows ).

  ENDMETHOD.


  METHOD _generate_struct.

    DATA row_ref        TYPE REF TO data.
    CREATE DATA row_ref LIKE LINE OF table.

    DATA(lines) = _get_lines( ).

    _struct_randomizer->_set_catalog( struct ).

    DO lines TIMES.

      ASSIGN row_ref->* TO FIELD-SYMBOL(<row>).

      _struct_randomizer->generate( IMPORTING struct = <row> ).

      INSERT <row> INTO TABLE table.

    ENDDO.

  ENDMETHOD.


  METHOD _generate_value.

    DATA row_ref        TYPE REF TO data.
    CREATE DATA row_ref LIKE LINE OF table.

    DATA(lines) = _get_lines( ).

    DO lines TIMES.

      ASSIGN row_ref->* TO FIELD-SYMBOL(<val>).

      _value_randomizer->generate( IMPORTING value = <val> ).

      INSERT <val> INTO TABLE table.

    ENDDO.

  ENDMETHOD.


  METHOD _get_lines.

    r = SWITCH #( _rows_dimension->is_range( )
      WHEN abap_true
        THEN _randomizer->intinrange( low = CONV i( _rows_dimension->min( ) ) high = CONV i( _rows_dimension->max( ) ) )
      ELSE _rows_dimension->get( ) ).

  ENDMETHOD.
ENDCLASS.
