class ZTBOX_CL_RAND_TABLE definition
  public
  final
  create private

  global friends ZTBOX_CL_RAND .

public section.

  methods GENERATE
    exporting
      !TABLE type ANY TABLE .
  methods FIELD
    importing
      !FIELD_NAME type NAME_FELD
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE .
  methods ROWS
    importing
      !ROWS type STRING .
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

  methods CONSTRUCTOR
    importing
      !SEED type I .
ENDCLASS.



CLASS ZTBOX_CL_RAND_TABLE IMPLEMENTATION.


  METHOD constructor.

    _seed               = seed.

    _randomizer         = cl_abap_random=>create( seed = _seed ).

    _struct_randomizer  = ztbox_cl_rand=>struct( ).

  ENDMETHOD.


  METHOD field.

    r = _struct_randomizer->field( field_name ).

  ENDMETHOD.


  METHOD generate.

    DATA(tab_desc)    = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( table ) ).
    DATA(row_struct)  = CAST cl_abap_structdescr( tab_desc->get_table_line_type( ) ).

    _struct_randomizer->_set_catalog( row_struct ).

    IF _rows_dimension IS NOT BOUND.
      rows( `100` ).
    ENDIF.

    DATA(lines) = COND #(
      WHEN _rows_dimension->is_range( )
        THEN _randomizer->intinrange( low = CONV i( _rows_dimension->min( ) ) high = CONV i( _rows_dimension->max( ) ) )
      ELSE _rows_dimension->get( ) ).

    DATA row_ref TYPE REF TO data.
    CREATE DATA row_ref LIKE LINE OF table.

    DO lines TIMES.

      ASSIGN row_ref->* TO FIELD-SYMBOL(<row>).

      _struct_randomizer->generate( IMPORTING struct = <row> ).

      INSERT <row> INTO TABLE table.

    ENDDO.

  ENDMETHOD.


  METHOD rows.

    _rows_dimension = NEW #( rows ).

  ENDMETHOD.
ENDCLASS.
