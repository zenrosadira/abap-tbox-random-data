class ZTBOX_CL_RAND definition
  public
  final
  create public

  global friends ZTBOX_CL_RAND_STRUCT
                 ZTBOX_CL_RAND_TABLE
                 ZTBOX_CL_RAND_VALUE .

public section.

  class-methods STRUCT
    returning
      value(R) type ref to ZTBOX_CL_RAND_STRUCT .
  class-methods TABLE
    returning
      value(R) type ref to ZTBOX_CL_RAND_TABLE .
  class-methods VALUE
    returning
      value(R) type ref to ZTBOX_CL_RAND_VALUE .
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

  class-methods _DOMAIN_VALUES
    importing
      !ELEM type ref to CL_ABAP_ELEMDESCR
    returning
      value(R) type STRING_TABLE .
  class-methods _CHECK_VALUES
    importing
      !ELEM type ref to CL_ABAP_ELEMDESCR
    returning
      value(R) type STRING_TABLE .
ENDCLASS.



CLASS ZTBOX_CL_RAND IMPLEMENTATION.


  METHOD STRUCT.

    r = NEW #( cl_abap_random=>seed( ) ).

  ENDMETHOD.


  METHOD TABLE.

    r = NEW #( cl_abap_random=>seed( ) ).

  ENDMETHOD.


  METHOD value.

    r = NEW #( cl_abap_random=>seed( ) ).

  ENDMETHOD.


  METHOD _check_values.

    elem->get_ddic_field(
      RECEIVING
        p_flddescr    = DATA(ddic)
      EXCEPTIONS
        no_ddic_type  = 1
        not_found     = 2 ).

    CHECK sy-subrc EQ 0 AND ddic-domname IS NOT INITIAL.

    SELECT SINGLE entitytab FROM dd01l
      WHERE domname   EQ @ddic-domname
        AND as4local  EQ `A`
        AND as4vers   EQ `0000`
        AND entitytab NE @space INTO @DATA(table).

    CHECK sy-subrc EQ 0 AND table IS NOT INITIAL.

    DATA(ddic_tab)  = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( table ) ).
    DATA(fields)    = ddic_tab->get_ddic_field_list( ).
    DATA(field)     = VALUE #( fields[ domname = ddic-domname keyflag = abap_true ]-fieldname OPTIONAL ).

    CHECK field IS NOT INITIAL.

    DATA tab_ref TYPE REF TO data.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.
    CREATE DATA tab_ref TYPE TABLE OF (table).
    ASSIGN tab_ref->* TO <tab>.

    SELECT DISTINCT (field) FROM (table) INTO CORRESPONDING FIELDS OF TABLE @<tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).

      ASSIGN COMPONENT field OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      APPEND CONV #( <val> ) TO r.

    ENDLOOP.

  ENDMETHOD.


  METHOD _DOMAIN_VALUES.

    elem->get_ddic_fixed_values(
      RECEIVING
        p_fixed_values    = DATA(fixed_values)
      EXCEPTIONS
        no_ddic_type  = 1
        not_found     = 2 ).

    CHECK sy-subrc EQ 0.

    r = VALUE #( FOR _val IN fixed_values WHERE ( option EQ if_fsbp_const_range=>option_equal ) ( CONV #( _val-low ) ) ).

    LOOP AT fixed_values INTO DATA(fix_val) WHERE option EQ if_fsbp_const_range=>option_between AND low CA '0123456789'.

      DATA(ix) = CONV i( fix_val-low ).

      WHILE ix LE fix_val-high.

        APPEND CONV #( ix ) TO r.
        ADD 1 TO ix.

      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
