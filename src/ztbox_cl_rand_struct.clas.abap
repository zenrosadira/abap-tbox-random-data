class ZTBOX_CL_RAND_STRUCT definition
  public
  final
  create private

  global friends ZTBOX_CL_RAND
                 ZTBOX_CL_RAND_TABLE .

public section.

  methods GENERATE
    exporting
      !STRUCT type ANY
    raising
      resumable(ZCX_TBOX_RAND) .
  methods FIELD
    importing
      !FIELD_NAME type NAME_FELD
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

  data _SEED type I .
  data _CATALOG type TY_CATALOG_T .
  data _RANDOMIZER type ref to CL_ABAP_RANDOM .
  data _CATALOG_SET type ABAP_BOOL .

  methods CONSTRUCTOR
    importing
      !SEED type I .
  methods _SET_CATALOG
    importing
      !STRUCT type ref to CL_ABAP_STRUCTDESCR .
ENDCLASS.



CLASS ZTBOX_CL_RAND_STRUCT IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    _seed       = seed.

    _randomizer = cl_abap_random=>create( seed = _seed ).

  ENDMETHOD.


  METHOD field.

    DATA(cat) = VALUE #( _catalog[ field_name = field_name ] OPTIONAL ).

    IF cat IS INITIAL.

      r = ztbox_cl_rand=>value( ).

      APPEND VALUE #(
        field_name  = field_name
        randomizer  = r ) TO _catalog.

    ELSE.

      r = cat-randomizer.

    ENDIF.

  ENDMETHOD.


  METHOD generate.

    IF _catalog_set EQ abap_false.

      DATA(data_desc) = cl_abap_typedescr=>describe_by_data( struct ).
      IF data_desc->kind NE cl_abap_typedescr=>kind_struct.
        RAISE EXCEPTION TYPE zcx_tbox_rand EXPORTING textid = zcx_tbox_rand=>structure_type_not_supported.
      ENDIF.

      _set_catalog( CAST cl_abap_structdescr( data_desc ) ).

    ENDIF.

    LOOP AT _catalog INTO DATA(cat).

      ASSIGN COMPONENT cat-field_name OF STRUCTURE struct TO FIELD-SYMBOL(<val>).
      CHECK sy-subrc EQ 0.

      cat-randomizer->generate( IMPORTING value = <val> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_catalog.

    LOOP AT struct->components INTO DATA(comp).

      DATA(comp_type)     = struct->get_component_type( comp-name ).
      CHECK comp_type->kind EQ cl_abap_typedescr=>kind_elem.

      DATA(elem)          = CAST cl_abap_elemdescr( comp_type ).
      DATA(check_values)  = ztbox_cl_rand=>_check_values( elem ).
      DATA(domain_values) = ztbox_cl_rand=>_domain_values( elem ).

      DATA(cat) = VALUE #( _catalog[ field_name = comp-name ] OPTIONAL ).

      IF cat IS INITIAL.

        DATA(randomizer) = ztbox_cl_rand=>value( ).
        randomizer->_set_element( elem ).
        randomizer->_set_check_values( check_values ).
        randomizer->_set_domain_values( domain_values ).

        APPEND VALUE #(
          field_name  = comp-name
          elem_descr  = elem
          randomizer  = randomizer ) TO _catalog.

      ELSE.

        cat-elem_descr  = elem.
        cat-randomizer->_set_element( elem ).
        cat-randomizer->_set_check_values( check_values ).
        cat-randomizer->_set_domain_values( domain_values ).

      ENDIF.

    ENDLOOP.

    _catalog_set = abap_true.

  ENDMETHOD.
ENDCLASS.
