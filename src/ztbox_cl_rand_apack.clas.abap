class ZTBOX_CL_RAND_APACK definition
  public
  create private .

public section.

  interfaces ZIF_APACK_MANIFEST .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZTBOX_CL_RAND_APACK IMPLEMENTATION.


  METHOD constructor.

    zif_apack_manifest~descriptor = VALUE #(
      group_id    = 'ztbox'
      artifact_id = 'abap-tbox-randon-data'
      version     = '0.1'
      git_url     = 'https://github.com/zenrosadira/abap-tbox-random-data.git' ).

  ENDMETHOD.
ENDCLASS.
