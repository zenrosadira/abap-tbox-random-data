class ZCX_TBOX_RAND definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  constants TABLE_TYPE_NOT_SUPPORTED type SOTR_CONC value '00155DC71EB51EEDB4D97BD5ECCDC75C' ##NO_TEXT.
  constants VALUE_NOT_SUPPORTED type SOTR_CONC value '00155DC71EB51EEDB4D99DE8EC644771' ##NO_TEXT.
  constants STRUCTURE_TYPE_NOT_SUPPORTED type SOTR_CONC value '00155DC71EB51EEDB4DA71DE9F1087F6' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_TBOX_RAND IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
  endmethod.
ENDCLASS.
