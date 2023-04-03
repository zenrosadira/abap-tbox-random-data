# ABAP Random Data Generator

<img src="https://i.ibb.co/r3DVCdF/random-data-ex-png.jpg" alt="random-data-ex-png" width="800" height="400">

A utility class that simplifies the generation of random data and allows for quick population of variables (tables, structures, scalar data). 
The data generated is devoid of meaning but respects the constraints of the domain: if fixed values are expected, one of these values will be randomly assigned; if a check table is assigned to the domain, values will be extracted from the check table. Additionally, it is possible to configure ranges and value dimensions. 

 It can be useful for preparing performance stress tests or managing test automation.
 
 ## Quick Start
 
 ```abap
 * To generate a random table
 DATA t_sflight TYPE TABLE OF sflight.
 DATA(tab_rand) = ztbox_cl_rand=>table( ).
 tab_rand->generate( IMPORTING table = t_sflight ).
 
 * To generate a random structure
 DATA s_sflight TYPE sflight.
 DATA(str_rand) = ztbox_cl_rand=>struct( ).
 str_rand->generate( IMPORTING struct = s_sflight ).
 
 * To generate a random value
 DATA amount TYPE wrbtr.
 DATA(val_rand) = ztbox_cl_rand=>value( ).
 val_rand->generate( IMPORTING value = amount ).
 ```
 
 ## Configuration

 You can decide the number of rows in the generated table
 
 ```abap
 DATA t_sbook TYPE TABLE OF sbook.
 DATA(tab_rand) = ztbox_cl_rand=>table( ).

 tab_rand->rows( `10000` ). " Default is 100
 tab_rand->generate( IMPORTING table = t_sbook ). 
 " Now t_sbook has exactly 10000 rows 
 ```
 The number of rows can also be a range in the form `[min, max]`
 
 ```abap
 tab_rand->rows( `[10, 2000]` ).
 tab_rand->generate( IMPORTING table = t_sbook ). 
 " Now t_sbook has a number of rows randomly choosen between 10 and 2000
 ```
 You can manage fields configuration using `->field( )` method.
 
 ```abap
 tab_rand->field( `MANDT` )->fixed( sy-mandt ). " To always assign the same value
 tab_rand->field( `FORCURAM` )->range( `[1, 1000]` ). " To assign value randomly chosen from an interval
 tab_rand->field( `FLDATE` )->range( `[19990101, 20251231]` ). " As above, also applies to dates and times
 tab_rand->field( `FORCURAM` )->decimals( 2 ). " To set decimals precision for packed/float fields
 tab_rand->field( `PASSNAME` )->len( `5` ). " To assign char-value with fixed length
 tab_rand->field( `PASSNAME` )->len( `[3, 14]` ). " To assign char-value with a randomly chosen length
 tab_rand->field( `PASSNAME` )->ascii( ). " To genere words using ASCII characters
 tab_rand->field( `PASSNAME` )->words_upper( ). " To generate words in upper case
 tab_rand->field( `WUNIT` )->use_check_table( abap_false ). " To de-activate the use of domain check-table
 
 tab_rand->generate( IMPORTING table = t_sbook ).
 ```
 The same `->field( )` configurations can be applied to structure generator too.
 
 ```abap
 DATA s_uni TYPE bapimtcs_unicode.
 DATA(str_rand) = ztbox_cl_rand=>struct( ).
 * For string field generation you can set the number of words to generate and the length of each words 
 * (for both, fixed or randomly chosen from a range),
 str_rand->field( `DATA` )->words_number( `[1, 15]` )->words_len( `[5, 10]` ). 
 
 str_rand->generate( IMPORTING struct = s_uni ).
 ```

And you can configure the single value too.

```abap
DATA order_num TYPE n LENGTH 10.
DATA(val_rand) = ztbox_cl_rand=>value( ).
val_rand->len( 6 ).
val_rand->generate( IMPORTING value = order_num ). " It could be any value of the form `0000XXXXXX`.
```

## Installation
Install this project using [abapGit](https://abapgit.org/) ![abapGit](https://docs.abapgit.org/img/favicon.png)
