### Understanding the Structure of Tube Assemblies
1. Each Tube Assembly (TA-xxxxx) is made of multiple parts. There are 21198 Tube Assemblies.

2. The main part of TA-xxxxx is the main tube. Properties of the main tube are:
  1. material spec (SP-xxxx)
  2. diameter
  3. wall thickness
  4. length
  5. \# bends
  6. bend radius
  7. end A
    * length < 1\*diameter
    * length < 2*diameter
    * End Form ID (EF-xxx)
    * physically formed {1,0}
  8. end X
      * length < 1\*diameter
      * length < 2*diameter
      * End Form ID (EF-xxx)
      * physically formed {1,0}
  9. \# permanent bosses
  10. \# permanent brackets
  11. \# permanent other features (clips, etc.)

3. A Tube Assembly TA-xxxxx contains the main tube and several components. Any of 2048 Components (C-xxxx) can be attached to the main tube.

4. Each component is a bag of properties:
  * name: 297 possible

  * type: 30 possible

  * category: 11 possible

      * adaptor
      * boss
      * elbow
      * float
      * hfl
      * nut
      * other
      * sleeve
      * straight
      * tee
      * threaded

  * other properties:

      * weight
      * length
      * orientation
      * angles
      * shapes
      * ends

5. Each Tube Assembly TA-xxxxx has a set of unique specifications (SP-xxxx).

### Data file types

| File Name    | Contains     | rows | columns| rownames |
| :------------- | :------------- |
| TA.tube | main tube properties | 21198 | 18|TA-xxxxx|
|TA.specs|unique specifications of each TA|21198|11|TA-xxxxx|
|TA.components|names & quantities of components in each TA|21198|17|TA-xxxxx|
|comp|name, type and category of components|2048|4|C-xxxx|
|comp.threaded|properties of components of category 'threaded'|194|32|C-xxxx|
|comp.tee|properties of components of category 'tee'|4|14|C-xxxx|
|comp.straight|properties of components of category 'straight'|361|12|C-xxxx|
|comp.sleeve|properties of components of category 'sleeve'|50|10|C-xxxx|
|comp.other|properties of components of category 'other'|1001|4|C-xxxx|
|comp.nut|properties of components of category 'nut'|65|11|C-xxxx|
|comp.hfl|properties of components of category 'hfl'|6|9|C-xxxx|
|comp.float|properties of components of category 'float'|16|7|C-xxxx|
|comp.elbow|properties of components of category 'elbow'|178|16|C-xxxx|
|comp.boss|properties of components of category 'boss'|147|15|C-xxxx|
|comp.adaptor|properties of components of category 'adaptor'|25|20|C-xxxx|
|train.csv|price quotation train data|30213|8|{TA-xxxxx, date, annual_usage, min_order_quantity, quantity}|
|test.csv|price quotation test data|30235|8|{TA-xxxxx, date, annual_usage, min_order_quantity, quantity}|
|

### Comments on Train and Test Data
* If *bracket_pricing*='Yes' then *min_order_quantity* has no relevance. If we set it to NA then duplicate train/test vectors are born but with different costs!

* If *bracket_pricing*='No' then *quantity* has no relevance.

### Result of load.R

* TA.tube [dataframe]

|Column Name|Type| Values |\# unique values|min|max|Meaning|
| :------------- | :-------------
| tube_assembly_id |string|TA-xxxxx|21198||||
|material_id|string|{SP-xxxx, NA}|19, NA||||
|diameter|numeric|REAL|REAL|3.18|203.2|outer diameter of tube|
|wall|numeric|REAL|REAL|0.71|7.9|thickness of wall|
|length|numeric|REAL|REAL|1|1333||
|num_bends|integer|0-17|18|0|17||
|bend_radius|numeric|REAL, NA, 0 (no bend)|REAL|3.75|565||
|end_a_1x|factor|{0,1}|2|||Is end length less than 1 times the tube diameter? |
|end_a_2x|factor|{0,1}|2|||Is end length less than 2 times the tube diameter? |
|end_x_1x|factor|{0,1}|2|||Is end length less than 1 times the tube diameter? |
|end_x_2x|factor|{0,1}|2|||Is end length less than 2 times the tube diameter? |
|end_a|factor|{EF-xxx,9999}|25||||
|end_x|factor|{EF-xxx,9999}|26||||
|num_boss|integer|0-5|6|0|5||
|num_bracket|integer|0-5|6|0|5||
|other|integer|0-8|9|0|8||
|end_a_forming|factor|{0,1}|2||||
|end_x_forming|factor|{0,1}|2||||
|


*  TA.specs [list]

| item names     | contains     | max vector length|possible values of SP-xxxx|
| :------------- | :------------- |
|   TA-xxxxx   | vector of SP-xxxx  |10|85|

* TA.components [list]

| item names     | contains     | max vector length|possible values of C-xxxx|
| :------------- | :------------- |
|   TA-xxxxx   | dataframe of components (C-xxxx) and quantities  |8|2047 + '9999'|
|

-- TA-21143, TA-21144 have only 1 component '9999'

* comp [dataframe]

|Column Name|Type| Values |\# unique values|Meaning|
| :------------- | :-------------|
|component_id|string|{C-xxx,9999}|2048|ID of component|
|name|string|'char'|297 + 1 NA|Name of component|
|component_type_id|string|{CP-xxx,OTHER}|29|Type of component|
|category|string|'char'|11|categ of component|
|
-- C-0238 has no 'name'

-- Only TA-01309 has one


 C-0238

* comp.adaptor [dataframe]

|Column Name|Type| Values |\# unique values|min|max|Meaning|
| :------------- | :------------- |
|component_id|string|C-xxxx|25||||
|component_type_id|string|CP-xxx|2||||
|adaptor_angle|numeric|{NA,90}|2||||
|overall_length|numeric|REAL, NA||13.2|58.4||
|end_form_id_1|string|A-xxx|4||||
|connection_type_id_1|string|B-xxx|5||||
|

### TIPS to improve Prediction Accuracy
1. Predict cost on training data itself. Then check which suppliers are always quoting above/below the predicted price. And adjust the test predictions accordingly.

### Prediction Results
*basic features* = TA.specs one hot, TA.specs count, TA.tube, quote features

xgboost GBDT on *basic features* +

1. No component features = 0.278
2. No component features - nzv [ 123 ] = 0.277 ~ 0.275
3. c-xxxx-sign-one-hot features [ 2969 ] = 0.269
4. c-xxxx-sign-one-hot features - nzv [ 196 ] = 0.266
5. c-xxxx-quantity-one-hot features [ 2969 ] = 0.261
6. c-xxxx-quantity-one-hot features - nzv [ 193 ] = 0.259
7. name-c-xxxx-sign-one-hot features [ 1218 ] = 0.250
8. name-c-xxxx-sign-one-hot features - nzv [ 161 ] = 0.252
9. name-c-xxxx-quantity-one-hot features [ 1218 ] = 0.252
10. name-c-xxxx-quantity-one-hot features - nzv [ 159 ] = 0.2500
11. type-c-xxxx-sign-one-hot features [ 950 ] = 0.248
12. type-c-xxxx-sign-one-hot features - nzv [ 138 ] = 0.250
13. type-c-xxxx-quantity-one-hot features [ 950 ] = 0.251
14. type-c-xxxx-quantity-one-hot features - nzv [ 138 ] = 0.251
15. category-c-xxxx-sign-one-hot features [ 932 ] = 0.252
16. category-c-xxxx-sign-one-hot features - nzv [ 131 ] = 0.252
17. category-c-xxxx-quantity-one-hot features [ 932 ] = 0.252
18. category-c-xxxx-quantity-one-hot features - nzv [ 131 ] = 0.252
